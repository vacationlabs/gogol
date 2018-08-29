{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
-- Module      : Network.Google.Internal.Auth
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Internal types and helpers for constructing OAuth credentials.
module Network.Google.Internal.Auth where

import           Control.Exception.Lens         (exception)
import           Control.Lens                   (Prism', prism, (<&>))
import           Control.Monad.Catch
import           Control.Monad.IO.Class         (MonadIO (..))
import           Crypto.PubKey.RSA.Types        (PrivateKey)
import           Data.Aeson
import           Data.Aeson.Types               (Pair)
import           Data.ByteArray                 (ByteArray)
import           Data.ByteArray.Encoding
import           Data.ByteString                (ByteString)
import           Data.ByteString.Builder        ()
import qualified Data.ByteString.Lazy           as LBS
import           Data.String                    (IsString)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import           Data.Time
import           Data.X509                      (PrivKey (..))
import           Data.X509.Memory               (readKeyFileFromMemory)
import           GHC.TypeLits                   (Symbol)
import           Network.Google.Internal.Logger
import           Network.Google.Prelude
import           Network.HTTP.Conduit           (HttpException, Manager)
import qualified Network.HTTP.Conduit           as Client
import           Network.HTTP.Types             (Status, hContentType)

-- | The supported credential mechanisms.
data Credentials (s :: [Symbol])
    = FromMetadata !ServiceId
      -- ^ Obtain and refresh access tokens from the underlying GCE host metadata
      -- at @http:\/\/169.254.169.254@.

    | FromClient !OAuthClient !(OAuthCode s)
      -- ^ Obtain and refresh access tokens using the specified client secret
      -- and authorization code obtained from.
      --
      -- See the <https://developers.google.com/accounts/docs/OAuth2InstalledApp OAuth2 Installed Application>
      -- documentation for more information.

    | FromAccount !ServiceAccount
      -- ^ Use the specified @service_account@ and scopes to sign and request
      -- an access token. The 'ServiceAccount' will also be used for subsequent
      -- token refreshes.
      --
      -- A 'ServiceAccount' is typically generated through the
      -- Google Developer Console.

    | FromUser !AuthorizedUser
      -- ^ Use the specified @authorized_user@ to obtain and refresh access tokens.
      --
      -- An 'AuthorizedUser' is typically created by the @gcloud init@ command
      -- of the Google CloudSDK Tools.

    | FromServerSideWebApp !OAuthClient !(OAuthCode s) !Text

{-| Service Account credentials which are typically generated/download
from the Google Developer console of the following form:

> {
>   \"type\": \"service_account\",
>   \"private_key_id\": \"303ad77e5efdf2ce952DFa\",
>   \"private_key\": \"-----BEGIN PRIVATE KEY-----\n...\n\",
>   \"client_email\": \"email@serviceaccount.com\",
>   \"client_id\": \"035-2-310.useraccount.com\"
> }

The private key is used to sign a JSON Web Token (JWT) of the
grant_type @urn:ietf:params:oauth:grant-type:jwt-bearer@, which is sent to
'accountsURL' to obtain a valid 'OAuthToken'. This process requires explicitly
specifying which 'Scope's the resulting 'OAuthToken' is authorized to access.

/See:/ <https://developers.google.com/identity/protocols/OAuth2ServiceAccount#delegatingauthority Delegating authority to your service account>.
-}
data ServiceAccount = ServiceAccount
    { _serviceId          :: !ClientId
    , _serviceEmail       :: !Text
    , _serviceKeyId       :: !Text
    , _servicePrivateKey  :: !PrivateKey
    , _serviceAccountUser :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON ServiceAccount where
    parseJSON = withObject "service_account" $ \o -> do
        bs <- Text.encodeUtf8 <$> o .: "private_key"
        k  <- case listToMaybe (readKeyFileFromMemory bs) of
            Just (PrivKeyRSA k) -> pure k
            _                   ->
                fail "Unable to parse key contents from \"private_key\""
        ServiceAccount
            <$> o .: "client_id"
            <*> o .: "client_email"
            <*> o .: "private_key_id"
            <*> pure k
            <*> pure Nothing

{-| Authorized User credentials which are typically generated by the Cloud SDK
Tools such as @gcloud init@, of the following form:

> {
>   \"type\": \"authorized_user\",
>   \"client_id\": \"32555940559.apps.googleusercontent.com\",
>   \"client_secret\": \"Zms2qjJy2998hD4CTg2ejr2\",
>   \"refresh_token\": \"1/B3gM1x35v.VtqffS1n5w-rSJ\"
> }

The secret and refresh token are used to obtain a valid 'OAuthToken' from
'accountsURL' using grant_type @refresh_token@.
-}
data AuthorizedUser = AuthorizedUser
    { _userId      :: !ClientId
    , _userRefresh :: !RefreshToken
    , _userSecret  :: !Secret
    } deriving (Eq, Show)

instance ToJSON AuthorizedUser where
    toJSON (AuthorizedUser i r s) =
        object [ "client_id"     .= i
               , "refresh_token" .= r
               , "client_secret" .= s
               ]

instance FromJSON AuthorizedUser where
    parseJSON = withObject "authorized_user" $ \o -> AuthorizedUser
        <$> o .: "client_id"
        <*> o .: "refresh_token"
        <*> o .: "client_secret"

-- | A client identifier and accompanying secret used to obtain/refresh a token.
data OAuthClient = OAuthClient
    { _clientId     :: !ClientId
    , _clientSecret :: !Secret
    } deriving (Eq, Show)

{-| An OAuth bearer type token of the following form:

> {
>   \"token_type\": \"Bearer\",
>   \"access_token\": \"eyJhbGci...\",
>   \"refresh_token\": \"1/B3gq9K...\",
>   \"expires_in\": 3600,
>   ...
> }

The '_tokenAccess' field will be inserted verbatim into the
@Authorization: Bearer ...@ header for all HTTP requests.
-}
data OAuthToken (s :: [Symbol]) = OAuthToken
    { _tokenAccess  :: !AccessToken
    , _tokenRefresh :: !(Maybe RefreshToken)
    , _tokenExpiry  :: !UTCTime
    } deriving (Eq, Show)

instance FromJSON (UTCTime -> OAuthToken s) where
    parseJSON = withObject "bearer" $ \o -> do
        t <- o .:  "access_token"
        r <- o .:? "refresh_token"
        e <- o .:  "expires_in" <&> fromInteger
        pure (OAuthToken t r . addUTCTime e)

-- | An OAuth client authorization code.
newtype OAuthCode (s :: [Symbol]) = OAuthCode Text
    deriving (Eq, Ord, Show, Read, IsString, Generic, Typeable, FromJSON, ToJSON)

instance ToHttpApiData (OAuthCode s) where
    toQueryParam (OAuthCode c) = c
    toHeader     (OAuthCode c) = Text.encodeUtf8 c

-- | An error thrown when attempting to read/write AuthN/AuthZ information.
data AuthError
    = RetrievalError    HttpException
    | MissingFileError  FilePath
    | InvalidFileError  FilePath Text
    | TokenRefreshError Status Text (Maybe Text)
    | FileExistError    FilePath
      deriving (Show, Typeable)

instance Exception AuthError

class AsAuthError a where
    -- | A general authentication error.
    _AuthError        :: Prism' a AuthError
    {-# MINIMAL _AuthError #-}

    -- | An error occured while communicating over HTTP with either then
    -- local metadata or remote accounts.google.com endpoints.
    _RetrievalError   :: Prism' a HttpException

    -- | The specified default credentials file could not be found.
    _MissingFileError :: Prism' a FilePath

    -- | An error occured parsing the default credentials file.
    _InvalidFileError :: Prism' a (FilePath, Text)

    -- | An error occured when attempting to refresh a token.
    _TokenRefreshError :: Prism' a (Status, Text, Maybe Text)

    _RetrievalError    = _AuthError . _RetrievalError
    _MissingFileError  = _AuthError . _MissingFileError
    _InvalidFileError  = _AuthError . _InvalidFileError
    _TokenRefreshError = _AuthError . _TokenRefreshError

instance AsAuthError SomeException where
    _AuthError = exception

instance AsAuthError AuthError where
    _AuthError = id

    _RetrievalError = prism RetrievalError $ \case
        RetrievalError   e -> Right e
        x                  -> Left  x

    _MissingFileError = prism MissingFileError $ \case
        MissingFileError f -> Right f
        x                  -> Left  x

    _InvalidFileError = prism
        (uncurry InvalidFileError)
        (\case
            InvalidFileError f e -> Right (f, e)
            x                    -> Left  x)

    _TokenRefreshError = prism
        (\(s, e, d) -> TokenRefreshError s e d)
        (\case
            TokenRefreshError s e d -> Right (s, e, d)
            x                       -> Left  x)

data RefreshError = RefreshError
    { _error       :: !Text
    , _description :: !(Maybe Text)
    }

instance FromJSON RefreshError where
    parseJSON = withObject "refresh_error" $ \o -> RefreshError
        <$> o .:  "error"
        <*> o .:? "error_description"

-- | @https://accounts.google.com/o/oauth2/v2/auth@.
accountsURL :: Text
accountsURL = "https://accounts.google.com/o/oauth2/v2/auth"

accountsRequest :: Client.Request
accountsRequest = Client.defaultRequest
    { Client.host           = "accounts.google.com"
    , Client.port           = 443
    , Client.secure         = True
    , Client.method         = "POST"
    , Client.path           = "/o/oauth2/v2/auth"
    , Client.requestHeaders =
        [ (hContentType, "application/x-www-form-urlencoded")
        ]
    }

-- | @https://www.googleapis.com/oauth2/v4/token@.
tokenURL :: Text
tokenURL = "https://www.googleapis.com/oauth2/v4/token"

tokenRequest :: Client.Request
tokenRequest = Client.defaultRequest
    { Client.host           = "www.googleapis.com"
    , Client.port           = 443
    , Client.secure         = True
    , Client.method         = "POST"
    , Client.path           = "/oauth2/v4/token"
    , Client.requestHeaders =
        [ (hContentType, "application/x-www-form-urlencoded")
        ]
    }

refreshRequest :: (MonadIO m, MonadCatch m)
               => Client.Request
               -> Logger
               -> Manager
               -> m (OAuthToken s)
refreshRequest rq l m = do
    logDebug l rq                          -- debug:ClientRequest

    rs <- liftIO (Client.httpLbs rq m) `catch` (throwM . RetrievalError)

    let bs = Client.responseBody   rs
        s  = Client.responseStatus rs

    logDebug l rs                          -- debug:ClientResponse
    logTrace l $ "[Response Body]\n" <> bs -- trace:ResponseBody

    if fromEnum s == 200
        then success s bs
        else failure s bs
  where
    success s bs = do
        f  <- parseErr s bs
        ts <- liftIO getCurrentTime
        pure (f ts)

    failure s bs = do
        let e = "Failure refreshing token from " <> host <> path
        logError l $ "[Refresh Error] " <> build e
        case parseLBS bs of
            Right x -> refreshErr s (_error x) (_description x)
            Left  _ -> refreshErr s e Nothing

    parseErr s bs =
        case parseLBS bs of
            Right !x -> pure x
            Left   e -> do
                logError l $
                    "[Parse Error] Failure parsing token refresh " <> build e
                refreshErr s e Nothing

    refreshErr :: MonadThrow m => Status -> Text -> Maybe Text -> m a
    refreshErr s e = throwM . TokenRefreshError s e

    host = Text.decodeUtf8 (Client.host rq)
    path = Text.decodeUtf8 (Client.path rq)

parseLBS :: FromJSON a => LBS.ByteString -> Either Text a
parseLBS = either (Left . Text.pack) Right . eitherDecode'

base64Encode :: [Pair] -> ByteString
base64Encode = base64 . LBS.toStrict . encode . object

base64 :: ByteArray a => a -> ByteString
base64 = convertToBase Base64URLUnpadded

textBody :: Text -> RequestBody
textBody = Client.RequestBodyBS . Text.encodeUtf8
