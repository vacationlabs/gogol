{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- |
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user link on an account or property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.userLinks.patch@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Patch
    (
    -- * REST Resource
      AccountsUserLinksPatchResource

    -- * Creating a Request
    , accountsUserLinksPatch
    , AccountsUserLinksPatch

    -- * Request Lenses
    , aulpXgafv
    , aulpUploadProtocol
    , aulpAccessToken
    , aulpUploadType
    , aulpPayload
    , aulpName
    , aulpCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.userLinks.patch@ method which the
-- 'AccountsUserLinksPatch' request conforms to.
type AccountsUserLinksPatchResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] GoogleAnalyticsAdminV1alphaUserLink
                       :> Patch '[JSON] GoogleAnalyticsAdminV1alphaUserLink

-- | Updates a user link on an account or property.
--
-- /See:/ 'accountsUserLinksPatch' smart constructor.
data AccountsUserLinksPatch =
  AccountsUserLinksPatch'
    { _aulpXgafv :: !(Maybe Xgafv)
    , _aulpUploadProtocol :: !(Maybe Text)
    , _aulpAccessToken :: !(Maybe Text)
    , _aulpUploadType :: !(Maybe Text)
    , _aulpPayload :: !GoogleAnalyticsAdminV1alphaUserLink
    , _aulpName :: !Text
    , _aulpCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsUserLinksPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aulpXgafv'
--
-- * 'aulpUploadProtocol'
--
-- * 'aulpAccessToken'
--
-- * 'aulpUploadType'
--
-- * 'aulpPayload'
--
-- * 'aulpName'
--
-- * 'aulpCallback'
accountsUserLinksPatch
    :: GoogleAnalyticsAdminV1alphaUserLink -- ^ 'aulpPayload'
    -> Text -- ^ 'aulpName'
    -> AccountsUserLinksPatch
accountsUserLinksPatch pAulpPayload_ pAulpName_ =
  AccountsUserLinksPatch'
    { _aulpXgafv = Nothing
    , _aulpUploadProtocol = Nothing
    , _aulpAccessToken = Nothing
    , _aulpUploadType = Nothing
    , _aulpPayload = pAulpPayload_
    , _aulpName = pAulpName_
    , _aulpCallback = Nothing
    }


-- | V1 error format.
aulpXgafv :: Lens' AccountsUserLinksPatch (Maybe Xgafv)
aulpXgafv
  = lens _aulpXgafv (\ s a -> s{_aulpXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aulpUploadProtocol :: Lens' AccountsUserLinksPatch (Maybe Text)
aulpUploadProtocol
  = lens _aulpUploadProtocol
      (\ s a -> s{_aulpUploadProtocol = a})

-- | OAuth access token.
aulpAccessToken :: Lens' AccountsUserLinksPatch (Maybe Text)
aulpAccessToken
  = lens _aulpAccessToken
      (\ s a -> s{_aulpAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aulpUploadType :: Lens' AccountsUserLinksPatch (Maybe Text)
aulpUploadType
  = lens _aulpUploadType
      (\ s a -> s{_aulpUploadType = a})

-- | Multipart request metadata.
aulpPayload :: Lens' AccountsUserLinksPatch GoogleAnalyticsAdminV1alphaUserLink
aulpPayload
  = lens _aulpPayload (\ s a -> s{_aulpPayload = a})

-- | Example format: properties\/1234\/userLinks\/5678
aulpName :: Lens' AccountsUserLinksPatch Text
aulpName = lens _aulpName (\ s a -> s{_aulpName = a})

-- | JSONP
aulpCallback :: Lens' AccountsUserLinksPatch (Maybe Text)
aulpCallback
  = lens _aulpCallback (\ s a -> s{_aulpCallback = a})

instance GoogleRequest AccountsUserLinksPatch where
        type Rs AccountsUserLinksPatch =
             GoogleAnalyticsAdminV1alphaUserLink
        type Scopes AccountsUserLinksPatch =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient AccountsUserLinksPatch'{..}
          = go _aulpName _aulpXgafv _aulpUploadProtocol
              _aulpAccessToken
              _aulpUploadType
              _aulpCallback
              (Just AltJSON)
              _aulpPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsUserLinksPatchResource)
                      mempty
