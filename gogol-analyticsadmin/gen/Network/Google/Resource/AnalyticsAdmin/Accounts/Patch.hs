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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an account.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.patch@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.Patch
    (
    -- * REST Resource
      AccountsPatchResource

    -- * Creating a Request
    , accountsPatch
    , AccountsPatch

    -- * Request Lenses
    , apXgafv
    , apUploadProtocol
    , apUpdateMask
    , apAccessToken
    , apUploadType
    , apPayload
    , apName
    , apCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.patch@ method which the
-- 'AccountsPatch' request conforms to.
type AccountsPatchResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "updateMask" GFieldMask :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON] GoogleAnalyticsAdminV1alphaAccount :>
                         Patch '[JSON] GoogleAnalyticsAdminV1alphaAccount

-- | Updates an account.
--
-- /See:/ 'accountsPatch' smart constructor.
data AccountsPatch =
  AccountsPatch'
    { _apXgafv :: !(Maybe Xgafv)
    , _apUploadProtocol :: !(Maybe Text)
    , _apUpdateMask :: !(Maybe GFieldMask)
    , _apAccessToken :: !(Maybe Text)
    , _apUploadType :: !(Maybe Text)
    , _apPayload :: !GoogleAnalyticsAdminV1alphaAccount
    , _apName :: !Text
    , _apCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apXgafv'
--
-- * 'apUploadProtocol'
--
-- * 'apUpdateMask'
--
-- * 'apAccessToken'
--
-- * 'apUploadType'
--
-- * 'apPayload'
--
-- * 'apName'
--
-- * 'apCallback'
accountsPatch
    :: GoogleAnalyticsAdminV1alphaAccount -- ^ 'apPayload'
    -> Text -- ^ 'apName'
    -> AccountsPatch
accountsPatch pApPayload_ pApName_ =
  AccountsPatch'
    { _apXgafv = Nothing
    , _apUploadProtocol = Nothing
    , _apUpdateMask = Nothing
    , _apAccessToken = Nothing
    , _apUploadType = Nothing
    , _apPayload = pApPayload_
    , _apName = pApName_
    , _apCallback = Nothing
    }


-- | V1 error format.
apXgafv :: Lens' AccountsPatch (Maybe Xgafv)
apXgafv = lens _apXgafv (\ s a -> s{_apXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
apUploadProtocol :: Lens' AccountsPatch (Maybe Text)
apUploadProtocol
  = lens _apUploadProtocol
      (\ s a -> s{_apUploadProtocol = a})

-- | Required. The list of fields to be updated. Omitted fields will not be
-- updated. To replace the entire entity, use one path with the string
-- \"*\" to match all fields.
apUpdateMask :: Lens' AccountsPatch (Maybe GFieldMask)
apUpdateMask
  = lens _apUpdateMask (\ s a -> s{_apUpdateMask = a})

-- | OAuth access token.
apAccessToken :: Lens' AccountsPatch (Maybe Text)
apAccessToken
  = lens _apAccessToken
      (\ s a -> s{_apAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
apUploadType :: Lens' AccountsPatch (Maybe Text)
apUploadType
  = lens _apUploadType (\ s a -> s{_apUploadType = a})

-- | Multipart request metadata.
apPayload :: Lens' AccountsPatch GoogleAnalyticsAdminV1alphaAccount
apPayload
  = lens _apPayload (\ s a -> s{_apPayload = a})

-- | Output only. Resource name of this account. Format: accounts\/{account}
-- Example: \"accounts\/100\"
apName :: Lens' AccountsPatch Text
apName = lens _apName (\ s a -> s{_apName = a})

-- | JSONP
apCallback :: Lens' AccountsPatch (Maybe Text)
apCallback
  = lens _apCallback (\ s a -> s{_apCallback = a})

instance GoogleRequest AccountsPatch where
        type Rs AccountsPatch =
             GoogleAnalyticsAdminV1alphaAccount
        type Scopes AccountsPatch =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient AccountsPatch'{..}
          = go _apName _apXgafv _apUploadProtocol _apUpdateMask
              _apAccessToken
              _apUploadType
              _apCallback
              (Just AltJSON)
              _apPayload
              analyticsAdminService
          where go
                  = buildClient (Proxy :: Proxy AccountsPatchResource)
                      mempty
