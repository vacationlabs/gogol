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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Audit
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all user links on an account or property, including implicit ones
-- that come from effective permissions granted by groups or organization
-- admin roles. If a returned user link does not have direct permissions,
-- they cannot be removed from the account or property directly with the
-- DeleteUserLink command. They have to be removed from the group\/etc that
-- gives them permissions, which is currently only usable\/discoverable in
-- the GA or GMP UIs.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.userLinks.audit@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Audit
    (
    -- * REST Resource
      AccountsUserLinksAuditResource

    -- * Creating a Request
    , accountsUserLinksAudit
    , AccountsUserLinksAudit

    -- * Request Lenses
    , aulaParent
    , aulaXgafv
    , aulaUploadProtocol
    , aulaAccessToken
    , aulaUploadType
    , aulaPayload
    , aulaCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.userLinks.audit@ method which the
-- 'AccountsUserLinksAudit' request conforms to.
type AccountsUserLinksAuditResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "userLinks:audit" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaAuditUserLinksRequest
                         :>
                         Post '[JSON]
                           GoogleAnalyticsAdminV1alphaAuditUserLinksResponse

-- | Lists all user links on an account or property, including implicit ones
-- that come from effective permissions granted by groups or organization
-- admin roles. If a returned user link does not have direct permissions,
-- they cannot be removed from the account or property directly with the
-- DeleteUserLink command. They have to be removed from the group\/etc that
-- gives them permissions, which is currently only usable\/discoverable in
-- the GA or GMP UIs.
--
-- /See:/ 'accountsUserLinksAudit' smart constructor.
data AccountsUserLinksAudit =
  AccountsUserLinksAudit'
    { _aulaParent :: !Text
    , _aulaXgafv :: !(Maybe Xgafv)
    , _aulaUploadProtocol :: !(Maybe Text)
    , _aulaAccessToken :: !(Maybe Text)
    , _aulaUploadType :: !(Maybe Text)
    , _aulaPayload :: !GoogleAnalyticsAdminV1alphaAuditUserLinksRequest
    , _aulaCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsUserLinksAudit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aulaParent'
--
-- * 'aulaXgafv'
--
-- * 'aulaUploadProtocol'
--
-- * 'aulaAccessToken'
--
-- * 'aulaUploadType'
--
-- * 'aulaPayload'
--
-- * 'aulaCallback'
accountsUserLinksAudit
    :: Text -- ^ 'aulaParent'
    -> GoogleAnalyticsAdminV1alphaAuditUserLinksRequest -- ^ 'aulaPayload'
    -> AccountsUserLinksAudit
accountsUserLinksAudit pAulaParent_ pAulaPayload_ =
  AccountsUserLinksAudit'
    { _aulaParent = pAulaParent_
    , _aulaXgafv = Nothing
    , _aulaUploadProtocol = Nothing
    , _aulaAccessToken = Nothing
    , _aulaUploadType = Nothing
    , _aulaPayload = pAulaPayload_
    , _aulaCallback = Nothing
    }


-- | Required. Example format: accounts\/1234
aulaParent :: Lens' AccountsUserLinksAudit Text
aulaParent
  = lens _aulaParent (\ s a -> s{_aulaParent = a})

-- | V1 error format.
aulaXgafv :: Lens' AccountsUserLinksAudit (Maybe Xgafv)
aulaXgafv
  = lens _aulaXgafv (\ s a -> s{_aulaXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aulaUploadProtocol :: Lens' AccountsUserLinksAudit (Maybe Text)
aulaUploadProtocol
  = lens _aulaUploadProtocol
      (\ s a -> s{_aulaUploadProtocol = a})

-- | OAuth access token.
aulaAccessToken :: Lens' AccountsUserLinksAudit (Maybe Text)
aulaAccessToken
  = lens _aulaAccessToken
      (\ s a -> s{_aulaAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aulaUploadType :: Lens' AccountsUserLinksAudit (Maybe Text)
aulaUploadType
  = lens _aulaUploadType
      (\ s a -> s{_aulaUploadType = a})

-- | Multipart request metadata.
aulaPayload :: Lens' AccountsUserLinksAudit GoogleAnalyticsAdminV1alphaAuditUserLinksRequest
aulaPayload
  = lens _aulaPayload (\ s a -> s{_aulaPayload = a})

-- | JSONP
aulaCallback :: Lens' AccountsUserLinksAudit (Maybe Text)
aulaCallback
  = lens _aulaCallback (\ s a -> s{_aulaCallback = a})

instance GoogleRequest AccountsUserLinksAudit where
        type Rs AccountsUserLinksAudit =
             GoogleAnalyticsAdminV1alphaAuditUserLinksResponse
        type Scopes AccountsUserLinksAudit =
             '["https://www.googleapis.com/auth/analytics.manage.users.readonly"]
        requestClient AccountsUserLinksAudit'{..}
          = go _aulaParent _aulaXgafv _aulaUploadProtocol
              _aulaAccessToken
              _aulaUploadType
              _aulaCallback
              (Just AltJSON)
              _aulaPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsUserLinksAuditResource)
                      mempty
