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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Audit
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
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.userLinks.audit@.
module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Audit
    (
    -- * REST Resource
      PropertiesUserLinksAuditResource

    -- * Creating a Request
    , propertiesUserLinksAudit
    , PropertiesUserLinksAudit

    -- * Request Lenses
    , pulaParent
    , pulaXgafv
    , pulaUploadProtocol
    , pulaAccessToken
    , pulaUploadType
    , pulaPayload
    , pulaCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.userLinks.audit@ method which the
-- 'PropertiesUserLinksAudit' request conforms to.
type PropertiesUserLinksAuditResource =
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
-- /See:/ 'propertiesUserLinksAudit' smart constructor.
data PropertiesUserLinksAudit =
  PropertiesUserLinksAudit'
    { _pulaParent :: !Text
    , _pulaXgafv :: !(Maybe Xgafv)
    , _pulaUploadProtocol :: !(Maybe Text)
    , _pulaAccessToken :: !(Maybe Text)
    , _pulaUploadType :: !(Maybe Text)
    , _pulaPayload :: !GoogleAnalyticsAdminV1alphaAuditUserLinksRequest
    , _pulaCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesUserLinksAudit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pulaParent'
--
-- * 'pulaXgafv'
--
-- * 'pulaUploadProtocol'
--
-- * 'pulaAccessToken'
--
-- * 'pulaUploadType'
--
-- * 'pulaPayload'
--
-- * 'pulaCallback'
propertiesUserLinksAudit
    :: Text -- ^ 'pulaParent'
    -> GoogleAnalyticsAdminV1alphaAuditUserLinksRequest -- ^ 'pulaPayload'
    -> PropertiesUserLinksAudit
propertiesUserLinksAudit pPulaParent_ pPulaPayload_ =
  PropertiesUserLinksAudit'
    { _pulaParent = pPulaParent_
    , _pulaXgafv = Nothing
    , _pulaUploadProtocol = Nothing
    , _pulaAccessToken = Nothing
    , _pulaUploadType = Nothing
    , _pulaPayload = pPulaPayload_
    , _pulaCallback = Nothing
    }


-- | Required. Example format: accounts\/1234
pulaParent :: Lens' PropertiesUserLinksAudit Text
pulaParent
  = lens _pulaParent (\ s a -> s{_pulaParent = a})

-- | V1 error format.
pulaXgafv :: Lens' PropertiesUserLinksAudit (Maybe Xgafv)
pulaXgafv
  = lens _pulaXgafv (\ s a -> s{_pulaXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pulaUploadProtocol :: Lens' PropertiesUserLinksAudit (Maybe Text)
pulaUploadProtocol
  = lens _pulaUploadProtocol
      (\ s a -> s{_pulaUploadProtocol = a})

-- | OAuth access token.
pulaAccessToken :: Lens' PropertiesUserLinksAudit (Maybe Text)
pulaAccessToken
  = lens _pulaAccessToken
      (\ s a -> s{_pulaAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pulaUploadType :: Lens' PropertiesUserLinksAudit (Maybe Text)
pulaUploadType
  = lens _pulaUploadType
      (\ s a -> s{_pulaUploadType = a})

-- | Multipart request metadata.
pulaPayload :: Lens' PropertiesUserLinksAudit GoogleAnalyticsAdminV1alphaAuditUserLinksRequest
pulaPayload
  = lens _pulaPayload (\ s a -> s{_pulaPayload = a})

-- | JSONP
pulaCallback :: Lens' PropertiesUserLinksAudit (Maybe Text)
pulaCallback
  = lens _pulaCallback (\ s a -> s{_pulaCallback = a})

instance GoogleRequest PropertiesUserLinksAudit where
        type Rs PropertiesUserLinksAudit =
             GoogleAnalyticsAdminV1alphaAuditUserLinksResponse
        type Scopes PropertiesUserLinksAudit =
             '["https://www.googleapis.com/auth/analytics.manage.users.readonly"]
        requestClient PropertiesUserLinksAudit'{..}
          = go _pulaParent _pulaXgafv _pulaUploadProtocol
              _pulaAccessToken
              _pulaUploadType
              _pulaCallback
              (Just AltJSON)
              _pulaPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesUserLinksAuditResource)
                      mempty
