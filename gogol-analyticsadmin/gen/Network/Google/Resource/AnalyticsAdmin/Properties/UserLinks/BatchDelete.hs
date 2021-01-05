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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchDelete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes information about multiple users\' links to an account or
-- property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.userLinks.batchDelete@.
module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchDelete
    (
    -- * REST Resource
      PropertiesUserLinksBatchDeleteResource

    -- * Creating a Request
    , propertiesUserLinksBatchDelete
    , PropertiesUserLinksBatchDelete

    -- * Request Lenses
    , pulbdParent
    , pulbdXgafv
    , pulbdUploadProtocol
    , pulbdAccessToken
    , pulbdUploadType
    , pulbdPayload
    , pulbdCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.userLinks.batchDelete@ method which the
-- 'PropertiesUserLinksBatchDelete' request conforms to.
type PropertiesUserLinksBatchDeleteResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "userLinks:batchDelete" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
                         :> Post '[JSON] GoogleProtobufEmpty

-- | Deletes information about multiple users\' links to an account or
-- property.
--
-- /See:/ 'propertiesUserLinksBatchDelete' smart constructor.
data PropertiesUserLinksBatchDelete =
  PropertiesUserLinksBatchDelete'
    { _pulbdParent :: !Text
    , _pulbdXgafv :: !(Maybe Xgafv)
    , _pulbdUploadProtocol :: !(Maybe Text)
    , _pulbdAccessToken :: !(Maybe Text)
    , _pulbdUploadType :: !(Maybe Text)
    , _pulbdPayload :: !GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
    , _pulbdCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesUserLinksBatchDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pulbdParent'
--
-- * 'pulbdXgafv'
--
-- * 'pulbdUploadProtocol'
--
-- * 'pulbdAccessToken'
--
-- * 'pulbdUploadType'
--
-- * 'pulbdPayload'
--
-- * 'pulbdCallback'
propertiesUserLinksBatchDelete
    :: Text -- ^ 'pulbdParent'
    -> GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest -- ^ 'pulbdPayload'
    -> PropertiesUserLinksBatchDelete
propertiesUserLinksBatchDelete pPulbdParent_ pPulbdPayload_ =
  PropertiesUserLinksBatchDelete'
    { _pulbdParent = pPulbdParent_
    , _pulbdXgafv = Nothing
    , _pulbdUploadProtocol = Nothing
    , _pulbdAccessToken = Nothing
    , _pulbdUploadType = Nothing
    , _pulbdPayload = pPulbdPayload_
    , _pulbdCallback = Nothing
    }


-- | Required. The account or property that all user links in the request are
-- for. The parent of all values for user link names to delete must match
-- this field. Example format: accounts\/1234
pulbdParent :: Lens' PropertiesUserLinksBatchDelete Text
pulbdParent
  = lens _pulbdParent (\ s a -> s{_pulbdParent = a})

-- | V1 error format.
pulbdXgafv :: Lens' PropertiesUserLinksBatchDelete (Maybe Xgafv)
pulbdXgafv
  = lens _pulbdXgafv (\ s a -> s{_pulbdXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pulbdUploadProtocol :: Lens' PropertiesUserLinksBatchDelete (Maybe Text)
pulbdUploadProtocol
  = lens _pulbdUploadProtocol
      (\ s a -> s{_pulbdUploadProtocol = a})

-- | OAuth access token.
pulbdAccessToken :: Lens' PropertiesUserLinksBatchDelete (Maybe Text)
pulbdAccessToken
  = lens _pulbdAccessToken
      (\ s a -> s{_pulbdAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pulbdUploadType :: Lens' PropertiesUserLinksBatchDelete (Maybe Text)
pulbdUploadType
  = lens _pulbdUploadType
      (\ s a -> s{_pulbdUploadType = a})

-- | Multipart request metadata.
pulbdPayload :: Lens' PropertiesUserLinksBatchDelete GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
pulbdPayload
  = lens _pulbdPayload (\ s a -> s{_pulbdPayload = a})

-- | JSONP
pulbdCallback :: Lens' PropertiesUserLinksBatchDelete (Maybe Text)
pulbdCallback
  = lens _pulbdCallback
      (\ s a -> s{_pulbdCallback = a})

instance GoogleRequest PropertiesUserLinksBatchDelete
         where
        type Rs PropertiesUserLinksBatchDelete =
             GoogleProtobufEmpty
        type Scopes PropertiesUserLinksBatchDelete =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient PropertiesUserLinksBatchDelete'{..}
          = go _pulbdParent _pulbdXgafv _pulbdUploadProtocol
              _pulbdAccessToken
              _pulbdUploadType
              _pulbdCallback
              (Just AltJSON)
              _pulbdPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesUserLinksBatchDeleteResource)
                      mempty
