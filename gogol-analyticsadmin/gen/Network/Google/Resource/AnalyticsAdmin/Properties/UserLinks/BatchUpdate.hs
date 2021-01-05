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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchUpdate
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about multiple users\' links to an account or
-- property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.userLinks.batchUpdate@.
module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchUpdate
    (
    -- * REST Resource
      PropertiesUserLinksBatchUpdateResource

    -- * Creating a Request
    , propertiesUserLinksBatchUpdate
    , PropertiesUserLinksBatchUpdate

    -- * Request Lenses
    , pulbuParent
    , pulbuXgafv
    , pulbuUploadProtocol
    , pulbuAccessToken
    , pulbuUploadType
    , pulbuPayload
    , pulbuCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.userLinks.batchUpdate@ method which the
-- 'PropertiesUserLinksBatchUpdate' request conforms to.
type PropertiesUserLinksBatchUpdateResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "userLinks:batchUpdate" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
                         :>
                         Post '[JSON]
                           GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse

-- | Updates information about multiple users\' links to an account or
-- property.
--
-- /See:/ 'propertiesUserLinksBatchUpdate' smart constructor.
data PropertiesUserLinksBatchUpdate =
  PropertiesUserLinksBatchUpdate'
    { _pulbuParent :: !Text
    , _pulbuXgafv :: !(Maybe Xgafv)
    , _pulbuUploadProtocol :: !(Maybe Text)
    , _pulbuAccessToken :: !(Maybe Text)
    , _pulbuUploadType :: !(Maybe Text)
    , _pulbuPayload :: !GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
    , _pulbuCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesUserLinksBatchUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pulbuParent'
--
-- * 'pulbuXgafv'
--
-- * 'pulbuUploadProtocol'
--
-- * 'pulbuAccessToken'
--
-- * 'pulbuUploadType'
--
-- * 'pulbuPayload'
--
-- * 'pulbuCallback'
propertiesUserLinksBatchUpdate
    :: Text -- ^ 'pulbuParent'
    -> GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest -- ^ 'pulbuPayload'
    -> PropertiesUserLinksBatchUpdate
propertiesUserLinksBatchUpdate pPulbuParent_ pPulbuPayload_ =
  PropertiesUserLinksBatchUpdate'
    { _pulbuParent = pPulbuParent_
    , _pulbuXgafv = Nothing
    , _pulbuUploadProtocol = Nothing
    , _pulbuAccessToken = Nothing
    , _pulbuUploadType = Nothing
    , _pulbuPayload = pPulbuPayload_
    , _pulbuCallback = Nothing
    }


-- | Required. The account or property that all user links in the request are
-- for. The parent field in the UpdateUserLinkRequest messages must either
-- be empty or match this field. Example format: accounts\/1234
pulbuParent :: Lens' PropertiesUserLinksBatchUpdate Text
pulbuParent
  = lens _pulbuParent (\ s a -> s{_pulbuParent = a})

-- | V1 error format.
pulbuXgafv :: Lens' PropertiesUserLinksBatchUpdate (Maybe Xgafv)
pulbuXgafv
  = lens _pulbuXgafv (\ s a -> s{_pulbuXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pulbuUploadProtocol :: Lens' PropertiesUserLinksBatchUpdate (Maybe Text)
pulbuUploadProtocol
  = lens _pulbuUploadProtocol
      (\ s a -> s{_pulbuUploadProtocol = a})

-- | OAuth access token.
pulbuAccessToken :: Lens' PropertiesUserLinksBatchUpdate (Maybe Text)
pulbuAccessToken
  = lens _pulbuAccessToken
      (\ s a -> s{_pulbuAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pulbuUploadType :: Lens' PropertiesUserLinksBatchUpdate (Maybe Text)
pulbuUploadType
  = lens _pulbuUploadType
      (\ s a -> s{_pulbuUploadType = a})

-- | Multipart request metadata.
pulbuPayload :: Lens' PropertiesUserLinksBatchUpdate GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
pulbuPayload
  = lens _pulbuPayload (\ s a -> s{_pulbuPayload = a})

-- | JSONP
pulbuCallback :: Lens' PropertiesUserLinksBatchUpdate (Maybe Text)
pulbuCallback
  = lens _pulbuCallback
      (\ s a -> s{_pulbuCallback = a})

instance GoogleRequest PropertiesUserLinksBatchUpdate
         where
        type Rs PropertiesUserLinksBatchUpdate =
             GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse
        type Scopes PropertiesUserLinksBatchUpdate =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient PropertiesUserLinksBatchUpdate'{..}
          = go _pulbuParent _pulbuXgafv _pulbuUploadProtocol
              _pulbuAccessToken
              _pulbuUploadType
              _pulbuCallback
              (Just AltJSON)
              _pulbuPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesUserLinksBatchUpdateResource)
                      mempty
