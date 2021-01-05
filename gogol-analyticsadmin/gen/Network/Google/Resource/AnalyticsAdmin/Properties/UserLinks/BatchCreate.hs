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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchCreate
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates information about multiple users\' links to an account or
-- property. This method is transactional. If any UserLink cannot be
-- created, none of the UserLinks will be created.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.userLinks.batchCreate@.
module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchCreate
    (
    -- * REST Resource
      PropertiesUserLinksBatchCreateResource

    -- * Creating a Request
    , propertiesUserLinksBatchCreate
    , PropertiesUserLinksBatchCreate

    -- * Request Lenses
    , pulbcParent
    , pulbcXgafv
    , pulbcUploadProtocol
    , pulbcAccessToken
    , pulbcUploadType
    , pulbcPayload
    , pulbcCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.userLinks.batchCreate@ method which the
-- 'PropertiesUserLinksBatchCreate' request conforms to.
type PropertiesUserLinksBatchCreateResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "userLinks:batchCreate" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
                         :>
                         Post '[JSON]
                           GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse

-- | Creates information about multiple users\' links to an account or
-- property. This method is transactional. If any UserLink cannot be
-- created, none of the UserLinks will be created.
--
-- /See:/ 'propertiesUserLinksBatchCreate' smart constructor.
data PropertiesUserLinksBatchCreate =
  PropertiesUserLinksBatchCreate'
    { _pulbcParent :: !Text
    , _pulbcXgafv :: !(Maybe Xgafv)
    , _pulbcUploadProtocol :: !(Maybe Text)
    , _pulbcAccessToken :: !(Maybe Text)
    , _pulbcUploadType :: !(Maybe Text)
    , _pulbcPayload :: !GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
    , _pulbcCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesUserLinksBatchCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pulbcParent'
--
-- * 'pulbcXgafv'
--
-- * 'pulbcUploadProtocol'
--
-- * 'pulbcAccessToken'
--
-- * 'pulbcUploadType'
--
-- * 'pulbcPayload'
--
-- * 'pulbcCallback'
propertiesUserLinksBatchCreate
    :: Text -- ^ 'pulbcParent'
    -> GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest -- ^ 'pulbcPayload'
    -> PropertiesUserLinksBatchCreate
propertiesUserLinksBatchCreate pPulbcParent_ pPulbcPayload_ =
  PropertiesUserLinksBatchCreate'
    { _pulbcParent = pPulbcParent_
    , _pulbcXgafv = Nothing
    , _pulbcUploadProtocol = Nothing
    , _pulbcAccessToken = Nothing
    , _pulbcUploadType = Nothing
    , _pulbcPayload = pPulbcPayload_
    , _pulbcCallback = Nothing
    }


-- | Required. The account or property that all user links in the request are
-- for. This field is required. The parent field in the
-- CreateUserLinkRequest messages must either be empty or match this field.
-- Example format: accounts\/1234
pulbcParent :: Lens' PropertiesUserLinksBatchCreate Text
pulbcParent
  = lens _pulbcParent (\ s a -> s{_pulbcParent = a})

-- | V1 error format.
pulbcXgafv :: Lens' PropertiesUserLinksBatchCreate (Maybe Xgafv)
pulbcXgafv
  = lens _pulbcXgafv (\ s a -> s{_pulbcXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pulbcUploadProtocol :: Lens' PropertiesUserLinksBatchCreate (Maybe Text)
pulbcUploadProtocol
  = lens _pulbcUploadProtocol
      (\ s a -> s{_pulbcUploadProtocol = a})

-- | OAuth access token.
pulbcAccessToken :: Lens' PropertiesUserLinksBatchCreate (Maybe Text)
pulbcAccessToken
  = lens _pulbcAccessToken
      (\ s a -> s{_pulbcAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pulbcUploadType :: Lens' PropertiesUserLinksBatchCreate (Maybe Text)
pulbcUploadType
  = lens _pulbcUploadType
      (\ s a -> s{_pulbcUploadType = a})

-- | Multipart request metadata.
pulbcPayload :: Lens' PropertiesUserLinksBatchCreate GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
pulbcPayload
  = lens _pulbcPayload (\ s a -> s{_pulbcPayload = a})

-- | JSONP
pulbcCallback :: Lens' PropertiesUserLinksBatchCreate (Maybe Text)
pulbcCallback
  = lens _pulbcCallback
      (\ s a -> s{_pulbcCallback = a})

instance GoogleRequest PropertiesUserLinksBatchCreate
         where
        type Rs PropertiesUserLinksBatchCreate =
             GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse
        type Scopes PropertiesUserLinksBatchCreate =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient PropertiesUserLinksBatchCreate'{..}
          = go _pulbcParent _pulbcXgafv _pulbcUploadProtocol
              _pulbcAccessToken
              _pulbcUploadType
              _pulbcCallback
              (Just AltJSON)
              _pulbcPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesUserLinksBatchCreateResource)
                      mempty
