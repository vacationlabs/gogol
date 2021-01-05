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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchGet
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about multiple users\' links to an account or property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.userLinks.batchGet@.
module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchGet
    (
    -- * REST Resource
      PropertiesUserLinksBatchGetResource

    -- * Creating a Request
    , propertiesUserLinksBatchGet
    , PropertiesUserLinksBatchGet

    -- * Request Lenses
    , pulbgParent
    , pulbgXgafv
    , pulbgUploadProtocol
    , pulbgAccessToken
    , pulbgUploadType
    , pulbgNames
    , pulbgCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.userLinks.batchGet@ method which the
-- 'PropertiesUserLinksBatchGet' request conforms to.
type PropertiesUserLinksBatchGetResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "userLinks:batchGet" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParams "names" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "alt" AltJSON :>
                         Get '[JSON]
                           GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse

-- | Gets information about multiple users\' links to an account or property.
--
-- /See:/ 'propertiesUserLinksBatchGet' smart constructor.
data PropertiesUserLinksBatchGet =
  PropertiesUserLinksBatchGet'
    { _pulbgParent :: !Text
    , _pulbgXgafv :: !(Maybe Xgafv)
    , _pulbgUploadProtocol :: !(Maybe Text)
    , _pulbgAccessToken :: !(Maybe Text)
    , _pulbgUploadType :: !(Maybe Text)
    , _pulbgNames :: !(Maybe [Text])
    , _pulbgCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesUserLinksBatchGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pulbgParent'
--
-- * 'pulbgXgafv'
--
-- * 'pulbgUploadProtocol'
--
-- * 'pulbgAccessToken'
--
-- * 'pulbgUploadType'
--
-- * 'pulbgNames'
--
-- * 'pulbgCallback'
propertiesUserLinksBatchGet
    :: Text -- ^ 'pulbgParent'
    -> PropertiesUserLinksBatchGet
propertiesUserLinksBatchGet pPulbgParent_ =
  PropertiesUserLinksBatchGet'
    { _pulbgParent = pPulbgParent_
    , _pulbgXgafv = Nothing
    , _pulbgUploadProtocol = Nothing
    , _pulbgAccessToken = Nothing
    , _pulbgUploadType = Nothing
    , _pulbgNames = Nothing
    , _pulbgCallback = Nothing
    }


-- | Required. The account or property that all user links in the request are
-- for. The parent of all provided values for the \'names\' field must
-- match this field. Example format: accounts\/1234
pulbgParent :: Lens' PropertiesUserLinksBatchGet Text
pulbgParent
  = lens _pulbgParent (\ s a -> s{_pulbgParent = a})

-- | V1 error format.
pulbgXgafv :: Lens' PropertiesUserLinksBatchGet (Maybe Xgafv)
pulbgXgafv
  = lens _pulbgXgafv (\ s a -> s{_pulbgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pulbgUploadProtocol :: Lens' PropertiesUserLinksBatchGet (Maybe Text)
pulbgUploadProtocol
  = lens _pulbgUploadProtocol
      (\ s a -> s{_pulbgUploadProtocol = a})

-- | OAuth access token.
pulbgAccessToken :: Lens' PropertiesUserLinksBatchGet (Maybe Text)
pulbgAccessToken
  = lens _pulbgAccessToken
      (\ s a -> s{_pulbgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pulbgUploadType :: Lens' PropertiesUserLinksBatchGet (Maybe Text)
pulbgUploadType
  = lens _pulbgUploadType
      (\ s a -> s{_pulbgUploadType = a})

-- | Required. The names of the user links to retrieve. A maximum of 1000
-- user links can be retrieved in a batch. Format:
-- accounts\/{accountId}\/userLinks\/{userLinkId}
pulbgNames :: Lens' PropertiesUserLinksBatchGet [Text]
pulbgNames
  = lens _pulbgNames (\ s a -> s{_pulbgNames = a}) .
      _Default
      . _Coerce

-- | JSONP
pulbgCallback :: Lens' PropertiesUserLinksBatchGet (Maybe Text)
pulbgCallback
  = lens _pulbgCallback
      (\ s a -> s{_pulbgCallback = a})

instance GoogleRequest PropertiesUserLinksBatchGet
         where
        type Rs PropertiesUserLinksBatchGet =
             GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse
        type Scopes PropertiesUserLinksBatchGet =
             '["https://www.googleapis.com/auth/analytics.manage.users.readonly"]
        requestClient PropertiesUserLinksBatchGet'{..}
          = go _pulbgParent _pulbgXgafv _pulbgUploadProtocol
              _pulbgAccessToken
              _pulbgUploadType
              (_pulbgNames ^. _Default)
              _pulbgCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesUserLinksBatchGetResource)
                      mempty
