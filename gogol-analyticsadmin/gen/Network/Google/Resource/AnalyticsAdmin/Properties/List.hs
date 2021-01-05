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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns child Properties under the specified parent Account. Only
-- \"GA4\" properties will be returned. Properties will be excluded if the
-- caller does not have access. Soft-deleted (ie: \"trashed\") properties
-- are excluded by default. Returns an empty list if no relevant properties
-- are found.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.list@.
module Network.Google.Resource.AnalyticsAdmin.Properties.List
    (
    -- * REST Resource
      PropertiesListResource

    -- * Creating a Request
    , propertiesList
    , PropertiesList

    -- * Request Lenses
    , plXgafv
    , plUploadProtocol
    , plAccessToken
    , plUploadType
    , plShowDeleted
    , plFilter
    , plPageToken
    , plPageSize
    , plCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.list@ method which the
-- 'PropertiesList' request conforms to.
type PropertiesListResource =
     "v1alpha" :>
       "properties" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "showDeleted" Bool :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "pageSize" (Textual Int32) :>
                         QueryParam "callback" Text :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON]
                               GoogleAnalyticsAdminV1alphaListPropertiesResponse

-- | Returns child Properties under the specified parent Account. Only
-- \"GA4\" properties will be returned. Properties will be excluded if the
-- caller does not have access. Soft-deleted (ie: \"trashed\") properties
-- are excluded by default. Returns an empty list if no relevant properties
-- are found.
--
-- /See:/ 'propertiesList' smart constructor.
data PropertiesList =
  PropertiesList'
    { _plXgafv :: !(Maybe Xgafv)
    , _plUploadProtocol :: !(Maybe Text)
    , _plAccessToken :: !(Maybe Text)
    , _plUploadType :: !(Maybe Text)
    , _plShowDeleted :: !(Maybe Bool)
    , _plFilter :: !(Maybe Text)
    , _plPageToken :: !(Maybe Text)
    , _plPageSize :: !(Maybe (Textual Int32))
    , _plCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plXgafv'
--
-- * 'plUploadProtocol'
--
-- * 'plAccessToken'
--
-- * 'plUploadType'
--
-- * 'plShowDeleted'
--
-- * 'plFilter'
--
-- * 'plPageToken'
--
-- * 'plPageSize'
--
-- * 'plCallback'
propertiesList
    :: PropertiesList
propertiesList =
  PropertiesList'
    { _plXgafv = Nothing
    , _plUploadProtocol = Nothing
    , _plAccessToken = Nothing
    , _plUploadType = Nothing
    , _plShowDeleted = Nothing
    , _plFilter = Nothing
    , _plPageToken = Nothing
    , _plPageSize = Nothing
    , _plCallback = Nothing
    }


-- | V1 error format.
plXgafv :: Lens' PropertiesList (Maybe Xgafv)
plXgafv = lens _plXgafv (\ s a -> s{_plXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
plUploadProtocol :: Lens' PropertiesList (Maybe Text)
plUploadProtocol
  = lens _plUploadProtocol
      (\ s a -> s{_plUploadProtocol = a})

-- | OAuth access token.
plAccessToken :: Lens' PropertiesList (Maybe Text)
plAccessToken
  = lens _plAccessToken
      (\ s a -> s{_plAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
plUploadType :: Lens' PropertiesList (Maybe Text)
plUploadType
  = lens _plUploadType (\ s a -> s{_plUploadType = a})

-- | Whether to include soft-deleted (ie: \"trashed\") Properties in the
-- results. Properties can be inspected to determine whether they are
-- deleted or not.
plShowDeleted :: Lens' PropertiesList (Maybe Bool)
plShowDeleted
  = lens _plShowDeleted
      (\ s a -> s{_plShowDeleted = a})

-- | Required. An expression for filtering the results of the request. Fields
-- eligible for filtering are: \`parent:\`(The resource name of the parent
-- account) or \`firebase_project:\`(The id or number of the linked
-- firebase project). Some examples of filters: | Filter | Description |
-- |-----------------------------|-------------------------------------------|
-- | parent:accounts\/123 | The account with account id: 123. | |
-- firebase_project:project-id | The firebase project with id: project-id.
-- | | firebase_project:123 | The firebase project with number: 123. |
plFilter :: Lens' PropertiesList (Maybe Text)
plFilter = lens _plFilter (\ s a -> s{_plFilter = a})

-- | A page token, received from a previous \`ListProperties\` call. Provide
-- this to retrieve the subsequent page. When paginating, all other
-- parameters provided to \`ListProperties\` must match the call that
-- provided the page token.
plPageToken :: Lens' PropertiesList (Maybe Text)
plPageToken
  = lens _plPageToken (\ s a -> s{_plPageToken = a})

-- | The maximum number of resources to return. The service may return fewer
-- than this value, even if there are additional pages. If unspecified, at
-- most 50 resources will be returned. The maximum value is 200; (higher
-- values will be coerced to the maximum)
plPageSize :: Lens' PropertiesList (Maybe Int32)
plPageSize
  = lens _plPageSize (\ s a -> s{_plPageSize = a}) .
      mapping _Coerce

-- | JSONP
plCallback :: Lens' PropertiesList (Maybe Text)
plCallback
  = lens _plCallback (\ s a -> s{_plCallback = a})

instance GoogleRequest PropertiesList where
        type Rs PropertiesList =
             GoogleAnalyticsAdminV1alphaListPropertiesResponse
        type Scopes PropertiesList =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient PropertiesList'{..}
          = go _plXgafv _plUploadProtocol _plAccessToken
              _plUploadType
              _plShowDeleted
              _plFilter
              _plPageToken
              _plPageSize
              _plCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient (Proxy :: Proxy PropertiesListResource)
                      mempty
