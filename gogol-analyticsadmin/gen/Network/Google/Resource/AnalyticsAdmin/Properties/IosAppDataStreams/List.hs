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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns child iOS app data streams under the specified parent property.
-- iOS app data streams will be excluded if the caller does not have
-- access. Returns an empty list if no relevant iOS app data streams are
-- found.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.iosAppDataStreams.list@.
module Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.List
    (
    -- * REST Resource
      PropertiesIosAppDataStreamsListResource

    -- * Creating a Request
    , propertiesIosAppDataStreamsList
    , PropertiesIosAppDataStreamsList

    -- * Request Lenses
    , piadslParent
    , piadslXgafv
    , piadslUploadProtocol
    , piadslAccessToken
    , piadslUploadType
    , piadslPageToken
    , piadslPageSize
    , piadslCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.iosAppDataStreams.list@ method which the
-- 'PropertiesIosAppDataStreamsList' request conforms to.
type PropertiesIosAppDataStreamsListResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "iosAppDataStreams" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "pageToken" Text :>
                     QueryParam "pageSize" (Textual Int32) :>
                       QueryParam "callback" Text :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON]
                             GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse

-- | Returns child iOS app data streams under the specified parent property.
-- iOS app data streams will be excluded if the caller does not have
-- access. Returns an empty list if no relevant iOS app data streams are
-- found.
--
-- /See:/ 'propertiesIosAppDataStreamsList' smart constructor.
data PropertiesIosAppDataStreamsList =
  PropertiesIosAppDataStreamsList'
    { _piadslParent :: !Text
    , _piadslXgafv :: !(Maybe Xgafv)
    , _piadslUploadProtocol :: !(Maybe Text)
    , _piadslAccessToken :: !(Maybe Text)
    , _piadslUploadType :: !(Maybe Text)
    , _piadslPageToken :: !(Maybe Text)
    , _piadslPageSize :: !(Maybe (Textual Int32))
    , _piadslCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesIosAppDataStreamsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piadslParent'
--
-- * 'piadslXgafv'
--
-- * 'piadslUploadProtocol'
--
-- * 'piadslAccessToken'
--
-- * 'piadslUploadType'
--
-- * 'piadslPageToken'
--
-- * 'piadslPageSize'
--
-- * 'piadslCallback'
propertiesIosAppDataStreamsList
    :: Text -- ^ 'piadslParent'
    -> PropertiesIosAppDataStreamsList
propertiesIosAppDataStreamsList pPiadslParent_ =
  PropertiesIosAppDataStreamsList'
    { _piadslParent = pPiadslParent_
    , _piadslXgafv = Nothing
    , _piadslUploadProtocol = Nothing
    , _piadslAccessToken = Nothing
    , _piadslUploadType = Nothing
    , _piadslPageToken = Nothing
    , _piadslPageSize = Nothing
    , _piadslCallback = Nothing
    }


-- | Required. The name of the parent property. For example, to list results
-- of app streams under the property with Id 123: \"properties\/123\"
piadslParent :: Lens' PropertiesIosAppDataStreamsList Text
piadslParent
  = lens _piadslParent (\ s a -> s{_piadslParent = a})

-- | V1 error format.
piadslXgafv :: Lens' PropertiesIosAppDataStreamsList (Maybe Xgafv)
piadslXgafv
  = lens _piadslXgafv (\ s a -> s{_piadslXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
piadslUploadProtocol :: Lens' PropertiesIosAppDataStreamsList (Maybe Text)
piadslUploadProtocol
  = lens _piadslUploadProtocol
      (\ s a -> s{_piadslUploadProtocol = a})

-- | OAuth access token.
piadslAccessToken :: Lens' PropertiesIosAppDataStreamsList (Maybe Text)
piadslAccessToken
  = lens _piadslAccessToken
      (\ s a -> s{_piadslAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
piadslUploadType :: Lens' PropertiesIosAppDataStreamsList (Maybe Text)
piadslUploadType
  = lens _piadslUploadType
      (\ s a -> s{_piadslUploadType = a})

-- | A page token, received from a previous \`ListIosAppDataStreams\` call.
-- Provide this to retrieve the subsequent page. When paginating, all other
-- parameters provided to \`ListIosAppDataStreams\` must match the call
-- that provided the page token.
piadslPageToken :: Lens' PropertiesIosAppDataStreamsList (Maybe Text)
piadslPageToken
  = lens _piadslPageToken
      (\ s a -> s{_piadslPageToken = a})

-- | The maximum number of resources to return. If unspecified, at most 50
-- resources will be returned. The maximum value is 200; (higher values
-- will be coerced to the maximum)
piadslPageSize :: Lens' PropertiesIosAppDataStreamsList (Maybe Int32)
piadslPageSize
  = lens _piadslPageSize
      (\ s a -> s{_piadslPageSize = a})
      . mapping _Coerce

-- | JSONP
piadslCallback :: Lens' PropertiesIosAppDataStreamsList (Maybe Text)
piadslCallback
  = lens _piadslCallback
      (\ s a -> s{_piadslCallback = a})

instance GoogleRequest
           PropertiesIosAppDataStreamsList
         where
        type Rs PropertiesIosAppDataStreamsList =
             GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse
        type Scopes PropertiesIosAppDataStreamsList =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient PropertiesIosAppDataStreamsList'{..}
          = go _piadslParent _piadslXgafv _piadslUploadProtocol
              _piadslAccessToken
              _piadslUploadType
              _piadslPageToken
              _piadslPageSize
              _piadslCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesIosAppDataStreamsListResource)
                      mempty
