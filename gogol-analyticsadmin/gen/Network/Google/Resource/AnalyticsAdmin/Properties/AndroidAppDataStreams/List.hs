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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns child android app streams under the specified parent property.
-- Android app streams will be excluded if the caller does not have access.
-- Returns an empty list if no relevant android app streams are found.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.androidAppDataStreams.list@.
module Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.List
    (
    -- * REST Resource
      PropertiesAndroidAppDataStreamsListResource

    -- * Creating a Request
    , propertiesAndroidAppDataStreamsList
    , PropertiesAndroidAppDataStreamsList

    -- * Request Lenses
    , paadslParent
    , paadslXgafv
    , paadslUploadProtocol
    , paadslAccessToken
    , paadslUploadType
    , paadslPageToken
    , paadslPageSize
    , paadslCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.androidAppDataStreams.list@ method which the
-- 'PropertiesAndroidAppDataStreamsList' request conforms to.
type PropertiesAndroidAppDataStreamsListResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "androidAppDataStreams" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "pageToken" Text :>
                     QueryParam "pageSize" (Textual Int32) :>
                       QueryParam "callback" Text :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON]
                             GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse

-- | Returns child android app streams under the specified parent property.
-- Android app streams will be excluded if the caller does not have access.
-- Returns an empty list if no relevant android app streams are found.
--
-- /See:/ 'propertiesAndroidAppDataStreamsList' smart constructor.
data PropertiesAndroidAppDataStreamsList =
  PropertiesAndroidAppDataStreamsList'
    { _paadslParent :: !Text
    , _paadslXgafv :: !(Maybe Xgafv)
    , _paadslUploadProtocol :: !(Maybe Text)
    , _paadslAccessToken :: !(Maybe Text)
    , _paadslUploadType :: !(Maybe Text)
    , _paadslPageToken :: !(Maybe Text)
    , _paadslPageSize :: !(Maybe (Textual Int32))
    , _paadslCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesAndroidAppDataStreamsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paadslParent'
--
-- * 'paadslXgafv'
--
-- * 'paadslUploadProtocol'
--
-- * 'paadslAccessToken'
--
-- * 'paadslUploadType'
--
-- * 'paadslPageToken'
--
-- * 'paadslPageSize'
--
-- * 'paadslCallback'
propertiesAndroidAppDataStreamsList
    :: Text -- ^ 'paadslParent'
    -> PropertiesAndroidAppDataStreamsList
propertiesAndroidAppDataStreamsList pPaadslParent_ =
  PropertiesAndroidAppDataStreamsList'
    { _paadslParent = pPaadslParent_
    , _paadslXgafv = Nothing
    , _paadslUploadProtocol = Nothing
    , _paadslAccessToken = Nothing
    , _paadslUploadType = Nothing
    , _paadslPageToken = Nothing
    , _paadslPageSize = Nothing
    , _paadslCallback = Nothing
    }


-- | Required. The name of the parent property. For example, to limit results
-- to app streams under the property with Id 123: \"properties\/123\"
paadslParent :: Lens' PropertiesAndroidAppDataStreamsList Text
paadslParent
  = lens _paadslParent (\ s a -> s{_paadslParent = a})

-- | V1 error format.
paadslXgafv :: Lens' PropertiesAndroidAppDataStreamsList (Maybe Xgafv)
paadslXgafv
  = lens _paadslXgafv (\ s a -> s{_paadslXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
paadslUploadProtocol :: Lens' PropertiesAndroidAppDataStreamsList (Maybe Text)
paadslUploadProtocol
  = lens _paadslUploadProtocol
      (\ s a -> s{_paadslUploadProtocol = a})

-- | OAuth access token.
paadslAccessToken :: Lens' PropertiesAndroidAppDataStreamsList (Maybe Text)
paadslAccessToken
  = lens _paadslAccessToken
      (\ s a -> s{_paadslAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
paadslUploadType :: Lens' PropertiesAndroidAppDataStreamsList (Maybe Text)
paadslUploadType
  = lens _paadslUploadType
      (\ s a -> s{_paadslUploadType = a})

-- | A page token, received from a previous call. Provide this to retrieve
-- the subsequent page. When paginating, all other parameters provided to
-- \`ListAndroidAppDataStreams\` must match the call that provided the page
-- token.
paadslPageToken :: Lens' PropertiesAndroidAppDataStreamsList (Maybe Text)
paadslPageToken
  = lens _paadslPageToken
      (\ s a -> s{_paadslPageToken = a})

-- | The maximum number of resources to return. If unspecified, at most 50
-- resources will be returned. The maximum value is 200; (higher values
-- will be coerced to the maximum)
paadslPageSize :: Lens' PropertiesAndroidAppDataStreamsList (Maybe Int32)
paadslPageSize
  = lens _paadslPageSize
      (\ s a -> s{_paadslPageSize = a})
      . mapping _Coerce

-- | JSONP
paadslCallback :: Lens' PropertiesAndroidAppDataStreamsList (Maybe Text)
paadslCallback
  = lens _paadslCallback
      (\ s a -> s{_paadslCallback = a})

instance GoogleRequest
           PropertiesAndroidAppDataStreamsList
         where
        type Rs PropertiesAndroidAppDataStreamsList =
             GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse
        type Scopes PropertiesAndroidAppDataStreamsList =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient
          PropertiesAndroidAppDataStreamsList'{..}
          = go _paadslParent _paadslXgafv _paadslUploadProtocol
              _paadslAccessToken
              _paadslUploadType
              _paadslPageToken
              _paadslPageSize
              _paadslCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesAndroidAppDataStreamsListResource)
                      mempty
