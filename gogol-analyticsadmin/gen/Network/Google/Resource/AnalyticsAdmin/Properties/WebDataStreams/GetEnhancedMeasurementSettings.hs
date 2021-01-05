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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.GetEnhancedMeasurementSettings
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the singleton enhanced measurement settings for this web stream.
-- Note that the stream must enable enhanced measurement for these settings
-- to take effect.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.webDataStreams.getEnhancedMeasurementSettings@.
module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.GetEnhancedMeasurementSettings
    (
    -- * REST Resource
      PropertiesWebDataStreamsGetEnhancedMeasurementSettingsResource

    -- * Creating a Request
    , propertiesWebDataStreamsGetEnhancedMeasurementSettings
    , PropertiesWebDataStreamsGetEnhancedMeasurementSettings

    -- * Request Lenses
    , pwdsgemsXgafv
    , pwdsgemsUploadProtocol
    , pwdsgemsAccessToken
    , pwdsgemsUploadType
    , pwdsgemsName
    , pwdsgemsCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.webDataStreams.getEnhancedMeasurementSettings@ method which the
-- 'PropertiesWebDataStreamsGetEnhancedMeasurementSettings' request conforms to.
type PropertiesWebDataStreamsGetEnhancedMeasurementSettingsResource
     =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON]
                       GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings

-- | Returns the singleton enhanced measurement settings for this web stream.
-- Note that the stream must enable enhanced measurement for these settings
-- to take effect.
--
-- /See:/ 'propertiesWebDataStreamsGetEnhancedMeasurementSettings' smart constructor.
data PropertiesWebDataStreamsGetEnhancedMeasurementSettings =
  PropertiesWebDataStreamsGetEnhancedMeasurementSettings'
    { _pwdsgemsXgafv :: !(Maybe Xgafv)
    , _pwdsgemsUploadProtocol :: !(Maybe Text)
    , _pwdsgemsAccessToken :: !(Maybe Text)
    , _pwdsgemsUploadType :: !(Maybe Text)
    , _pwdsgemsName :: !Text
    , _pwdsgemsCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesWebDataStreamsGetEnhancedMeasurementSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwdsgemsXgafv'
--
-- * 'pwdsgemsUploadProtocol'
--
-- * 'pwdsgemsAccessToken'
--
-- * 'pwdsgemsUploadType'
--
-- * 'pwdsgemsName'
--
-- * 'pwdsgemsCallback'
propertiesWebDataStreamsGetEnhancedMeasurementSettings
    :: Text -- ^ 'pwdsgemsName'
    -> PropertiesWebDataStreamsGetEnhancedMeasurementSettings
propertiesWebDataStreamsGetEnhancedMeasurementSettings pPwdsgemsName_ =
  PropertiesWebDataStreamsGetEnhancedMeasurementSettings'
    { _pwdsgemsXgafv = Nothing
    , _pwdsgemsUploadProtocol = Nothing
    , _pwdsgemsAccessToken = Nothing
    , _pwdsgemsUploadType = Nothing
    , _pwdsgemsName = pPwdsgemsName_
    , _pwdsgemsCallback = Nothing
    }


-- | V1 error format.
pwdsgemsXgafv :: Lens' PropertiesWebDataStreamsGetEnhancedMeasurementSettings (Maybe Xgafv)
pwdsgemsXgafv
  = lens _pwdsgemsXgafv
      (\ s a -> s{_pwdsgemsXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pwdsgemsUploadProtocol :: Lens' PropertiesWebDataStreamsGetEnhancedMeasurementSettings (Maybe Text)
pwdsgemsUploadProtocol
  = lens _pwdsgemsUploadProtocol
      (\ s a -> s{_pwdsgemsUploadProtocol = a})

-- | OAuth access token.
pwdsgemsAccessToken :: Lens' PropertiesWebDataStreamsGetEnhancedMeasurementSettings (Maybe Text)
pwdsgemsAccessToken
  = lens _pwdsgemsAccessToken
      (\ s a -> s{_pwdsgemsAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pwdsgemsUploadType :: Lens' PropertiesWebDataStreamsGetEnhancedMeasurementSettings (Maybe Text)
pwdsgemsUploadType
  = lens _pwdsgemsUploadType
      (\ s a -> s{_pwdsgemsUploadType = a})

-- | Required. The name of the settings to lookup. Format:
-- properties\/{property_id}\/webDataStreams\/{stream_id}\/enhancedMeasurementSettings
-- Example:
-- \"properties\/1000\/webDataStreams\/2000\/enhancedMeasurementSettings\"
pwdsgemsName :: Lens' PropertiesWebDataStreamsGetEnhancedMeasurementSettings Text
pwdsgemsName
  = lens _pwdsgemsName (\ s a -> s{_pwdsgemsName = a})

-- | JSONP
pwdsgemsCallback :: Lens' PropertiesWebDataStreamsGetEnhancedMeasurementSettings (Maybe Text)
pwdsgemsCallback
  = lens _pwdsgemsCallback
      (\ s a -> s{_pwdsgemsCallback = a})

instance GoogleRequest
           PropertiesWebDataStreamsGetEnhancedMeasurementSettings
         where
        type Rs
               PropertiesWebDataStreamsGetEnhancedMeasurementSettings
             =
             GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings
        type Scopes
               PropertiesWebDataStreamsGetEnhancedMeasurementSettings
             =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient
          PropertiesWebDataStreamsGetEnhancedMeasurementSettings'{..}
          = go _pwdsgemsName _pwdsgemsXgafv
              _pwdsgemsUploadProtocol
              _pwdsgemsAccessToken
              _pwdsgemsUploadType
              _pwdsgemsCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           PropertiesWebDataStreamsGetEnhancedMeasurementSettingsResource)
                      mempty
