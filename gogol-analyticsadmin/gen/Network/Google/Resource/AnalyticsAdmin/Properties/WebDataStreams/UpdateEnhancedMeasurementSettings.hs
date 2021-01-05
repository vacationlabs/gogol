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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.UpdateEnhancedMeasurementSettings
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the singleton enhanced measurement settings for this web stream.
-- Note that the stream must enable enhanced measurement for these settings
-- to take effect.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.webDataStreams.updateEnhancedMeasurementSettings@.
module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.UpdateEnhancedMeasurementSettings
    (
    -- * REST Resource
      PropertiesWebDataStreamsUpdateEnhancedMeasurementSettingsResource

    -- * Creating a Request
    , propertiesWebDataStreamsUpdateEnhancedMeasurementSettings
    , PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings

    -- * Request Lenses
    , pwdsuemsXgafv
    , pwdsuemsUploadProtocol
    , pwdsuemsUpdateMask
    , pwdsuemsAccessToken
    , pwdsuemsUploadType
    , pwdsuemsPayload
    , pwdsuemsName
    , pwdsuemsCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.webDataStreams.updateEnhancedMeasurementSettings@ method which the
-- 'PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings' request conforms to.
type PropertiesWebDataStreamsUpdateEnhancedMeasurementSettingsResource
     =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "updateMask" GFieldMask :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings
                         :>
                         Patch '[JSON]
                           GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings

-- | Updates the singleton enhanced measurement settings for this web stream.
-- Note that the stream must enable enhanced measurement for these settings
-- to take effect.
--
-- /See:/ 'propertiesWebDataStreamsUpdateEnhancedMeasurementSettings' smart constructor.
data PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings =
  PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings'
    { _pwdsuemsXgafv :: !(Maybe Xgafv)
    , _pwdsuemsUploadProtocol :: !(Maybe Text)
    , _pwdsuemsUpdateMask :: !(Maybe GFieldMask)
    , _pwdsuemsAccessToken :: !(Maybe Text)
    , _pwdsuemsUploadType :: !(Maybe Text)
    , _pwdsuemsPayload :: !GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings
    , _pwdsuemsName :: !Text
    , _pwdsuemsCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwdsuemsXgafv'
--
-- * 'pwdsuemsUploadProtocol'
--
-- * 'pwdsuemsUpdateMask'
--
-- * 'pwdsuemsAccessToken'
--
-- * 'pwdsuemsUploadType'
--
-- * 'pwdsuemsPayload'
--
-- * 'pwdsuemsName'
--
-- * 'pwdsuemsCallback'
propertiesWebDataStreamsUpdateEnhancedMeasurementSettings
    :: GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings -- ^ 'pwdsuemsPayload'
    -> Text -- ^ 'pwdsuemsName'
    -> PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings
propertiesWebDataStreamsUpdateEnhancedMeasurementSettings pPwdsuemsPayload_ pPwdsuemsName_ =
  PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings'
    { _pwdsuemsXgafv = Nothing
    , _pwdsuemsUploadProtocol = Nothing
    , _pwdsuemsUpdateMask = Nothing
    , _pwdsuemsAccessToken = Nothing
    , _pwdsuemsUploadType = Nothing
    , _pwdsuemsPayload = pPwdsuemsPayload_
    , _pwdsuemsName = pPwdsuemsName_
    , _pwdsuemsCallback = Nothing
    }


-- | V1 error format.
pwdsuemsXgafv :: Lens' PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings (Maybe Xgafv)
pwdsuemsXgafv
  = lens _pwdsuemsXgafv
      (\ s a -> s{_pwdsuemsXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pwdsuemsUploadProtocol :: Lens' PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings (Maybe Text)
pwdsuemsUploadProtocol
  = lens _pwdsuemsUploadProtocol
      (\ s a -> s{_pwdsuemsUploadProtocol = a})

-- | Required. The list of fields to be updated. Omitted fields will not be
-- updated. To replace the entire entity, use one path with the string
-- \"*\" to match all fields.
pwdsuemsUpdateMask :: Lens' PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings (Maybe GFieldMask)
pwdsuemsUpdateMask
  = lens _pwdsuemsUpdateMask
      (\ s a -> s{_pwdsuemsUpdateMask = a})

-- | OAuth access token.
pwdsuemsAccessToken :: Lens' PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings (Maybe Text)
pwdsuemsAccessToken
  = lens _pwdsuemsAccessToken
      (\ s a -> s{_pwdsuemsAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pwdsuemsUploadType :: Lens' PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings (Maybe Text)
pwdsuemsUploadType
  = lens _pwdsuemsUploadType
      (\ s a -> s{_pwdsuemsUploadType = a})

-- | Multipart request metadata.
pwdsuemsPayload :: Lens' PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings
pwdsuemsPayload
  = lens _pwdsuemsPayload
      (\ s a -> s{_pwdsuemsPayload = a})

-- | Output only. Resource name of this Data Stream. Format:
-- properties\/{property_id}\/webDataStreams\/{stream_id}\/enhancedMeasurementSettings
-- Example:
-- \"properties\/1000\/webDataStreams\/2000\/enhancedMeasurementSettings\"
pwdsuemsName :: Lens' PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings Text
pwdsuemsName
  = lens _pwdsuemsName (\ s a -> s{_pwdsuemsName = a})

-- | JSONP
pwdsuemsCallback :: Lens' PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings (Maybe Text)
pwdsuemsCallback
  = lens _pwdsuemsCallback
      (\ s a -> s{_pwdsuemsCallback = a})

instance GoogleRequest
           PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings
         where
        type Rs
               PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings
             =
             GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings
        type Scopes
               PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings
             = '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient
          PropertiesWebDataStreamsUpdateEnhancedMeasurementSettings'{..}
          = go _pwdsuemsName _pwdsuemsXgafv
              _pwdsuemsUploadProtocol
              _pwdsuemsUpdateMask
              _pwdsuemsAccessToken
              _pwdsuemsUploadType
              _pwdsuemsCallback
              (Just AltJSON)
              _pwdsuemsPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           PropertiesWebDataStreamsUpdateEnhancedMeasurementSettingsResource)
                      mempty
