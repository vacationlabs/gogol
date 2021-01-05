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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an android app stream on a property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.androidAppDataStreams.patch@.
module Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Patch
    (
    -- * REST Resource
      PropertiesAndroidAppDataStreamsPatchResource

    -- * Creating a Request
    , propertiesAndroidAppDataStreamsPatch
    , PropertiesAndroidAppDataStreamsPatch

    -- * Request Lenses
    , paadspXgafv
    , paadspUploadProtocol
    , paadspUpdateMask
    , paadspAccessToken
    , paadspUploadType
    , paadspPayload
    , paadspName
    , paadspCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.androidAppDataStreams.patch@ method which the
-- 'PropertiesAndroidAppDataStreamsPatch' request conforms to.
type PropertiesAndroidAppDataStreamsPatchResource =
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
                         GoogleAnalyticsAdminV1alphaAndroidAppDataStream
                         :>
                         Patch '[JSON]
                           GoogleAnalyticsAdminV1alphaAndroidAppDataStream

-- | Updates an android app stream on a property.
--
-- /See:/ 'propertiesAndroidAppDataStreamsPatch' smart constructor.
data PropertiesAndroidAppDataStreamsPatch =
  PropertiesAndroidAppDataStreamsPatch'
    { _paadspXgafv :: !(Maybe Xgafv)
    , _paadspUploadProtocol :: !(Maybe Text)
    , _paadspUpdateMask :: !(Maybe GFieldMask)
    , _paadspAccessToken :: !(Maybe Text)
    , _paadspUploadType :: !(Maybe Text)
    , _paadspPayload :: !GoogleAnalyticsAdminV1alphaAndroidAppDataStream
    , _paadspName :: !Text
    , _paadspCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesAndroidAppDataStreamsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paadspXgafv'
--
-- * 'paadspUploadProtocol'
--
-- * 'paadspUpdateMask'
--
-- * 'paadspAccessToken'
--
-- * 'paadspUploadType'
--
-- * 'paadspPayload'
--
-- * 'paadspName'
--
-- * 'paadspCallback'
propertiesAndroidAppDataStreamsPatch
    :: GoogleAnalyticsAdminV1alphaAndroidAppDataStream -- ^ 'paadspPayload'
    -> Text -- ^ 'paadspName'
    -> PropertiesAndroidAppDataStreamsPatch
propertiesAndroidAppDataStreamsPatch pPaadspPayload_ pPaadspName_ =
  PropertiesAndroidAppDataStreamsPatch'
    { _paadspXgafv = Nothing
    , _paadspUploadProtocol = Nothing
    , _paadspUpdateMask = Nothing
    , _paadspAccessToken = Nothing
    , _paadspUploadType = Nothing
    , _paadspPayload = pPaadspPayload_
    , _paadspName = pPaadspName_
    , _paadspCallback = Nothing
    }


-- | V1 error format.
paadspXgafv :: Lens' PropertiesAndroidAppDataStreamsPatch (Maybe Xgafv)
paadspXgafv
  = lens _paadspXgafv (\ s a -> s{_paadspXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
paadspUploadProtocol :: Lens' PropertiesAndroidAppDataStreamsPatch (Maybe Text)
paadspUploadProtocol
  = lens _paadspUploadProtocol
      (\ s a -> s{_paadspUploadProtocol = a})

-- | Required. The list of fields to be updated. Omitted fields will not be
-- updated. To replace the entire entity, use one path with the string
-- \"*\" to match all fields.
paadspUpdateMask :: Lens' PropertiesAndroidAppDataStreamsPatch (Maybe GFieldMask)
paadspUpdateMask
  = lens _paadspUpdateMask
      (\ s a -> s{_paadspUpdateMask = a})

-- | OAuth access token.
paadspAccessToken :: Lens' PropertiesAndroidAppDataStreamsPatch (Maybe Text)
paadspAccessToken
  = lens _paadspAccessToken
      (\ s a -> s{_paadspAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
paadspUploadType :: Lens' PropertiesAndroidAppDataStreamsPatch (Maybe Text)
paadspUploadType
  = lens _paadspUploadType
      (\ s a -> s{_paadspUploadType = a})

-- | Multipart request metadata.
paadspPayload :: Lens' PropertiesAndroidAppDataStreamsPatch GoogleAnalyticsAdminV1alphaAndroidAppDataStream
paadspPayload
  = lens _paadspPayload
      (\ s a -> s{_paadspPayload = a})

-- | Output only. Resource name of this Data Stream. Format:
-- properties\/{property_id}\/androidAppDataStreams\/{stream_id} Example:
-- \"properties\/1000\/androidAppDataStreams\/2000\"
paadspName :: Lens' PropertiesAndroidAppDataStreamsPatch Text
paadspName
  = lens _paadspName (\ s a -> s{_paadspName = a})

-- | JSONP
paadspCallback :: Lens' PropertiesAndroidAppDataStreamsPatch (Maybe Text)
paadspCallback
  = lens _paadspCallback
      (\ s a -> s{_paadspCallback = a})

instance GoogleRequest
           PropertiesAndroidAppDataStreamsPatch
         where
        type Rs PropertiesAndroidAppDataStreamsPatch =
             GoogleAnalyticsAdminV1alphaAndroidAppDataStream
        type Scopes PropertiesAndroidAppDataStreamsPatch =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient
          PropertiesAndroidAppDataStreamsPatch'{..}
          = go _paadspName _paadspXgafv _paadspUploadProtocol
              _paadspUpdateMask
              _paadspAccessToken
              _paadspUploadType
              _paadspCallback
              (Just AltJSON)
              _paadspPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesAndroidAppDataStreamsPatchResource)
                      mempty
