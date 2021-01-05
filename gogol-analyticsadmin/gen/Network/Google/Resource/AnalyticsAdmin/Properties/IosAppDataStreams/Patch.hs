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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an iOS app stream on a property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.iosAppDataStreams.patch@.
module Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Patch
    (
    -- * REST Resource
      PropertiesIosAppDataStreamsPatchResource

    -- * Creating a Request
    , propertiesIosAppDataStreamsPatch
    , PropertiesIosAppDataStreamsPatch

    -- * Request Lenses
    , piadspXgafv
    , piadspUploadProtocol
    , piadspUpdateMask
    , piadspAccessToken
    , piadspUploadType
    , piadspPayload
    , piadspName
    , piadspCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.iosAppDataStreams.patch@ method which the
-- 'PropertiesIosAppDataStreamsPatch' request conforms to.
type PropertiesIosAppDataStreamsPatchResource =
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
                         GoogleAnalyticsAdminV1alphaIosAppDataStream
                         :>
                         Patch '[JSON]
                           GoogleAnalyticsAdminV1alphaIosAppDataStream

-- | Updates an iOS app stream on a property.
--
-- /See:/ 'propertiesIosAppDataStreamsPatch' smart constructor.
data PropertiesIosAppDataStreamsPatch =
  PropertiesIosAppDataStreamsPatch'
    { _piadspXgafv :: !(Maybe Xgafv)
    , _piadspUploadProtocol :: !(Maybe Text)
    , _piadspUpdateMask :: !(Maybe GFieldMask)
    , _piadspAccessToken :: !(Maybe Text)
    , _piadspUploadType :: !(Maybe Text)
    , _piadspPayload :: !GoogleAnalyticsAdminV1alphaIosAppDataStream
    , _piadspName :: !Text
    , _piadspCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesIosAppDataStreamsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piadspXgafv'
--
-- * 'piadspUploadProtocol'
--
-- * 'piadspUpdateMask'
--
-- * 'piadspAccessToken'
--
-- * 'piadspUploadType'
--
-- * 'piadspPayload'
--
-- * 'piadspName'
--
-- * 'piadspCallback'
propertiesIosAppDataStreamsPatch
    :: GoogleAnalyticsAdminV1alphaIosAppDataStream -- ^ 'piadspPayload'
    -> Text -- ^ 'piadspName'
    -> PropertiesIosAppDataStreamsPatch
propertiesIosAppDataStreamsPatch pPiadspPayload_ pPiadspName_ =
  PropertiesIosAppDataStreamsPatch'
    { _piadspXgafv = Nothing
    , _piadspUploadProtocol = Nothing
    , _piadspUpdateMask = Nothing
    , _piadspAccessToken = Nothing
    , _piadspUploadType = Nothing
    , _piadspPayload = pPiadspPayload_
    , _piadspName = pPiadspName_
    , _piadspCallback = Nothing
    }


-- | V1 error format.
piadspXgafv :: Lens' PropertiesIosAppDataStreamsPatch (Maybe Xgafv)
piadspXgafv
  = lens _piadspXgafv (\ s a -> s{_piadspXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
piadspUploadProtocol :: Lens' PropertiesIosAppDataStreamsPatch (Maybe Text)
piadspUploadProtocol
  = lens _piadspUploadProtocol
      (\ s a -> s{_piadspUploadProtocol = a})

-- | Required. The list of fields to be updated. Omitted fields will not be
-- updated. To replace the entire entity, use one path with the string
-- \"*\" to match all fields.
piadspUpdateMask :: Lens' PropertiesIosAppDataStreamsPatch (Maybe GFieldMask)
piadspUpdateMask
  = lens _piadspUpdateMask
      (\ s a -> s{_piadspUpdateMask = a})

-- | OAuth access token.
piadspAccessToken :: Lens' PropertiesIosAppDataStreamsPatch (Maybe Text)
piadspAccessToken
  = lens _piadspAccessToken
      (\ s a -> s{_piadspAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
piadspUploadType :: Lens' PropertiesIosAppDataStreamsPatch (Maybe Text)
piadspUploadType
  = lens _piadspUploadType
      (\ s a -> s{_piadspUploadType = a})

-- | Multipart request metadata.
piadspPayload :: Lens' PropertiesIosAppDataStreamsPatch GoogleAnalyticsAdminV1alphaIosAppDataStream
piadspPayload
  = lens _piadspPayload
      (\ s a -> s{_piadspPayload = a})

-- | Output only. Resource name of this Data Stream. Format:
-- properties\/{property_id}\/iosAppDataStreams\/{stream_id} Example:
-- \"properties\/1000\/iosAppDataStreams\/2000\"
piadspName :: Lens' PropertiesIosAppDataStreamsPatch Text
piadspName
  = lens _piadspName (\ s a -> s{_piadspName = a})

-- | JSONP
piadspCallback :: Lens' PropertiesIosAppDataStreamsPatch (Maybe Text)
piadspCallback
  = lens _piadspCallback
      (\ s a -> s{_piadspCallback = a})

instance GoogleRequest
           PropertiesIosAppDataStreamsPatch
         where
        type Rs PropertiesIosAppDataStreamsPatch =
             GoogleAnalyticsAdminV1alphaIosAppDataStream
        type Scopes PropertiesIosAppDataStreamsPatch =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesIosAppDataStreamsPatch'{..}
          = go _piadspName _piadspXgafv _piadspUploadProtocol
              _piadspUpdateMask
              _piadspAccessToken
              _piadspUploadType
              _piadspCallback
              (Just AltJSON)
              _piadspPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesIosAppDataStreamsPatchResource)
                      mempty
