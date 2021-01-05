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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an iOS app stream on a property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.iosAppDataStreams.delete@.
module Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Delete
    (
    -- * REST Resource
      PropertiesIosAppDataStreamsDeleteResource

    -- * Creating a Request
    , propertiesIosAppDataStreamsDelete
    , PropertiesIosAppDataStreamsDelete

    -- * Request Lenses
    , piadsdXgafv
    , piadsdUploadProtocol
    , piadsdAccessToken
    , piadsdUploadType
    , piadsdName
    , piadsdCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.iosAppDataStreams.delete@ method which the
-- 'PropertiesIosAppDataStreamsDelete' request conforms to.
type PropertiesIosAppDataStreamsDeleteResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Delete '[JSON] GoogleProtobufEmpty

-- | Deletes an iOS app stream on a property.
--
-- /See:/ 'propertiesIosAppDataStreamsDelete' smart constructor.
data PropertiesIosAppDataStreamsDelete =
  PropertiesIosAppDataStreamsDelete'
    { _piadsdXgafv :: !(Maybe Xgafv)
    , _piadsdUploadProtocol :: !(Maybe Text)
    , _piadsdAccessToken :: !(Maybe Text)
    , _piadsdUploadType :: !(Maybe Text)
    , _piadsdName :: !Text
    , _piadsdCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesIosAppDataStreamsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piadsdXgafv'
--
-- * 'piadsdUploadProtocol'
--
-- * 'piadsdAccessToken'
--
-- * 'piadsdUploadType'
--
-- * 'piadsdName'
--
-- * 'piadsdCallback'
propertiesIosAppDataStreamsDelete
    :: Text -- ^ 'piadsdName'
    -> PropertiesIosAppDataStreamsDelete
propertiesIosAppDataStreamsDelete pPiadsdName_ =
  PropertiesIosAppDataStreamsDelete'
    { _piadsdXgafv = Nothing
    , _piadsdUploadProtocol = Nothing
    , _piadsdAccessToken = Nothing
    , _piadsdUploadType = Nothing
    , _piadsdName = pPiadsdName_
    , _piadsdCallback = Nothing
    }


-- | V1 error format.
piadsdXgafv :: Lens' PropertiesIosAppDataStreamsDelete (Maybe Xgafv)
piadsdXgafv
  = lens _piadsdXgafv (\ s a -> s{_piadsdXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
piadsdUploadProtocol :: Lens' PropertiesIosAppDataStreamsDelete (Maybe Text)
piadsdUploadProtocol
  = lens _piadsdUploadProtocol
      (\ s a -> s{_piadsdUploadProtocol = a})

-- | OAuth access token.
piadsdAccessToken :: Lens' PropertiesIosAppDataStreamsDelete (Maybe Text)
piadsdAccessToken
  = lens _piadsdAccessToken
      (\ s a -> s{_piadsdAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
piadsdUploadType :: Lens' PropertiesIosAppDataStreamsDelete (Maybe Text)
piadsdUploadType
  = lens _piadsdUploadType
      (\ s a -> s{_piadsdUploadType = a})

-- | Required. The name of the iOS app data stream to delete. Format:
-- properties\/{property_id}\/iosAppDataStreams\/{stream_id} Example:
-- \"properties\/123\/iosAppDataStreams\/456\"
piadsdName :: Lens' PropertiesIosAppDataStreamsDelete Text
piadsdName
  = lens _piadsdName (\ s a -> s{_piadsdName = a})

-- | JSONP
piadsdCallback :: Lens' PropertiesIosAppDataStreamsDelete (Maybe Text)
piadsdCallback
  = lens _piadsdCallback
      (\ s a -> s{_piadsdCallback = a})

instance GoogleRequest
           PropertiesIosAppDataStreamsDelete
         where
        type Rs PropertiesIosAppDataStreamsDelete =
             GoogleProtobufEmpty
        type Scopes PropertiesIosAppDataStreamsDelete =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesIosAppDataStreamsDelete'{..}
          = go _piadsdName _piadsdXgafv _piadsdUploadProtocol
              _piadsdAccessToken
              _piadsdUploadType
              _piadsdCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesIosAppDataStreamsDeleteResource)
                      mempty
