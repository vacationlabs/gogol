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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an android app stream on a property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.androidAppDataStreams.delete@.
module Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Delete
    (
    -- * REST Resource
      PropertiesAndroidAppDataStreamsDeleteResource

    -- * Creating a Request
    , propertiesAndroidAppDataStreamsDelete
    , PropertiesAndroidAppDataStreamsDelete

    -- * Request Lenses
    , paadsdXgafv
    , paadsdUploadProtocol
    , paadsdAccessToken
    , paadsdUploadType
    , paadsdName
    , paadsdCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.androidAppDataStreams.delete@ method which the
-- 'PropertiesAndroidAppDataStreamsDelete' request conforms to.
type PropertiesAndroidAppDataStreamsDeleteResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Delete '[JSON] GoogleProtobufEmpty

-- | Deletes an android app stream on a property.
--
-- /See:/ 'propertiesAndroidAppDataStreamsDelete' smart constructor.
data PropertiesAndroidAppDataStreamsDelete =
  PropertiesAndroidAppDataStreamsDelete'
    { _paadsdXgafv :: !(Maybe Xgafv)
    , _paadsdUploadProtocol :: !(Maybe Text)
    , _paadsdAccessToken :: !(Maybe Text)
    , _paadsdUploadType :: !(Maybe Text)
    , _paadsdName :: !Text
    , _paadsdCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesAndroidAppDataStreamsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paadsdXgafv'
--
-- * 'paadsdUploadProtocol'
--
-- * 'paadsdAccessToken'
--
-- * 'paadsdUploadType'
--
-- * 'paadsdName'
--
-- * 'paadsdCallback'
propertiesAndroidAppDataStreamsDelete
    :: Text -- ^ 'paadsdName'
    -> PropertiesAndroidAppDataStreamsDelete
propertiesAndroidAppDataStreamsDelete pPaadsdName_ =
  PropertiesAndroidAppDataStreamsDelete'
    { _paadsdXgafv = Nothing
    , _paadsdUploadProtocol = Nothing
    , _paadsdAccessToken = Nothing
    , _paadsdUploadType = Nothing
    , _paadsdName = pPaadsdName_
    , _paadsdCallback = Nothing
    }


-- | V1 error format.
paadsdXgafv :: Lens' PropertiesAndroidAppDataStreamsDelete (Maybe Xgafv)
paadsdXgafv
  = lens _paadsdXgafv (\ s a -> s{_paadsdXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
paadsdUploadProtocol :: Lens' PropertiesAndroidAppDataStreamsDelete (Maybe Text)
paadsdUploadProtocol
  = lens _paadsdUploadProtocol
      (\ s a -> s{_paadsdUploadProtocol = a})

-- | OAuth access token.
paadsdAccessToken :: Lens' PropertiesAndroidAppDataStreamsDelete (Maybe Text)
paadsdAccessToken
  = lens _paadsdAccessToken
      (\ s a -> s{_paadsdAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
paadsdUploadType :: Lens' PropertiesAndroidAppDataStreamsDelete (Maybe Text)
paadsdUploadType
  = lens _paadsdUploadType
      (\ s a -> s{_paadsdUploadType = a})

-- | Required. The name of the android app data stream to delete. Format:
-- properties\/{property_id}\/androidAppDataStreams\/{stream_id} Example:
-- \"properties\/123\/androidAppDataStreams\/456\"
paadsdName :: Lens' PropertiesAndroidAppDataStreamsDelete Text
paadsdName
  = lens _paadsdName (\ s a -> s{_paadsdName = a})

-- | JSONP
paadsdCallback :: Lens' PropertiesAndroidAppDataStreamsDelete (Maybe Text)
paadsdCallback
  = lens _paadsdCallback
      (\ s a -> s{_paadsdCallback = a})

instance GoogleRequest
           PropertiesAndroidAppDataStreamsDelete
         where
        type Rs PropertiesAndroidAppDataStreamsDelete =
             GoogleProtobufEmpty
        type Scopes PropertiesAndroidAppDataStreamsDelete =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient
          PropertiesAndroidAppDataStreamsDelete'{..}
          = go _paadsdName _paadsdXgafv _paadsdUploadProtocol
              _paadsdAccessToken
              _paadsdUploadType
              _paadsdCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesAndroidAppDataStreamsDeleteResource)
                      mempty
