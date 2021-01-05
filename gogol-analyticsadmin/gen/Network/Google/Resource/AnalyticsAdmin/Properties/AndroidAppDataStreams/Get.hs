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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lookup for a single AndroidAppDataStream
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.androidAppDataStreams.get@.
module Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Get
    (
    -- * REST Resource
      PropertiesAndroidAppDataStreamsGetResource

    -- * Creating a Request
    , propertiesAndroidAppDataStreamsGet
    , PropertiesAndroidAppDataStreamsGet

    -- * Request Lenses
    , paadsgXgafv
    , paadsgUploadProtocol
    , paadsgAccessToken
    , paadsgUploadType
    , paadsgName
    , paadsgCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.androidAppDataStreams.get@ method which the
-- 'PropertiesAndroidAppDataStreamsGet' request conforms to.
type PropertiesAndroidAppDataStreamsGetResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON]
                       GoogleAnalyticsAdminV1alphaAndroidAppDataStream

-- | Lookup for a single AndroidAppDataStream
--
-- /See:/ 'propertiesAndroidAppDataStreamsGet' smart constructor.
data PropertiesAndroidAppDataStreamsGet =
  PropertiesAndroidAppDataStreamsGet'
    { _paadsgXgafv :: !(Maybe Xgafv)
    , _paadsgUploadProtocol :: !(Maybe Text)
    , _paadsgAccessToken :: !(Maybe Text)
    , _paadsgUploadType :: !(Maybe Text)
    , _paadsgName :: !Text
    , _paadsgCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesAndroidAppDataStreamsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paadsgXgafv'
--
-- * 'paadsgUploadProtocol'
--
-- * 'paadsgAccessToken'
--
-- * 'paadsgUploadType'
--
-- * 'paadsgName'
--
-- * 'paadsgCallback'
propertiesAndroidAppDataStreamsGet
    :: Text -- ^ 'paadsgName'
    -> PropertiesAndroidAppDataStreamsGet
propertiesAndroidAppDataStreamsGet pPaadsgName_ =
  PropertiesAndroidAppDataStreamsGet'
    { _paadsgXgafv = Nothing
    , _paadsgUploadProtocol = Nothing
    , _paadsgAccessToken = Nothing
    , _paadsgUploadType = Nothing
    , _paadsgName = pPaadsgName_
    , _paadsgCallback = Nothing
    }


-- | V1 error format.
paadsgXgafv :: Lens' PropertiesAndroidAppDataStreamsGet (Maybe Xgafv)
paadsgXgafv
  = lens _paadsgXgafv (\ s a -> s{_paadsgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
paadsgUploadProtocol :: Lens' PropertiesAndroidAppDataStreamsGet (Maybe Text)
paadsgUploadProtocol
  = lens _paadsgUploadProtocol
      (\ s a -> s{_paadsgUploadProtocol = a})

-- | OAuth access token.
paadsgAccessToken :: Lens' PropertiesAndroidAppDataStreamsGet (Maybe Text)
paadsgAccessToken
  = lens _paadsgAccessToken
      (\ s a -> s{_paadsgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
paadsgUploadType :: Lens' PropertiesAndroidAppDataStreamsGet (Maybe Text)
paadsgUploadType
  = lens _paadsgUploadType
      (\ s a -> s{_paadsgUploadType = a})

-- | Required. The name of the android app data stream to lookup. Format:
-- properties\/{property_id}\/androidAppDataStreams\/{stream_id} Example:
-- \"properties\/123\/androidAppDataStreams\/456\"
paadsgName :: Lens' PropertiesAndroidAppDataStreamsGet Text
paadsgName
  = lens _paadsgName (\ s a -> s{_paadsgName = a})

-- | JSONP
paadsgCallback :: Lens' PropertiesAndroidAppDataStreamsGet (Maybe Text)
paadsgCallback
  = lens _paadsgCallback
      (\ s a -> s{_paadsgCallback = a})

instance GoogleRequest
           PropertiesAndroidAppDataStreamsGet
         where
        type Rs PropertiesAndroidAppDataStreamsGet =
             GoogleAnalyticsAdminV1alphaAndroidAppDataStream
        type Scopes PropertiesAndroidAppDataStreamsGet =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient PropertiesAndroidAppDataStreamsGet'{..}
          = go _paadsgName _paadsgXgafv _paadsgUploadProtocol
              _paadsgAccessToken
              _paadsgUploadType
              _paadsgCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesAndroidAppDataStreamsGetResource)
                      mempty
