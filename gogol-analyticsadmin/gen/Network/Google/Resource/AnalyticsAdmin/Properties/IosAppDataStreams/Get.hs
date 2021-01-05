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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lookup for a single IosAppDataStream
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.iosAppDataStreams.get@.
module Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Get
    (
    -- * REST Resource
      PropertiesIosAppDataStreamsGetResource

    -- * Creating a Request
    , propertiesIosAppDataStreamsGet
    , PropertiesIosAppDataStreamsGet

    -- * Request Lenses
    , piadsgXgafv
    , piadsgUploadProtocol
    , piadsgAccessToken
    , piadsgUploadType
    , piadsgName
    , piadsgCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.iosAppDataStreams.get@ method which the
-- 'PropertiesIosAppDataStreamsGet' request conforms to.
type PropertiesIosAppDataStreamsGetResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON]
                       GoogleAnalyticsAdminV1alphaIosAppDataStream

-- | Lookup for a single IosAppDataStream
--
-- /See:/ 'propertiesIosAppDataStreamsGet' smart constructor.
data PropertiesIosAppDataStreamsGet =
  PropertiesIosAppDataStreamsGet'
    { _piadsgXgafv :: !(Maybe Xgafv)
    , _piadsgUploadProtocol :: !(Maybe Text)
    , _piadsgAccessToken :: !(Maybe Text)
    , _piadsgUploadType :: !(Maybe Text)
    , _piadsgName :: !Text
    , _piadsgCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesIosAppDataStreamsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piadsgXgafv'
--
-- * 'piadsgUploadProtocol'
--
-- * 'piadsgAccessToken'
--
-- * 'piadsgUploadType'
--
-- * 'piadsgName'
--
-- * 'piadsgCallback'
propertiesIosAppDataStreamsGet
    :: Text -- ^ 'piadsgName'
    -> PropertiesIosAppDataStreamsGet
propertiesIosAppDataStreamsGet pPiadsgName_ =
  PropertiesIosAppDataStreamsGet'
    { _piadsgXgafv = Nothing
    , _piadsgUploadProtocol = Nothing
    , _piadsgAccessToken = Nothing
    , _piadsgUploadType = Nothing
    , _piadsgName = pPiadsgName_
    , _piadsgCallback = Nothing
    }


-- | V1 error format.
piadsgXgafv :: Lens' PropertiesIosAppDataStreamsGet (Maybe Xgafv)
piadsgXgafv
  = lens _piadsgXgafv (\ s a -> s{_piadsgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
piadsgUploadProtocol :: Lens' PropertiesIosAppDataStreamsGet (Maybe Text)
piadsgUploadProtocol
  = lens _piadsgUploadProtocol
      (\ s a -> s{_piadsgUploadProtocol = a})

-- | OAuth access token.
piadsgAccessToken :: Lens' PropertiesIosAppDataStreamsGet (Maybe Text)
piadsgAccessToken
  = lens _piadsgAccessToken
      (\ s a -> s{_piadsgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
piadsgUploadType :: Lens' PropertiesIosAppDataStreamsGet (Maybe Text)
piadsgUploadType
  = lens _piadsgUploadType
      (\ s a -> s{_piadsgUploadType = a})

-- | Required. The name of the iOS app data stream to lookup. Format:
-- properties\/{property_id}\/iosAppDataStreams\/{stream_id} Example:
-- \"properties\/123\/iosAppDataStreams\/456\"
piadsgName :: Lens' PropertiesIosAppDataStreamsGet Text
piadsgName
  = lens _piadsgName (\ s a -> s{_piadsgName = a})

-- | JSONP
piadsgCallback :: Lens' PropertiesIosAppDataStreamsGet (Maybe Text)
piadsgCallback
  = lens _piadsgCallback
      (\ s a -> s{_piadsgCallback = a})

instance GoogleRequest PropertiesIosAppDataStreamsGet
         where
        type Rs PropertiesIosAppDataStreamsGet =
             GoogleAnalyticsAdminV1alphaIosAppDataStream
        type Scopes PropertiesIosAppDataStreamsGet =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient PropertiesIosAppDataStreamsGet'{..}
          = go _piadsgName _piadsgXgafv _piadsgUploadProtocol
              _piadsgAccessToken
              _piadsgUploadType
              _piadsgCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesIosAppDataStreamsGetResource)
                      mempty
