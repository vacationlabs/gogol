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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an iOS app data stream with the specified location and
-- attributes.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.iosAppDataStreams.create@.
module Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Create
    (
    -- * REST Resource
      PropertiesIosAppDataStreamsCreateResource

    -- * Creating a Request
    , propertiesIosAppDataStreamsCreate
    , PropertiesIosAppDataStreamsCreate

    -- * Request Lenses
    , piadscParent
    , piadscXgafv
    , piadscUploadProtocol
    , piadscAccessToken
    , piadscUploadType
    , piadscPayload
    , piadscCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.iosAppDataStreams.create@ method which the
-- 'PropertiesIosAppDataStreamsCreate' request conforms to.
type PropertiesIosAppDataStreamsCreateResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "iosAppDataStreams" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaIosAppDataStream
                         :>
                         Post '[JSON]
                           GoogleAnalyticsAdminV1alphaIosAppDataStream

-- | Creates an iOS app data stream with the specified location and
-- attributes.
--
-- /See:/ 'propertiesIosAppDataStreamsCreate' smart constructor.
data PropertiesIosAppDataStreamsCreate =
  PropertiesIosAppDataStreamsCreate'
    { _piadscParent :: !Text
    , _piadscXgafv :: !(Maybe Xgafv)
    , _piadscUploadProtocol :: !(Maybe Text)
    , _piadscAccessToken :: !(Maybe Text)
    , _piadscUploadType :: !(Maybe Text)
    , _piadscPayload :: !GoogleAnalyticsAdminV1alphaIosAppDataStream
    , _piadscCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesIosAppDataStreamsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piadscParent'
--
-- * 'piadscXgafv'
--
-- * 'piadscUploadProtocol'
--
-- * 'piadscAccessToken'
--
-- * 'piadscUploadType'
--
-- * 'piadscPayload'
--
-- * 'piadscCallback'
propertiesIosAppDataStreamsCreate
    :: Text -- ^ 'piadscParent'
    -> GoogleAnalyticsAdminV1alphaIosAppDataStream -- ^ 'piadscPayload'
    -> PropertiesIosAppDataStreamsCreate
propertiesIosAppDataStreamsCreate pPiadscParent_ pPiadscPayload_ =
  PropertiesIosAppDataStreamsCreate'
    { _piadscParent = pPiadscParent_
    , _piadscXgafv = Nothing
    , _piadscUploadProtocol = Nothing
    , _piadscAccessToken = Nothing
    , _piadscUploadType = Nothing
    , _piadscPayload = pPiadscPayload_
    , _piadscCallback = Nothing
    }


-- | Required. The parent resource where this ios app data stream will be
-- created. Format: properties\/123
piadscParent :: Lens' PropertiesIosAppDataStreamsCreate Text
piadscParent
  = lens _piadscParent (\ s a -> s{_piadscParent = a})

-- | V1 error format.
piadscXgafv :: Lens' PropertiesIosAppDataStreamsCreate (Maybe Xgafv)
piadscXgafv
  = lens _piadscXgafv (\ s a -> s{_piadscXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
piadscUploadProtocol :: Lens' PropertiesIosAppDataStreamsCreate (Maybe Text)
piadscUploadProtocol
  = lens _piadscUploadProtocol
      (\ s a -> s{_piadscUploadProtocol = a})

-- | OAuth access token.
piadscAccessToken :: Lens' PropertiesIosAppDataStreamsCreate (Maybe Text)
piadscAccessToken
  = lens _piadscAccessToken
      (\ s a -> s{_piadscAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
piadscUploadType :: Lens' PropertiesIosAppDataStreamsCreate (Maybe Text)
piadscUploadType
  = lens _piadscUploadType
      (\ s a -> s{_piadscUploadType = a})

-- | Multipart request metadata.
piadscPayload :: Lens' PropertiesIosAppDataStreamsCreate GoogleAnalyticsAdminV1alphaIosAppDataStream
piadscPayload
  = lens _piadscPayload
      (\ s a -> s{_piadscPayload = a})

-- | JSONP
piadscCallback :: Lens' PropertiesIosAppDataStreamsCreate (Maybe Text)
piadscCallback
  = lens _piadscCallback
      (\ s a -> s{_piadscCallback = a})

instance GoogleRequest
           PropertiesIosAppDataStreamsCreate
         where
        type Rs PropertiesIosAppDataStreamsCreate =
             GoogleAnalyticsAdminV1alphaIosAppDataStream
        type Scopes PropertiesIosAppDataStreamsCreate =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesIosAppDataStreamsCreate'{..}
          = go _piadscParent _piadscXgafv _piadscUploadProtocol
              _piadscAccessToken
              _piadscUploadType
              _piadscCallback
              (Just AltJSON)
              _piadscPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesIosAppDataStreamsCreateResource)
                      mempty
