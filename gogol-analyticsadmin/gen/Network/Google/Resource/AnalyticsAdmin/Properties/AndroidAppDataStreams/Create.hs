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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an android app stream with the specified location and
-- attributes.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.androidAppDataStreams.create@.
module Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Create
    (
    -- * REST Resource
      PropertiesAndroidAppDataStreamsCreateResource

    -- * Creating a Request
    , propertiesAndroidAppDataStreamsCreate
    , PropertiesAndroidAppDataStreamsCreate

    -- * Request Lenses
    , paadscParent
    , paadscXgafv
    , paadscUploadProtocol
    , paadscAccessToken
    , paadscUploadType
    , paadscPayload
    , paadscCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.androidAppDataStreams.create@ method which the
-- 'PropertiesAndroidAppDataStreamsCreate' request conforms to.
type PropertiesAndroidAppDataStreamsCreateResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "androidAppDataStreams" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaAndroidAppDataStream
                         :>
                         Post '[JSON]
                           GoogleAnalyticsAdminV1alphaAndroidAppDataStream

-- | Creates an android app stream with the specified location and
-- attributes.
--
-- /See:/ 'propertiesAndroidAppDataStreamsCreate' smart constructor.
data PropertiesAndroidAppDataStreamsCreate =
  PropertiesAndroidAppDataStreamsCreate'
    { _paadscParent :: !Text
    , _paadscXgafv :: !(Maybe Xgafv)
    , _paadscUploadProtocol :: !(Maybe Text)
    , _paadscAccessToken :: !(Maybe Text)
    , _paadscUploadType :: !(Maybe Text)
    , _paadscPayload :: !GoogleAnalyticsAdminV1alphaAndroidAppDataStream
    , _paadscCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesAndroidAppDataStreamsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paadscParent'
--
-- * 'paadscXgafv'
--
-- * 'paadscUploadProtocol'
--
-- * 'paadscAccessToken'
--
-- * 'paadscUploadType'
--
-- * 'paadscPayload'
--
-- * 'paadscCallback'
propertiesAndroidAppDataStreamsCreate
    :: Text -- ^ 'paadscParent'
    -> GoogleAnalyticsAdminV1alphaAndroidAppDataStream -- ^ 'paadscPayload'
    -> PropertiesAndroidAppDataStreamsCreate
propertiesAndroidAppDataStreamsCreate pPaadscParent_ pPaadscPayload_ =
  PropertiesAndroidAppDataStreamsCreate'
    { _paadscParent = pPaadscParent_
    , _paadscXgafv = Nothing
    , _paadscUploadProtocol = Nothing
    , _paadscAccessToken = Nothing
    , _paadscUploadType = Nothing
    , _paadscPayload = pPaadscPayload_
    , _paadscCallback = Nothing
    }


-- | Required. The parent resource where this android app data stream will be
-- created. Format: properties\/123
paadscParent :: Lens' PropertiesAndroidAppDataStreamsCreate Text
paadscParent
  = lens _paadscParent (\ s a -> s{_paadscParent = a})

-- | V1 error format.
paadscXgafv :: Lens' PropertiesAndroidAppDataStreamsCreate (Maybe Xgafv)
paadscXgafv
  = lens _paadscXgafv (\ s a -> s{_paadscXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
paadscUploadProtocol :: Lens' PropertiesAndroidAppDataStreamsCreate (Maybe Text)
paadscUploadProtocol
  = lens _paadscUploadProtocol
      (\ s a -> s{_paadscUploadProtocol = a})

-- | OAuth access token.
paadscAccessToken :: Lens' PropertiesAndroidAppDataStreamsCreate (Maybe Text)
paadscAccessToken
  = lens _paadscAccessToken
      (\ s a -> s{_paadscAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
paadscUploadType :: Lens' PropertiesAndroidAppDataStreamsCreate (Maybe Text)
paadscUploadType
  = lens _paadscUploadType
      (\ s a -> s{_paadscUploadType = a})

-- | Multipart request metadata.
paadscPayload :: Lens' PropertiesAndroidAppDataStreamsCreate GoogleAnalyticsAdminV1alphaAndroidAppDataStream
paadscPayload
  = lens _paadscPayload
      (\ s a -> s{_paadscPayload = a})

-- | JSONP
paadscCallback :: Lens' PropertiesAndroidAppDataStreamsCreate (Maybe Text)
paadscCallback
  = lens _paadscCallback
      (\ s a -> s{_paadscCallback = a})

instance GoogleRequest
           PropertiesAndroidAppDataStreamsCreate
         where
        type Rs PropertiesAndroidAppDataStreamsCreate =
             GoogleAnalyticsAdminV1alphaAndroidAppDataStream
        type Scopes PropertiesAndroidAppDataStreamsCreate =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient
          PropertiesAndroidAppDataStreamsCreate'{..}
          = go _paadscParent _paadscXgafv _paadscUploadProtocol
              _paadscAccessToken
              _paadscUploadType
              _paadscCallback
              (Just AltJSON)
              _paadscPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesAndroidAppDataStreamsCreateResource)
                      mempty
