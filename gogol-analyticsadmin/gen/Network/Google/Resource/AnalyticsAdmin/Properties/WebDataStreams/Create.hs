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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a web stream with the specified location and attributes.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.webDataStreams.create@.
module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Create
    (
    -- * REST Resource
      PropertiesWebDataStreamsCreateResource

    -- * Creating a Request
    , propertiesWebDataStreamsCreate
    , PropertiesWebDataStreamsCreate

    -- * Request Lenses
    , pwdscParent
    , pwdscXgafv
    , pwdscUploadProtocol
    , pwdscAccessToken
    , pwdscUploadType
    , pwdscPayload
    , pwdscCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.webDataStreams.create@ method which the
-- 'PropertiesWebDataStreamsCreate' request conforms to.
type PropertiesWebDataStreamsCreateResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "webDataStreams" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaWebDataStream
                         :>
                         Post '[JSON] GoogleAnalyticsAdminV1alphaWebDataStream

-- | Creates a web stream with the specified location and attributes.
--
-- /See:/ 'propertiesWebDataStreamsCreate' smart constructor.
data PropertiesWebDataStreamsCreate =
  PropertiesWebDataStreamsCreate'
    { _pwdscParent :: !Text
    , _pwdscXgafv :: !(Maybe Xgafv)
    , _pwdscUploadProtocol :: !(Maybe Text)
    , _pwdscAccessToken :: !(Maybe Text)
    , _pwdscUploadType :: !(Maybe Text)
    , _pwdscPayload :: !GoogleAnalyticsAdminV1alphaWebDataStream
    , _pwdscCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesWebDataStreamsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwdscParent'
--
-- * 'pwdscXgafv'
--
-- * 'pwdscUploadProtocol'
--
-- * 'pwdscAccessToken'
--
-- * 'pwdscUploadType'
--
-- * 'pwdscPayload'
--
-- * 'pwdscCallback'
propertiesWebDataStreamsCreate
    :: Text -- ^ 'pwdscParent'
    -> GoogleAnalyticsAdminV1alphaWebDataStream -- ^ 'pwdscPayload'
    -> PropertiesWebDataStreamsCreate
propertiesWebDataStreamsCreate pPwdscParent_ pPwdscPayload_ =
  PropertiesWebDataStreamsCreate'
    { _pwdscParent = pPwdscParent_
    , _pwdscXgafv = Nothing
    , _pwdscUploadProtocol = Nothing
    , _pwdscAccessToken = Nothing
    , _pwdscUploadType = Nothing
    , _pwdscPayload = pPwdscPayload_
    , _pwdscCallback = Nothing
    }


-- | Required. The parent resource where this web data stream will be
-- created. Format: properties\/123
pwdscParent :: Lens' PropertiesWebDataStreamsCreate Text
pwdscParent
  = lens _pwdscParent (\ s a -> s{_pwdscParent = a})

-- | V1 error format.
pwdscXgafv :: Lens' PropertiesWebDataStreamsCreate (Maybe Xgafv)
pwdscXgafv
  = lens _pwdscXgafv (\ s a -> s{_pwdscXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pwdscUploadProtocol :: Lens' PropertiesWebDataStreamsCreate (Maybe Text)
pwdscUploadProtocol
  = lens _pwdscUploadProtocol
      (\ s a -> s{_pwdscUploadProtocol = a})

-- | OAuth access token.
pwdscAccessToken :: Lens' PropertiesWebDataStreamsCreate (Maybe Text)
pwdscAccessToken
  = lens _pwdscAccessToken
      (\ s a -> s{_pwdscAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pwdscUploadType :: Lens' PropertiesWebDataStreamsCreate (Maybe Text)
pwdscUploadType
  = lens _pwdscUploadType
      (\ s a -> s{_pwdscUploadType = a})

-- | Multipart request metadata.
pwdscPayload :: Lens' PropertiesWebDataStreamsCreate GoogleAnalyticsAdminV1alphaWebDataStream
pwdscPayload
  = lens _pwdscPayload (\ s a -> s{_pwdscPayload = a})

-- | JSONP
pwdscCallback :: Lens' PropertiesWebDataStreamsCreate (Maybe Text)
pwdscCallback
  = lens _pwdscCallback
      (\ s a -> s{_pwdscCallback = a})

instance GoogleRequest PropertiesWebDataStreamsCreate
         where
        type Rs PropertiesWebDataStreamsCreate =
             GoogleAnalyticsAdminV1alphaWebDataStream
        type Scopes PropertiesWebDataStreamsCreate =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesWebDataStreamsCreate'{..}
          = go _pwdscParent _pwdscXgafv _pwdscUploadProtocol
              _pwdscAccessToken
              _pwdscUploadType
              _pwdscCallback
              (Just AltJSON)
              _pwdscPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesWebDataStreamsCreateResource)
                      mempty
