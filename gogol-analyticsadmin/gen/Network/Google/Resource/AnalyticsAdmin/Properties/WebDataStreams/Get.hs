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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lookup for a single WebDataStream
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.webDataStreams.get@.
module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Get
    (
    -- * REST Resource
      PropertiesWebDataStreamsGetResource

    -- * Creating a Request
    , propertiesWebDataStreamsGet
    , PropertiesWebDataStreamsGet

    -- * Request Lenses
    , pwdsgXgafv
    , pwdsgUploadProtocol
    , pwdsgAccessToken
    , pwdsgUploadType
    , pwdsgName
    , pwdsgCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.webDataStreams.get@ method which the
-- 'PropertiesWebDataStreamsGet' request conforms to.
type PropertiesWebDataStreamsGetResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] GoogleAnalyticsAdminV1alphaWebDataStream

-- | Lookup for a single WebDataStream
--
-- /See:/ 'propertiesWebDataStreamsGet' smart constructor.
data PropertiesWebDataStreamsGet =
  PropertiesWebDataStreamsGet'
    { _pwdsgXgafv :: !(Maybe Xgafv)
    , _pwdsgUploadProtocol :: !(Maybe Text)
    , _pwdsgAccessToken :: !(Maybe Text)
    , _pwdsgUploadType :: !(Maybe Text)
    , _pwdsgName :: !Text
    , _pwdsgCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesWebDataStreamsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwdsgXgafv'
--
-- * 'pwdsgUploadProtocol'
--
-- * 'pwdsgAccessToken'
--
-- * 'pwdsgUploadType'
--
-- * 'pwdsgName'
--
-- * 'pwdsgCallback'
propertiesWebDataStreamsGet
    :: Text -- ^ 'pwdsgName'
    -> PropertiesWebDataStreamsGet
propertiesWebDataStreamsGet pPwdsgName_ =
  PropertiesWebDataStreamsGet'
    { _pwdsgXgafv = Nothing
    , _pwdsgUploadProtocol = Nothing
    , _pwdsgAccessToken = Nothing
    , _pwdsgUploadType = Nothing
    , _pwdsgName = pPwdsgName_
    , _pwdsgCallback = Nothing
    }


-- | V1 error format.
pwdsgXgafv :: Lens' PropertiesWebDataStreamsGet (Maybe Xgafv)
pwdsgXgafv
  = lens _pwdsgXgafv (\ s a -> s{_pwdsgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pwdsgUploadProtocol :: Lens' PropertiesWebDataStreamsGet (Maybe Text)
pwdsgUploadProtocol
  = lens _pwdsgUploadProtocol
      (\ s a -> s{_pwdsgUploadProtocol = a})

-- | OAuth access token.
pwdsgAccessToken :: Lens' PropertiesWebDataStreamsGet (Maybe Text)
pwdsgAccessToken
  = lens _pwdsgAccessToken
      (\ s a -> s{_pwdsgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pwdsgUploadType :: Lens' PropertiesWebDataStreamsGet (Maybe Text)
pwdsgUploadType
  = lens _pwdsgUploadType
      (\ s a -> s{_pwdsgUploadType = a})

-- | Required. The name of the web data stream to lookup. Format:
-- properties\/{property_id}\/webDataStreams\/{stream_id} Example:
-- \"properties\/123\/webDataStreams\/456\"
pwdsgName :: Lens' PropertiesWebDataStreamsGet Text
pwdsgName
  = lens _pwdsgName (\ s a -> s{_pwdsgName = a})

-- | JSONP
pwdsgCallback :: Lens' PropertiesWebDataStreamsGet (Maybe Text)
pwdsgCallback
  = lens _pwdsgCallback
      (\ s a -> s{_pwdsgCallback = a})

instance GoogleRequest PropertiesWebDataStreamsGet
         where
        type Rs PropertiesWebDataStreamsGet =
             GoogleAnalyticsAdminV1alphaWebDataStream
        type Scopes PropertiesWebDataStreamsGet =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient PropertiesWebDataStreamsGet'{..}
          = go _pwdsgName _pwdsgXgafv _pwdsgUploadProtocol
              _pwdsgAccessToken
              _pwdsgUploadType
              _pwdsgCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesWebDataStreamsGetResource)
                      mempty
