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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.GetGlobalSiteTag
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the Site Tag for the specified web stream. Site Tags are
-- immutable singletons.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.webDataStreams.getGlobalSiteTag@.
module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.GetGlobalSiteTag
    (
    -- * REST Resource
      PropertiesWebDataStreamsGetGlobalSiteTagResource

    -- * Creating a Request
    , propertiesWebDataStreamsGetGlobalSiteTag
    , PropertiesWebDataStreamsGetGlobalSiteTag

    -- * Request Lenses
    , pwdsggstXgafv
    , pwdsggstUploadProtocol
    , pwdsggstAccessToken
    , pwdsggstUploadType
    , pwdsggstName
    , pwdsggstCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.webDataStreams.getGlobalSiteTag@ method which the
-- 'PropertiesWebDataStreamsGetGlobalSiteTag' request conforms to.
type PropertiesWebDataStreamsGetGlobalSiteTagResource
     =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] GoogleAnalyticsAdminV1alphaGlobalSiteTag

-- | Returns the Site Tag for the specified web stream. Site Tags are
-- immutable singletons.
--
-- /See:/ 'propertiesWebDataStreamsGetGlobalSiteTag' smart constructor.
data PropertiesWebDataStreamsGetGlobalSiteTag =
  PropertiesWebDataStreamsGetGlobalSiteTag'
    { _pwdsggstXgafv :: !(Maybe Xgafv)
    , _pwdsggstUploadProtocol :: !(Maybe Text)
    , _pwdsggstAccessToken :: !(Maybe Text)
    , _pwdsggstUploadType :: !(Maybe Text)
    , _pwdsggstName :: !Text
    , _pwdsggstCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesWebDataStreamsGetGlobalSiteTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwdsggstXgafv'
--
-- * 'pwdsggstUploadProtocol'
--
-- * 'pwdsggstAccessToken'
--
-- * 'pwdsggstUploadType'
--
-- * 'pwdsggstName'
--
-- * 'pwdsggstCallback'
propertiesWebDataStreamsGetGlobalSiteTag
    :: Text -- ^ 'pwdsggstName'
    -> PropertiesWebDataStreamsGetGlobalSiteTag
propertiesWebDataStreamsGetGlobalSiteTag pPwdsggstName_ =
  PropertiesWebDataStreamsGetGlobalSiteTag'
    { _pwdsggstXgafv = Nothing
    , _pwdsggstUploadProtocol = Nothing
    , _pwdsggstAccessToken = Nothing
    , _pwdsggstUploadType = Nothing
    , _pwdsggstName = pPwdsggstName_
    , _pwdsggstCallback = Nothing
    }


-- | V1 error format.
pwdsggstXgafv :: Lens' PropertiesWebDataStreamsGetGlobalSiteTag (Maybe Xgafv)
pwdsggstXgafv
  = lens _pwdsggstXgafv
      (\ s a -> s{_pwdsggstXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pwdsggstUploadProtocol :: Lens' PropertiesWebDataStreamsGetGlobalSiteTag (Maybe Text)
pwdsggstUploadProtocol
  = lens _pwdsggstUploadProtocol
      (\ s a -> s{_pwdsggstUploadProtocol = a})

-- | OAuth access token.
pwdsggstAccessToken :: Lens' PropertiesWebDataStreamsGetGlobalSiteTag (Maybe Text)
pwdsggstAccessToken
  = lens _pwdsggstAccessToken
      (\ s a -> s{_pwdsggstAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pwdsggstUploadType :: Lens' PropertiesWebDataStreamsGetGlobalSiteTag (Maybe Text)
pwdsggstUploadType
  = lens _pwdsggstUploadType
      (\ s a -> s{_pwdsggstUploadType = a})

-- | Required. The name of the site tag to lookup. Note that site tags are
-- singletons and do not have unique IDs. Format:
-- properties\/{property_id}\/webDataStreams\/{stream_id}\/globalSiteTag
-- Example: \"properties\/123\/webDataStreams\/456\/globalSiteTag\"
pwdsggstName :: Lens' PropertiesWebDataStreamsGetGlobalSiteTag Text
pwdsggstName
  = lens _pwdsggstName (\ s a -> s{_pwdsggstName = a})

-- | JSONP
pwdsggstCallback :: Lens' PropertiesWebDataStreamsGetGlobalSiteTag (Maybe Text)
pwdsggstCallback
  = lens _pwdsggstCallback
      (\ s a -> s{_pwdsggstCallback = a})

instance GoogleRequest
           PropertiesWebDataStreamsGetGlobalSiteTag
         where
        type Rs PropertiesWebDataStreamsGetGlobalSiteTag =
             GoogleAnalyticsAdminV1alphaGlobalSiteTag
        type Scopes PropertiesWebDataStreamsGetGlobalSiteTag
             =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient
          PropertiesWebDataStreamsGetGlobalSiteTag'{..}
          = go _pwdsggstName _pwdsggstXgafv
              _pwdsggstUploadProtocol
              _pwdsggstAccessToken
              _pwdsggstUploadType
              _pwdsggstCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           PropertiesWebDataStreamsGetGlobalSiteTagResource)
                      mempty
