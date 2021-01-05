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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a user\'s link to an account or property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.userLinks.get@.
module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Get
    (
    -- * REST Resource
      PropertiesUserLinksGetResource

    -- * Creating a Request
    , propertiesUserLinksGet
    , PropertiesUserLinksGet

    -- * Request Lenses
    , pulgXgafv
    , pulgUploadProtocol
    , pulgAccessToken
    , pulgUploadType
    , pulgName
    , pulgCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.userLinks.get@ method which the
-- 'PropertiesUserLinksGet' request conforms to.
type PropertiesUserLinksGetResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] GoogleAnalyticsAdminV1alphaUserLink

-- | Gets information about a user\'s link to an account or property.
--
-- /See:/ 'propertiesUserLinksGet' smart constructor.
data PropertiesUserLinksGet =
  PropertiesUserLinksGet'
    { _pulgXgafv :: !(Maybe Xgafv)
    , _pulgUploadProtocol :: !(Maybe Text)
    , _pulgAccessToken :: !(Maybe Text)
    , _pulgUploadType :: !(Maybe Text)
    , _pulgName :: !Text
    , _pulgCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesUserLinksGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pulgXgafv'
--
-- * 'pulgUploadProtocol'
--
-- * 'pulgAccessToken'
--
-- * 'pulgUploadType'
--
-- * 'pulgName'
--
-- * 'pulgCallback'
propertiesUserLinksGet
    :: Text -- ^ 'pulgName'
    -> PropertiesUserLinksGet
propertiesUserLinksGet pPulgName_ =
  PropertiesUserLinksGet'
    { _pulgXgafv = Nothing
    , _pulgUploadProtocol = Nothing
    , _pulgAccessToken = Nothing
    , _pulgUploadType = Nothing
    , _pulgName = pPulgName_
    , _pulgCallback = Nothing
    }


-- | V1 error format.
pulgXgafv :: Lens' PropertiesUserLinksGet (Maybe Xgafv)
pulgXgafv
  = lens _pulgXgafv (\ s a -> s{_pulgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pulgUploadProtocol :: Lens' PropertiesUserLinksGet (Maybe Text)
pulgUploadProtocol
  = lens _pulgUploadProtocol
      (\ s a -> s{_pulgUploadProtocol = a})

-- | OAuth access token.
pulgAccessToken :: Lens' PropertiesUserLinksGet (Maybe Text)
pulgAccessToken
  = lens _pulgAccessToken
      (\ s a -> s{_pulgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pulgUploadType :: Lens' PropertiesUserLinksGet (Maybe Text)
pulgUploadType
  = lens _pulgUploadType
      (\ s a -> s{_pulgUploadType = a})

-- | Required. Example format: accounts\/1234\/userLinks\/5678
pulgName :: Lens' PropertiesUserLinksGet Text
pulgName = lens _pulgName (\ s a -> s{_pulgName = a})

-- | JSONP
pulgCallback :: Lens' PropertiesUserLinksGet (Maybe Text)
pulgCallback
  = lens _pulgCallback (\ s a -> s{_pulgCallback = a})

instance GoogleRequest PropertiesUserLinksGet where
        type Rs PropertiesUserLinksGet =
             GoogleAnalyticsAdminV1alphaUserLink
        type Scopes PropertiesUserLinksGet =
             '["https://www.googleapis.com/auth/analytics.manage.users.readonly"]
        requestClient PropertiesUserLinksGet'{..}
          = go _pulgName _pulgXgafv _pulgUploadProtocol
              _pulgAccessToken
              _pulgUploadType
              _pulgCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesUserLinksGetResource)
                      mempty
