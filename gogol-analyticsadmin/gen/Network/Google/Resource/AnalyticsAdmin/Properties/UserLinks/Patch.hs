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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user link on an account or property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.userLinks.patch@.
module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Patch
    (
    -- * REST Resource
      PropertiesUserLinksPatchResource

    -- * Creating a Request
    , propertiesUserLinksPatch
    , PropertiesUserLinksPatch

    -- * Request Lenses
    , pulpXgafv
    , pulpUploadProtocol
    , pulpAccessToken
    , pulpUploadType
    , pulpPayload
    , pulpName
    , pulpCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.userLinks.patch@ method which the
-- 'PropertiesUserLinksPatch' request conforms to.
type PropertiesUserLinksPatchResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] GoogleAnalyticsAdminV1alphaUserLink
                       :> Patch '[JSON] GoogleAnalyticsAdminV1alphaUserLink

-- | Updates a user link on an account or property.
--
-- /See:/ 'propertiesUserLinksPatch' smart constructor.
data PropertiesUserLinksPatch =
  PropertiesUserLinksPatch'
    { _pulpXgafv :: !(Maybe Xgafv)
    , _pulpUploadProtocol :: !(Maybe Text)
    , _pulpAccessToken :: !(Maybe Text)
    , _pulpUploadType :: !(Maybe Text)
    , _pulpPayload :: !GoogleAnalyticsAdminV1alphaUserLink
    , _pulpName :: !Text
    , _pulpCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesUserLinksPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pulpXgafv'
--
-- * 'pulpUploadProtocol'
--
-- * 'pulpAccessToken'
--
-- * 'pulpUploadType'
--
-- * 'pulpPayload'
--
-- * 'pulpName'
--
-- * 'pulpCallback'
propertiesUserLinksPatch
    :: GoogleAnalyticsAdminV1alphaUserLink -- ^ 'pulpPayload'
    -> Text -- ^ 'pulpName'
    -> PropertiesUserLinksPatch
propertiesUserLinksPatch pPulpPayload_ pPulpName_ =
  PropertiesUserLinksPatch'
    { _pulpXgafv = Nothing
    , _pulpUploadProtocol = Nothing
    , _pulpAccessToken = Nothing
    , _pulpUploadType = Nothing
    , _pulpPayload = pPulpPayload_
    , _pulpName = pPulpName_
    , _pulpCallback = Nothing
    }


-- | V1 error format.
pulpXgafv :: Lens' PropertiesUserLinksPatch (Maybe Xgafv)
pulpXgafv
  = lens _pulpXgafv (\ s a -> s{_pulpXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pulpUploadProtocol :: Lens' PropertiesUserLinksPatch (Maybe Text)
pulpUploadProtocol
  = lens _pulpUploadProtocol
      (\ s a -> s{_pulpUploadProtocol = a})

-- | OAuth access token.
pulpAccessToken :: Lens' PropertiesUserLinksPatch (Maybe Text)
pulpAccessToken
  = lens _pulpAccessToken
      (\ s a -> s{_pulpAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pulpUploadType :: Lens' PropertiesUserLinksPatch (Maybe Text)
pulpUploadType
  = lens _pulpUploadType
      (\ s a -> s{_pulpUploadType = a})

-- | Multipart request metadata.
pulpPayload :: Lens' PropertiesUserLinksPatch GoogleAnalyticsAdminV1alphaUserLink
pulpPayload
  = lens _pulpPayload (\ s a -> s{_pulpPayload = a})

-- | Example format: properties\/1234\/userLinks\/5678
pulpName :: Lens' PropertiesUserLinksPatch Text
pulpName = lens _pulpName (\ s a -> s{_pulpName = a})

-- | JSONP
pulpCallback :: Lens' PropertiesUserLinksPatch (Maybe Text)
pulpCallback
  = lens _pulpCallback (\ s a -> s{_pulpCallback = a})

instance GoogleRequest PropertiesUserLinksPatch where
        type Rs PropertiesUserLinksPatch =
             GoogleAnalyticsAdminV1alphaUserLink
        type Scopes PropertiesUserLinksPatch =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient PropertiesUserLinksPatch'{..}
          = go _pulpName _pulpXgafv _pulpUploadProtocol
              _pulpAccessToken
              _pulpUploadType
              _pulpCallback
              (Just AltJSON)
              _pulpPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesUserLinksPatchResource)
                      mempty
