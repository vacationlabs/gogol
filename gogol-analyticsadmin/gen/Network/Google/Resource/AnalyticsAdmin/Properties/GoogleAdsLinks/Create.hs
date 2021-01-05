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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a GoogleAdsLink.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.googleAdsLinks.create@.
module Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.Create
    (
    -- * REST Resource
      PropertiesGoogleAdsLinksCreateResource

    -- * Creating a Request
    , propertiesGoogleAdsLinksCreate
    , PropertiesGoogleAdsLinksCreate

    -- * Request Lenses
    , pgalcParent
    , pgalcXgafv
    , pgalcUploadProtocol
    , pgalcAccessToken
    , pgalcUploadType
    , pgalcPayload
    , pgalcCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.googleAdsLinks.create@ method which the
-- 'PropertiesGoogleAdsLinksCreate' request conforms to.
type PropertiesGoogleAdsLinksCreateResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "googleAdsLinks" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaGoogleAdsLink
                         :>
                         Post '[JSON] GoogleAnalyticsAdminV1alphaGoogleAdsLink

-- | Creates a GoogleAdsLink.
--
-- /See:/ 'propertiesGoogleAdsLinksCreate' smart constructor.
data PropertiesGoogleAdsLinksCreate =
  PropertiesGoogleAdsLinksCreate'
    { _pgalcParent :: !Text
    , _pgalcXgafv :: !(Maybe Xgafv)
    , _pgalcUploadProtocol :: !(Maybe Text)
    , _pgalcAccessToken :: !(Maybe Text)
    , _pgalcUploadType :: !(Maybe Text)
    , _pgalcPayload :: !GoogleAnalyticsAdminV1alphaGoogleAdsLink
    , _pgalcCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesGoogleAdsLinksCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgalcParent'
--
-- * 'pgalcXgafv'
--
-- * 'pgalcUploadProtocol'
--
-- * 'pgalcAccessToken'
--
-- * 'pgalcUploadType'
--
-- * 'pgalcPayload'
--
-- * 'pgalcCallback'
propertiesGoogleAdsLinksCreate
    :: Text -- ^ 'pgalcParent'
    -> GoogleAnalyticsAdminV1alphaGoogleAdsLink -- ^ 'pgalcPayload'
    -> PropertiesGoogleAdsLinksCreate
propertiesGoogleAdsLinksCreate pPgalcParent_ pPgalcPayload_ =
  PropertiesGoogleAdsLinksCreate'
    { _pgalcParent = pPgalcParent_
    , _pgalcXgafv = Nothing
    , _pgalcUploadProtocol = Nothing
    , _pgalcAccessToken = Nothing
    , _pgalcUploadType = Nothing
    , _pgalcPayload = pPgalcPayload_
    , _pgalcCallback = Nothing
    }


-- | Required. Example format: properties\/1234
pgalcParent :: Lens' PropertiesGoogleAdsLinksCreate Text
pgalcParent
  = lens _pgalcParent (\ s a -> s{_pgalcParent = a})

-- | V1 error format.
pgalcXgafv :: Lens' PropertiesGoogleAdsLinksCreate (Maybe Xgafv)
pgalcXgafv
  = lens _pgalcXgafv (\ s a -> s{_pgalcXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pgalcUploadProtocol :: Lens' PropertiesGoogleAdsLinksCreate (Maybe Text)
pgalcUploadProtocol
  = lens _pgalcUploadProtocol
      (\ s a -> s{_pgalcUploadProtocol = a})

-- | OAuth access token.
pgalcAccessToken :: Lens' PropertiesGoogleAdsLinksCreate (Maybe Text)
pgalcAccessToken
  = lens _pgalcAccessToken
      (\ s a -> s{_pgalcAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pgalcUploadType :: Lens' PropertiesGoogleAdsLinksCreate (Maybe Text)
pgalcUploadType
  = lens _pgalcUploadType
      (\ s a -> s{_pgalcUploadType = a})

-- | Multipart request metadata.
pgalcPayload :: Lens' PropertiesGoogleAdsLinksCreate GoogleAnalyticsAdminV1alphaGoogleAdsLink
pgalcPayload
  = lens _pgalcPayload (\ s a -> s{_pgalcPayload = a})

-- | JSONP
pgalcCallback :: Lens' PropertiesGoogleAdsLinksCreate (Maybe Text)
pgalcCallback
  = lens _pgalcCallback
      (\ s a -> s{_pgalcCallback = a})

instance GoogleRequest PropertiesGoogleAdsLinksCreate
         where
        type Rs PropertiesGoogleAdsLinksCreate =
             GoogleAnalyticsAdminV1alphaGoogleAdsLink
        type Scopes PropertiesGoogleAdsLinksCreate =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesGoogleAdsLinksCreate'{..}
          = go _pgalcParent _pgalcXgafv _pgalcUploadProtocol
              _pgalcAccessToken
              _pgalcUploadType
              _pgalcCallback
              (Just AltJSON)
              _pgalcPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesGoogleAdsLinksCreateResource)
                      mempty
