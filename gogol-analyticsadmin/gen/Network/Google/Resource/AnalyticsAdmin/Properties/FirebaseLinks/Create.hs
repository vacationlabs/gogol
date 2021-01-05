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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a FirebaseLink. Properties can have at most one FirebaseLink.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.firebaseLinks.create@.
module Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.Create
    (
    -- * REST Resource
      PropertiesFirebaseLinksCreateResource

    -- * Creating a Request
    , propertiesFirebaseLinksCreate
    , PropertiesFirebaseLinksCreate

    -- * Request Lenses
    , pflcParent
    , pflcXgafv
    , pflcUploadProtocol
    , pflcAccessToken
    , pflcUploadType
    , pflcPayload
    , pflcCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.firebaseLinks.create@ method which the
-- 'PropertiesFirebaseLinksCreate' request conforms to.
type PropertiesFirebaseLinksCreateResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "firebaseLinks" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaFirebaseLink
                         :>
                         Post '[JSON] GoogleAnalyticsAdminV1alphaFirebaseLink

-- | Creates a FirebaseLink. Properties can have at most one FirebaseLink.
--
-- /See:/ 'propertiesFirebaseLinksCreate' smart constructor.
data PropertiesFirebaseLinksCreate =
  PropertiesFirebaseLinksCreate'
    { _pflcParent :: !Text
    , _pflcXgafv :: !(Maybe Xgafv)
    , _pflcUploadProtocol :: !(Maybe Text)
    , _pflcAccessToken :: !(Maybe Text)
    , _pflcUploadType :: !(Maybe Text)
    , _pflcPayload :: !GoogleAnalyticsAdminV1alphaFirebaseLink
    , _pflcCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesFirebaseLinksCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pflcParent'
--
-- * 'pflcXgafv'
--
-- * 'pflcUploadProtocol'
--
-- * 'pflcAccessToken'
--
-- * 'pflcUploadType'
--
-- * 'pflcPayload'
--
-- * 'pflcCallback'
propertiesFirebaseLinksCreate
    :: Text -- ^ 'pflcParent'
    -> GoogleAnalyticsAdminV1alphaFirebaseLink -- ^ 'pflcPayload'
    -> PropertiesFirebaseLinksCreate
propertiesFirebaseLinksCreate pPflcParent_ pPflcPayload_ =
  PropertiesFirebaseLinksCreate'
    { _pflcParent = pPflcParent_
    , _pflcXgafv = Nothing
    , _pflcUploadProtocol = Nothing
    , _pflcAccessToken = Nothing
    , _pflcUploadType = Nothing
    , _pflcPayload = pPflcPayload_
    , _pflcCallback = Nothing
    }


-- | Required. Format: properties\/{property_id} Example: properties\/1234
pflcParent :: Lens' PropertiesFirebaseLinksCreate Text
pflcParent
  = lens _pflcParent (\ s a -> s{_pflcParent = a})

-- | V1 error format.
pflcXgafv :: Lens' PropertiesFirebaseLinksCreate (Maybe Xgafv)
pflcXgafv
  = lens _pflcXgafv (\ s a -> s{_pflcXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pflcUploadProtocol :: Lens' PropertiesFirebaseLinksCreate (Maybe Text)
pflcUploadProtocol
  = lens _pflcUploadProtocol
      (\ s a -> s{_pflcUploadProtocol = a})

-- | OAuth access token.
pflcAccessToken :: Lens' PropertiesFirebaseLinksCreate (Maybe Text)
pflcAccessToken
  = lens _pflcAccessToken
      (\ s a -> s{_pflcAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pflcUploadType :: Lens' PropertiesFirebaseLinksCreate (Maybe Text)
pflcUploadType
  = lens _pflcUploadType
      (\ s a -> s{_pflcUploadType = a})

-- | Multipart request metadata.
pflcPayload :: Lens' PropertiesFirebaseLinksCreate GoogleAnalyticsAdminV1alphaFirebaseLink
pflcPayload
  = lens _pflcPayload (\ s a -> s{_pflcPayload = a})

-- | JSONP
pflcCallback :: Lens' PropertiesFirebaseLinksCreate (Maybe Text)
pflcCallback
  = lens _pflcCallback (\ s a -> s{_pflcCallback = a})

instance GoogleRequest PropertiesFirebaseLinksCreate
         where
        type Rs PropertiesFirebaseLinksCreate =
             GoogleAnalyticsAdminV1alphaFirebaseLink
        type Scopes PropertiesFirebaseLinksCreate =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesFirebaseLinksCreate'{..}
          = go _pflcParent _pflcXgafv _pflcUploadProtocol
              _pflcAccessToken
              _pflcUploadType
              _pflcCallback
              (Just AltJSON)
              _pflcPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesFirebaseLinksCreateResource)
                      mempty
