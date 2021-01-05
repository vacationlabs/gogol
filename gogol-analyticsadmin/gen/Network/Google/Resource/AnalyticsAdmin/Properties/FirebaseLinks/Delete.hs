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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a FirebaseLink on a property
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.firebaseLinks.delete@.
module Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.Delete
    (
    -- * REST Resource
      PropertiesFirebaseLinksDeleteResource

    -- * Creating a Request
    , propertiesFirebaseLinksDelete
    , PropertiesFirebaseLinksDelete

    -- * Request Lenses
    , pfldXgafv
    , pfldUploadProtocol
    , pfldAccessToken
    , pfldUploadType
    , pfldName
    , pfldCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.firebaseLinks.delete@ method which the
-- 'PropertiesFirebaseLinksDelete' request conforms to.
type PropertiesFirebaseLinksDeleteResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Delete '[JSON] GoogleProtobufEmpty

-- | Deletes a FirebaseLink on a property
--
-- /See:/ 'propertiesFirebaseLinksDelete' smart constructor.
data PropertiesFirebaseLinksDelete =
  PropertiesFirebaseLinksDelete'
    { _pfldXgafv :: !(Maybe Xgafv)
    , _pfldUploadProtocol :: !(Maybe Text)
    , _pfldAccessToken :: !(Maybe Text)
    , _pfldUploadType :: !(Maybe Text)
    , _pfldName :: !Text
    , _pfldCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesFirebaseLinksDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfldXgafv'
--
-- * 'pfldUploadProtocol'
--
-- * 'pfldAccessToken'
--
-- * 'pfldUploadType'
--
-- * 'pfldName'
--
-- * 'pfldCallback'
propertiesFirebaseLinksDelete
    :: Text -- ^ 'pfldName'
    -> PropertiesFirebaseLinksDelete
propertiesFirebaseLinksDelete pPfldName_ =
  PropertiesFirebaseLinksDelete'
    { _pfldXgafv = Nothing
    , _pfldUploadProtocol = Nothing
    , _pfldAccessToken = Nothing
    , _pfldUploadType = Nothing
    , _pfldName = pPfldName_
    , _pfldCallback = Nothing
    }


-- | V1 error format.
pfldXgafv :: Lens' PropertiesFirebaseLinksDelete (Maybe Xgafv)
pfldXgafv
  = lens _pfldXgafv (\ s a -> s{_pfldXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pfldUploadProtocol :: Lens' PropertiesFirebaseLinksDelete (Maybe Text)
pfldUploadProtocol
  = lens _pfldUploadProtocol
      (\ s a -> s{_pfldUploadProtocol = a})

-- | OAuth access token.
pfldAccessToken :: Lens' PropertiesFirebaseLinksDelete (Maybe Text)
pfldAccessToken
  = lens _pfldAccessToken
      (\ s a -> s{_pfldAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pfldUploadType :: Lens' PropertiesFirebaseLinksDelete (Maybe Text)
pfldUploadType
  = lens _pfldUploadType
      (\ s a -> s{_pfldUploadType = a})

-- | Required. Format:
-- properties\/{property_id}\/firebaseLinks\/{firebase_link_id} Example:
-- properties\/1234\/firebaseLinks\/5678
pfldName :: Lens' PropertiesFirebaseLinksDelete Text
pfldName = lens _pfldName (\ s a -> s{_pfldName = a})

-- | JSONP
pfldCallback :: Lens' PropertiesFirebaseLinksDelete (Maybe Text)
pfldCallback
  = lens _pfldCallback (\ s a -> s{_pfldCallback = a})

instance GoogleRequest PropertiesFirebaseLinksDelete
         where
        type Rs PropertiesFirebaseLinksDelete =
             GoogleProtobufEmpty
        type Scopes PropertiesFirebaseLinksDelete =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesFirebaseLinksDelete'{..}
          = go _pfldName _pfldXgafv _pfldUploadProtocol
              _pfldAccessToken
              _pfldUploadType
              _pfldCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesFirebaseLinksDeleteResource)
                      mempty
