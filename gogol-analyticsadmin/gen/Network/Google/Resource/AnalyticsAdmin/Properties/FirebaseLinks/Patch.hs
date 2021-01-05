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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a FirebaseLink on a property
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.firebaseLinks.patch@.
module Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.Patch
    (
    -- * REST Resource
      PropertiesFirebaseLinksPatchResource

    -- * Creating a Request
    , propertiesFirebaseLinksPatch
    , PropertiesFirebaseLinksPatch

    -- * Request Lenses
    , pflpXgafv
    , pflpUploadProtocol
    , pflpUpdateMask
    , pflpAccessToken
    , pflpUploadType
    , pflpPayload
    , pflpName
    , pflpCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.firebaseLinks.patch@ method which the
-- 'PropertiesFirebaseLinksPatch' request conforms to.
type PropertiesFirebaseLinksPatchResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "updateMask" GFieldMask :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaFirebaseLink
                         :>
                         Patch '[JSON] GoogleAnalyticsAdminV1alphaFirebaseLink

-- | Updates a FirebaseLink on a property
--
-- /See:/ 'propertiesFirebaseLinksPatch' smart constructor.
data PropertiesFirebaseLinksPatch =
  PropertiesFirebaseLinksPatch'
    { _pflpXgafv :: !(Maybe Xgafv)
    , _pflpUploadProtocol :: !(Maybe Text)
    , _pflpUpdateMask :: !(Maybe GFieldMask)
    , _pflpAccessToken :: !(Maybe Text)
    , _pflpUploadType :: !(Maybe Text)
    , _pflpPayload :: !GoogleAnalyticsAdminV1alphaFirebaseLink
    , _pflpName :: !Text
    , _pflpCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesFirebaseLinksPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pflpXgafv'
--
-- * 'pflpUploadProtocol'
--
-- * 'pflpUpdateMask'
--
-- * 'pflpAccessToken'
--
-- * 'pflpUploadType'
--
-- * 'pflpPayload'
--
-- * 'pflpName'
--
-- * 'pflpCallback'
propertiesFirebaseLinksPatch
    :: GoogleAnalyticsAdminV1alphaFirebaseLink -- ^ 'pflpPayload'
    -> Text -- ^ 'pflpName'
    -> PropertiesFirebaseLinksPatch
propertiesFirebaseLinksPatch pPflpPayload_ pPflpName_ =
  PropertiesFirebaseLinksPatch'
    { _pflpXgafv = Nothing
    , _pflpUploadProtocol = Nothing
    , _pflpUpdateMask = Nothing
    , _pflpAccessToken = Nothing
    , _pflpUploadType = Nothing
    , _pflpPayload = pPflpPayload_
    , _pflpName = pPflpName_
    , _pflpCallback = Nothing
    }


-- | V1 error format.
pflpXgafv :: Lens' PropertiesFirebaseLinksPatch (Maybe Xgafv)
pflpXgafv
  = lens _pflpXgafv (\ s a -> s{_pflpXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pflpUploadProtocol :: Lens' PropertiesFirebaseLinksPatch (Maybe Text)
pflpUploadProtocol
  = lens _pflpUploadProtocol
      (\ s a -> s{_pflpUploadProtocol = a})

-- | Required. The list of fields to be updated. Omitted fields will not be
-- updated. To replace the entire entity, use one path with the string
-- \"*\" to match all fields.
pflpUpdateMask :: Lens' PropertiesFirebaseLinksPatch (Maybe GFieldMask)
pflpUpdateMask
  = lens _pflpUpdateMask
      (\ s a -> s{_pflpUpdateMask = a})

-- | OAuth access token.
pflpAccessToken :: Lens' PropertiesFirebaseLinksPatch (Maybe Text)
pflpAccessToken
  = lens _pflpAccessToken
      (\ s a -> s{_pflpAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pflpUploadType :: Lens' PropertiesFirebaseLinksPatch (Maybe Text)
pflpUploadType
  = lens _pflpUploadType
      (\ s a -> s{_pflpUploadType = a})

-- | Multipart request metadata.
pflpPayload :: Lens' PropertiesFirebaseLinksPatch GoogleAnalyticsAdminV1alphaFirebaseLink
pflpPayload
  = lens _pflpPayload (\ s a -> s{_pflpPayload = a})

-- | Output only. Example format: properties\/1234\/firebaseLinks\/5678
pflpName :: Lens' PropertiesFirebaseLinksPatch Text
pflpName = lens _pflpName (\ s a -> s{_pflpName = a})

-- | JSONP
pflpCallback :: Lens' PropertiesFirebaseLinksPatch (Maybe Text)
pflpCallback
  = lens _pflpCallback (\ s a -> s{_pflpCallback = a})

instance GoogleRequest PropertiesFirebaseLinksPatch
         where
        type Rs PropertiesFirebaseLinksPatch =
             GoogleAnalyticsAdminV1alphaFirebaseLink
        type Scopes PropertiesFirebaseLinksPatch =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesFirebaseLinksPatch'{..}
          = go _pflpName _pflpXgafv _pflpUploadProtocol
              _pflpUpdateMask
              _pflpAccessToken
              _pflpUploadType
              _pflpCallback
              (Just AltJSON)
              _pflpPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesFirebaseLinksPatchResource)
                      mempty
