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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.patch@.
module Network.Google.Resource.AnalyticsAdmin.Properties.Patch
    (
    -- * REST Resource
      PropertiesPatchResource

    -- * Creating a Request
    , propertiesPatch
    , PropertiesPatch

    -- * Request Lenses
    , ppXgafv
    , ppUploadProtocol
    , ppUpdateMask
    , ppAccessToken
    , ppUploadType
    , ppPayload
    , ppName
    , ppCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.patch@ method which the
-- 'PropertiesPatch' request conforms to.
type PropertiesPatchResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "updateMask" GFieldMask :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON] GoogleAnalyticsAdminV1alphaProperty
                         :> Patch '[JSON] GoogleAnalyticsAdminV1alphaProperty

-- | Updates a property.
--
-- /See:/ 'propertiesPatch' smart constructor.
data PropertiesPatch =
  PropertiesPatch'
    { _ppXgafv :: !(Maybe Xgafv)
    , _ppUploadProtocol :: !(Maybe Text)
    , _ppUpdateMask :: !(Maybe GFieldMask)
    , _ppAccessToken :: !(Maybe Text)
    , _ppUploadType :: !(Maybe Text)
    , _ppPayload :: !GoogleAnalyticsAdminV1alphaProperty
    , _ppName :: !Text
    , _ppCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppXgafv'
--
-- * 'ppUploadProtocol'
--
-- * 'ppUpdateMask'
--
-- * 'ppAccessToken'
--
-- * 'ppUploadType'
--
-- * 'ppPayload'
--
-- * 'ppName'
--
-- * 'ppCallback'
propertiesPatch
    :: GoogleAnalyticsAdminV1alphaProperty -- ^ 'ppPayload'
    -> Text -- ^ 'ppName'
    -> PropertiesPatch
propertiesPatch pPpPayload_ pPpName_ =
  PropertiesPatch'
    { _ppXgafv = Nothing
    , _ppUploadProtocol = Nothing
    , _ppUpdateMask = Nothing
    , _ppAccessToken = Nothing
    , _ppUploadType = Nothing
    , _ppPayload = pPpPayload_
    , _ppName = pPpName_
    , _ppCallback = Nothing
    }


-- | V1 error format.
ppXgafv :: Lens' PropertiesPatch (Maybe Xgafv)
ppXgafv = lens _ppXgafv (\ s a -> s{_ppXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
ppUploadProtocol :: Lens' PropertiesPatch (Maybe Text)
ppUploadProtocol
  = lens _ppUploadProtocol
      (\ s a -> s{_ppUploadProtocol = a})

-- | Required. The list of fields to be updated. Omitted fields will not be
-- updated. To replace the entire entity, use one path with the string
-- \"*\" to match all fields.
ppUpdateMask :: Lens' PropertiesPatch (Maybe GFieldMask)
ppUpdateMask
  = lens _ppUpdateMask (\ s a -> s{_ppUpdateMask = a})

-- | OAuth access token.
ppAccessToken :: Lens' PropertiesPatch (Maybe Text)
ppAccessToken
  = lens _ppAccessToken
      (\ s a -> s{_ppAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
ppUploadType :: Lens' PropertiesPatch (Maybe Text)
ppUploadType
  = lens _ppUploadType (\ s a -> s{_ppUploadType = a})

-- | Multipart request metadata.
ppPayload :: Lens' PropertiesPatch GoogleAnalyticsAdminV1alphaProperty
ppPayload
  = lens _ppPayload (\ s a -> s{_ppPayload = a})

-- | Output only. Resource name of this property. Format:
-- properties\/{property_id} Example: \"properties\/1000\"
ppName :: Lens' PropertiesPatch Text
ppName = lens _ppName (\ s a -> s{_ppName = a})

-- | JSONP
ppCallback :: Lens' PropertiesPatch (Maybe Text)
ppCallback
  = lens _ppCallback (\ s a -> s{_ppCallback = a})

instance GoogleRequest PropertiesPatch where
        type Rs PropertiesPatch =
             GoogleAnalyticsAdminV1alphaProperty
        type Scopes PropertiesPatch =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesPatch'{..}
          = go _ppName _ppXgafv _ppUploadProtocol _ppUpdateMask
              _ppAccessToken
              _ppUploadType
              _ppCallback
              (Just AltJSON)
              _ppPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesPatchResource)
                      mempty
