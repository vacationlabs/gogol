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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks target Property as soft-deleted (ie: \"trashed\") and returns it.
-- This API does not have a method to restore soft-deleted properties.
-- However, they can be restored using the Trash Can UI. If the properties
-- are not restored before the expiration time, the Property and all child
-- resources (eg: GoogleAdsLinks, Streams, UserLinks) will be permanently
-- purged. https:\/\/support.google.com\/analytics\/answer\/6154772 Returns
-- an error if the target is not found, or is not an GA4 Property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.delete@.
module Network.Google.Resource.AnalyticsAdmin.Properties.Delete
    (
    -- * REST Resource
      PropertiesDeleteResource

    -- * Creating a Request
    , propertiesDelete
    , PropertiesDelete

    -- * Request Lenses
    , pdXgafv
    , pdUploadProtocol
    , pdAccessToken
    , pdUploadType
    , pdName
    , pdCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.delete@ method which the
-- 'PropertiesDelete' request conforms to.
type PropertiesDeleteResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Delete '[JSON] GoogleProtobufEmpty

-- | Marks target Property as soft-deleted (ie: \"trashed\") and returns it.
-- This API does not have a method to restore soft-deleted properties.
-- However, they can be restored using the Trash Can UI. If the properties
-- are not restored before the expiration time, the Property and all child
-- resources (eg: GoogleAdsLinks, Streams, UserLinks) will be permanently
-- purged. https:\/\/support.google.com\/analytics\/answer\/6154772 Returns
-- an error if the target is not found, or is not an GA4 Property.
--
-- /See:/ 'propertiesDelete' smart constructor.
data PropertiesDelete =
  PropertiesDelete'
    { _pdXgafv :: !(Maybe Xgafv)
    , _pdUploadProtocol :: !(Maybe Text)
    , _pdAccessToken :: !(Maybe Text)
    , _pdUploadType :: !(Maybe Text)
    , _pdName :: !Text
    , _pdCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdXgafv'
--
-- * 'pdUploadProtocol'
--
-- * 'pdAccessToken'
--
-- * 'pdUploadType'
--
-- * 'pdName'
--
-- * 'pdCallback'
propertiesDelete
    :: Text -- ^ 'pdName'
    -> PropertiesDelete
propertiesDelete pPdName_ =
  PropertiesDelete'
    { _pdXgafv = Nothing
    , _pdUploadProtocol = Nothing
    , _pdAccessToken = Nothing
    , _pdUploadType = Nothing
    , _pdName = pPdName_
    , _pdCallback = Nothing
    }


-- | V1 error format.
pdXgafv :: Lens' PropertiesDelete (Maybe Xgafv)
pdXgafv = lens _pdXgafv (\ s a -> s{_pdXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pdUploadProtocol :: Lens' PropertiesDelete (Maybe Text)
pdUploadProtocol
  = lens _pdUploadProtocol
      (\ s a -> s{_pdUploadProtocol = a})

-- | OAuth access token.
pdAccessToken :: Lens' PropertiesDelete (Maybe Text)
pdAccessToken
  = lens _pdAccessToken
      (\ s a -> s{_pdAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pdUploadType :: Lens' PropertiesDelete (Maybe Text)
pdUploadType
  = lens _pdUploadType (\ s a -> s{_pdUploadType = a})

-- | Required. The name of the Property to soft-delete. Format:
-- properties\/{property_id} Example: \"properties\/1000\"
pdName :: Lens' PropertiesDelete Text
pdName = lens _pdName (\ s a -> s{_pdName = a})

-- | JSONP
pdCallback :: Lens' PropertiesDelete (Maybe Text)
pdCallback
  = lens _pdCallback (\ s a -> s{_pdCallback = a})

instance GoogleRequest PropertiesDelete where
        type Rs PropertiesDelete = GoogleProtobufEmpty
        type Scopes PropertiesDelete =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesDelete'{..}
          = go _pdName _pdXgafv _pdUploadProtocol
              _pdAccessToken
              _pdUploadType
              _pdCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesDeleteResource)
                      mempty
