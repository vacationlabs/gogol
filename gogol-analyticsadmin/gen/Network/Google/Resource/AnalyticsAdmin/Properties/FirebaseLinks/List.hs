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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists FirebaseLinks on a property. Properties can have at most one
-- FirebaseLink.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.firebaseLinks.list@.
module Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.List
    (
    -- * REST Resource
      PropertiesFirebaseLinksListResource

    -- * Creating a Request
    , propertiesFirebaseLinksList
    , PropertiesFirebaseLinksList

    -- * Request Lenses
    , pfllParent
    , pfllXgafv
    , pfllUploadProtocol
    , pfllAccessToken
    , pfllUploadType
    , pfllPageToken
    , pfllPageSize
    , pfllCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.firebaseLinks.list@ method which the
-- 'PropertiesFirebaseLinksList' request conforms to.
type PropertiesFirebaseLinksListResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "firebaseLinks" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "pageToken" Text :>
                     QueryParam "pageSize" (Textual Int32) :>
                       QueryParam "callback" Text :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON]
                             GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse

-- | Lists FirebaseLinks on a property. Properties can have at most one
-- FirebaseLink.
--
-- /See:/ 'propertiesFirebaseLinksList' smart constructor.
data PropertiesFirebaseLinksList =
  PropertiesFirebaseLinksList'
    { _pfllParent :: !Text
    , _pfllXgafv :: !(Maybe Xgafv)
    , _pfllUploadProtocol :: !(Maybe Text)
    , _pfllAccessToken :: !(Maybe Text)
    , _pfllUploadType :: !(Maybe Text)
    , _pfllPageToken :: !(Maybe Text)
    , _pfllPageSize :: !(Maybe (Textual Int32))
    , _pfllCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesFirebaseLinksList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfllParent'
--
-- * 'pfllXgafv'
--
-- * 'pfllUploadProtocol'
--
-- * 'pfllAccessToken'
--
-- * 'pfllUploadType'
--
-- * 'pfllPageToken'
--
-- * 'pfllPageSize'
--
-- * 'pfllCallback'
propertiesFirebaseLinksList
    :: Text -- ^ 'pfllParent'
    -> PropertiesFirebaseLinksList
propertiesFirebaseLinksList pPfllParent_ =
  PropertiesFirebaseLinksList'
    { _pfllParent = pPfllParent_
    , _pfllXgafv = Nothing
    , _pfllUploadProtocol = Nothing
    , _pfllAccessToken = Nothing
    , _pfllUploadType = Nothing
    , _pfllPageToken = Nothing
    , _pfllPageSize = Nothing
    , _pfllCallback = Nothing
    }


-- | Required. Format: properties\/{property_id} Example: properties\/1234
pfllParent :: Lens' PropertiesFirebaseLinksList Text
pfllParent
  = lens _pfllParent (\ s a -> s{_pfllParent = a})

-- | V1 error format.
pfllXgafv :: Lens' PropertiesFirebaseLinksList (Maybe Xgafv)
pfllXgafv
  = lens _pfllXgafv (\ s a -> s{_pfllXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pfllUploadProtocol :: Lens' PropertiesFirebaseLinksList (Maybe Text)
pfllUploadProtocol
  = lens _pfllUploadProtocol
      (\ s a -> s{_pfllUploadProtocol = a})

-- | OAuth access token.
pfllAccessToken :: Lens' PropertiesFirebaseLinksList (Maybe Text)
pfllAccessToken
  = lens _pfllAccessToken
      (\ s a -> s{_pfllAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pfllUploadType :: Lens' PropertiesFirebaseLinksList (Maybe Text)
pfllUploadType
  = lens _pfllUploadType
      (\ s a -> s{_pfllUploadType = a})

-- | A page token, received from a previous \`ListFirebaseLinks\` call.
-- Provide this to retrieve the subsequent page. When paginating, all other
-- parameters provided to \`ListProperties\` must match the call that
-- provided the page token.
pfllPageToken :: Lens' PropertiesFirebaseLinksList (Maybe Text)
pfllPageToken
  = lens _pfllPageToken
      (\ s a -> s{_pfllPageToken = a})

-- | The maximum number of resources to return. The service may return fewer
-- than this value, even if there are additional pages. If unspecified, at
-- most 50 resources will be returned. The maximum value is 200; (higher
-- values will be coerced to the maximum)
pfllPageSize :: Lens' PropertiesFirebaseLinksList (Maybe Int32)
pfllPageSize
  = lens _pfllPageSize (\ s a -> s{_pfllPageSize = a})
      . mapping _Coerce

-- | JSONP
pfllCallback :: Lens' PropertiesFirebaseLinksList (Maybe Text)
pfllCallback
  = lens _pfllCallback (\ s a -> s{_pfllCallback = a})

instance GoogleRequest PropertiesFirebaseLinksList
         where
        type Rs PropertiesFirebaseLinksList =
             GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse
        type Scopes PropertiesFirebaseLinksList =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient PropertiesFirebaseLinksList'{..}
          = go _pfllParent _pfllXgafv _pfllUploadProtocol
              _pfllAccessToken
              _pfllUploadType
              _pfllPageToken
              _pfllPageSize
              _pfllCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesFirebaseLinksListResource)
                      mempty
