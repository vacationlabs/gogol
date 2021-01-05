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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists GoogleAdsLinks on a property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.googleAdsLinks.list@.
module Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.List
    (
    -- * REST Resource
      PropertiesGoogleAdsLinksListResource

    -- * Creating a Request
    , propertiesGoogleAdsLinksList
    , PropertiesGoogleAdsLinksList

    -- * Request Lenses
    , pgallParent
    , pgallXgafv
    , pgallUploadProtocol
    , pgallAccessToken
    , pgallUploadType
    , pgallPageToken
    , pgallPageSize
    , pgallCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.googleAdsLinks.list@ method which the
-- 'PropertiesGoogleAdsLinksList' request conforms to.
type PropertiesGoogleAdsLinksListResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "googleAdsLinks" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "pageToken" Text :>
                     QueryParam "pageSize" (Textual Int32) :>
                       QueryParam "callback" Text :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON]
                             GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse

-- | Lists GoogleAdsLinks on a property.
--
-- /See:/ 'propertiesGoogleAdsLinksList' smart constructor.
data PropertiesGoogleAdsLinksList =
  PropertiesGoogleAdsLinksList'
    { _pgallParent :: !Text
    , _pgallXgafv :: !(Maybe Xgafv)
    , _pgallUploadProtocol :: !(Maybe Text)
    , _pgallAccessToken :: !(Maybe Text)
    , _pgallUploadType :: !(Maybe Text)
    , _pgallPageToken :: !(Maybe Text)
    , _pgallPageSize :: !(Maybe (Textual Int32))
    , _pgallCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesGoogleAdsLinksList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgallParent'
--
-- * 'pgallXgafv'
--
-- * 'pgallUploadProtocol'
--
-- * 'pgallAccessToken'
--
-- * 'pgallUploadType'
--
-- * 'pgallPageToken'
--
-- * 'pgallPageSize'
--
-- * 'pgallCallback'
propertiesGoogleAdsLinksList
    :: Text -- ^ 'pgallParent'
    -> PropertiesGoogleAdsLinksList
propertiesGoogleAdsLinksList pPgallParent_ =
  PropertiesGoogleAdsLinksList'
    { _pgallParent = pPgallParent_
    , _pgallXgafv = Nothing
    , _pgallUploadProtocol = Nothing
    , _pgallAccessToken = Nothing
    , _pgallUploadType = Nothing
    , _pgallPageToken = Nothing
    , _pgallPageSize = Nothing
    , _pgallCallback = Nothing
    }


-- | Required. Example format: properties\/1234
pgallParent :: Lens' PropertiesGoogleAdsLinksList Text
pgallParent
  = lens _pgallParent (\ s a -> s{_pgallParent = a})

-- | V1 error format.
pgallXgafv :: Lens' PropertiesGoogleAdsLinksList (Maybe Xgafv)
pgallXgafv
  = lens _pgallXgafv (\ s a -> s{_pgallXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pgallUploadProtocol :: Lens' PropertiesGoogleAdsLinksList (Maybe Text)
pgallUploadProtocol
  = lens _pgallUploadProtocol
      (\ s a -> s{_pgallUploadProtocol = a})

-- | OAuth access token.
pgallAccessToken :: Lens' PropertiesGoogleAdsLinksList (Maybe Text)
pgallAccessToken
  = lens _pgallAccessToken
      (\ s a -> s{_pgallAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pgallUploadType :: Lens' PropertiesGoogleAdsLinksList (Maybe Text)
pgallUploadType
  = lens _pgallUploadType
      (\ s a -> s{_pgallUploadType = a})

-- | A page token, received from a previous \`ListGoogleAdsLinks\` call.
-- Provide this to retrieve the subsequent page. When paginating, all other
-- parameters provided to \`ListGoogleAdsLinks\` must match the call that
-- provided the page token.
pgallPageToken :: Lens' PropertiesGoogleAdsLinksList (Maybe Text)
pgallPageToken
  = lens _pgallPageToken
      (\ s a -> s{_pgallPageToken = a})

-- | The maximum number of resources to return. If unspecified, at most 50
-- resources will be returned. The maximum value is 200 (higher values will
-- be coerced to the maximum).
pgallPageSize :: Lens' PropertiesGoogleAdsLinksList (Maybe Int32)
pgallPageSize
  = lens _pgallPageSize
      (\ s a -> s{_pgallPageSize = a})
      . mapping _Coerce

-- | JSONP
pgallCallback :: Lens' PropertiesGoogleAdsLinksList (Maybe Text)
pgallCallback
  = lens _pgallCallback
      (\ s a -> s{_pgallCallback = a})

instance GoogleRequest PropertiesGoogleAdsLinksList
         where
        type Rs PropertiesGoogleAdsLinksList =
             GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse
        type Scopes PropertiesGoogleAdsLinksList =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient PropertiesGoogleAdsLinksList'{..}
          = go _pgallParent _pgallXgafv _pgallUploadProtocol
              _pgallAccessToken
              _pgallUploadType
              _pgallPageToken
              _pgallPageSize
              _pgallCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesGoogleAdsLinksListResource)
                      mempty
