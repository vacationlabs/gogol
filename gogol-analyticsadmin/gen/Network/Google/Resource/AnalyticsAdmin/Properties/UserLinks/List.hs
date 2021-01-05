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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all user links on an account or property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.userLinks.list@.
module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.List
    (
    -- * REST Resource
      PropertiesUserLinksListResource

    -- * Creating a Request
    , propertiesUserLinksList
    , PropertiesUserLinksList

    -- * Request Lenses
    , pullParent
    , pullXgafv
    , pullUploadProtocol
    , pullAccessToken
    , pullUploadType
    , pullPageToken
    , pullPageSize
    , pullCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.userLinks.list@ method which the
-- 'PropertiesUserLinksList' request conforms to.
type PropertiesUserLinksListResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "userLinks" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "pageToken" Text :>
                     QueryParam "pageSize" (Textual Int32) :>
                       QueryParam "callback" Text :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON]
                             GoogleAnalyticsAdminV1alphaListUserLinksResponse

-- | Lists all user links on an account or property.
--
-- /See:/ 'propertiesUserLinksList' smart constructor.
data PropertiesUserLinksList =
  PropertiesUserLinksList'
    { _pullParent :: !Text
    , _pullXgafv :: !(Maybe Xgafv)
    , _pullUploadProtocol :: !(Maybe Text)
    , _pullAccessToken :: !(Maybe Text)
    , _pullUploadType :: !(Maybe Text)
    , _pullPageToken :: !(Maybe Text)
    , _pullPageSize :: !(Maybe (Textual Int32))
    , _pullCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesUserLinksList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pullParent'
--
-- * 'pullXgafv'
--
-- * 'pullUploadProtocol'
--
-- * 'pullAccessToken'
--
-- * 'pullUploadType'
--
-- * 'pullPageToken'
--
-- * 'pullPageSize'
--
-- * 'pullCallback'
propertiesUserLinksList
    :: Text -- ^ 'pullParent'
    -> PropertiesUserLinksList
propertiesUserLinksList pPullParent_ =
  PropertiesUserLinksList'
    { _pullParent = pPullParent_
    , _pullXgafv = Nothing
    , _pullUploadProtocol = Nothing
    , _pullAccessToken = Nothing
    , _pullUploadType = Nothing
    , _pullPageToken = Nothing
    , _pullPageSize = Nothing
    , _pullCallback = Nothing
    }


-- | Required. Example format: accounts\/1234
pullParent :: Lens' PropertiesUserLinksList Text
pullParent
  = lens _pullParent (\ s a -> s{_pullParent = a})

-- | V1 error format.
pullXgafv :: Lens' PropertiesUserLinksList (Maybe Xgafv)
pullXgafv
  = lens _pullXgafv (\ s a -> s{_pullXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pullUploadProtocol :: Lens' PropertiesUserLinksList (Maybe Text)
pullUploadProtocol
  = lens _pullUploadProtocol
      (\ s a -> s{_pullUploadProtocol = a})

-- | OAuth access token.
pullAccessToken :: Lens' PropertiesUserLinksList (Maybe Text)
pullAccessToken
  = lens _pullAccessToken
      (\ s a -> s{_pullAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pullUploadType :: Lens' PropertiesUserLinksList (Maybe Text)
pullUploadType
  = lens _pullUploadType
      (\ s a -> s{_pullUploadType = a})

-- | A page token, received from a previous \`ListUserLinks\` call. Provide
-- this to retrieve the subsequent page. When paginating, all other
-- parameters provided to \`ListUserLinks\` must match the call that
-- provided the page token.
pullPageToken :: Lens' PropertiesUserLinksList (Maybe Text)
pullPageToken
  = lens _pullPageToken
      (\ s a -> s{_pullPageToken = a})

-- | The maximum number of user links to return. The service may return fewer
-- than this value. If unspecified, at most 200 user links will be
-- returned. The maximum value is 500; values above 500 will be coerced to
-- 500.
pullPageSize :: Lens' PropertiesUserLinksList (Maybe Int32)
pullPageSize
  = lens _pullPageSize (\ s a -> s{_pullPageSize = a})
      . mapping _Coerce

-- | JSONP
pullCallback :: Lens' PropertiesUserLinksList (Maybe Text)
pullCallback
  = lens _pullCallback (\ s a -> s{_pullCallback = a})

instance GoogleRequest PropertiesUserLinksList where
        type Rs PropertiesUserLinksList =
             GoogleAnalyticsAdminV1alphaListUserLinksResponse
        type Scopes PropertiesUserLinksList =
             '["https://www.googleapis.com/auth/analytics.manage.users.readonly"]
        requestClient PropertiesUserLinksList'{..}
          = go _pullParent _pullXgafv _pullUploadProtocol
              _pullAccessToken
              _pullUploadType
              _pullPageToken
              _pullPageSize
              _pullCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesUserLinksListResource)
                      mempty
