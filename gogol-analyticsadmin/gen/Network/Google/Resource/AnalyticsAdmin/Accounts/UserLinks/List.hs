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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all user links on an account or property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.userLinks.list@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.List
    (
    -- * REST Resource
      AccountsUserLinksListResource

    -- * Creating a Request
    , accountsUserLinksList
    , AccountsUserLinksList

    -- * Request Lenses
    , aullParent
    , aullXgafv
    , aullUploadProtocol
    , aullAccessToken
    , aullUploadType
    , aullPageToken
    , aullPageSize
    , aullCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.userLinks.list@ method which the
-- 'AccountsUserLinksList' request conforms to.
type AccountsUserLinksListResource =
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
-- /See:/ 'accountsUserLinksList' smart constructor.
data AccountsUserLinksList =
  AccountsUserLinksList'
    { _aullParent :: !Text
    , _aullXgafv :: !(Maybe Xgafv)
    , _aullUploadProtocol :: !(Maybe Text)
    , _aullAccessToken :: !(Maybe Text)
    , _aullUploadType :: !(Maybe Text)
    , _aullPageToken :: !(Maybe Text)
    , _aullPageSize :: !(Maybe (Textual Int32))
    , _aullCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsUserLinksList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aullParent'
--
-- * 'aullXgafv'
--
-- * 'aullUploadProtocol'
--
-- * 'aullAccessToken'
--
-- * 'aullUploadType'
--
-- * 'aullPageToken'
--
-- * 'aullPageSize'
--
-- * 'aullCallback'
accountsUserLinksList
    :: Text -- ^ 'aullParent'
    -> AccountsUserLinksList
accountsUserLinksList pAullParent_ =
  AccountsUserLinksList'
    { _aullParent = pAullParent_
    , _aullXgafv = Nothing
    , _aullUploadProtocol = Nothing
    , _aullAccessToken = Nothing
    , _aullUploadType = Nothing
    , _aullPageToken = Nothing
    , _aullPageSize = Nothing
    , _aullCallback = Nothing
    }


-- | Required. Example format: accounts\/1234
aullParent :: Lens' AccountsUserLinksList Text
aullParent
  = lens _aullParent (\ s a -> s{_aullParent = a})

-- | V1 error format.
aullXgafv :: Lens' AccountsUserLinksList (Maybe Xgafv)
aullXgafv
  = lens _aullXgafv (\ s a -> s{_aullXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aullUploadProtocol :: Lens' AccountsUserLinksList (Maybe Text)
aullUploadProtocol
  = lens _aullUploadProtocol
      (\ s a -> s{_aullUploadProtocol = a})

-- | OAuth access token.
aullAccessToken :: Lens' AccountsUserLinksList (Maybe Text)
aullAccessToken
  = lens _aullAccessToken
      (\ s a -> s{_aullAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aullUploadType :: Lens' AccountsUserLinksList (Maybe Text)
aullUploadType
  = lens _aullUploadType
      (\ s a -> s{_aullUploadType = a})

-- | A page token, received from a previous \`ListUserLinks\` call. Provide
-- this to retrieve the subsequent page. When paginating, all other
-- parameters provided to \`ListUserLinks\` must match the call that
-- provided the page token.
aullPageToken :: Lens' AccountsUserLinksList (Maybe Text)
aullPageToken
  = lens _aullPageToken
      (\ s a -> s{_aullPageToken = a})

-- | The maximum number of user links to return. The service may return fewer
-- than this value. If unspecified, at most 200 user links will be
-- returned. The maximum value is 500; values above 500 will be coerced to
-- 500.
aullPageSize :: Lens' AccountsUserLinksList (Maybe Int32)
aullPageSize
  = lens _aullPageSize (\ s a -> s{_aullPageSize = a})
      . mapping _Coerce

-- | JSONP
aullCallback :: Lens' AccountsUserLinksList (Maybe Text)
aullCallback
  = lens _aullCallback (\ s a -> s{_aullCallback = a})

instance GoogleRequest AccountsUserLinksList where
        type Rs AccountsUserLinksList =
             GoogleAnalyticsAdminV1alphaListUserLinksResponse
        type Scopes AccountsUserLinksList =
             '["https://www.googleapis.com/auth/analytics.manage.users.readonly"]
        requestClient AccountsUserLinksList'{..}
          = go _aullParent _aullXgafv _aullUploadProtocol
              _aullAccessToken
              _aullUploadType
              _aullPageToken
              _aullPageSize
              _aullCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsUserLinksListResource)
                      mempty
