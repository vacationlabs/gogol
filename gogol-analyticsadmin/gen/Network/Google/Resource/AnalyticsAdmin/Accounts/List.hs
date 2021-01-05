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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all accounts accessible by the caller. Note that these accounts
-- might not currently have GA4 properties. Soft-deleted (ie: \"trashed\")
-- accounts are excluded by default. Returns an empty list if no relevant
-- accounts are found.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.list@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.List
    (
    -- * REST Resource
      AccountsListResource

    -- * Creating a Request
    , accountsList
    , AccountsList

    -- * Request Lenses
    , alXgafv
    , alUploadProtocol
    , alAccessToken
    , alUploadType
    , alShowDeleted
    , alPageToken
    , alPageSize
    , alCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.list@ method which the
-- 'AccountsList' request conforms to.
type AccountsListResource =
     "v1alpha" :>
       "accounts" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "showDeleted" Bool :>
                   QueryParam "pageToken" Text :>
                     QueryParam "pageSize" (Textual Int32) :>
                       QueryParam "callback" Text :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON]
                             GoogleAnalyticsAdminV1alphaListAccountsResponse

-- | Returns all accounts accessible by the caller. Note that these accounts
-- might not currently have GA4 properties. Soft-deleted (ie: \"trashed\")
-- accounts are excluded by default. Returns an empty list if no relevant
-- accounts are found.
--
-- /See:/ 'accountsList' smart constructor.
data AccountsList =
  AccountsList'
    { _alXgafv :: !(Maybe Xgafv)
    , _alUploadProtocol :: !(Maybe Text)
    , _alAccessToken :: !(Maybe Text)
    , _alUploadType :: !(Maybe Text)
    , _alShowDeleted :: !(Maybe Bool)
    , _alPageToken :: !(Maybe Text)
    , _alPageSize :: !(Maybe (Textual Int32))
    , _alCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alXgafv'
--
-- * 'alUploadProtocol'
--
-- * 'alAccessToken'
--
-- * 'alUploadType'
--
-- * 'alShowDeleted'
--
-- * 'alPageToken'
--
-- * 'alPageSize'
--
-- * 'alCallback'
accountsList
    :: AccountsList
accountsList =
  AccountsList'
    { _alXgafv = Nothing
    , _alUploadProtocol = Nothing
    , _alAccessToken = Nothing
    , _alUploadType = Nothing
    , _alShowDeleted = Nothing
    , _alPageToken = Nothing
    , _alPageSize = Nothing
    , _alCallback = Nothing
    }


-- | V1 error format.
alXgafv :: Lens' AccountsList (Maybe Xgafv)
alXgafv = lens _alXgafv (\ s a -> s{_alXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
alUploadProtocol :: Lens' AccountsList (Maybe Text)
alUploadProtocol
  = lens _alUploadProtocol
      (\ s a -> s{_alUploadProtocol = a})

-- | OAuth access token.
alAccessToken :: Lens' AccountsList (Maybe Text)
alAccessToken
  = lens _alAccessToken
      (\ s a -> s{_alAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
alUploadType :: Lens' AccountsList (Maybe Text)
alUploadType
  = lens _alUploadType (\ s a -> s{_alUploadType = a})

-- | Whether to include soft-deleted (ie: \"trashed\") Accounts in the
-- results. Accounts can be inspected to determine whether they are deleted
-- or not.
alShowDeleted :: Lens' AccountsList (Maybe Bool)
alShowDeleted
  = lens _alShowDeleted
      (\ s a -> s{_alShowDeleted = a})

-- | A page token, received from a previous \`ListAccounts\` call. Provide
-- this to retrieve the subsequent page. When paginating, all other
-- parameters provided to \`ListAccounts\` must match the call that
-- provided the page token.
alPageToken :: Lens' AccountsList (Maybe Text)
alPageToken
  = lens _alPageToken (\ s a -> s{_alPageToken = a})

-- | The maximum number of resources to return. The service may return fewer
-- than this value, even if there are additional pages. If unspecified, at
-- most 50 resources will be returned. The maximum value is 200; (higher
-- values will be coerced to the maximum)
alPageSize :: Lens' AccountsList (Maybe Int32)
alPageSize
  = lens _alPageSize (\ s a -> s{_alPageSize = a}) .
      mapping _Coerce

-- | JSONP
alCallback :: Lens' AccountsList (Maybe Text)
alCallback
  = lens _alCallback (\ s a -> s{_alCallback = a})

instance GoogleRequest AccountsList where
        type Rs AccountsList =
             GoogleAnalyticsAdminV1alphaListAccountsResponse
        type Scopes AccountsList =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient AccountsList'{..}
          = go _alXgafv _alUploadProtocol _alAccessToken
              _alUploadType
              _alShowDeleted
              _alPageToken
              _alPageSize
              _alCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient (Proxy :: Proxy AccountsListResource)
                      mempty
