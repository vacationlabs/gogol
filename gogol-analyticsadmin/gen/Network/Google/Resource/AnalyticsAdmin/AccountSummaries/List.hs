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
-- Module      : Network.Google.Resource.AnalyticsAdmin.AccountSummaries.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of all accounts accessible by the caller.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accountSummaries.list@.
module Network.Google.Resource.AnalyticsAdmin.AccountSummaries.List
    (
    -- * REST Resource
      AccountSummariesListResource

    -- * Creating a Request
    , accountSummariesList
    , AccountSummariesList

    -- * Request Lenses
    , aslXgafv
    , aslUploadProtocol
    , aslAccessToken
    , aslUploadType
    , aslPageToken
    , aslPageSize
    , aslCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accountSummaries.list@ method which the
-- 'AccountSummariesList' request conforms to.
type AccountSummariesListResource =
     "v1alpha" :>
       "accountSummaries" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "pageToken" Text :>
                   QueryParam "pageSize" (Textual Int32) :>
                     QueryParam "callback" Text :>
                       QueryParam "alt" AltJSON :>
                         Get '[JSON]
                           GoogleAnalyticsAdminV1alphaListAccountSummariesResponse

-- | Returns summaries of all accounts accessible by the caller.
--
-- /See:/ 'accountSummariesList' smart constructor.
data AccountSummariesList =
  AccountSummariesList'
    { _aslXgafv :: !(Maybe Xgafv)
    , _aslUploadProtocol :: !(Maybe Text)
    , _aslAccessToken :: !(Maybe Text)
    , _aslUploadType :: !(Maybe Text)
    , _aslPageToken :: !(Maybe Text)
    , _aslPageSize :: !(Maybe (Textual Int32))
    , _aslCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountSummariesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aslXgafv'
--
-- * 'aslUploadProtocol'
--
-- * 'aslAccessToken'
--
-- * 'aslUploadType'
--
-- * 'aslPageToken'
--
-- * 'aslPageSize'
--
-- * 'aslCallback'
accountSummariesList
    :: AccountSummariesList
accountSummariesList =
  AccountSummariesList'
    { _aslXgafv = Nothing
    , _aslUploadProtocol = Nothing
    , _aslAccessToken = Nothing
    , _aslUploadType = Nothing
    , _aslPageToken = Nothing
    , _aslPageSize = Nothing
    , _aslCallback = Nothing
    }


-- | V1 error format.
aslXgafv :: Lens' AccountSummariesList (Maybe Xgafv)
aslXgafv = lens _aslXgafv (\ s a -> s{_aslXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aslUploadProtocol :: Lens' AccountSummariesList (Maybe Text)
aslUploadProtocol
  = lens _aslUploadProtocol
      (\ s a -> s{_aslUploadProtocol = a})

-- | OAuth access token.
aslAccessToken :: Lens' AccountSummariesList (Maybe Text)
aslAccessToken
  = lens _aslAccessToken
      (\ s a -> s{_aslAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aslUploadType :: Lens' AccountSummariesList (Maybe Text)
aslUploadType
  = lens _aslUploadType
      (\ s a -> s{_aslUploadType = a})

-- | A page token, received from a previous \`ListAccountSummaries\` call.
-- Provide this to retrieve the subsequent page. When paginating, all other
-- parameters provided to \`ListAccountSummaries\` must match the call that
-- provided the page token.
aslPageToken :: Lens' AccountSummariesList (Maybe Text)
aslPageToken
  = lens _aslPageToken (\ s a -> s{_aslPageToken = a})

-- | The maximum number of AccountSummary resources to return. The service
-- may return fewer than this value, even if there are additional pages. If
-- unspecified, at most 50 resources will be returned. The maximum value is
-- 200; (higher values will be coerced to the maximum)
aslPageSize :: Lens' AccountSummariesList (Maybe Int32)
aslPageSize
  = lens _aslPageSize (\ s a -> s{_aslPageSize = a}) .
      mapping _Coerce

-- | JSONP
aslCallback :: Lens' AccountSummariesList (Maybe Text)
aslCallback
  = lens _aslCallback (\ s a -> s{_aslCallback = a})

instance GoogleRequest AccountSummariesList where
        type Rs AccountSummariesList =
             GoogleAnalyticsAdminV1alphaListAccountSummariesResponse
        type Scopes AccountSummariesList =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient AccountSummariesList'{..}
          = go _aslXgafv _aslUploadProtocol _aslAccessToken
              _aslUploadType
              _aslPageToken
              _aslPageSize
              _aslCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountSummariesListResource)
                      mempty
