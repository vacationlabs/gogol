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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchUpdate
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about multiple users\' links to an account or
-- property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.userLinks.batchUpdate@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchUpdate
    (
    -- * REST Resource
      AccountsUserLinksBatchUpdateResource

    -- * Creating a Request
    , accountsUserLinksBatchUpdate
    , AccountsUserLinksBatchUpdate

    -- * Request Lenses
    , aulbuParent
    , aulbuXgafv
    , aulbuUploadProtocol
    , aulbuAccessToken
    , aulbuUploadType
    , aulbuPayload
    , aulbuCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.userLinks.batchUpdate@ method which the
-- 'AccountsUserLinksBatchUpdate' request conforms to.
type AccountsUserLinksBatchUpdateResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "userLinks:batchUpdate" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
                         :>
                         Post '[JSON]
                           GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse

-- | Updates information about multiple users\' links to an account or
-- property.
--
-- /See:/ 'accountsUserLinksBatchUpdate' smart constructor.
data AccountsUserLinksBatchUpdate =
  AccountsUserLinksBatchUpdate'
    { _aulbuParent :: !Text
    , _aulbuXgafv :: !(Maybe Xgafv)
    , _aulbuUploadProtocol :: !(Maybe Text)
    , _aulbuAccessToken :: !(Maybe Text)
    , _aulbuUploadType :: !(Maybe Text)
    , _aulbuPayload :: !GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
    , _aulbuCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsUserLinksBatchUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aulbuParent'
--
-- * 'aulbuXgafv'
--
-- * 'aulbuUploadProtocol'
--
-- * 'aulbuAccessToken'
--
-- * 'aulbuUploadType'
--
-- * 'aulbuPayload'
--
-- * 'aulbuCallback'
accountsUserLinksBatchUpdate
    :: Text -- ^ 'aulbuParent'
    -> GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest -- ^ 'aulbuPayload'
    -> AccountsUserLinksBatchUpdate
accountsUserLinksBatchUpdate pAulbuParent_ pAulbuPayload_ =
  AccountsUserLinksBatchUpdate'
    { _aulbuParent = pAulbuParent_
    , _aulbuXgafv = Nothing
    , _aulbuUploadProtocol = Nothing
    , _aulbuAccessToken = Nothing
    , _aulbuUploadType = Nothing
    , _aulbuPayload = pAulbuPayload_
    , _aulbuCallback = Nothing
    }


-- | Required. The account or property that all user links in the request are
-- for. The parent field in the UpdateUserLinkRequest messages must either
-- be empty or match this field. Example format: accounts\/1234
aulbuParent :: Lens' AccountsUserLinksBatchUpdate Text
aulbuParent
  = lens _aulbuParent (\ s a -> s{_aulbuParent = a})

-- | V1 error format.
aulbuXgafv :: Lens' AccountsUserLinksBatchUpdate (Maybe Xgafv)
aulbuXgafv
  = lens _aulbuXgafv (\ s a -> s{_aulbuXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aulbuUploadProtocol :: Lens' AccountsUserLinksBatchUpdate (Maybe Text)
aulbuUploadProtocol
  = lens _aulbuUploadProtocol
      (\ s a -> s{_aulbuUploadProtocol = a})

-- | OAuth access token.
aulbuAccessToken :: Lens' AccountsUserLinksBatchUpdate (Maybe Text)
aulbuAccessToken
  = lens _aulbuAccessToken
      (\ s a -> s{_aulbuAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aulbuUploadType :: Lens' AccountsUserLinksBatchUpdate (Maybe Text)
aulbuUploadType
  = lens _aulbuUploadType
      (\ s a -> s{_aulbuUploadType = a})

-- | Multipart request metadata.
aulbuPayload :: Lens' AccountsUserLinksBatchUpdate GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
aulbuPayload
  = lens _aulbuPayload (\ s a -> s{_aulbuPayload = a})

-- | JSONP
aulbuCallback :: Lens' AccountsUserLinksBatchUpdate (Maybe Text)
aulbuCallback
  = lens _aulbuCallback
      (\ s a -> s{_aulbuCallback = a})

instance GoogleRequest AccountsUserLinksBatchUpdate
         where
        type Rs AccountsUserLinksBatchUpdate =
             GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse
        type Scopes AccountsUserLinksBatchUpdate =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient AccountsUserLinksBatchUpdate'{..}
          = go _aulbuParent _aulbuXgafv _aulbuUploadProtocol
              _aulbuAccessToken
              _aulbuUploadType
              _aulbuCallback
              (Just AltJSON)
              _aulbuPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsUserLinksBatchUpdateResource)
                      mempty
