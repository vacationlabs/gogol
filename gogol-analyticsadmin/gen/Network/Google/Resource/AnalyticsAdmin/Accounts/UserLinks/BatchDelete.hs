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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchDelete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes information about multiple users\' links to an account or
-- property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.userLinks.batchDelete@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchDelete
    (
    -- * REST Resource
      AccountsUserLinksBatchDeleteResource

    -- * Creating a Request
    , accountsUserLinksBatchDelete
    , AccountsUserLinksBatchDelete

    -- * Request Lenses
    , aulbdParent
    , aulbdXgafv
    , aulbdUploadProtocol
    , aulbdAccessToken
    , aulbdUploadType
    , aulbdPayload
    , aulbdCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.userLinks.batchDelete@ method which the
-- 'AccountsUserLinksBatchDelete' request conforms to.
type AccountsUserLinksBatchDeleteResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "userLinks:batchDelete" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
                         :> Post '[JSON] GoogleProtobufEmpty

-- | Deletes information about multiple users\' links to an account or
-- property.
--
-- /See:/ 'accountsUserLinksBatchDelete' smart constructor.
data AccountsUserLinksBatchDelete =
  AccountsUserLinksBatchDelete'
    { _aulbdParent :: !Text
    , _aulbdXgafv :: !(Maybe Xgafv)
    , _aulbdUploadProtocol :: !(Maybe Text)
    , _aulbdAccessToken :: !(Maybe Text)
    , _aulbdUploadType :: !(Maybe Text)
    , _aulbdPayload :: !GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
    , _aulbdCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsUserLinksBatchDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aulbdParent'
--
-- * 'aulbdXgafv'
--
-- * 'aulbdUploadProtocol'
--
-- * 'aulbdAccessToken'
--
-- * 'aulbdUploadType'
--
-- * 'aulbdPayload'
--
-- * 'aulbdCallback'
accountsUserLinksBatchDelete
    :: Text -- ^ 'aulbdParent'
    -> GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest -- ^ 'aulbdPayload'
    -> AccountsUserLinksBatchDelete
accountsUserLinksBatchDelete pAulbdParent_ pAulbdPayload_ =
  AccountsUserLinksBatchDelete'
    { _aulbdParent = pAulbdParent_
    , _aulbdXgafv = Nothing
    , _aulbdUploadProtocol = Nothing
    , _aulbdAccessToken = Nothing
    , _aulbdUploadType = Nothing
    , _aulbdPayload = pAulbdPayload_
    , _aulbdCallback = Nothing
    }


-- | Required. The account or property that all user links in the request are
-- for. The parent of all values for user link names to delete must match
-- this field. Example format: accounts\/1234
aulbdParent :: Lens' AccountsUserLinksBatchDelete Text
aulbdParent
  = lens _aulbdParent (\ s a -> s{_aulbdParent = a})

-- | V1 error format.
aulbdXgafv :: Lens' AccountsUserLinksBatchDelete (Maybe Xgafv)
aulbdXgafv
  = lens _aulbdXgafv (\ s a -> s{_aulbdXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aulbdUploadProtocol :: Lens' AccountsUserLinksBatchDelete (Maybe Text)
aulbdUploadProtocol
  = lens _aulbdUploadProtocol
      (\ s a -> s{_aulbdUploadProtocol = a})

-- | OAuth access token.
aulbdAccessToken :: Lens' AccountsUserLinksBatchDelete (Maybe Text)
aulbdAccessToken
  = lens _aulbdAccessToken
      (\ s a -> s{_aulbdAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aulbdUploadType :: Lens' AccountsUserLinksBatchDelete (Maybe Text)
aulbdUploadType
  = lens _aulbdUploadType
      (\ s a -> s{_aulbdUploadType = a})

-- | Multipart request metadata.
aulbdPayload :: Lens' AccountsUserLinksBatchDelete GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
aulbdPayload
  = lens _aulbdPayload (\ s a -> s{_aulbdPayload = a})

-- | JSONP
aulbdCallback :: Lens' AccountsUserLinksBatchDelete (Maybe Text)
aulbdCallback
  = lens _aulbdCallback
      (\ s a -> s{_aulbdCallback = a})

instance GoogleRequest AccountsUserLinksBatchDelete
         where
        type Rs AccountsUserLinksBatchDelete =
             GoogleProtobufEmpty
        type Scopes AccountsUserLinksBatchDelete =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient AccountsUserLinksBatchDelete'{..}
          = go _aulbdParent _aulbdXgafv _aulbdUploadProtocol
              _aulbdAccessToken
              _aulbdUploadType
              _aulbdCallback
              (Just AltJSON)
              _aulbdPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsUserLinksBatchDeleteResource)
                      mempty
