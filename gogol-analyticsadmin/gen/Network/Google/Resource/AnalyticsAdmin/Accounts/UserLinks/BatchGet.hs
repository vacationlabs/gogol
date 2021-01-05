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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchGet
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about multiple users\' links to an account or property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.userLinks.batchGet@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchGet
    (
    -- * REST Resource
      AccountsUserLinksBatchGetResource

    -- * Creating a Request
    , accountsUserLinksBatchGet
    , AccountsUserLinksBatchGet

    -- * Request Lenses
    , aulbgParent
    , aulbgXgafv
    , aulbgUploadProtocol
    , aulbgAccessToken
    , aulbgUploadType
    , aulbgNames
    , aulbgCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.userLinks.batchGet@ method which the
-- 'AccountsUserLinksBatchGet' request conforms to.
type AccountsUserLinksBatchGetResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "userLinks:batchGet" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParams "names" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "alt" AltJSON :>
                         Get '[JSON]
                           GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse

-- | Gets information about multiple users\' links to an account or property.
--
-- /See:/ 'accountsUserLinksBatchGet' smart constructor.
data AccountsUserLinksBatchGet =
  AccountsUserLinksBatchGet'
    { _aulbgParent :: !Text
    , _aulbgXgafv :: !(Maybe Xgafv)
    , _aulbgUploadProtocol :: !(Maybe Text)
    , _aulbgAccessToken :: !(Maybe Text)
    , _aulbgUploadType :: !(Maybe Text)
    , _aulbgNames :: !(Maybe [Text])
    , _aulbgCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsUserLinksBatchGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aulbgParent'
--
-- * 'aulbgXgafv'
--
-- * 'aulbgUploadProtocol'
--
-- * 'aulbgAccessToken'
--
-- * 'aulbgUploadType'
--
-- * 'aulbgNames'
--
-- * 'aulbgCallback'
accountsUserLinksBatchGet
    :: Text -- ^ 'aulbgParent'
    -> AccountsUserLinksBatchGet
accountsUserLinksBatchGet pAulbgParent_ =
  AccountsUserLinksBatchGet'
    { _aulbgParent = pAulbgParent_
    , _aulbgXgafv = Nothing
    , _aulbgUploadProtocol = Nothing
    , _aulbgAccessToken = Nothing
    , _aulbgUploadType = Nothing
    , _aulbgNames = Nothing
    , _aulbgCallback = Nothing
    }


-- | Required. The account or property that all user links in the request are
-- for. The parent of all provided values for the \'names\' field must
-- match this field. Example format: accounts\/1234
aulbgParent :: Lens' AccountsUserLinksBatchGet Text
aulbgParent
  = lens _aulbgParent (\ s a -> s{_aulbgParent = a})

-- | V1 error format.
aulbgXgafv :: Lens' AccountsUserLinksBatchGet (Maybe Xgafv)
aulbgXgafv
  = lens _aulbgXgafv (\ s a -> s{_aulbgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aulbgUploadProtocol :: Lens' AccountsUserLinksBatchGet (Maybe Text)
aulbgUploadProtocol
  = lens _aulbgUploadProtocol
      (\ s a -> s{_aulbgUploadProtocol = a})

-- | OAuth access token.
aulbgAccessToken :: Lens' AccountsUserLinksBatchGet (Maybe Text)
aulbgAccessToken
  = lens _aulbgAccessToken
      (\ s a -> s{_aulbgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aulbgUploadType :: Lens' AccountsUserLinksBatchGet (Maybe Text)
aulbgUploadType
  = lens _aulbgUploadType
      (\ s a -> s{_aulbgUploadType = a})

-- | Required. The names of the user links to retrieve. A maximum of 1000
-- user links can be retrieved in a batch. Format:
-- accounts\/{accountId}\/userLinks\/{userLinkId}
aulbgNames :: Lens' AccountsUserLinksBatchGet [Text]
aulbgNames
  = lens _aulbgNames (\ s a -> s{_aulbgNames = a}) .
      _Default
      . _Coerce

-- | JSONP
aulbgCallback :: Lens' AccountsUserLinksBatchGet (Maybe Text)
aulbgCallback
  = lens _aulbgCallback
      (\ s a -> s{_aulbgCallback = a})

instance GoogleRequest AccountsUserLinksBatchGet
         where
        type Rs AccountsUserLinksBatchGet =
             GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse
        type Scopes AccountsUserLinksBatchGet =
             '["https://www.googleapis.com/auth/analytics.manage.users.readonly"]
        requestClient AccountsUserLinksBatchGet'{..}
          = go _aulbgParent _aulbgXgafv _aulbgUploadProtocol
              _aulbgAccessToken
              _aulbgUploadType
              (_aulbgNames ^. _Default)
              _aulbgCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsUserLinksBatchGetResource)
                      mempty
