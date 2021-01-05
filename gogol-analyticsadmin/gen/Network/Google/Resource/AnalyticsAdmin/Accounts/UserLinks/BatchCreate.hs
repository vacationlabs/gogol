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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchCreate
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates information about multiple users\' links to an account or
-- property. This method is transactional. If any UserLink cannot be
-- created, none of the UserLinks will be created.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.userLinks.batchCreate@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchCreate
    (
    -- * REST Resource
      AccountsUserLinksBatchCreateResource

    -- * Creating a Request
    , accountsUserLinksBatchCreate
    , AccountsUserLinksBatchCreate

    -- * Request Lenses
    , aulbcParent
    , aulbcXgafv
    , aulbcUploadProtocol
    , aulbcAccessToken
    , aulbcUploadType
    , aulbcPayload
    , aulbcCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.userLinks.batchCreate@ method which the
-- 'AccountsUserLinksBatchCreate' request conforms to.
type AccountsUserLinksBatchCreateResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "userLinks:batchCreate" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
                         :>
                         Post '[JSON]
                           GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse

-- | Creates information about multiple users\' links to an account or
-- property. This method is transactional. If any UserLink cannot be
-- created, none of the UserLinks will be created.
--
-- /See:/ 'accountsUserLinksBatchCreate' smart constructor.
data AccountsUserLinksBatchCreate =
  AccountsUserLinksBatchCreate'
    { _aulbcParent :: !Text
    , _aulbcXgafv :: !(Maybe Xgafv)
    , _aulbcUploadProtocol :: !(Maybe Text)
    , _aulbcAccessToken :: !(Maybe Text)
    , _aulbcUploadType :: !(Maybe Text)
    , _aulbcPayload :: !GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
    , _aulbcCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsUserLinksBatchCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aulbcParent'
--
-- * 'aulbcXgafv'
--
-- * 'aulbcUploadProtocol'
--
-- * 'aulbcAccessToken'
--
-- * 'aulbcUploadType'
--
-- * 'aulbcPayload'
--
-- * 'aulbcCallback'
accountsUserLinksBatchCreate
    :: Text -- ^ 'aulbcParent'
    -> GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest -- ^ 'aulbcPayload'
    -> AccountsUserLinksBatchCreate
accountsUserLinksBatchCreate pAulbcParent_ pAulbcPayload_ =
  AccountsUserLinksBatchCreate'
    { _aulbcParent = pAulbcParent_
    , _aulbcXgafv = Nothing
    , _aulbcUploadProtocol = Nothing
    , _aulbcAccessToken = Nothing
    , _aulbcUploadType = Nothing
    , _aulbcPayload = pAulbcPayload_
    , _aulbcCallback = Nothing
    }


-- | Required. The account or property that all user links in the request are
-- for. This field is required. The parent field in the
-- CreateUserLinkRequest messages must either be empty or match this field.
-- Example format: accounts\/1234
aulbcParent :: Lens' AccountsUserLinksBatchCreate Text
aulbcParent
  = lens _aulbcParent (\ s a -> s{_aulbcParent = a})

-- | V1 error format.
aulbcXgafv :: Lens' AccountsUserLinksBatchCreate (Maybe Xgafv)
aulbcXgafv
  = lens _aulbcXgafv (\ s a -> s{_aulbcXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aulbcUploadProtocol :: Lens' AccountsUserLinksBatchCreate (Maybe Text)
aulbcUploadProtocol
  = lens _aulbcUploadProtocol
      (\ s a -> s{_aulbcUploadProtocol = a})

-- | OAuth access token.
aulbcAccessToken :: Lens' AccountsUserLinksBatchCreate (Maybe Text)
aulbcAccessToken
  = lens _aulbcAccessToken
      (\ s a -> s{_aulbcAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aulbcUploadType :: Lens' AccountsUserLinksBatchCreate (Maybe Text)
aulbcUploadType
  = lens _aulbcUploadType
      (\ s a -> s{_aulbcUploadType = a})

-- | Multipart request metadata.
aulbcPayload :: Lens' AccountsUserLinksBatchCreate GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
aulbcPayload
  = lens _aulbcPayload (\ s a -> s{_aulbcPayload = a})

-- | JSONP
aulbcCallback :: Lens' AccountsUserLinksBatchCreate (Maybe Text)
aulbcCallback
  = lens _aulbcCallback
      (\ s a -> s{_aulbcCallback = a})

instance GoogleRequest AccountsUserLinksBatchCreate
         where
        type Rs AccountsUserLinksBatchCreate =
             GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse
        type Scopes AccountsUserLinksBatchCreate =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient AccountsUserLinksBatchCreate'{..}
          = go _aulbcParent _aulbcXgafv _aulbcUploadProtocol
              _aulbcAccessToken
              _aulbcUploadType
              _aulbcCallback
              (Just AltJSON)
              _aulbcPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsUserLinksBatchCreateResource)
                      mempty
