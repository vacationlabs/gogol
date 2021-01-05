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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user link on an account or property. If the user with the
-- specified email already has permissions on the account or property, then
-- the user\'s existing permissions will be unioned with the permissions
-- specified in the new UserLink.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.userLinks.create@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Create
    (
    -- * REST Resource
      AccountsUserLinksCreateResource

    -- * Creating a Request
    , accountsUserLinksCreate
    , AccountsUserLinksCreate

    -- * Request Lenses
    , aulcParent
    , aulcXgafv
    , aulcUploadProtocol
    , aulcAccessToken
    , aulcUploadType
    , aulcNotifyNewUser
    , aulcPayload
    , aulcCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.userLinks.create@ method which the
-- 'AccountsUserLinksCreate' request conforms to.
type AccountsUserLinksCreateResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "userLinks" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "notifyNewUser" Bool :>
                     QueryParam "callback" Text :>
                       QueryParam "alt" AltJSON :>
                         ReqBody '[JSON] GoogleAnalyticsAdminV1alphaUserLink
                           :> Post '[JSON] GoogleAnalyticsAdminV1alphaUserLink

-- | Creates a user link on an account or property. If the user with the
-- specified email already has permissions on the account or property, then
-- the user\'s existing permissions will be unioned with the permissions
-- specified in the new UserLink.
--
-- /See:/ 'accountsUserLinksCreate' smart constructor.
data AccountsUserLinksCreate =
  AccountsUserLinksCreate'
    { _aulcParent :: !Text
    , _aulcXgafv :: !(Maybe Xgafv)
    , _aulcUploadProtocol :: !(Maybe Text)
    , _aulcAccessToken :: !(Maybe Text)
    , _aulcUploadType :: !(Maybe Text)
    , _aulcNotifyNewUser :: !(Maybe Bool)
    , _aulcPayload :: !GoogleAnalyticsAdminV1alphaUserLink
    , _aulcCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsUserLinksCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aulcParent'
--
-- * 'aulcXgafv'
--
-- * 'aulcUploadProtocol'
--
-- * 'aulcAccessToken'
--
-- * 'aulcUploadType'
--
-- * 'aulcNotifyNewUser'
--
-- * 'aulcPayload'
--
-- * 'aulcCallback'
accountsUserLinksCreate
    :: Text -- ^ 'aulcParent'
    -> GoogleAnalyticsAdminV1alphaUserLink -- ^ 'aulcPayload'
    -> AccountsUserLinksCreate
accountsUserLinksCreate pAulcParent_ pAulcPayload_ =
  AccountsUserLinksCreate'
    { _aulcParent = pAulcParent_
    , _aulcXgafv = Nothing
    , _aulcUploadProtocol = Nothing
    , _aulcAccessToken = Nothing
    , _aulcUploadType = Nothing
    , _aulcNotifyNewUser = Nothing
    , _aulcPayload = pAulcPayload_
    , _aulcCallback = Nothing
    }


-- | Required. Example format: accounts\/1234
aulcParent :: Lens' AccountsUserLinksCreate Text
aulcParent
  = lens _aulcParent (\ s a -> s{_aulcParent = a})

-- | V1 error format.
aulcXgafv :: Lens' AccountsUserLinksCreate (Maybe Xgafv)
aulcXgafv
  = lens _aulcXgafv (\ s a -> s{_aulcXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aulcUploadProtocol :: Lens' AccountsUserLinksCreate (Maybe Text)
aulcUploadProtocol
  = lens _aulcUploadProtocol
      (\ s a -> s{_aulcUploadProtocol = a})

-- | OAuth access token.
aulcAccessToken :: Lens' AccountsUserLinksCreate (Maybe Text)
aulcAccessToken
  = lens _aulcAccessToken
      (\ s a -> s{_aulcAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aulcUploadType :: Lens' AccountsUserLinksCreate (Maybe Text)
aulcUploadType
  = lens _aulcUploadType
      (\ s a -> s{_aulcUploadType = a})

-- | Optional. If set, then email the new user notifying them that they\'ve
-- been granted permissions to the resource.
aulcNotifyNewUser :: Lens' AccountsUserLinksCreate (Maybe Bool)
aulcNotifyNewUser
  = lens _aulcNotifyNewUser
      (\ s a -> s{_aulcNotifyNewUser = a})

-- | Multipart request metadata.
aulcPayload :: Lens' AccountsUserLinksCreate GoogleAnalyticsAdminV1alphaUserLink
aulcPayload
  = lens _aulcPayload (\ s a -> s{_aulcPayload = a})

-- | JSONP
aulcCallback :: Lens' AccountsUserLinksCreate (Maybe Text)
aulcCallback
  = lens _aulcCallback (\ s a -> s{_aulcCallback = a})

instance GoogleRequest AccountsUserLinksCreate where
        type Rs AccountsUserLinksCreate =
             GoogleAnalyticsAdminV1alphaUserLink
        type Scopes AccountsUserLinksCreate =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient AccountsUserLinksCreate'{..}
          = go _aulcParent _aulcXgafv _aulcUploadProtocol
              _aulcAccessToken
              _aulcUploadType
              _aulcNotifyNewUser
              _aulcCallback
              (Just AltJSON)
              _aulcPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsUserLinksCreateResource)
                      mempty
