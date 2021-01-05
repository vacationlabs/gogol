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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user link on an account or property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.userLinks.delete@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Delete
    (
    -- * REST Resource
      AccountsUserLinksDeleteResource

    -- * Creating a Request
    , accountsUserLinksDelete
    , AccountsUserLinksDelete

    -- * Request Lenses
    , auldXgafv
    , auldUploadProtocol
    , auldAccessToken
    , auldUploadType
    , auldName
    , auldCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.userLinks.delete@ method which the
-- 'AccountsUserLinksDelete' request conforms to.
type AccountsUserLinksDeleteResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Delete '[JSON] GoogleProtobufEmpty

-- | Deletes a user link on an account or property.
--
-- /See:/ 'accountsUserLinksDelete' smart constructor.
data AccountsUserLinksDelete =
  AccountsUserLinksDelete'
    { _auldXgafv :: !(Maybe Xgafv)
    , _auldUploadProtocol :: !(Maybe Text)
    , _auldAccessToken :: !(Maybe Text)
    , _auldUploadType :: !(Maybe Text)
    , _auldName :: !Text
    , _auldCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsUserLinksDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'auldXgafv'
--
-- * 'auldUploadProtocol'
--
-- * 'auldAccessToken'
--
-- * 'auldUploadType'
--
-- * 'auldName'
--
-- * 'auldCallback'
accountsUserLinksDelete
    :: Text -- ^ 'auldName'
    -> AccountsUserLinksDelete
accountsUserLinksDelete pAuldName_ =
  AccountsUserLinksDelete'
    { _auldXgafv = Nothing
    , _auldUploadProtocol = Nothing
    , _auldAccessToken = Nothing
    , _auldUploadType = Nothing
    , _auldName = pAuldName_
    , _auldCallback = Nothing
    }


-- | V1 error format.
auldXgafv :: Lens' AccountsUserLinksDelete (Maybe Xgafv)
auldXgafv
  = lens _auldXgafv (\ s a -> s{_auldXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
auldUploadProtocol :: Lens' AccountsUserLinksDelete (Maybe Text)
auldUploadProtocol
  = lens _auldUploadProtocol
      (\ s a -> s{_auldUploadProtocol = a})

-- | OAuth access token.
auldAccessToken :: Lens' AccountsUserLinksDelete (Maybe Text)
auldAccessToken
  = lens _auldAccessToken
      (\ s a -> s{_auldAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
auldUploadType :: Lens' AccountsUserLinksDelete (Maybe Text)
auldUploadType
  = lens _auldUploadType
      (\ s a -> s{_auldUploadType = a})

-- | Required. Example format: accounts\/1234\/userLinks\/5678
auldName :: Lens' AccountsUserLinksDelete Text
auldName = lens _auldName (\ s a -> s{_auldName = a})

-- | JSONP
auldCallback :: Lens' AccountsUserLinksDelete (Maybe Text)
auldCallback
  = lens _auldCallback (\ s a -> s{_auldCallback = a})

instance GoogleRequest AccountsUserLinksDelete where
        type Rs AccountsUserLinksDelete = GoogleProtobufEmpty
        type Scopes AccountsUserLinksDelete =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient AccountsUserLinksDelete'{..}
          = go _auldName _auldXgafv _auldUploadProtocol
              _auldAccessToken
              _auldUploadType
              _auldCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsUserLinksDeleteResource)
                      mempty
