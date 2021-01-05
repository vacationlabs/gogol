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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks target Account as soft-deleted (ie: \"trashed\") and returns it.
-- This API does not have a method to restore soft-deleted accounts.
-- However, they can be restored using the Trash Can UI. If the accounts
-- are not restored before the expiration time, the account and all child
-- resources (eg: Properties, GoogleAdsLinks, Streams, UserLinks) will be
-- permanently purged.
-- https:\/\/support.google.com\/analytics\/answer\/6154772 Returns an
-- error if the target is not found.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.delete@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.Delete
    (
    -- * REST Resource
      AccountsDeleteResource

    -- * Creating a Request
    , accountsDelete
    , AccountsDelete

    -- * Request Lenses
    , adXgafv
    , adUploadProtocol
    , adAccessToken
    , adUploadType
    , adName
    , adCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.delete@ method which the
-- 'AccountsDelete' request conforms to.
type AccountsDeleteResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Delete '[JSON] GoogleProtobufEmpty

-- | Marks target Account as soft-deleted (ie: \"trashed\") and returns it.
-- This API does not have a method to restore soft-deleted accounts.
-- However, they can be restored using the Trash Can UI. If the accounts
-- are not restored before the expiration time, the account and all child
-- resources (eg: Properties, GoogleAdsLinks, Streams, UserLinks) will be
-- permanently purged.
-- https:\/\/support.google.com\/analytics\/answer\/6154772 Returns an
-- error if the target is not found.
--
-- /See:/ 'accountsDelete' smart constructor.
data AccountsDelete =
  AccountsDelete'
    { _adXgafv :: !(Maybe Xgafv)
    , _adUploadProtocol :: !(Maybe Text)
    , _adAccessToken :: !(Maybe Text)
    , _adUploadType :: !(Maybe Text)
    , _adName :: !Text
    , _adCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adXgafv'
--
-- * 'adUploadProtocol'
--
-- * 'adAccessToken'
--
-- * 'adUploadType'
--
-- * 'adName'
--
-- * 'adCallback'
accountsDelete
    :: Text -- ^ 'adName'
    -> AccountsDelete
accountsDelete pAdName_ =
  AccountsDelete'
    { _adXgafv = Nothing
    , _adUploadProtocol = Nothing
    , _adAccessToken = Nothing
    , _adUploadType = Nothing
    , _adName = pAdName_
    , _adCallback = Nothing
    }


-- | V1 error format.
adXgafv :: Lens' AccountsDelete (Maybe Xgafv)
adXgafv = lens _adXgafv (\ s a -> s{_adXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
adUploadProtocol :: Lens' AccountsDelete (Maybe Text)
adUploadProtocol
  = lens _adUploadProtocol
      (\ s a -> s{_adUploadProtocol = a})

-- | OAuth access token.
adAccessToken :: Lens' AccountsDelete (Maybe Text)
adAccessToken
  = lens _adAccessToken
      (\ s a -> s{_adAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
adUploadType :: Lens' AccountsDelete (Maybe Text)
adUploadType
  = lens _adUploadType (\ s a -> s{_adUploadType = a})

-- | Required. The name of the Account to soft-delete. Format:
-- accounts\/{account} Example: \"accounts\/100\"
adName :: Lens' AccountsDelete Text
adName = lens _adName (\ s a -> s{_adName = a})

-- | JSONP
adCallback :: Lens' AccountsDelete (Maybe Text)
adCallback
  = lens _adCallback (\ s a -> s{_adCallback = a})

instance GoogleRequest AccountsDelete where
        type Rs AccountsDelete = GoogleProtobufEmpty
        type Scopes AccountsDelete =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient AccountsDelete'{..}
          = go _adName _adXgafv _adUploadProtocol
              _adAccessToken
              _adUploadType
              _adCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient (Proxy :: Proxy AccountsDeleteResource)
                      mempty
