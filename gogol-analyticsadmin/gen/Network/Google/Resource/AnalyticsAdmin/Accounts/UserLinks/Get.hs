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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a user\'s link to an account or property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.userLinks.get@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Get
    (
    -- * REST Resource
      AccountsUserLinksGetResource

    -- * Creating a Request
    , accountsUserLinksGet
    , AccountsUserLinksGet

    -- * Request Lenses
    , aulgXgafv
    , aulgUploadProtocol
    , aulgAccessToken
    , aulgUploadType
    , aulgName
    , aulgCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.userLinks.get@ method which the
-- 'AccountsUserLinksGet' request conforms to.
type AccountsUserLinksGetResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] GoogleAnalyticsAdminV1alphaUserLink

-- | Gets information about a user\'s link to an account or property.
--
-- /See:/ 'accountsUserLinksGet' smart constructor.
data AccountsUserLinksGet =
  AccountsUserLinksGet'
    { _aulgXgafv :: !(Maybe Xgafv)
    , _aulgUploadProtocol :: !(Maybe Text)
    , _aulgAccessToken :: !(Maybe Text)
    , _aulgUploadType :: !(Maybe Text)
    , _aulgName :: !Text
    , _aulgCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsUserLinksGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aulgXgafv'
--
-- * 'aulgUploadProtocol'
--
-- * 'aulgAccessToken'
--
-- * 'aulgUploadType'
--
-- * 'aulgName'
--
-- * 'aulgCallback'
accountsUserLinksGet
    :: Text -- ^ 'aulgName'
    -> AccountsUserLinksGet
accountsUserLinksGet pAulgName_ =
  AccountsUserLinksGet'
    { _aulgXgafv = Nothing
    , _aulgUploadProtocol = Nothing
    , _aulgAccessToken = Nothing
    , _aulgUploadType = Nothing
    , _aulgName = pAulgName_
    , _aulgCallback = Nothing
    }


-- | V1 error format.
aulgXgafv :: Lens' AccountsUserLinksGet (Maybe Xgafv)
aulgXgafv
  = lens _aulgXgafv (\ s a -> s{_aulgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aulgUploadProtocol :: Lens' AccountsUserLinksGet (Maybe Text)
aulgUploadProtocol
  = lens _aulgUploadProtocol
      (\ s a -> s{_aulgUploadProtocol = a})

-- | OAuth access token.
aulgAccessToken :: Lens' AccountsUserLinksGet (Maybe Text)
aulgAccessToken
  = lens _aulgAccessToken
      (\ s a -> s{_aulgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aulgUploadType :: Lens' AccountsUserLinksGet (Maybe Text)
aulgUploadType
  = lens _aulgUploadType
      (\ s a -> s{_aulgUploadType = a})

-- | Required. Example format: accounts\/1234\/userLinks\/5678
aulgName :: Lens' AccountsUserLinksGet Text
aulgName = lens _aulgName (\ s a -> s{_aulgName = a})

-- | JSONP
aulgCallback :: Lens' AccountsUserLinksGet (Maybe Text)
aulgCallback
  = lens _aulgCallback (\ s a -> s{_aulgCallback = a})

instance GoogleRequest AccountsUserLinksGet where
        type Rs AccountsUserLinksGet =
             GoogleAnalyticsAdminV1alphaUserLink
        type Scopes AccountsUserLinksGet =
             '["https://www.googleapis.com/auth/analytics.manage.users.readonly"]
        requestClient AccountsUserLinksGet'{..}
          = go _aulgName _aulgXgafv _aulgUploadProtocol
              _aulgAccessToken
              _aulgUploadType
              _aulgCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsUserLinksGetResource)
                      mempty
