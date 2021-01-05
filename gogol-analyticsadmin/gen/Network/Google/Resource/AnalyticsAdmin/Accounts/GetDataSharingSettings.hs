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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.GetDataSharingSettings
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get data sharing settings on an account. Data sharing settings are
-- singletons.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.getDataSharingSettings@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.GetDataSharingSettings
    (
    -- * REST Resource
      AccountsGetDataSharingSettingsResource

    -- * Creating a Request
    , accountsGetDataSharingSettings
    , AccountsGetDataSharingSettings

    -- * Request Lenses
    , agdssXgafv
    , agdssUploadProtocol
    , agdssAccessToken
    , agdssUploadType
    , agdssName
    , agdssCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.getDataSharingSettings@ method which the
-- 'AccountsGetDataSharingSettings' request conforms to.
type AccountsGetDataSharingSettingsResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON]
                       GoogleAnalyticsAdminV1alphaDataSharingSettings

-- | Get data sharing settings on an account. Data sharing settings are
-- singletons.
--
-- /See:/ 'accountsGetDataSharingSettings' smart constructor.
data AccountsGetDataSharingSettings =
  AccountsGetDataSharingSettings'
    { _agdssXgafv :: !(Maybe Xgafv)
    , _agdssUploadProtocol :: !(Maybe Text)
    , _agdssAccessToken :: !(Maybe Text)
    , _agdssUploadType :: !(Maybe Text)
    , _agdssName :: !Text
    , _agdssCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsGetDataSharingSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agdssXgafv'
--
-- * 'agdssUploadProtocol'
--
-- * 'agdssAccessToken'
--
-- * 'agdssUploadType'
--
-- * 'agdssName'
--
-- * 'agdssCallback'
accountsGetDataSharingSettings
    :: Text -- ^ 'agdssName'
    -> AccountsGetDataSharingSettings
accountsGetDataSharingSettings pAgdssName_ =
  AccountsGetDataSharingSettings'
    { _agdssXgafv = Nothing
    , _agdssUploadProtocol = Nothing
    , _agdssAccessToken = Nothing
    , _agdssUploadType = Nothing
    , _agdssName = pAgdssName_
    , _agdssCallback = Nothing
    }


-- | V1 error format.
agdssXgafv :: Lens' AccountsGetDataSharingSettings (Maybe Xgafv)
agdssXgafv
  = lens _agdssXgafv (\ s a -> s{_agdssXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
agdssUploadProtocol :: Lens' AccountsGetDataSharingSettings (Maybe Text)
agdssUploadProtocol
  = lens _agdssUploadProtocol
      (\ s a -> s{_agdssUploadProtocol = a})

-- | OAuth access token.
agdssAccessToken :: Lens' AccountsGetDataSharingSettings (Maybe Text)
agdssAccessToken
  = lens _agdssAccessToken
      (\ s a -> s{_agdssAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
agdssUploadType :: Lens' AccountsGetDataSharingSettings (Maybe Text)
agdssUploadType
  = lens _agdssUploadType
      (\ s a -> s{_agdssUploadType = a})

-- | Required. The name of the settings to lookup. Format:
-- accounts\/{account}\/dataSharingSettings Example:
-- \"accounts\/1000\/dataSharingSettings\"
agdssName :: Lens' AccountsGetDataSharingSettings Text
agdssName
  = lens _agdssName (\ s a -> s{_agdssName = a})

-- | JSONP
agdssCallback :: Lens' AccountsGetDataSharingSettings (Maybe Text)
agdssCallback
  = lens _agdssCallback
      (\ s a -> s{_agdssCallback = a})

instance GoogleRequest AccountsGetDataSharingSettings
         where
        type Rs AccountsGetDataSharingSettings =
             GoogleAnalyticsAdminV1alphaDataSharingSettings
        type Scopes AccountsGetDataSharingSettings =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient AccountsGetDataSharingSettings'{..}
          = go _agdssName _agdssXgafv _agdssUploadProtocol
              _agdssAccessToken
              _agdssUploadType
              _agdssCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AccountsGetDataSharingSettingsResource)
                      mempty
