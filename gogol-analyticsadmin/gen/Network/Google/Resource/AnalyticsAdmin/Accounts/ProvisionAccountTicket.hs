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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Accounts.ProvisionAccountTicket
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a ticket for creating an account.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.accounts.provisionAccountTicket@.
module Network.Google.Resource.AnalyticsAdmin.Accounts.ProvisionAccountTicket
    (
    -- * REST Resource
      AccountsProvisionAccountTicketResource

    -- * Creating a Request
    , accountsProvisionAccountTicket
    , AccountsProvisionAccountTicket

    -- * Request Lenses
    , apatXgafv
    , apatUploadProtocol
    , apatAccessToken
    , apatUploadType
    , apatPayload
    , apatCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.accounts.provisionAccountTicket@ method which the
-- 'AccountsProvisionAccountTicket' request conforms to.
type AccountsProvisionAccountTicketResource =
     "v1alpha" :>
       "accounts:provisionAccountTicket" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON]
                       GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest
                       :>
                       Post '[JSON]
                         GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse

-- | Requests a ticket for creating an account.
--
-- /See:/ 'accountsProvisionAccountTicket' smart constructor.
data AccountsProvisionAccountTicket =
  AccountsProvisionAccountTicket'
    { _apatXgafv :: !(Maybe Xgafv)
    , _apatUploadProtocol :: !(Maybe Text)
    , _apatAccessToken :: !(Maybe Text)
    , _apatUploadType :: !(Maybe Text)
    , _apatPayload :: !GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest
    , _apatCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountsProvisionAccountTicket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apatXgafv'
--
-- * 'apatUploadProtocol'
--
-- * 'apatAccessToken'
--
-- * 'apatUploadType'
--
-- * 'apatPayload'
--
-- * 'apatCallback'
accountsProvisionAccountTicket
    :: GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest -- ^ 'apatPayload'
    -> AccountsProvisionAccountTicket
accountsProvisionAccountTicket pApatPayload_ =
  AccountsProvisionAccountTicket'
    { _apatXgafv = Nothing
    , _apatUploadProtocol = Nothing
    , _apatAccessToken = Nothing
    , _apatUploadType = Nothing
    , _apatPayload = pApatPayload_
    , _apatCallback = Nothing
    }


-- | V1 error format.
apatXgafv :: Lens' AccountsProvisionAccountTicket (Maybe Xgafv)
apatXgafv
  = lens _apatXgafv (\ s a -> s{_apatXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
apatUploadProtocol :: Lens' AccountsProvisionAccountTicket (Maybe Text)
apatUploadProtocol
  = lens _apatUploadProtocol
      (\ s a -> s{_apatUploadProtocol = a})

-- | OAuth access token.
apatAccessToken :: Lens' AccountsProvisionAccountTicket (Maybe Text)
apatAccessToken
  = lens _apatAccessToken
      (\ s a -> s{_apatAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
apatUploadType :: Lens' AccountsProvisionAccountTicket (Maybe Text)
apatUploadType
  = lens _apatUploadType
      (\ s a -> s{_apatUploadType = a})

-- | Multipart request metadata.
apatPayload :: Lens' AccountsProvisionAccountTicket GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest
apatPayload
  = lens _apatPayload (\ s a -> s{_apatPayload = a})

-- | JSONP
apatCallback :: Lens' AccountsProvisionAccountTicket (Maybe Text)
apatCallback
  = lens _apatCallback (\ s a -> s{_apatCallback = a})

instance GoogleRequest AccountsProvisionAccountTicket
         where
        type Rs AccountsProvisionAccountTicket =
             GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse
        type Scopes AccountsProvisionAccountTicket =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient AccountsProvisionAccountTicket'{..}
          = go _apatXgafv _apatUploadProtocol _apatAccessToken
              _apatUploadType
              _apatCallback
              (Just AltJSON)
              _apatPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AccountsProvisionAccountTicketResource)
                      mempty
