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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Create
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
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.userLinks.create@.
module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Create
    (
    -- * REST Resource
      PropertiesUserLinksCreateResource

    -- * Creating a Request
    , propertiesUserLinksCreate
    , PropertiesUserLinksCreate

    -- * Request Lenses
    , pulcParent
    , pulcXgafv
    , pulcUploadProtocol
    , pulcAccessToken
    , pulcUploadType
    , pulcNotifyNewUser
    , pulcPayload
    , pulcCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.userLinks.create@ method which the
-- 'PropertiesUserLinksCreate' request conforms to.
type PropertiesUserLinksCreateResource =
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
-- /See:/ 'propertiesUserLinksCreate' smart constructor.
data PropertiesUserLinksCreate =
  PropertiesUserLinksCreate'
    { _pulcParent :: !Text
    , _pulcXgafv :: !(Maybe Xgafv)
    , _pulcUploadProtocol :: !(Maybe Text)
    , _pulcAccessToken :: !(Maybe Text)
    , _pulcUploadType :: !(Maybe Text)
    , _pulcNotifyNewUser :: !(Maybe Bool)
    , _pulcPayload :: !GoogleAnalyticsAdminV1alphaUserLink
    , _pulcCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesUserLinksCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pulcParent'
--
-- * 'pulcXgafv'
--
-- * 'pulcUploadProtocol'
--
-- * 'pulcAccessToken'
--
-- * 'pulcUploadType'
--
-- * 'pulcNotifyNewUser'
--
-- * 'pulcPayload'
--
-- * 'pulcCallback'
propertiesUserLinksCreate
    :: Text -- ^ 'pulcParent'
    -> GoogleAnalyticsAdminV1alphaUserLink -- ^ 'pulcPayload'
    -> PropertiesUserLinksCreate
propertiesUserLinksCreate pPulcParent_ pPulcPayload_ =
  PropertiesUserLinksCreate'
    { _pulcParent = pPulcParent_
    , _pulcXgafv = Nothing
    , _pulcUploadProtocol = Nothing
    , _pulcAccessToken = Nothing
    , _pulcUploadType = Nothing
    , _pulcNotifyNewUser = Nothing
    , _pulcPayload = pPulcPayload_
    , _pulcCallback = Nothing
    }


-- | Required. Example format: accounts\/1234
pulcParent :: Lens' PropertiesUserLinksCreate Text
pulcParent
  = lens _pulcParent (\ s a -> s{_pulcParent = a})

-- | V1 error format.
pulcXgafv :: Lens' PropertiesUserLinksCreate (Maybe Xgafv)
pulcXgafv
  = lens _pulcXgafv (\ s a -> s{_pulcXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pulcUploadProtocol :: Lens' PropertiesUserLinksCreate (Maybe Text)
pulcUploadProtocol
  = lens _pulcUploadProtocol
      (\ s a -> s{_pulcUploadProtocol = a})

-- | OAuth access token.
pulcAccessToken :: Lens' PropertiesUserLinksCreate (Maybe Text)
pulcAccessToken
  = lens _pulcAccessToken
      (\ s a -> s{_pulcAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pulcUploadType :: Lens' PropertiesUserLinksCreate (Maybe Text)
pulcUploadType
  = lens _pulcUploadType
      (\ s a -> s{_pulcUploadType = a})

-- | Optional. If set, then email the new user notifying them that they\'ve
-- been granted permissions to the resource.
pulcNotifyNewUser :: Lens' PropertiesUserLinksCreate (Maybe Bool)
pulcNotifyNewUser
  = lens _pulcNotifyNewUser
      (\ s a -> s{_pulcNotifyNewUser = a})

-- | Multipart request metadata.
pulcPayload :: Lens' PropertiesUserLinksCreate GoogleAnalyticsAdminV1alphaUserLink
pulcPayload
  = lens _pulcPayload (\ s a -> s{_pulcPayload = a})

-- | JSONP
pulcCallback :: Lens' PropertiesUserLinksCreate (Maybe Text)
pulcCallback
  = lens _pulcCallback (\ s a -> s{_pulcCallback = a})

instance GoogleRequest PropertiesUserLinksCreate
         where
        type Rs PropertiesUserLinksCreate =
             GoogleAnalyticsAdminV1alphaUserLink
        type Scopes PropertiesUserLinksCreate =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient PropertiesUserLinksCreate'{..}
          = go _pulcParent _pulcXgafv _pulcUploadProtocol
              _pulcAccessToken
              _pulcUploadType
              _pulcNotifyNewUser
              _pulcCallback
              (Just AltJSON)
              _pulcPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesUserLinksCreateResource)
                      mempty
