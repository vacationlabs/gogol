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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user link on an account or property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.userLinks.delete@.
module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Delete
    (
    -- * REST Resource
      PropertiesUserLinksDeleteResource

    -- * Creating a Request
    , propertiesUserLinksDelete
    , PropertiesUserLinksDelete

    -- * Request Lenses
    , puldXgafv
    , puldUploadProtocol
    , puldAccessToken
    , puldUploadType
    , puldName
    , puldCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.userLinks.delete@ method which the
-- 'PropertiesUserLinksDelete' request conforms to.
type PropertiesUserLinksDeleteResource =
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
-- /See:/ 'propertiesUserLinksDelete' smart constructor.
data PropertiesUserLinksDelete =
  PropertiesUserLinksDelete'
    { _puldXgafv :: !(Maybe Xgafv)
    , _puldUploadProtocol :: !(Maybe Text)
    , _puldAccessToken :: !(Maybe Text)
    , _puldUploadType :: !(Maybe Text)
    , _puldName :: !Text
    , _puldCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesUserLinksDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'puldXgafv'
--
-- * 'puldUploadProtocol'
--
-- * 'puldAccessToken'
--
-- * 'puldUploadType'
--
-- * 'puldName'
--
-- * 'puldCallback'
propertiesUserLinksDelete
    :: Text -- ^ 'puldName'
    -> PropertiesUserLinksDelete
propertiesUserLinksDelete pPuldName_ =
  PropertiesUserLinksDelete'
    { _puldXgafv = Nothing
    , _puldUploadProtocol = Nothing
    , _puldAccessToken = Nothing
    , _puldUploadType = Nothing
    , _puldName = pPuldName_
    , _puldCallback = Nothing
    }


-- | V1 error format.
puldXgafv :: Lens' PropertiesUserLinksDelete (Maybe Xgafv)
puldXgafv
  = lens _puldXgafv (\ s a -> s{_puldXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
puldUploadProtocol :: Lens' PropertiesUserLinksDelete (Maybe Text)
puldUploadProtocol
  = lens _puldUploadProtocol
      (\ s a -> s{_puldUploadProtocol = a})

-- | OAuth access token.
puldAccessToken :: Lens' PropertiesUserLinksDelete (Maybe Text)
puldAccessToken
  = lens _puldAccessToken
      (\ s a -> s{_puldAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
puldUploadType :: Lens' PropertiesUserLinksDelete (Maybe Text)
puldUploadType
  = lens _puldUploadType
      (\ s a -> s{_puldUploadType = a})

-- | Required. Example format: accounts\/1234\/userLinks\/5678
puldName :: Lens' PropertiesUserLinksDelete Text
puldName = lens _puldName (\ s a -> s{_puldName = a})

-- | JSONP
puldCallback :: Lens' PropertiesUserLinksDelete (Maybe Text)
puldCallback
  = lens _puldCallback (\ s a -> s{_puldCallback = a})

instance GoogleRequest PropertiesUserLinksDelete
         where
        type Rs PropertiesUserLinksDelete =
             GoogleProtobufEmpty
        type Scopes PropertiesUserLinksDelete =
             '["https://www.googleapis.com/auth/analytics.manage.users"]
        requestClient PropertiesUserLinksDelete'{..}
          = go _puldName _puldXgafv _puldUploadProtocol
              _puldAccessToken
              _puldUploadType
              _puldCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesUserLinksDeleteResource)
                      mempty
