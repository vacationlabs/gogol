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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a web stream on a property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.webDataStreams.delete@.
module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Delete
    (
    -- * REST Resource
      PropertiesWebDataStreamsDeleteResource

    -- * Creating a Request
    , propertiesWebDataStreamsDelete
    , PropertiesWebDataStreamsDelete

    -- * Request Lenses
    , pwdsdXgafv
    , pwdsdUploadProtocol
    , pwdsdAccessToken
    , pwdsdUploadType
    , pwdsdName
    , pwdsdCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.webDataStreams.delete@ method which the
-- 'PropertiesWebDataStreamsDelete' request conforms to.
type PropertiesWebDataStreamsDeleteResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Delete '[JSON] GoogleProtobufEmpty

-- | Deletes a web stream on a property.
--
-- /See:/ 'propertiesWebDataStreamsDelete' smart constructor.
data PropertiesWebDataStreamsDelete =
  PropertiesWebDataStreamsDelete'
    { _pwdsdXgafv :: !(Maybe Xgafv)
    , _pwdsdUploadProtocol :: !(Maybe Text)
    , _pwdsdAccessToken :: !(Maybe Text)
    , _pwdsdUploadType :: !(Maybe Text)
    , _pwdsdName :: !Text
    , _pwdsdCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesWebDataStreamsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwdsdXgafv'
--
-- * 'pwdsdUploadProtocol'
--
-- * 'pwdsdAccessToken'
--
-- * 'pwdsdUploadType'
--
-- * 'pwdsdName'
--
-- * 'pwdsdCallback'
propertiesWebDataStreamsDelete
    :: Text -- ^ 'pwdsdName'
    -> PropertiesWebDataStreamsDelete
propertiesWebDataStreamsDelete pPwdsdName_ =
  PropertiesWebDataStreamsDelete'
    { _pwdsdXgafv = Nothing
    , _pwdsdUploadProtocol = Nothing
    , _pwdsdAccessToken = Nothing
    , _pwdsdUploadType = Nothing
    , _pwdsdName = pPwdsdName_
    , _pwdsdCallback = Nothing
    }


-- | V1 error format.
pwdsdXgafv :: Lens' PropertiesWebDataStreamsDelete (Maybe Xgafv)
pwdsdXgafv
  = lens _pwdsdXgafv (\ s a -> s{_pwdsdXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pwdsdUploadProtocol :: Lens' PropertiesWebDataStreamsDelete (Maybe Text)
pwdsdUploadProtocol
  = lens _pwdsdUploadProtocol
      (\ s a -> s{_pwdsdUploadProtocol = a})

-- | OAuth access token.
pwdsdAccessToken :: Lens' PropertiesWebDataStreamsDelete (Maybe Text)
pwdsdAccessToken
  = lens _pwdsdAccessToken
      (\ s a -> s{_pwdsdAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pwdsdUploadType :: Lens' PropertiesWebDataStreamsDelete (Maybe Text)
pwdsdUploadType
  = lens _pwdsdUploadType
      (\ s a -> s{_pwdsdUploadType = a})

-- | Required. The name of the web data stream to delete. Format:
-- properties\/{property_id}\/webDataStreams\/{stream_id} Example:
-- \"properties\/123\/webDataStreams\/456\"
pwdsdName :: Lens' PropertiesWebDataStreamsDelete Text
pwdsdName
  = lens _pwdsdName (\ s a -> s{_pwdsdName = a})

-- | JSONP
pwdsdCallback :: Lens' PropertiesWebDataStreamsDelete (Maybe Text)
pwdsdCallback
  = lens _pwdsdCallback
      (\ s a -> s{_pwdsdCallback = a})

instance GoogleRequest PropertiesWebDataStreamsDelete
         where
        type Rs PropertiesWebDataStreamsDelete =
             GoogleProtobufEmpty
        type Scopes PropertiesWebDataStreamsDelete =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesWebDataStreamsDelete'{..}
          = go _pwdsdName _pwdsdXgafv _pwdsdUploadProtocol
              _pwdsdAccessToken
              _pwdsdUploadType
              _pwdsdCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesWebDataStreamsDeleteResource)
                      mempty
