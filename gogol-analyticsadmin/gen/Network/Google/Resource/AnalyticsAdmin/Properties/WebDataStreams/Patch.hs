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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a web stream on a property.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.webDataStreams.patch@.
module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Patch
    (
    -- * REST Resource
      PropertiesWebDataStreamsPatchResource

    -- * Creating a Request
    , propertiesWebDataStreamsPatch
    , PropertiesWebDataStreamsPatch

    -- * Request Lenses
    , pwdspXgafv
    , pwdspUploadProtocol
    , pwdspUpdateMask
    , pwdspAccessToken
    , pwdspUploadType
    , pwdspPayload
    , pwdspName
    , pwdspCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.webDataStreams.patch@ method which the
-- 'PropertiesWebDataStreamsPatch' request conforms to.
type PropertiesWebDataStreamsPatchResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "updateMask" GFieldMask :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "callback" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON]
                         GoogleAnalyticsAdminV1alphaWebDataStream
                         :>
                         Patch '[JSON]
                           GoogleAnalyticsAdminV1alphaWebDataStream

-- | Updates a web stream on a property.
--
-- /See:/ 'propertiesWebDataStreamsPatch' smart constructor.
data PropertiesWebDataStreamsPatch =
  PropertiesWebDataStreamsPatch'
    { _pwdspXgafv :: !(Maybe Xgafv)
    , _pwdspUploadProtocol :: !(Maybe Text)
    , _pwdspUpdateMask :: !(Maybe GFieldMask)
    , _pwdspAccessToken :: !(Maybe Text)
    , _pwdspUploadType :: !(Maybe Text)
    , _pwdspPayload :: !GoogleAnalyticsAdminV1alphaWebDataStream
    , _pwdspName :: !Text
    , _pwdspCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesWebDataStreamsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwdspXgafv'
--
-- * 'pwdspUploadProtocol'
--
-- * 'pwdspUpdateMask'
--
-- * 'pwdspAccessToken'
--
-- * 'pwdspUploadType'
--
-- * 'pwdspPayload'
--
-- * 'pwdspName'
--
-- * 'pwdspCallback'
propertiesWebDataStreamsPatch
    :: GoogleAnalyticsAdminV1alphaWebDataStream -- ^ 'pwdspPayload'
    -> Text -- ^ 'pwdspName'
    -> PropertiesWebDataStreamsPatch
propertiesWebDataStreamsPatch pPwdspPayload_ pPwdspName_ =
  PropertiesWebDataStreamsPatch'
    { _pwdspXgafv = Nothing
    , _pwdspUploadProtocol = Nothing
    , _pwdspUpdateMask = Nothing
    , _pwdspAccessToken = Nothing
    , _pwdspUploadType = Nothing
    , _pwdspPayload = pPwdspPayload_
    , _pwdspName = pPwdspName_
    , _pwdspCallback = Nothing
    }


-- | V1 error format.
pwdspXgafv :: Lens' PropertiesWebDataStreamsPatch (Maybe Xgafv)
pwdspXgafv
  = lens _pwdspXgafv (\ s a -> s{_pwdspXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pwdspUploadProtocol :: Lens' PropertiesWebDataStreamsPatch (Maybe Text)
pwdspUploadProtocol
  = lens _pwdspUploadProtocol
      (\ s a -> s{_pwdspUploadProtocol = a})

-- | Required. The list of fields to be updated. Omitted fields will not be
-- updated. To replace the entire entity, use one path with the string
-- \"*\" to match all fields.
pwdspUpdateMask :: Lens' PropertiesWebDataStreamsPatch (Maybe GFieldMask)
pwdspUpdateMask
  = lens _pwdspUpdateMask
      (\ s a -> s{_pwdspUpdateMask = a})

-- | OAuth access token.
pwdspAccessToken :: Lens' PropertiesWebDataStreamsPatch (Maybe Text)
pwdspAccessToken
  = lens _pwdspAccessToken
      (\ s a -> s{_pwdspAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pwdspUploadType :: Lens' PropertiesWebDataStreamsPatch (Maybe Text)
pwdspUploadType
  = lens _pwdspUploadType
      (\ s a -> s{_pwdspUploadType = a})

-- | Multipart request metadata.
pwdspPayload :: Lens' PropertiesWebDataStreamsPatch GoogleAnalyticsAdminV1alphaWebDataStream
pwdspPayload
  = lens _pwdspPayload (\ s a -> s{_pwdspPayload = a})

-- | Output only. Resource name of this Data Stream. Format:
-- properties\/{property_id}\/webDataStreams\/{stream_id} Example:
-- \"properties\/1000\/webDataStreams\/2000\"
pwdspName :: Lens' PropertiesWebDataStreamsPatch Text
pwdspName
  = lens _pwdspName (\ s a -> s{_pwdspName = a})

-- | JSONP
pwdspCallback :: Lens' PropertiesWebDataStreamsPatch (Maybe Text)
pwdspCallback
  = lens _pwdspCallback
      (\ s a -> s{_pwdspCallback = a})

instance GoogleRequest PropertiesWebDataStreamsPatch
         where
        type Rs PropertiesWebDataStreamsPatch =
             GoogleAnalyticsAdminV1alphaWebDataStream
        type Scopes PropertiesWebDataStreamsPatch =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesWebDataStreamsPatch'{..}
          = go _pwdspName _pwdspXgafv _pwdspUploadProtocol
              _pwdspUpdateMask
              _pwdspAccessToken
              _pwdspUploadType
              _pwdspCallback
              (Just AltJSON)
              _pwdspPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesWebDataStreamsPatchResource)
                      mempty
