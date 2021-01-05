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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns child web data streams under the specified parent property. Web
-- data streams will be excluded if the caller does not have access.
-- Returns an empty list if no relevant web data streams are found.
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.webDataStreams.list@.
module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.List
    (
    -- * REST Resource
      PropertiesWebDataStreamsListResource

    -- * Creating a Request
    , propertiesWebDataStreamsList
    , PropertiesWebDataStreamsList

    -- * Request Lenses
    , pwdslParent
    , pwdslXgafv
    , pwdslUploadProtocol
    , pwdslAccessToken
    , pwdslUploadType
    , pwdslPageToken
    , pwdslPageSize
    , pwdslCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.webDataStreams.list@ method which the
-- 'PropertiesWebDataStreamsList' request conforms to.
type PropertiesWebDataStreamsListResource =
     "v1alpha" :>
       Capture "parent" Text :>
         "webDataStreams" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "pageToken" Text :>
                     QueryParam "pageSize" (Textual Int32) :>
                       QueryParam "callback" Text :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON]
                             GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse

-- | Returns child web data streams under the specified parent property. Web
-- data streams will be excluded if the caller does not have access.
-- Returns an empty list if no relevant web data streams are found.
--
-- /See:/ 'propertiesWebDataStreamsList' smart constructor.
data PropertiesWebDataStreamsList =
  PropertiesWebDataStreamsList'
    { _pwdslParent :: !Text
    , _pwdslXgafv :: !(Maybe Xgafv)
    , _pwdslUploadProtocol :: !(Maybe Text)
    , _pwdslAccessToken :: !(Maybe Text)
    , _pwdslUploadType :: !(Maybe Text)
    , _pwdslPageToken :: !(Maybe Text)
    , _pwdslPageSize :: !(Maybe (Textual Int32))
    , _pwdslCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesWebDataStreamsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwdslParent'
--
-- * 'pwdslXgafv'
--
-- * 'pwdslUploadProtocol'
--
-- * 'pwdslAccessToken'
--
-- * 'pwdslUploadType'
--
-- * 'pwdslPageToken'
--
-- * 'pwdslPageSize'
--
-- * 'pwdslCallback'
propertiesWebDataStreamsList
    :: Text -- ^ 'pwdslParent'
    -> PropertiesWebDataStreamsList
propertiesWebDataStreamsList pPwdslParent_ =
  PropertiesWebDataStreamsList'
    { _pwdslParent = pPwdslParent_
    , _pwdslXgafv = Nothing
    , _pwdslUploadProtocol = Nothing
    , _pwdslAccessToken = Nothing
    , _pwdslUploadType = Nothing
    , _pwdslPageToken = Nothing
    , _pwdslPageSize = Nothing
    , _pwdslCallback = Nothing
    }


-- | Required. The name of the parent property. For example, to list results
-- of web streams under the property with Id 123: \"properties\/123\"
pwdslParent :: Lens' PropertiesWebDataStreamsList Text
pwdslParent
  = lens _pwdslParent (\ s a -> s{_pwdslParent = a})

-- | V1 error format.
pwdslXgafv :: Lens' PropertiesWebDataStreamsList (Maybe Xgafv)
pwdslXgafv
  = lens _pwdslXgafv (\ s a -> s{_pwdslXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pwdslUploadProtocol :: Lens' PropertiesWebDataStreamsList (Maybe Text)
pwdslUploadProtocol
  = lens _pwdslUploadProtocol
      (\ s a -> s{_pwdslUploadProtocol = a})

-- | OAuth access token.
pwdslAccessToken :: Lens' PropertiesWebDataStreamsList (Maybe Text)
pwdslAccessToken
  = lens _pwdslAccessToken
      (\ s a -> s{_pwdslAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pwdslUploadType :: Lens' PropertiesWebDataStreamsList (Maybe Text)
pwdslUploadType
  = lens _pwdslUploadType
      (\ s a -> s{_pwdslUploadType = a})

-- | A page token, received from a previous \`ListWebDataStreams\` call.
-- Provide this to retrieve the subsequent page. When paginating, all other
-- parameters provided to \`ListWebDataStreams\` must match the call that
-- provided the page token.
pwdslPageToken :: Lens' PropertiesWebDataStreamsList (Maybe Text)
pwdslPageToken
  = lens _pwdslPageToken
      (\ s a -> s{_pwdslPageToken = a})

-- | The maximum number of resources to return. If unspecified, at most 50
-- resources will be returned. The maximum value is 200; (higher values
-- will be coerced to the maximum)
pwdslPageSize :: Lens' PropertiesWebDataStreamsList (Maybe Int32)
pwdslPageSize
  = lens _pwdslPageSize
      (\ s a -> s{_pwdslPageSize = a})
      . mapping _Coerce

-- | JSONP
pwdslCallback :: Lens' PropertiesWebDataStreamsList (Maybe Text)
pwdslCallback
  = lens _pwdslCallback
      (\ s a -> s{_pwdslCallback = a})

instance GoogleRequest PropertiesWebDataStreamsList
         where
        type Rs PropertiesWebDataStreamsList =
             GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse
        type Scopes PropertiesWebDataStreamsList =
             '["https://www.googleapis.com/auth/analytics.readonly"]
        requestClient PropertiesWebDataStreamsList'{..}
          = go _pwdslParent _pwdslXgafv _pwdslUploadProtocol
              _pwdslAccessToken
              _pwdslUploadType
              _pwdslPageToken
              _pwdslPageSize
              _pwdslCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy :: Proxy PropertiesWebDataStreamsListResource)
                      mempty
