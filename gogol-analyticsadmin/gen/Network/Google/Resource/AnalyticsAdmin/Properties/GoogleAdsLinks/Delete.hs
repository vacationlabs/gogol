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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a GoogleAdsLink on a property
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.googleAdsLinks.delete@.
module Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.Delete
    (
    -- * REST Resource
      PropertiesGoogleAdsLinksDeleteResource

    -- * Creating a Request
    , propertiesGoogleAdsLinksDelete
    , PropertiesGoogleAdsLinksDelete

    -- * Request Lenses
    , pgaldXgafv
    , pgaldUploadProtocol
    , pgaldAccessToken
    , pgaldUploadType
    , pgaldName
    , pgaldCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.googleAdsLinks.delete@ method which the
-- 'PropertiesGoogleAdsLinksDelete' request conforms to.
type PropertiesGoogleAdsLinksDeleteResource =
     "v1alpha" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "access_token" Text :>
               QueryParam "uploadType" Text :>
                 QueryParam "callback" Text :>
                   QueryParam "alt" AltJSON :>
                     Delete '[JSON] GoogleProtobufEmpty

-- | Deletes a GoogleAdsLink on a property
--
-- /See:/ 'propertiesGoogleAdsLinksDelete' smart constructor.
data PropertiesGoogleAdsLinksDelete =
  PropertiesGoogleAdsLinksDelete'
    { _pgaldXgafv :: !(Maybe Xgafv)
    , _pgaldUploadProtocol :: !(Maybe Text)
    , _pgaldAccessToken :: !(Maybe Text)
    , _pgaldUploadType :: !(Maybe Text)
    , _pgaldName :: !Text
    , _pgaldCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesGoogleAdsLinksDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgaldXgafv'
--
-- * 'pgaldUploadProtocol'
--
-- * 'pgaldAccessToken'
--
-- * 'pgaldUploadType'
--
-- * 'pgaldName'
--
-- * 'pgaldCallback'
propertiesGoogleAdsLinksDelete
    :: Text -- ^ 'pgaldName'
    -> PropertiesGoogleAdsLinksDelete
propertiesGoogleAdsLinksDelete pPgaldName_ =
  PropertiesGoogleAdsLinksDelete'
    { _pgaldXgafv = Nothing
    , _pgaldUploadProtocol = Nothing
    , _pgaldAccessToken = Nothing
    , _pgaldUploadType = Nothing
    , _pgaldName = pPgaldName_
    , _pgaldCallback = Nothing
    }


-- | V1 error format.
pgaldXgafv :: Lens' PropertiesGoogleAdsLinksDelete (Maybe Xgafv)
pgaldXgafv
  = lens _pgaldXgafv (\ s a -> s{_pgaldXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pgaldUploadProtocol :: Lens' PropertiesGoogleAdsLinksDelete (Maybe Text)
pgaldUploadProtocol
  = lens _pgaldUploadProtocol
      (\ s a -> s{_pgaldUploadProtocol = a})

-- | OAuth access token.
pgaldAccessToken :: Lens' PropertiesGoogleAdsLinksDelete (Maybe Text)
pgaldAccessToken
  = lens _pgaldAccessToken
      (\ s a -> s{_pgaldAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pgaldUploadType :: Lens' PropertiesGoogleAdsLinksDelete (Maybe Text)
pgaldUploadType
  = lens _pgaldUploadType
      (\ s a -> s{_pgaldUploadType = a})

-- | Required. Example format: properties\/1234\/googleAdsLinks\/5678
pgaldName :: Lens' PropertiesGoogleAdsLinksDelete Text
pgaldName
  = lens _pgaldName (\ s a -> s{_pgaldName = a})

-- | JSONP
pgaldCallback :: Lens' PropertiesGoogleAdsLinksDelete (Maybe Text)
pgaldCallback
  = lens _pgaldCallback
      (\ s a -> s{_pgaldCallback = a})

instance GoogleRequest PropertiesGoogleAdsLinksDelete
         where
        type Rs PropertiesGoogleAdsLinksDelete =
             GoogleProtobufEmpty
        type Scopes PropertiesGoogleAdsLinksDelete =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesGoogleAdsLinksDelete'{..}
          = go _pgaldName _pgaldXgafv _pgaldUploadProtocol
              _pgaldAccessToken
              _pgaldUploadType
              _pgaldCallback
              (Just AltJSON)
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesGoogleAdsLinksDeleteResource)
                      mempty
