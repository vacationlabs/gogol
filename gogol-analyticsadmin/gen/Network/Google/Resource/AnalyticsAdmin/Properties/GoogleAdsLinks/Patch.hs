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
-- Module      : Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a GoogleAdsLink on a property
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference> for @analyticsadmin.properties.googleAdsLinks.patch@.
module Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.Patch
    (
    -- * REST Resource
      PropertiesGoogleAdsLinksPatchResource

    -- * Creating a Request
    , propertiesGoogleAdsLinksPatch
    , PropertiesGoogleAdsLinksPatch

    -- * Request Lenses
    , pgalpXgafv
    , pgalpUploadProtocol
    , pgalpUpdateMask
    , pgalpAccessToken
    , pgalpUploadType
    , pgalpPayload
    , pgalpName
    , pgalpCallback
    ) where

import Network.Google.AnalyticsAdmin.Types
import Network.Google.Prelude

-- | A resource alias for @analyticsadmin.properties.googleAdsLinks.patch@ method which the
-- 'PropertiesGoogleAdsLinksPatch' request conforms to.
type PropertiesGoogleAdsLinksPatchResource =
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
                         GoogleAnalyticsAdminV1alphaGoogleAdsLink
                         :>
                         Patch '[JSON]
                           GoogleAnalyticsAdminV1alphaGoogleAdsLink

-- | Updates a GoogleAdsLink on a property
--
-- /See:/ 'propertiesGoogleAdsLinksPatch' smart constructor.
data PropertiesGoogleAdsLinksPatch =
  PropertiesGoogleAdsLinksPatch'
    { _pgalpXgafv :: !(Maybe Xgafv)
    , _pgalpUploadProtocol :: !(Maybe Text)
    , _pgalpUpdateMask :: !(Maybe GFieldMask)
    , _pgalpAccessToken :: !(Maybe Text)
    , _pgalpUploadType :: !(Maybe Text)
    , _pgalpPayload :: !GoogleAnalyticsAdminV1alphaGoogleAdsLink
    , _pgalpName :: !Text
    , _pgalpCallback :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertiesGoogleAdsLinksPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgalpXgafv'
--
-- * 'pgalpUploadProtocol'
--
-- * 'pgalpUpdateMask'
--
-- * 'pgalpAccessToken'
--
-- * 'pgalpUploadType'
--
-- * 'pgalpPayload'
--
-- * 'pgalpName'
--
-- * 'pgalpCallback'
propertiesGoogleAdsLinksPatch
    :: GoogleAnalyticsAdminV1alphaGoogleAdsLink -- ^ 'pgalpPayload'
    -> Text -- ^ 'pgalpName'
    -> PropertiesGoogleAdsLinksPatch
propertiesGoogleAdsLinksPatch pPgalpPayload_ pPgalpName_ =
  PropertiesGoogleAdsLinksPatch'
    { _pgalpXgafv = Nothing
    , _pgalpUploadProtocol = Nothing
    , _pgalpUpdateMask = Nothing
    , _pgalpAccessToken = Nothing
    , _pgalpUploadType = Nothing
    , _pgalpPayload = pPgalpPayload_
    , _pgalpName = pPgalpName_
    , _pgalpCallback = Nothing
    }


-- | V1 error format.
pgalpXgafv :: Lens' PropertiesGoogleAdsLinksPatch (Maybe Xgafv)
pgalpXgafv
  = lens _pgalpXgafv (\ s a -> s{_pgalpXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pgalpUploadProtocol :: Lens' PropertiesGoogleAdsLinksPatch (Maybe Text)
pgalpUploadProtocol
  = lens _pgalpUploadProtocol
      (\ s a -> s{_pgalpUploadProtocol = a})

-- | Required. The list of fields to be updated. Omitted fields will not be
-- updated. To replace the entire entity, use one path with the string
-- \"*\" to match all fields.
pgalpUpdateMask :: Lens' PropertiesGoogleAdsLinksPatch (Maybe GFieldMask)
pgalpUpdateMask
  = lens _pgalpUpdateMask
      (\ s a -> s{_pgalpUpdateMask = a})

-- | OAuth access token.
pgalpAccessToken :: Lens' PropertiesGoogleAdsLinksPatch (Maybe Text)
pgalpAccessToken
  = lens _pgalpAccessToken
      (\ s a -> s{_pgalpAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pgalpUploadType :: Lens' PropertiesGoogleAdsLinksPatch (Maybe Text)
pgalpUploadType
  = lens _pgalpUploadType
      (\ s a -> s{_pgalpUploadType = a})

-- | Multipart request metadata.
pgalpPayload :: Lens' PropertiesGoogleAdsLinksPatch GoogleAnalyticsAdminV1alphaGoogleAdsLink
pgalpPayload
  = lens _pgalpPayload (\ s a -> s{_pgalpPayload = a})

-- | Output only. Format:
-- properties\/{propertyId}\/googleAdsLinks\/{googleAdsLinkId} Note:
-- googleAdsLinkId is not the Google Ads customer ID.
pgalpName :: Lens' PropertiesGoogleAdsLinksPatch Text
pgalpName
  = lens _pgalpName (\ s a -> s{_pgalpName = a})

-- | JSONP
pgalpCallback :: Lens' PropertiesGoogleAdsLinksPatch (Maybe Text)
pgalpCallback
  = lens _pgalpCallback
      (\ s a -> s{_pgalpCallback = a})

instance GoogleRequest PropertiesGoogleAdsLinksPatch
         where
        type Rs PropertiesGoogleAdsLinksPatch =
             GoogleAnalyticsAdminV1alphaGoogleAdsLink
        type Scopes PropertiesGoogleAdsLinksPatch =
             '["https://www.googleapis.com/auth/analytics.edit"]
        requestClient PropertiesGoogleAdsLinksPatch'{..}
          = go _pgalpName _pgalpXgafv _pgalpUploadProtocol
              _pgalpUpdateMask
              _pgalpAccessToken
              _pgalpUploadType
              _pgalpCallback
              (Just AltJSON)
              _pgalpPayload
              analyticsAdminService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PropertiesGoogleAdsLinksPatchResource)
                      mempty
