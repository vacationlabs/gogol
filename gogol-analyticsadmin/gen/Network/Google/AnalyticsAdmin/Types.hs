{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.AnalyticsAdmin.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.AnalyticsAdmin.Types
    (
    -- * Service Configuration
      analyticsAdminService

    -- * OAuth Scopes
    , analyticsManageUsersScope
    , analyticsManageUsersReadOnlyScope
    , analyticsReadOnlyScope
    , analyticsEditScope

    -- * GoogleAnalyticsAdminV1alphaProperty
    , GoogleAnalyticsAdminV1alphaProperty
    , googleAnalyticsAdminV1alphaProperty
    , gaavpParent
    , gaavpCurrencyCode
    , gaavpIndustryCategory
    , gaavpUpdateTime
    , gaavpName
    , gaavpDisplayName
    , gaavpDeleted
    , gaavpTimeZone
    , gaavpCreateTime

    -- * GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse
    , GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse
    , googleAnalyticsAdminV1alphaListFirebaseLinksResponse
    , gaavlflrNextPageToken
    , gaavlflrFirebaseLinks

    -- * GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse
    , GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse
    , googleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse
    , gaavlaadsrNextPageToken
    , gaavlaadsrAndroidAppDataStreams

    -- * GoogleAnalyticsAdminV1alphaCreateUserLinkRequest
    , GoogleAnalyticsAdminV1alphaCreateUserLinkRequest
    , googleAnalyticsAdminV1alphaCreateUserLinkRequest
    , gaavculrParent
    , gaavculrNotifyNewUser
    , gaavculrUserLink

    -- * GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse
    , GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse
    , googleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse
    , gaavbuulrUserLinks

    -- * GoogleAnalyticsAdminV1alphaWebDataStream
    , GoogleAnalyticsAdminV1alphaWebDataStream
    , googleAnalyticsAdminV1alphaWebDataStream
    , gaavwdsMeasurementId
    , gaavwdsDefaultURI
    , gaavwdsUpdateTime
    , gaavwdsName
    , gaavwdsDisplayName
    , gaavwdsCreateTime
    , gaavwdsFirebaseAppId

    -- * GoogleAnalyticsAdminV1alphaUserLink
    , GoogleAnalyticsAdminV1alphaUserLink
    , googleAnalyticsAdminV1alphaUserLink
    , gaavulDirectRoles
    , gaavulName
    , gaavulEmailAddress

    -- * GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse
    , GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse
    , googleAnalyticsAdminV1alphaListWebDataStreamsResponse
    , gaavlwdsrWebDataStreams
    , gaavlwdsrNextPageToken

    -- * GoogleAnalyticsAdminV1alphaAuditUserLinksRequest
    , GoogleAnalyticsAdminV1alphaAuditUserLinksRequest
    , googleAnalyticsAdminV1alphaAuditUserLinksRequest
    , gaavaulrPageToken
    , gaavaulrPageSize

    -- * GoogleAnalyticsAdminV1alphaAndroidAppDataStream
    , GoogleAnalyticsAdminV1alphaAndroidAppDataStream
    , googleAnalyticsAdminV1alphaAndroidAppDataStream
    , gaavaadsPackageName
    , gaavaadsUpdateTime
    , gaavaadsName
    , gaavaadsDisplayName
    , gaavaadsCreateTime
    , gaavaadsFirebaseAppId

    -- * GoogleAnalyticsAdminV1alphaListAccountsResponse
    , GoogleAnalyticsAdminV1alphaListAccountsResponse
    , googleAnalyticsAdminV1alphaListAccountsResponse
    , gaavlarNextPageToken
    , gaavlarAccounts

    -- * GoogleAnalyticsAdminV1alphaPropertySummary
    , GoogleAnalyticsAdminV1alphaPropertySummary
    , googleAnalyticsAdminV1alphaPropertySummary
    , gaavpsProperty
    , gaavpsDisplayName

    -- * GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse
    , GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse
    , googleAnalyticsAdminV1alphaBatchGetUserLinksResponse
    , gaavbgulrUserLinks

    -- * GoogleProtobufEmpty
    , GoogleProtobufEmpty
    , googleProtobufEmpty

    -- * GoogleAnalyticsAdminV1alphaFirebaseLink
    , GoogleAnalyticsAdminV1alphaFirebaseLink
    , googleAnalyticsAdminV1alphaFirebaseLink
    , gaavflProject
    , gaavflMaximumUserAccess
    , gaavflName
    , gaavflCreateTime

    -- * GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest
    , GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest
    , googleAnalyticsAdminV1alphaProvisionAccountTicketRequest
    , gaavpatrRedirectURI
    , gaavpatrAccount

    -- * GoogleAnalyticsAdminV1alphaAccountSummary
    , GoogleAnalyticsAdminV1alphaAccountSummary
    , googleAnalyticsAdminV1alphaAccountSummary
    , gaavasPropertySummaries
    , gaavasAccount
    , gaavasName
    , gaavasDisplayName

    -- * GoogleAnalyticsAdminV1alphaListAccountSummariesResponse
    , GoogleAnalyticsAdminV1alphaListAccountSummariesResponse
    , googleAnalyticsAdminV1alphaListAccountSummariesResponse
    , gaavlasrNextPageToken
    , gaavlasrAccountSummaries

    -- * GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse
    , GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse
    , googleAnalyticsAdminV1alphaProvisionAccountTicketResponse
    , gaavpatrAccountTicketId

    -- * GoogleAnalyticsAdminV1alphaGoogleAdsLink
    , GoogleAnalyticsAdminV1alphaGoogleAdsLink
    , googleAnalyticsAdminV1alphaGoogleAdsLink
    , gaavgalParent
    , gaavgalCustomerId
    , gaavgalUpdateTime
    , gaavgalName
    , gaavgalCanManageClients
    , gaavgalEmailAddress
    , gaavgalAdsPersonalizationEnabled
    , gaavgalCreateTime

    -- * GoogleAnalyticsAdminV1alphaAuditUserLink
    , GoogleAnalyticsAdminV1alphaAuditUserLink
    , googleAnalyticsAdminV1alphaAuditUserLink
    , gaavaulDirectRoles
    , gaavaulEffectiveRoles
    , gaavaulName
    , gaavaulEmailAddress

    -- * GoogleAnalyticsAdminV1alphaAuditUserLinksResponse
    , GoogleAnalyticsAdminV1alphaAuditUserLinksResponse
    , googleAnalyticsAdminV1alphaAuditUserLinksResponse
    , gaavaulrNextPageToken
    , gaavaulrUserLinks

    -- * GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse
    , GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse
    , googleAnalyticsAdminV1alphaBatchCreateUserLinksResponse
    , gaavbculrUserLinks

    -- * Xgafv
    , Xgafv (..)

    -- * GoogleAnalyticsAdminV1alphaAccount
    , GoogleAnalyticsAdminV1alphaAccount
    , googleAnalyticsAdminV1alphaAccount
    , gaavaUpdateTime
    , gaavaName
    , gaavaDisplayName
    , gaavaCountryCode
    , gaavaDeleted
    , gaavaCreateTime

    -- * GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest
    , GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest
    , googleAnalyticsAdminV1alphaUpdateUserLinkRequest
    , gaavuulrUserLink

    -- * GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest
    , GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest
    , googleAnalyticsAdminV1alphaDeleteUserLinkRequest
    , gaavdulrName

    -- * GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings
    , GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings
    , googleAnalyticsAdminV1alphaEnhancedMeasurementSettings
    , gaavemsOutboundClicksEnabled
    , gaavemsSearchQueryParameter
    , gaavemsURLQueryParameter
    , gaavemsSiteSearchEnabled
    , gaavemsVideoEngagementEnabled
    , gaavemsStreamEnabled
    , gaavemsExcludedDomains
    , gaavemsArticlesAndBlogsEnabled
    , gaavemsFileDownloadsEnabled
    , gaavemsName
    , gaavemsFormInteractionsEnabled
    , gaavemsContentViewsEnabled
    , gaavemsDataTaggedElementClicksEnabled
    , gaavemsPageViewsEnabled
    , gaavemsPageChangesEnabled
    , gaavemsPageLoadsEnabled
    , gaavemsScrollsEnabled
    , gaavemsProductsAndEcommerceEnabled

    -- * GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
    , GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
    , googleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
    , gaavbdulrRequests

    -- * GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
    , GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
    , googleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
    , gaavbuulrRequests

    -- * GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
    , GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
    , googleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
    , gaavbculrNotifyNewUsers
    , gaavbculrRequests

    -- * GoogleAnalyticsAdminV1alphaListUserLinksResponse
    , GoogleAnalyticsAdminV1alphaListUserLinksResponse
    , googleAnalyticsAdminV1alphaListUserLinksResponse
    , gaavlulrNextPageToken
    , gaavlulrUserLinks

    -- * GoogleAnalyticsAdminV1alphaIosAppDataStream
    , GoogleAnalyticsAdminV1alphaIosAppDataStream
    , googleAnalyticsAdminV1alphaIosAppDataStream
    , gaaviadsBundleId
    , gaaviadsUpdateTime
    , gaaviadsName
    , gaaviadsDisplayName
    , gaaviadsCreateTime
    , gaaviadsFirebaseAppId

    -- * GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess
    , GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess (..)

    -- * GoogleAnalyticsAdminV1alphaListPropertiesResponse
    , GoogleAnalyticsAdminV1alphaListPropertiesResponse
    , googleAnalyticsAdminV1alphaListPropertiesResponse
    , gaavlprNextPageToken
    , gaavlprProperties

    -- * GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse
    , GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse
    , googleAnalyticsAdminV1alphaListGoogleAdsLinksResponse
    , gaavlgalrGoogleAdsLinks
    , gaavlgalrNextPageToken

    -- * GoogleAnalyticsAdminV1alphaDataSharingSettings
    , GoogleAnalyticsAdminV1alphaDataSharingSettings
    , googleAnalyticsAdminV1alphaDataSharingSettings
    , gaavdssSharingWithGoogleAnySalesEnabled
    , gaavdssSharingWithGoogleAssignedSalesEnabled
    , gaavdssName
    , gaavdssSharingWithOthersEnabled
    , gaavdssSharingWithGoogleProductsEnabled
    , gaavdssSharingWithGoogleSupportEnabled

    -- * GoogleAnalyticsAdminV1alphaPropertyIndustryCategory
    , GoogleAnalyticsAdminV1alphaPropertyIndustryCategory (..)

    -- * GoogleAnalyticsAdminV1alphaGlobalSiteTag
    , GoogleAnalyticsAdminV1alphaGlobalSiteTag
    , googleAnalyticsAdminV1alphaGlobalSiteTag
    , gaavgstSnippet

    -- * GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse
    , GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse
    , googleAnalyticsAdminV1alphaListIosAppDataStreamsResponse
    , gaavliadsrNextPageToken
    , gaavliadsrIosAppDataStreams
    ) where

import Network.Google.AnalyticsAdmin.Types.Product
import Network.Google.AnalyticsAdmin.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v1alpha' of the Google Analytics Admin API. This contains the host and root path used as a starting point for constructing service requests.
analyticsAdminService :: ServiceConfig
analyticsAdminService
  = defaultService (ServiceId "analyticsadmin:v1alpha")
      "analyticsadmin.googleapis.com"

-- | Manage Google Analytics Account users by email address
analyticsManageUsersScope :: Proxy '["https://www.googleapis.com/auth/analytics.manage.users"]
analyticsManageUsersScope = Proxy

-- | View Google Analytics user permissions
analyticsManageUsersReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/analytics.manage.users.readonly"]
analyticsManageUsersReadOnlyScope = Proxy

-- | See and download your Google Analytics data
analyticsReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/analytics.readonly"]
analyticsReadOnlyScope = Proxy

-- | Edit Google Analytics management entities
analyticsEditScope :: Proxy '["https://www.googleapis.com/auth/analytics.edit"]
analyticsEditScope = Proxy
