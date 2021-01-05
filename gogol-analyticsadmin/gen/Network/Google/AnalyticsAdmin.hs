{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.AnalyticsAdmin
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- |
--
-- /See:/ <http://code.google.com/apis/analytics/docs/mgmt/home.html Google Analytics Admin API Reference>
module Network.Google.AnalyticsAdmin
    (
    -- * Service Configuration
      analyticsAdminService

    -- * OAuth Scopes
    , analyticsManageUsersScope
    , analyticsManageUsersReadOnlyScope
    , analyticsReadOnlyScope
    , analyticsEditScope

    -- * API Declaration
    , AnalyticsAdminAPI

    -- * Resources

    -- ** analyticsadmin.accountSummaries.list
    , module Network.Google.Resource.AnalyticsAdmin.AccountSummaries.List

    -- ** analyticsadmin.accounts.delete
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.Delete

    -- ** analyticsadmin.accounts.get
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.Get

    -- ** analyticsadmin.accounts.getDataSharingSettings
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.GetDataSharingSettings

    -- ** analyticsadmin.accounts.list
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.List

    -- ** analyticsadmin.accounts.patch
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.Patch

    -- ** analyticsadmin.accounts.provisionAccountTicket
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.ProvisionAccountTicket

    -- ** analyticsadmin.accounts.userLinks.audit
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Audit

    -- ** analyticsadmin.accounts.userLinks.batchCreate
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchCreate

    -- ** analyticsadmin.accounts.userLinks.batchDelete
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchDelete

    -- ** analyticsadmin.accounts.userLinks.batchGet
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchGet

    -- ** analyticsadmin.accounts.userLinks.batchUpdate
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchUpdate

    -- ** analyticsadmin.accounts.userLinks.create
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Create

    -- ** analyticsadmin.accounts.userLinks.delete
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Delete

    -- ** analyticsadmin.accounts.userLinks.get
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Get

    -- ** analyticsadmin.accounts.userLinks.list
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.List

    -- ** analyticsadmin.accounts.userLinks.patch
    , module Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Patch

    -- ** analyticsadmin.properties.androidAppDataStreams.create
    , module Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Create

    -- ** analyticsadmin.properties.androidAppDataStreams.delete
    , module Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Delete

    -- ** analyticsadmin.properties.androidAppDataStreams.get
    , module Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Get

    -- ** analyticsadmin.properties.androidAppDataStreams.list
    , module Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.List

    -- ** analyticsadmin.properties.androidAppDataStreams.patch
    , module Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Patch

    -- ** analyticsadmin.properties.create
    , module Network.Google.Resource.AnalyticsAdmin.Properties.Create

    -- ** analyticsadmin.properties.delete
    , module Network.Google.Resource.AnalyticsAdmin.Properties.Delete

    -- ** analyticsadmin.properties.firebaseLinks.create
    , module Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.Create

    -- ** analyticsadmin.properties.firebaseLinks.delete
    , module Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.Delete

    -- ** analyticsadmin.properties.firebaseLinks.list
    , module Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.List

    -- ** analyticsadmin.properties.firebaseLinks.patch
    , module Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.Patch

    -- ** analyticsadmin.properties.get
    , module Network.Google.Resource.AnalyticsAdmin.Properties.Get

    -- ** analyticsadmin.properties.googleAdsLinks.create
    , module Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.Create

    -- ** analyticsadmin.properties.googleAdsLinks.delete
    , module Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.Delete

    -- ** analyticsadmin.properties.googleAdsLinks.list
    , module Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.List

    -- ** analyticsadmin.properties.googleAdsLinks.patch
    , module Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.Patch

    -- ** analyticsadmin.properties.iosAppDataStreams.create
    , module Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Create

    -- ** analyticsadmin.properties.iosAppDataStreams.delete
    , module Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Delete

    -- ** analyticsadmin.properties.iosAppDataStreams.get
    , module Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Get

    -- ** analyticsadmin.properties.iosAppDataStreams.list
    , module Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.List

    -- ** analyticsadmin.properties.iosAppDataStreams.patch
    , module Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Patch

    -- ** analyticsadmin.properties.list
    , module Network.Google.Resource.AnalyticsAdmin.Properties.List

    -- ** analyticsadmin.properties.patch
    , module Network.Google.Resource.AnalyticsAdmin.Properties.Patch

    -- ** analyticsadmin.properties.userLinks.audit
    , module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Audit

    -- ** analyticsadmin.properties.userLinks.batchCreate
    , module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchCreate

    -- ** analyticsadmin.properties.userLinks.batchDelete
    , module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchDelete

    -- ** analyticsadmin.properties.userLinks.batchGet
    , module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchGet

    -- ** analyticsadmin.properties.userLinks.batchUpdate
    , module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchUpdate

    -- ** analyticsadmin.properties.userLinks.create
    , module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Create

    -- ** analyticsadmin.properties.userLinks.delete
    , module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Delete

    -- ** analyticsadmin.properties.userLinks.get
    , module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Get

    -- ** analyticsadmin.properties.userLinks.list
    , module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.List

    -- ** analyticsadmin.properties.userLinks.patch
    , module Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Patch

    -- ** analyticsadmin.properties.webDataStreams.create
    , module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Create

    -- ** analyticsadmin.properties.webDataStreams.delete
    , module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Delete

    -- ** analyticsadmin.properties.webDataStreams.get
    , module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Get

    -- ** analyticsadmin.properties.webDataStreams.getEnhancedMeasurementSettings
    , module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.GetEnhancedMeasurementSettings

    -- ** analyticsadmin.properties.webDataStreams.getGlobalSiteTag
    , module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.GetGlobalSiteTag

    -- ** analyticsadmin.properties.webDataStreams.list
    , module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.List

    -- ** analyticsadmin.properties.webDataStreams.patch
    , module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Patch

    -- ** analyticsadmin.properties.webDataStreams.updateEnhancedMeasurementSettings
    , module Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.UpdateEnhancedMeasurementSettings

    -- * Types

    -- ** GoogleAnalyticsAdminV1alphaProperty
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

    -- ** GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse
    , GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse
    , googleAnalyticsAdminV1alphaListFirebaseLinksResponse
    , gaavlflrNextPageToken
    , gaavlflrFirebaseLinks

    -- ** GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse
    , GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse
    , googleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse
    , gaavlaadsrNextPageToken
    , gaavlaadsrAndroidAppDataStreams

    -- ** GoogleAnalyticsAdminV1alphaCreateUserLinkRequest
    , GoogleAnalyticsAdminV1alphaCreateUserLinkRequest
    , googleAnalyticsAdminV1alphaCreateUserLinkRequest
    , gaavculrParent
    , gaavculrNotifyNewUser
    , gaavculrUserLink

    -- ** GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse
    , GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse
    , googleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse
    , gaavbuulrUserLinks

    -- ** GoogleAnalyticsAdminV1alphaWebDataStream
    , GoogleAnalyticsAdminV1alphaWebDataStream
    , googleAnalyticsAdminV1alphaWebDataStream
    , gaavwdsMeasurementId
    , gaavwdsDefaultURI
    , gaavwdsUpdateTime
    , gaavwdsName
    , gaavwdsDisplayName
    , gaavwdsCreateTime
    , gaavwdsFirebaseAppId

    -- ** GoogleAnalyticsAdminV1alphaUserLink
    , GoogleAnalyticsAdminV1alphaUserLink
    , googleAnalyticsAdminV1alphaUserLink
    , gaavulDirectRoles
    , gaavulName
    , gaavulEmailAddress

    -- ** GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse
    , GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse
    , googleAnalyticsAdminV1alphaListWebDataStreamsResponse
    , gaavlwdsrWebDataStreams
    , gaavlwdsrNextPageToken

    -- ** GoogleAnalyticsAdminV1alphaAuditUserLinksRequest
    , GoogleAnalyticsAdminV1alphaAuditUserLinksRequest
    , googleAnalyticsAdminV1alphaAuditUserLinksRequest
    , gaavaulrPageToken
    , gaavaulrPageSize

    -- ** GoogleAnalyticsAdminV1alphaAndroidAppDataStream
    , GoogleAnalyticsAdminV1alphaAndroidAppDataStream
    , googleAnalyticsAdminV1alphaAndroidAppDataStream
    , gaavaadsPackageName
    , gaavaadsUpdateTime
    , gaavaadsName
    , gaavaadsDisplayName
    , gaavaadsCreateTime
    , gaavaadsFirebaseAppId

    -- ** GoogleAnalyticsAdminV1alphaListAccountsResponse
    , GoogleAnalyticsAdminV1alphaListAccountsResponse
    , googleAnalyticsAdminV1alphaListAccountsResponse
    , gaavlarNextPageToken
    , gaavlarAccounts

    -- ** GoogleAnalyticsAdminV1alphaPropertySummary
    , GoogleAnalyticsAdminV1alphaPropertySummary
    , googleAnalyticsAdminV1alphaPropertySummary
    , gaavpsProperty
    , gaavpsDisplayName

    -- ** GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse
    , GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse
    , googleAnalyticsAdminV1alphaBatchGetUserLinksResponse
    , gaavbgulrUserLinks

    -- ** GoogleProtobufEmpty
    , GoogleProtobufEmpty
    , googleProtobufEmpty

    -- ** GoogleAnalyticsAdminV1alphaFirebaseLink
    , GoogleAnalyticsAdminV1alphaFirebaseLink
    , googleAnalyticsAdminV1alphaFirebaseLink
    , gaavflProject
    , gaavflMaximumUserAccess
    , gaavflName
    , gaavflCreateTime

    -- ** GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest
    , GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest
    , googleAnalyticsAdminV1alphaProvisionAccountTicketRequest
    , gaavpatrRedirectURI
    , gaavpatrAccount

    -- ** GoogleAnalyticsAdminV1alphaAccountSummary
    , GoogleAnalyticsAdminV1alphaAccountSummary
    , googleAnalyticsAdminV1alphaAccountSummary
    , gaavasPropertySummaries
    , gaavasAccount
    , gaavasName
    , gaavasDisplayName

    -- ** GoogleAnalyticsAdminV1alphaListAccountSummariesResponse
    , GoogleAnalyticsAdminV1alphaListAccountSummariesResponse
    , googleAnalyticsAdminV1alphaListAccountSummariesResponse
    , gaavlasrNextPageToken
    , gaavlasrAccountSummaries

    -- ** GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse
    , GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse
    , googleAnalyticsAdminV1alphaProvisionAccountTicketResponse
    , gaavpatrAccountTicketId

    -- ** GoogleAnalyticsAdminV1alphaGoogleAdsLink
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

    -- ** GoogleAnalyticsAdminV1alphaAuditUserLink
    , GoogleAnalyticsAdminV1alphaAuditUserLink
    , googleAnalyticsAdminV1alphaAuditUserLink
    , gaavaulDirectRoles
    , gaavaulEffectiveRoles
    , gaavaulName
    , gaavaulEmailAddress

    -- ** GoogleAnalyticsAdminV1alphaAuditUserLinksResponse
    , GoogleAnalyticsAdminV1alphaAuditUserLinksResponse
    , googleAnalyticsAdminV1alphaAuditUserLinksResponse
    , gaavaulrNextPageToken
    , gaavaulrUserLinks

    -- ** GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse
    , GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse
    , googleAnalyticsAdminV1alphaBatchCreateUserLinksResponse
    , gaavbculrUserLinks

    -- ** Xgafv
    , Xgafv (..)

    -- ** GoogleAnalyticsAdminV1alphaAccount
    , GoogleAnalyticsAdminV1alphaAccount
    , googleAnalyticsAdminV1alphaAccount
    , gaavaUpdateTime
    , gaavaName
    , gaavaDisplayName
    , gaavaCountryCode
    , gaavaDeleted
    , gaavaCreateTime

    -- ** GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest
    , GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest
    , googleAnalyticsAdminV1alphaUpdateUserLinkRequest
    , gaavuulrUserLink

    -- ** GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest
    , GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest
    , googleAnalyticsAdminV1alphaDeleteUserLinkRequest
    , gaavdulrName

    -- ** GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings
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

    -- ** GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
    , GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
    , googleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
    , gaavbdulrRequests

    -- ** GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
    , GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
    , googleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
    , gaavbuulrRequests

    -- ** GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
    , GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
    , googleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
    , gaavbculrNotifyNewUsers
    , gaavbculrRequests

    -- ** GoogleAnalyticsAdminV1alphaListUserLinksResponse
    , GoogleAnalyticsAdminV1alphaListUserLinksResponse
    , googleAnalyticsAdminV1alphaListUserLinksResponse
    , gaavlulrNextPageToken
    , gaavlulrUserLinks

    -- ** GoogleAnalyticsAdminV1alphaIosAppDataStream
    , GoogleAnalyticsAdminV1alphaIosAppDataStream
    , googleAnalyticsAdminV1alphaIosAppDataStream
    , gaaviadsBundleId
    , gaaviadsUpdateTime
    , gaaviadsName
    , gaaviadsDisplayName
    , gaaviadsCreateTime
    , gaaviadsFirebaseAppId

    -- ** GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess
    , GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess (..)

    -- ** GoogleAnalyticsAdminV1alphaListPropertiesResponse
    , GoogleAnalyticsAdminV1alphaListPropertiesResponse
    , googleAnalyticsAdminV1alphaListPropertiesResponse
    , gaavlprNextPageToken
    , gaavlprProperties

    -- ** GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse
    , GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse
    , googleAnalyticsAdminV1alphaListGoogleAdsLinksResponse
    , gaavlgalrGoogleAdsLinks
    , gaavlgalrNextPageToken

    -- ** GoogleAnalyticsAdminV1alphaDataSharingSettings
    , GoogleAnalyticsAdminV1alphaDataSharingSettings
    , googleAnalyticsAdminV1alphaDataSharingSettings
    , gaavdssSharingWithGoogleAnySalesEnabled
    , gaavdssSharingWithGoogleAssignedSalesEnabled
    , gaavdssName
    , gaavdssSharingWithOthersEnabled
    , gaavdssSharingWithGoogleProductsEnabled
    , gaavdssSharingWithGoogleSupportEnabled

    -- ** GoogleAnalyticsAdminV1alphaPropertyIndustryCategory
    , GoogleAnalyticsAdminV1alphaPropertyIndustryCategory (..)

    -- ** GoogleAnalyticsAdminV1alphaGlobalSiteTag
    , GoogleAnalyticsAdminV1alphaGlobalSiteTag
    , googleAnalyticsAdminV1alphaGlobalSiteTag
    , gaavgstSnippet

    -- ** GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse
    , GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse
    , googleAnalyticsAdminV1alphaListIosAppDataStreamsResponse
    , gaavliadsrNextPageToken
    , gaavliadsrIosAppDataStreams
    ) where

import Network.Google.Prelude
import Network.Google.AnalyticsAdmin.Types
import Network.Google.Resource.AnalyticsAdmin.AccountSummaries.List
import Network.Google.Resource.AnalyticsAdmin.Accounts.Delete
import Network.Google.Resource.AnalyticsAdmin.Accounts.Get
import Network.Google.Resource.AnalyticsAdmin.Accounts.GetDataSharingSettings
import Network.Google.Resource.AnalyticsAdmin.Accounts.List
import Network.Google.Resource.AnalyticsAdmin.Accounts.Patch
import Network.Google.Resource.AnalyticsAdmin.Accounts.ProvisionAccountTicket
import Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Audit
import Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchCreate
import Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchDelete
import Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchGet
import Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.BatchUpdate
import Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Create
import Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Delete
import Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Get
import Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.List
import Network.Google.Resource.AnalyticsAdmin.Accounts.UserLinks.Patch
import Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Create
import Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Delete
import Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Get
import Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.List
import Network.Google.Resource.AnalyticsAdmin.Properties.AndroidAppDataStreams.Patch
import Network.Google.Resource.AnalyticsAdmin.Properties.Create
import Network.Google.Resource.AnalyticsAdmin.Properties.Delete
import Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.Create
import Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.Delete
import Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.List
import Network.Google.Resource.AnalyticsAdmin.Properties.FirebaseLinks.Patch
import Network.Google.Resource.AnalyticsAdmin.Properties.Get
import Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.Create
import Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.Delete
import Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.List
import Network.Google.Resource.AnalyticsAdmin.Properties.GoogleAdsLinks.Patch
import Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Create
import Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Delete
import Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Get
import Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.List
import Network.Google.Resource.AnalyticsAdmin.Properties.IosAppDataStreams.Patch
import Network.Google.Resource.AnalyticsAdmin.Properties.List
import Network.Google.Resource.AnalyticsAdmin.Properties.Patch
import Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Audit
import Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchCreate
import Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchDelete
import Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchGet
import Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.BatchUpdate
import Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Create
import Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Delete
import Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Get
import Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.List
import Network.Google.Resource.AnalyticsAdmin.Properties.UserLinks.Patch
import Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Create
import Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Delete
import Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Get
import Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.GetEnhancedMeasurementSettings
import Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.GetGlobalSiteTag
import Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.List
import Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.Patch
import Network.Google.Resource.AnalyticsAdmin.Properties.WebDataStreams.UpdateEnhancedMeasurementSettings

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Analytics Admin API service.
type AnalyticsAdminAPI =
     AccountsUserLinksListResource :<|>
       AccountsUserLinksAuditResource
       :<|> AccountsUserLinksPatchResource
       :<|> AccountsUserLinksGetResource
       :<|> AccountsUserLinksCreateResource
       :<|> AccountsUserLinksBatchGetResource
       :<|> AccountsUserLinksBatchUpdateResource
       :<|> AccountsUserLinksBatchDeleteResource
       :<|> AccountsUserLinksBatchCreateResource
       :<|> AccountsUserLinksDeleteResource
       :<|> AccountsListResource
       :<|> AccountsGetDataSharingSettingsResource
       :<|> AccountsPatchResource
       :<|> AccountsGetResource
       :<|> AccountsProvisionAccountTicketResource
       :<|> AccountsDeleteResource
       :<|> AccountSummariesListResource
       :<|> PropertiesGoogleAdsLinksListResource
       :<|> PropertiesGoogleAdsLinksPatchResource
       :<|> PropertiesGoogleAdsLinksCreateResource
       :<|> PropertiesGoogleAdsLinksDeleteResource
       :<|> PropertiesWebDataStreamsListResource
       :<|>
       PropertiesWebDataStreamsUpdateEnhancedMeasurementSettingsResource
       :<|> PropertiesWebDataStreamsPatchResource
       :<|> PropertiesWebDataStreamsGetResource
       :<|> PropertiesWebDataStreamsCreateResource
       :<|> PropertiesWebDataStreamsGetGlobalSiteTagResource
       :<|>
       PropertiesWebDataStreamsGetEnhancedMeasurementSettingsResource
       :<|> PropertiesWebDataStreamsDeleteResource
       :<|> PropertiesUserLinksListResource
       :<|> PropertiesUserLinksAuditResource
       :<|> PropertiesUserLinksPatchResource
       :<|> PropertiesUserLinksGetResource
       :<|> PropertiesUserLinksCreateResource
       :<|> PropertiesUserLinksBatchGetResource
       :<|> PropertiesUserLinksBatchUpdateResource
       :<|> PropertiesUserLinksBatchDeleteResource
       :<|> PropertiesUserLinksBatchCreateResource
       :<|> PropertiesUserLinksDeleteResource
       :<|> PropertiesIosAppDataStreamsListResource
       :<|> PropertiesIosAppDataStreamsPatchResource
       :<|> PropertiesIosAppDataStreamsGetResource
       :<|> PropertiesIosAppDataStreamsCreateResource
       :<|> PropertiesIosAppDataStreamsDeleteResource
       :<|> PropertiesFirebaseLinksListResource
       :<|> PropertiesFirebaseLinksPatchResource
       :<|> PropertiesFirebaseLinksCreateResource
       :<|> PropertiesFirebaseLinksDeleteResource
       :<|> PropertiesAndroidAppDataStreamsListResource
       :<|> PropertiesAndroidAppDataStreamsPatchResource
       :<|> PropertiesAndroidAppDataStreamsGetResource
       :<|> PropertiesAndroidAppDataStreamsCreateResource
       :<|> PropertiesAndroidAppDataStreamsDeleteResource
       :<|> PropertiesListResource
       :<|> PropertiesPatchResource
       :<|> PropertiesGetResource
       :<|> PropertiesCreateResource
       :<|> PropertiesDeleteResource
