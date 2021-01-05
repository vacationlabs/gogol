{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.AnalyticsAdmin.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.AnalyticsAdmin.Types.Product where

import Network.Google.AnalyticsAdmin.Types.Sum
import Network.Google.Prelude

-- | A resource message representing a Google Analytics GA4 property.
--
-- /See:/ 'googleAnalyticsAdminV1alphaProperty' smart constructor.
data GoogleAnalyticsAdminV1alphaProperty =
  GoogleAnalyticsAdminV1alphaProperty'
    { _gaavpParent :: !(Maybe Text)
    , _gaavpCurrencyCode :: !(Maybe Text)
    , _gaavpIndustryCategory :: !(Maybe GoogleAnalyticsAdminV1alphaPropertyIndustryCategory)
    , _gaavpUpdateTime :: !(Maybe DateTime')
    , _gaavpName :: !(Maybe Text)
    , _gaavpDisplayName :: !(Maybe Text)
    , _gaavpDeleted :: !(Maybe Bool)
    , _gaavpTimeZone :: !(Maybe Text)
    , _gaavpCreateTime :: !(Maybe DateTime')
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavpParent'
--
-- * 'gaavpCurrencyCode'
--
-- * 'gaavpIndustryCategory'
--
-- * 'gaavpUpdateTime'
--
-- * 'gaavpName'
--
-- * 'gaavpDisplayName'
--
-- * 'gaavpDeleted'
--
-- * 'gaavpTimeZone'
--
-- * 'gaavpCreateTime'
googleAnalyticsAdminV1alphaProperty
    :: GoogleAnalyticsAdminV1alphaProperty
googleAnalyticsAdminV1alphaProperty =
  GoogleAnalyticsAdminV1alphaProperty'
    { _gaavpParent = Nothing
    , _gaavpCurrencyCode = Nothing
    , _gaavpIndustryCategory = Nothing
    , _gaavpUpdateTime = Nothing
    , _gaavpName = Nothing
    , _gaavpDisplayName = Nothing
    , _gaavpDeleted = Nothing
    , _gaavpTimeZone = Nothing
    , _gaavpCreateTime = Nothing
    }


-- | Immutable. Resource name of this property\'s logical parent. Note: The
-- Property-Moving UI can be used to change the parent. Format:
-- accounts\/{account} Example: \"accounts\/100\"
gaavpParent :: Lens' GoogleAnalyticsAdminV1alphaProperty (Maybe Text)
gaavpParent
  = lens _gaavpParent (\ s a -> s{_gaavpParent = a})

-- | The currency type used in reports involving monetary values. Format:
-- https:\/\/en.wikipedia.org\/wiki\/ISO_4217 Examples: \"USD\", \"EUR\",
-- \"JPY\"
gaavpCurrencyCode :: Lens' GoogleAnalyticsAdminV1alphaProperty (Maybe Text)
gaavpCurrencyCode
  = lens _gaavpCurrencyCode
      (\ s a -> s{_gaavpCurrencyCode = a})

-- | Industry associated with this property Example: AUTOMOTIVE,
-- FOOD_AND_DRINK
gaavpIndustryCategory :: Lens' GoogleAnalyticsAdminV1alphaProperty (Maybe GoogleAnalyticsAdminV1alphaPropertyIndustryCategory)
gaavpIndustryCategory
  = lens _gaavpIndustryCategory
      (\ s a -> s{_gaavpIndustryCategory = a})

-- | Output only. Time when entity payload fields were last updated.
gaavpUpdateTime :: Lens' GoogleAnalyticsAdminV1alphaProperty (Maybe UTCTime)
gaavpUpdateTime
  = lens _gaavpUpdateTime
      (\ s a -> s{_gaavpUpdateTime = a})
      . mapping _DateTime

-- | Output only. Resource name of this property. Format:
-- properties\/{property_id} Example: \"properties\/1000\"
gaavpName :: Lens' GoogleAnalyticsAdminV1alphaProperty (Maybe Text)
gaavpName
  = lens _gaavpName (\ s a -> s{_gaavpName = a})

-- | Required. Human-readable display name for this property. The max allowed
-- display name length is 100 UTF-16 code units.
gaavpDisplayName :: Lens' GoogleAnalyticsAdminV1alphaProperty (Maybe Text)
gaavpDisplayName
  = lens _gaavpDisplayName
      (\ s a -> s{_gaavpDisplayName = a})

-- | Output only. Indicates whether this Property is soft-deleted or not.
-- Deleted properties are excluded from List results unless specifically
-- requested.
gaavpDeleted :: Lens' GoogleAnalyticsAdminV1alphaProperty (Maybe Bool)
gaavpDeleted
  = lens _gaavpDeleted (\ s a -> s{_gaavpDeleted = a})

-- | Reporting Time Zone, used as the day boundary for reports, regardless of
-- where the data originates. If the time zone honors DST, Analytics will
-- automatically adjust for the changes. NOTE: Changing the time zone only
-- affects data going forward, and is not applied retroactively. Format:
-- https:\/\/www.iana.org\/time-zones Example: \"America\/Los_Angeles\"
gaavpTimeZone :: Lens' GoogleAnalyticsAdminV1alphaProperty (Maybe Text)
gaavpTimeZone
  = lens _gaavpTimeZone
      (\ s a -> s{_gaavpTimeZone = a})

-- | Output only. Time when the entity was originally created.
gaavpCreateTime :: Lens' GoogleAnalyticsAdminV1alphaProperty (Maybe UTCTime)
gaavpCreateTime
  = lens _gaavpCreateTime
      (\ s a -> s{_gaavpCreateTime = a})
      . mapping _DateTime

instance FromJSON GoogleAnalyticsAdminV1alphaProperty
         where
        parseJSON
          = withObject "GoogleAnalyticsAdminV1alphaProperty"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaProperty' <$>
                   (o .:? "parent") <*> (o .:? "currencyCode") <*>
                     (o .:? "industryCategory")
                     <*> (o .:? "updateTime")
                     <*> (o .:? "name")
                     <*> (o .:? "displayName")
                     <*> (o .:? "deleted")
                     <*> (o .:? "timeZone")
                     <*> (o .:? "createTime"))

instance ToJSON GoogleAnalyticsAdminV1alphaProperty
         where
        toJSON GoogleAnalyticsAdminV1alphaProperty'{..}
          = object
              (catMaybes
                 [("parent" .=) <$> _gaavpParent,
                  ("currencyCode" .=) <$> _gaavpCurrencyCode,
                  ("industryCategory" .=) <$> _gaavpIndustryCategory,
                  ("updateTime" .=) <$> _gaavpUpdateTime,
                  ("name" .=) <$> _gaavpName,
                  ("displayName" .=) <$> _gaavpDisplayName,
                  ("deleted" .=) <$> _gaavpDeleted,
                  ("timeZone" .=) <$> _gaavpTimeZone,
                  ("createTime" .=) <$> _gaavpCreateTime])

-- | Response message for ListFirebaseLinks RPC
--
-- /See:/ 'googleAnalyticsAdminV1alphaListFirebaseLinksResponse' smart constructor.
data GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse =
  GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse'
    { _gaavlflrNextPageToken :: !(Maybe Text)
    , _gaavlflrFirebaseLinks :: !(Maybe [GoogleAnalyticsAdminV1alphaFirebaseLink])
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavlflrNextPageToken'
--
-- * 'gaavlflrFirebaseLinks'
googleAnalyticsAdminV1alphaListFirebaseLinksResponse
    :: GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse
googleAnalyticsAdminV1alphaListFirebaseLinksResponse =
  GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse'
    {_gaavlflrNextPageToken = Nothing, _gaavlflrFirebaseLinks = Nothing}


-- | A token, which can be sent as \`page_token\` to retrieve the next page.
-- If this field is omitted, there are no subsequent pages. Currently,
-- Google Analytics supports only one FirebaseLink per property, so this
-- will never be populated.
gaavlflrNextPageToken :: Lens' GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse (Maybe Text)
gaavlflrNextPageToken
  = lens _gaavlflrNextPageToken
      (\ s a -> s{_gaavlflrNextPageToken = a})

-- | List of FirebaseLinks. This will have at most one value.
gaavlflrFirebaseLinks :: Lens' GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse [GoogleAnalyticsAdminV1alphaFirebaseLink]
gaavlflrFirebaseLinks
  = lens _gaavlflrFirebaseLinks
      (\ s a -> s{_gaavlflrFirebaseLinks = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse'
                   <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "firebaseLinks" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaListFirebaseLinksResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _gaavlflrNextPageToken,
                  ("firebaseLinks" .=) <$> _gaavlflrFirebaseLinks])

-- | Request message for ListAndroidDataStreams RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse' smart constructor.
data GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse =
  GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse'
    { _gaavlaadsrNextPageToken :: !(Maybe Text)
    , _gaavlaadsrAndroidAppDataStreams :: !(Maybe [GoogleAnalyticsAdminV1alphaAndroidAppDataStream])
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavlaadsrNextPageToken'
--
-- * 'gaavlaadsrAndroidAppDataStreams'
googleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse
    :: GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse
googleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse =
  GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse'
    { _gaavlaadsrNextPageToken = Nothing
    , _gaavlaadsrAndroidAppDataStreams = Nothing
    }


-- | A token, which can be sent as \`page_token\` to retrieve the next page.
-- If this field is omitted, there are no subsequent pages.
gaavlaadsrNextPageToken :: Lens' GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse (Maybe Text)
gaavlaadsrNextPageToken
  = lens _gaavlaadsrNextPageToken
      (\ s a -> s{_gaavlaadsrNextPageToken = a})

-- | Results that matched the filter criteria and were accessible to the
-- caller.
gaavlaadsrAndroidAppDataStreams :: Lens' GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse [GoogleAnalyticsAdminV1alphaAndroidAppDataStream]
gaavlaadsrAndroidAppDataStreams
  = lens _gaavlaadsrAndroidAppDataStreams
      (\ s a -> s{_gaavlaadsrAndroidAppDataStreams = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse'
                   <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "androidAppDataStreams" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaListAndroidAppDataStreamsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _gaavlaadsrNextPageToken,
                  ("androidAppDataStreams" .=) <$>
                    _gaavlaadsrAndroidAppDataStreams])

-- | Request message for CreateUserLink RPC. Users can have multiple email
-- addresses associated with their Google account, and one of these email
-- addresses is the \"primary\" email address. Any of the email addresses
-- associated with a Google account may be used for a new UserLink, but the
-- returned UserLink will always contain the \"primary\" email address. As
-- a result, the input and output email address for this request may
-- differ.
--
-- /See:/ 'googleAnalyticsAdminV1alphaCreateUserLinkRequest' smart constructor.
data GoogleAnalyticsAdminV1alphaCreateUserLinkRequest =
  GoogleAnalyticsAdminV1alphaCreateUserLinkRequest'
    { _gaavculrParent :: !(Maybe Text)
    , _gaavculrNotifyNewUser :: !(Maybe Bool)
    , _gaavculrUserLink :: !(Maybe GoogleAnalyticsAdminV1alphaUserLink)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaCreateUserLinkRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavculrParent'
--
-- * 'gaavculrNotifyNewUser'
--
-- * 'gaavculrUserLink'
googleAnalyticsAdminV1alphaCreateUserLinkRequest
    :: GoogleAnalyticsAdminV1alphaCreateUserLinkRequest
googleAnalyticsAdminV1alphaCreateUserLinkRequest =
  GoogleAnalyticsAdminV1alphaCreateUserLinkRequest'
    { _gaavculrParent = Nothing
    , _gaavculrNotifyNewUser = Nothing
    , _gaavculrUserLink = Nothing
    }


-- | Required. Example format: accounts\/1234
gaavculrParent :: Lens' GoogleAnalyticsAdminV1alphaCreateUserLinkRequest (Maybe Text)
gaavculrParent
  = lens _gaavculrParent
      (\ s a -> s{_gaavculrParent = a})

-- | Optional. If set, then email the new user notifying them that they\'ve
-- been granted permissions to the resource.
gaavculrNotifyNewUser :: Lens' GoogleAnalyticsAdminV1alphaCreateUserLinkRequest (Maybe Bool)
gaavculrNotifyNewUser
  = lens _gaavculrNotifyNewUser
      (\ s a -> s{_gaavculrNotifyNewUser = a})

-- | Required. The user link to create.
gaavculrUserLink :: Lens' GoogleAnalyticsAdminV1alphaCreateUserLinkRequest (Maybe GoogleAnalyticsAdminV1alphaUserLink)
gaavculrUserLink
  = lens _gaavculrUserLink
      (\ s a -> s{_gaavculrUserLink = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaCreateUserLinkRequest
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaCreateUserLinkRequest"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaCreateUserLinkRequest' <$>
                   (o .:? "parent") <*> (o .:? "notifyNewUser") <*>
                     (o .:? "userLink"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaCreateUserLinkRequest
         where
        toJSON
          GoogleAnalyticsAdminV1alphaCreateUserLinkRequest'{..}
          = object
              (catMaybes
                 [("parent" .=) <$> _gaavculrParent,
                  ("notifyNewUser" .=) <$> _gaavculrNotifyNewUser,
                  ("userLink" .=) <$> _gaavculrUserLink])

-- | Response message for BatchUpdateUserLinks RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse' smart constructor.
newtype GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse =
  GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse'
    { _gaavbuulrUserLinks :: Maybe [GoogleAnalyticsAdminV1alphaUserLink]
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavbuulrUserLinks'
googleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse
    :: GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse
googleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse =
  GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse'
    {_gaavbuulrUserLinks = Nothing}


-- | The user links updated.
gaavbuulrUserLinks :: Lens' GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse [GoogleAnalyticsAdminV1alphaUserLink]
gaavbuulrUserLinks
  = lens _gaavbuulrUserLinks
      (\ s a -> s{_gaavbuulrUserLinks = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse'
                   <$> (o .:? "userLinks" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksResponse'{..}
          = object
              (catMaybes
                 [("userLinks" .=) <$> _gaavbuulrUserLinks])

-- | A resource message representing a Google Analytics web stream.
--
-- /See:/ 'googleAnalyticsAdminV1alphaWebDataStream' smart constructor.
data GoogleAnalyticsAdminV1alphaWebDataStream =
  GoogleAnalyticsAdminV1alphaWebDataStream'
    { _gaavwdsMeasurementId :: !(Maybe Text)
    , _gaavwdsDefaultURI :: !(Maybe Text)
    , _gaavwdsUpdateTime :: !(Maybe DateTime')
    , _gaavwdsName :: !(Maybe Text)
    , _gaavwdsDisplayName :: !(Maybe Text)
    , _gaavwdsCreateTime :: !(Maybe DateTime')
    , _gaavwdsFirebaseAppId :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaWebDataStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavwdsMeasurementId'
--
-- * 'gaavwdsDefaultURI'
--
-- * 'gaavwdsUpdateTime'
--
-- * 'gaavwdsName'
--
-- * 'gaavwdsDisplayName'
--
-- * 'gaavwdsCreateTime'
--
-- * 'gaavwdsFirebaseAppId'
googleAnalyticsAdminV1alphaWebDataStream
    :: GoogleAnalyticsAdminV1alphaWebDataStream
googleAnalyticsAdminV1alphaWebDataStream =
  GoogleAnalyticsAdminV1alphaWebDataStream'
    { _gaavwdsMeasurementId = Nothing
    , _gaavwdsDefaultURI = Nothing
    , _gaavwdsUpdateTime = Nothing
    , _gaavwdsName = Nothing
    , _gaavwdsDisplayName = Nothing
    , _gaavwdsCreateTime = Nothing
    , _gaavwdsFirebaseAppId = Nothing
    }


-- | Output only. Analytics \"Measurement ID\", without the \"G-\" prefix.
-- Example: \"G-1A2BCD345E\" would just be \"1A2BCD345E\"
gaavwdsMeasurementId :: Lens' GoogleAnalyticsAdminV1alphaWebDataStream (Maybe Text)
gaavwdsMeasurementId
  = lens _gaavwdsMeasurementId
      (\ s a -> s{_gaavwdsMeasurementId = a})

-- | Immutable. Domain name of the web app being measured, or empty. Example:
-- \"http:\/\/www.google.com\", \"https:\/\/www.google.com\"
gaavwdsDefaultURI :: Lens' GoogleAnalyticsAdminV1alphaWebDataStream (Maybe Text)
gaavwdsDefaultURI
  = lens _gaavwdsDefaultURI
      (\ s a -> s{_gaavwdsDefaultURI = a})

-- | Output only. Time when stream payload fields were last updated.
gaavwdsUpdateTime :: Lens' GoogleAnalyticsAdminV1alphaWebDataStream (Maybe UTCTime)
gaavwdsUpdateTime
  = lens _gaavwdsUpdateTime
      (\ s a -> s{_gaavwdsUpdateTime = a})
      . mapping _DateTime

-- | Output only. Resource name of this Data Stream. Format:
-- properties\/{property_id}\/webDataStreams\/{stream_id} Example:
-- \"properties\/1000\/webDataStreams\/2000\"
gaavwdsName :: Lens' GoogleAnalyticsAdminV1alphaWebDataStream (Maybe Text)
gaavwdsName
  = lens _gaavwdsName (\ s a -> s{_gaavwdsName = a})

-- | Required. Human-readable display name for the Data Stream. The max
-- allowed display name length is 100 UTF-16 code units.
gaavwdsDisplayName :: Lens' GoogleAnalyticsAdminV1alphaWebDataStream (Maybe Text)
gaavwdsDisplayName
  = lens _gaavwdsDisplayName
      (\ s a -> s{_gaavwdsDisplayName = a})

-- | Output only. Time when this stream was originally created.
gaavwdsCreateTime :: Lens' GoogleAnalyticsAdminV1alphaWebDataStream (Maybe UTCTime)
gaavwdsCreateTime
  = lens _gaavwdsCreateTime
      (\ s a -> s{_gaavwdsCreateTime = a})
      . mapping _DateTime

-- | Output only. ID of the corresponding web app in Firebase, if any. This
-- ID can change if the web app is deleted and recreated.
gaavwdsFirebaseAppId :: Lens' GoogleAnalyticsAdminV1alphaWebDataStream (Maybe Text)
gaavwdsFirebaseAppId
  = lens _gaavwdsFirebaseAppId
      (\ s a -> s{_gaavwdsFirebaseAppId = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaWebDataStream
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaWebDataStream"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaWebDataStream' <$>
                   (o .:? "measurementId") <*> (o .:? "defaultUri") <*>
                     (o .:? "updateTime")
                     <*> (o .:? "name")
                     <*> (o .:? "displayName")
                     <*> (o .:? "createTime")
                     <*> (o .:? "firebaseAppId"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaWebDataStream
         where
        toJSON GoogleAnalyticsAdminV1alphaWebDataStream'{..}
          = object
              (catMaybes
                 [("measurementId" .=) <$> _gaavwdsMeasurementId,
                  ("defaultUri" .=) <$> _gaavwdsDefaultURI,
                  ("updateTime" .=) <$> _gaavwdsUpdateTime,
                  ("name" .=) <$> _gaavwdsName,
                  ("displayName" .=) <$> _gaavwdsDisplayName,
                  ("createTime" .=) <$> _gaavwdsCreateTime,
                  ("firebaseAppId" .=) <$> _gaavwdsFirebaseAppId])

-- | A resource message representing a user\'s permissions on an Account or
-- Property resource.
--
-- /See:/ 'googleAnalyticsAdminV1alphaUserLink' smart constructor.
data GoogleAnalyticsAdminV1alphaUserLink =
  GoogleAnalyticsAdminV1alphaUserLink'
    { _gaavulDirectRoles :: !(Maybe [Text])
    , _gaavulName :: !(Maybe Text)
    , _gaavulEmailAddress :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaUserLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavulDirectRoles'
--
-- * 'gaavulName'
--
-- * 'gaavulEmailAddress'
googleAnalyticsAdminV1alphaUserLink
    :: GoogleAnalyticsAdminV1alphaUserLink
googleAnalyticsAdminV1alphaUserLink =
  GoogleAnalyticsAdminV1alphaUserLink'
    { _gaavulDirectRoles = Nothing
    , _gaavulName = Nothing
    , _gaavulEmailAddress = Nothing
    }


-- | Roles directly assigned to this user for this account or property. Valid
-- values: predefinedRoles\/read predefinedRoles\/collaborate
-- predefinedRoles\/edit predefinedRoles\/manage-users Excludes roles that
-- are inherited from a higher-level entity, group, or organization admin
-- role. A UserLink that is updated to have an empty list of direct_roles
-- will be deleted.
gaavulDirectRoles :: Lens' GoogleAnalyticsAdminV1alphaUserLink [Text]
gaavulDirectRoles
  = lens _gaavulDirectRoles
      (\ s a -> s{_gaavulDirectRoles = a})
      . _Default
      . _Coerce

-- | Example format: properties\/1234\/userLinks\/5678
gaavulName :: Lens' GoogleAnalyticsAdminV1alphaUserLink (Maybe Text)
gaavulName
  = lens _gaavulName (\ s a -> s{_gaavulName = a})

-- | Email address of the user to link
gaavulEmailAddress :: Lens' GoogleAnalyticsAdminV1alphaUserLink (Maybe Text)
gaavulEmailAddress
  = lens _gaavulEmailAddress
      (\ s a -> s{_gaavulEmailAddress = a})

instance FromJSON GoogleAnalyticsAdminV1alphaUserLink
         where
        parseJSON
          = withObject "GoogleAnalyticsAdminV1alphaUserLink"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaUserLink' <$>
                   (o .:? "directRoles" .!= mempty) <*> (o .:? "name")
                     <*> (o .:? "emailAddress"))

instance ToJSON GoogleAnalyticsAdminV1alphaUserLink
         where
        toJSON GoogleAnalyticsAdminV1alphaUserLink'{..}
          = object
              (catMaybes
                 [("directRoles" .=) <$> _gaavulDirectRoles,
                  ("name" .=) <$> _gaavulName,
                  ("emailAddress" .=) <$> _gaavulEmailAddress])

-- | Request message for ListWebDataStreams RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaListWebDataStreamsResponse' smart constructor.
data GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse =
  GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse'
    { _gaavlwdsrWebDataStreams :: !(Maybe [GoogleAnalyticsAdminV1alphaWebDataStream])
    , _gaavlwdsrNextPageToken :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavlwdsrWebDataStreams'
--
-- * 'gaavlwdsrNextPageToken'
googleAnalyticsAdminV1alphaListWebDataStreamsResponse
    :: GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse
googleAnalyticsAdminV1alphaListWebDataStreamsResponse =
  GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse'
    {_gaavlwdsrWebDataStreams = Nothing, _gaavlwdsrNextPageToken = Nothing}


-- | Results that matched the filter criteria and were accessible to the
-- caller.
gaavlwdsrWebDataStreams :: Lens' GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse [GoogleAnalyticsAdminV1alphaWebDataStream]
gaavlwdsrWebDataStreams
  = lens _gaavlwdsrWebDataStreams
      (\ s a -> s{_gaavlwdsrWebDataStreams = a})
      . _Default
      . _Coerce

-- | A token, which can be sent as \`page_token\` to retrieve the next page.
-- If this field is omitted, there are no subsequent pages.
gaavlwdsrNextPageToken :: Lens' GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse (Maybe Text)
gaavlwdsrNextPageToken
  = lens _gaavlwdsrNextPageToken
      (\ s a -> s{_gaavlwdsrNextPageToken = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse'
                   <$>
                   (o .:? "webDataStreams" .!= mempty) <*>
                     (o .:? "nextPageToken"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaListWebDataStreamsResponse'{..}
          = object
              (catMaybes
                 [("webDataStreams" .=) <$> _gaavlwdsrWebDataStreams,
                  ("nextPageToken" .=) <$> _gaavlwdsrNextPageToken])

-- | Request message for AuditUserLinks RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaAuditUserLinksRequest' smart constructor.
data GoogleAnalyticsAdminV1alphaAuditUserLinksRequest =
  GoogleAnalyticsAdminV1alphaAuditUserLinksRequest'
    { _gaavaulrPageToken :: !(Maybe Text)
    , _gaavaulrPageSize :: !(Maybe (Textual Int32))
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaAuditUserLinksRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavaulrPageToken'
--
-- * 'gaavaulrPageSize'
googleAnalyticsAdminV1alphaAuditUserLinksRequest
    :: GoogleAnalyticsAdminV1alphaAuditUserLinksRequest
googleAnalyticsAdminV1alphaAuditUserLinksRequest =
  GoogleAnalyticsAdminV1alphaAuditUserLinksRequest'
    {_gaavaulrPageToken = Nothing, _gaavaulrPageSize = Nothing}


-- | A page token, received from a previous \`AuditUserLinks\` call. Provide
-- this to retrieve the subsequent page. When paginating, all other
-- parameters provided to \`AuditUserLinks\` must match the call that
-- provided the page token.
gaavaulrPageToken :: Lens' GoogleAnalyticsAdminV1alphaAuditUserLinksRequest (Maybe Text)
gaavaulrPageToken
  = lens _gaavaulrPageToken
      (\ s a -> s{_gaavaulrPageToken = a})

-- | The maximum number of user links to return. The service may return fewer
-- than this value. If unspecified, at most 1000 user links will be
-- returned. The maximum value is 5000; values above 5000 will be coerced
-- to 5000.
gaavaulrPageSize :: Lens' GoogleAnalyticsAdminV1alphaAuditUserLinksRequest (Maybe Int32)
gaavaulrPageSize
  = lens _gaavaulrPageSize
      (\ s a -> s{_gaavaulrPageSize = a})
      . mapping _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaAuditUserLinksRequest
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaAuditUserLinksRequest"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaAuditUserLinksRequest' <$>
                   (o .:? "pageToken") <*> (o .:? "pageSize"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaAuditUserLinksRequest
         where
        toJSON
          GoogleAnalyticsAdminV1alphaAuditUserLinksRequest'{..}
          = object
              (catMaybes
                 [("pageToken" .=) <$> _gaavaulrPageToken,
                  ("pageSize" .=) <$> _gaavaulrPageSize])

-- | A resource message representing a Google Analytics Android app stream.
--
-- /See:/ 'googleAnalyticsAdminV1alphaAndroidAppDataStream' smart constructor.
data GoogleAnalyticsAdminV1alphaAndroidAppDataStream =
  GoogleAnalyticsAdminV1alphaAndroidAppDataStream'
    { _gaavaadsPackageName :: !(Maybe Text)
    , _gaavaadsUpdateTime :: !(Maybe DateTime')
    , _gaavaadsName :: !(Maybe Text)
    , _gaavaadsDisplayName :: !(Maybe Text)
    , _gaavaadsCreateTime :: !(Maybe DateTime')
    , _gaavaadsFirebaseAppId :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaAndroidAppDataStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavaadsPackageName'
--
-- * 'gaavaadsUpdateTime'
--
-- * 'gaavaadsName'
--
-- * 'gaavaadsDisplayName'
--
-- * 'gaavaadsCreateTime'
--
-- * 'gaavaadsFirebaseAppId'
googleAnalyticsAdminV1alphaAndroidAppDataStream
    :: GoogleAnalyticsAdminV1alphaAndroidAppDataStream
googleAnalyticsAdminV1alphaAndroidAppDataStream =
  GoogleAnalyticsAdminV1alphaAndroidAppDataStream'
    { _gaavaadsPackageName = Nothing
    , _gaavaadsUpdateTime = Nothing
    , _gaavaadsName = Nothing
    , _gaavaadsDisplayName = Nothing
    , _gaavaadsCreateTime = Nothing
    , _gaavaadsFirebaseAppId = Nothing
    }


-- | Immutable. The package name for the app being measured. Example:
-- \"com.example.myandroidapp\"
gaavaadsPackageName :: Lens' GoogleAnalyticsAdminV1alphaAndroidAppDataStream (Maybe Text)
gaavaadsPackageName
  = lens _gaavaadsPackageName
      (\ s a -> s{_gaavaadsPackageName = a})

-- | Output only. Time when stream payload fields were last updated.
gaavaadsUpdateTime :: Lens' GoogleAnalyticsAdminV1alphaAndroidAppDataStream (Maybe UTCTime)
gaavaadsUpdateTime
  = lens _gaavaadsUpdateTime
      (\ s a -> s{_gaavaadsUpdateTime = a})
      . mapping _DateTime

-- | Output only. Resource name of this Data Stream. Format:
-- properties\/{property_id}\/androidAppDataStreams\/{stream_id} Example:
-- \"properties\/1000\/androidAppDataStreams\/2000\"
gaavaadsName :: Lens' GoogleAnalyticsAdminV1alphaAndroidAppDataStream (Maybe Text)
gaavaadsName
  = lens _gaavaadsName (\ s a -> s{_gaavaadsName = a})

-- | Human-readable display name for the Data Stream. The max allowed display
-- name length is 255 UTF-16 code units.
gaavaadsDisplayName :: Lens' GoogleAnalyticsAdminV1alphaAndroidAppDataStream (Maybe Text)
gaavaadsDisplayName
  = lens _gaavaadsDisplayName
      (\ s a -> s{_gaavaadsDisplayName = a})

-- | Output only. Time when this stream was originally created.
gaavaadsCreateTime :: Lens' GoogleAnalyticsAdminV1alphaAndroidAppDataStream (Maybe UTCTime)
gaavaadsCreateTime
  = lens _gaavaadsCreateTime
      (\ s a -> s{_gaavaadsCreateTime = a})
      . mapping _DateTime

-- | Output only. ID of the corresponding Android app in Firebase, if any.
-- This ID can change if the Android app is deleted and recreated.
gaavaadsFirebaseAppId :: Lens' GoogleAnalyticsAdminV1alphaAndroidAppDataStream (Maybe Text)
gaavaadsFirebaseAppId
  = lens _gaavaadsFirebaseAppId
      (\ s a -> s{_gaavaadsFirebaseAppId = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaAndroidAppDataStream
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaAndroidAppDataStream"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaAndroidAppDataStream' <$>
                   (o .:? "packageName") <*> (o .:? "updateTime") <*>
                     (o .:? "name")
                     <*> (o .:? "displayName")
                     <*> (o .:? "createTime")
                     <*> (o .:? "firebaseAppId"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaAndroidAppDataStream
         where
        toJSON
          GoogleAnalyticsAdminV1alphaAndroidAppDataStream'{..}
          = object
              (catMaybes
                 [("packageName" .=) <$> _gaavaadsPackageName,
                  ("updateTime" .=) <$> _gaavaadsUpdateTime,
                  ("name" .=) <$> _gaavaadsName,
                  ("displayName" .=) <$> _gaavaadsDisplayName,
                  ("createTime" .=) <$> _gaavaadsCreateTime,
                  ("firebaseAppId" .=) <$> _gaavaadsFirebaseAppId])

-- | Request message for ListAccounts RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaListAccountsResponse' smart constructor.
data GoogleAnalyticsAdminV1alphaListAccountsResponse =
  GoogleAnalyticsAdminV1alphaListAccountsResponse'
    { _gaavlarNextPageToken :: !(Maybe Text)
    , _gaavlarAccounts :: !(Maybe [GoogleAnalyticsAdminV1alphaAccount])
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaListAccountsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavlarNextPageToken'
--
-- * 'gaavlarAccounts'
googleAnalyticsAdminV1alphaListAccountsResponse
    :: GoogleAnalyticsAdminV1alphaListAccountsResponse
googleAnalyticsAdminV1alphaListAccountsResponse =
  GoogleAnalyticsAdminV1alphaListAccountsResponse'
    {_gaavlarNextPageToken = Nothing, _gaavlarAccounts = Nothing}


-- | A token, which can be sent as \`page_token\` to retrieve the next page.
-- If this field is omitted, there are no subsequent pages.
gaavlarNextPageToken :: Lens' GoogleAnalyticsAdminV1alphaListAccountsResponse (Maybe Text)
gaavlarNextPageToken
  = lens _gaavlarNextPageToken
      (\ s a -> s{_gaavlarNextPageToken = a})

-- | Results that were accessible to the caller.
gaavlarAccounts :: Lens' GoogleAnalyticsAdminV1alphaListAccountsResponse [GoogleAnalyticsAdminV1alphaAccount]
gaavlarAccounts
  = lens _gaavlarAccounts
      (\ s a -> s{_gaavlarAccounts = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaListAccountsResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaListAccountsResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaListAccountsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "accounts" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaListAccountsResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaListAccountsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _gaavlarNextPageToken,
                  ("accounts" .=) <$> _gaavlarAccounts])

-- | A virtual resource representing metadata for an GA4 property.
--
-- /See:/ 'googleAnalyticsAdminV1alphaPropertySummary' smart constructor.
data GoogleAnalyticsAdminV1alphaPropertySummary =
  GoogleAnalyticsAdminV1alphaPropertySummary'
    { _gaavpsProperty :: !(Maybe Text)
    , _gaavpsDisplayName :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaPropertySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavpsProperty'
--
-- * 'gaavpsDisplayName'
googleAnalyticsAdminV1alphaPropertySummary
    :: GoogleAnalyticsAdminV1alphaPropertySummary
googleAnalyticsAdminV1alphaPropertySummary =
  GoogleAnalyticsAdminV1alphaPropertySummary'
    {_gaavpsProperty = Nothing, _gaavpsDisplayName = Nothing}


-- | Resource name of property referred to by this property summary Format:
-- properties\/{property_id} Example: \"properties\/1000\"
gaavpsProperty :: Lens' GoogleAnalyticsAdminV1alphaPropertySummary (Maybe Text)
gaavpsProperty
  = lens _gaavpsProperty
      (\ s a -> s{_gaavpsProperty = a})

-- | Display name for the property referred to in this account summary.
gaavpsDisplayName :: Lens' GoogleAnalyticsAdminV1alphaPropertySummary (Maybe Text)
gaavpsDisplayName
  = lens _gaavpsDisplayName
      (\ s a -> s{_gaavpsDisplayName = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaPropertySummary
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaPropertySummary"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaPropertySummary' <$>
                   (o .:? "property") <*> (o .:? "displayName"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaPropertySummary
         where
        toJSON
          GoogleAnalyticsAdminV1alphaPropertySummary'{..}
          = object
              (catMaybes
                 [("property" .=) <$> _gaavpsProperty,
                  ("displayName" .=) <$> _gaavpsDisplayName])

-- | Response message for BatchGetUserLinks RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaBatchGetUserLinksResponse' smart constructor.
newtype GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse =
  GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse'
    { _gaavbgulrUserLinks :: Maybe [GoogleAnalyticsAdminV1alphaUserLink]
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavbgulrUserLinks'
googleAnalyticsAdminV1alphaBatchGetUserLinksResponse
    :: GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse
googleAnalyticsAdminV1alphaBatchGetUserLinksResponse =
  GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse'
    {_gaavbgulrUserLinks = Nothing}


-- | The requested user links.
gaavbgulrUserLinks :: Lens' GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse [GoogleAnalyticsAdminV1alphaUserLink]
gaavbgulrUserLinks
  = lens _gaavbgulrUserLinks
      (\ s a -> s{_gaavbgulrUserLinks = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse'
                   <$> (o .:? "userLinks" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaBatchGetUserLinksResponse'{..}
          = object
              (catMaybes
                 [("userLinks" .=) <$> _gaavbgulrUserLinks])

-- | A generic empty message that you can re-use to avoid defining duplicated
-- empty messages in your APIs. A typical example is to use it as the
-- request or the response type of an API method. For instance: service Foo
-- { rpc Bar(google.protobuf.Empty) returns (google.protobuf.Empty); } The
-- JSON representation for \`Empty\` is empty JSON object \`{}\`.
--
-- /See:/ 'googleProtobufEmpty' smart constructor.
data GoogleProtobufEmpty =
  GoogleProtobufEmpty'
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleProtobufEmpty' with the minimum fields required to make a request.
--
googleProtobufEmpty
    :: GoogleProtobufEmpty
googleProtobufEmpty = GoogleProtobufEmpty'


instance FromJSON GoogleProtobufEmpty where
        parseJSON
          = withObject "GoogleProtobufEmpty"
              (\ o -> pure GoogleProtobufEmpty')

instance ToJSON GoogleProtobufEmpty where
        toJSON = const emptyObject

-- | A link between an GA4 property and a Firebase project.
--
-- /See:/ 'googleAnalyticsAdminV1alphaFirebaseLink' smart constructor.
data GoogleAnalyticsAdminV1alphaFirebaseLink =
  GoogleAnalyticsAdminV1alphaFirebaseLink'
    { _gaavflProject :: !(Maybe Text)
    , _gaavflMaximumUserAccess :: !(Maybe GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess)
    , _gaavflName :: !(Maybe Text)
    , _gaavflCreateTime :: !(Maybe DateTime')
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaFirebaseLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavflProject'
--
-- * 'gaavflMaximumUserAccess'
--
-- * 'gaavflName'
--
-- * 'gaavflCreateTime'
googleAnalyticsAdminV1alphaFirebaseLink
    :: GoogleAnalyticsAdminV1alphaFirebaseLink
googleAnalyticsAdminV1alphaFirebaseLink =
  GoogleAnalyticsAdminV1alphaFirebaseLink'
    { _gaavflProject = Nothing
    , _gaavflMaximumUserAccess = Nothing
    , _gaavflName = Nothing
    , _gaavflCreateTime = Nothing
    }


-- | Immutable. Firebase project resource name. When creating a FirebaseLink,
-- you may provide this resource name using either a project number or
-- project ID. Once this resource has been created, returned FirebaseLinks
-- will always have a project_name that contains a project number. Format:
-- \'projects\/{project number}\' Example: \'projects\/1234\'
gaavflProject :: Lens' GoogleAnalyticsAdminV1alphaFirebaseLink (Maybe Text)
gaavflProject
  = lens _gaavflProject
      (\ s a -> s{_gaavflProject = a})

-- | Maximum user access to the GA4 property allowed to admins of the linked
-- Firebase project.
gaavflMaximumUserAccess :: Lens' GoogleAnalyticsAdminV1alphaFirebaseLink (Maybe GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess)
gaavflMaximumUserAccess
  = lens _gaavflMaximumUserAccess
      (\ s a -> s{_gaavflMaximumUserAccess = a})

-- | Output only. Example format: properties\/1234\/firebaseLinks\/5678
gaavflName :: Lens' GoogleAnalyticsAdminV1alphaFirebaseLink (Maybe Text)
gaavflName
  = lens _gaavflName (\ s a -> s{_gaavflName = a})

-- | Output only. Time when this FirebaseLink was originally created.
gaavflCreateTime :: Lens' GoogleAnalyticsAdminV1alphaFirebaseLink (Maybe UTCTime)
gaavflCreateTime
  = lens _gaavflCreateTime
      (\ s a -> s{_gaavflCreateTime = a})
      . mapping _DateTime

instance FromJSON
           GoogleAnalyticsAdminV1alphaFirebaseLink
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaFirebaseLink"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaFirebaseLink' <$>
                   (o .:? "project") <*> (o .:? "maximumUserAccess") <*>
                     (o .:? "name")
                     <*> (o .:? "createTime"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaFirebaseLink
         where
        toJSON GoogleAnalyticsAdminV1alphaFirebaseLink'{..}
          = object
              (catMaybes
                 [("project" .=) <$> _gaavflProject,
                  ("maximumUserAccess" .=) <$>
                    _gaavflMaximumUserAccess,
                  ("name" .=) <$> _gaavflName,
                  ("createTime" .=) <$> _gaavflCreateTime])

-- | Request message for ProvisionAccountTicket RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaProvisionAccountTicketRequest' smart constructor.
data GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest =
  GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest'
    { _gaavpatrRedirectURI :: !(Maybe Text)
    , _gaavpatrAccount :: !(Maybe GoogleAnalyticsAdminV1alphaAccount)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavpatrRedirectURI'
--
-- * 'gaavpatrAccount'
googleAnalyticsAdminV1alphaProvisionAccountTicketRequest
    :: GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest
googleAnalyticsAdminV1alphaProvisionAccountTicketRequest =
  GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest'
    {_gaavpatrRedirectURI = Nothing, _gaavpatrAccount = Nothing}


-- | Redirect URI where the user will be sent after accepting Terms of
-- Service. Must be configured in Developers Console as a Redirect URI
gaavpatrRedirectURI :: Lens' GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest (Maybe Text)
gaavpatrRedirectURI
  = lens _gaavpatrRedirectURI
      (\ s a -> s{_gaavpatrRedirectURI = a})

-- | The account to create.
gaavpatrAccount :: Lens' GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest (Maybe GoogleAnalyticsAdminV1alphaAccount)
gaavpatrAccount
  = lens _gaavpatrAccount
      (\ s a -> s{_gaavpatrAccount = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest'
                   <$> (o .:? "redirectUri") <*> (o .:? "account"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest
         where
        toJSON
          GoogleAnalyticsAdminV1alphaProvisionAccountTicketRequest'{..}
          = object
              (catMaybes
                 [("redirectUri" .=) <$> _gaavpatrRedirectURI,
                  ("account" .=) <$> _gaavpatrAccount])

-- | A virtual resource representing an overview of an account and all its
-- child GA4 properties.
--
-- /See:/ 'googleAnalyticsAdminV1alphaAccountSummary' smart constructor.
data GoogleAnalyticsAdminV1alphaAccountSummary =
  GoogleAnalyticsAdminV1alphaAccountSummary'
    { _gaavasPropertySummaries :: !(Maybe [GoogleAnalyticsAdminV1alphaPropertySummary])
    , _gaavasAccount :: !(Maybe Text)
    , _gaavasName :: !(Maybe Text)
    , _gaavasDisplayName :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaAccountSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavasPropertySummaries'
--
-- * 'gaavasAccount'
--
-- * 'gaavasName'
--
-- * 'gaavasDisplayName'
googleAnalyticsAdminV1alphaAccountSummary
    :: GoogleAnalyticsAdminV1alphaAccountSummary
googleAnalyticsAdminV1alphaAccountSummary =
  GoogleAnalyticsAdminV1alphaAccountSummary'
    { _gaavasPropertySummaries = Nothing
    , _gaavasAccount = Nothing
    , _gaavasName = Nothing
    , _gaavasDisplayName = Nothing
    }


-- | List of summaries for child accounts of this account.
gaavasPropertySummaries :: Lens' GoogleAnalyticsAdminV1alphaAccountSummary [GoogleAnalyticsAdminV1alphaPropertySummary]
gaavasPropertySummaries
  = lens _gaavasPropertySummaries
      (\ s a -> s{_gaavasPropertySummaries = a})
      . _Default
      . _Coerce

-- | Resource name of account referred to by this account summary Format:
-- accounts\/{account_id} Example: \"accounts\/1000\"
gaavasAccount :: Lens' GoogleAnalyticsAdminV1alphaAccountSummary (Maybe Text)
gaavasAccount
  = lens _gaavasAccount
      (\ s a -> s{_gaavasAccount = a})

-- | Resource name for this account summary. Format:
-- accountSummaries\/{account_id} Example: \"accountSummaries\/1000\"
gaavasName :: Lens' GoogleAnalyticsAdminV1alphaAccountSummary (Maybe Text)
gaavasName
  = lens _gaavasName (\ s a -> s{_gaavasName = a})

-- | Display name for the account referred to in this account summary.
gaavasDisplayName :: Lens' GoogleAnalyticsAdminV1alphaAccountSummary (Maybe Text)
gaavasDisplayName
  = lens _gaavasDisplayName
      (\ s a -> s{_gaavasDisplayName = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaAccountSummary
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaAccountSummary"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaAccountSummary' <$>
                   (o .:? "propertySummaries" .!= mempty) <*>
                     (o .:? "account")
                     <*> (o .:? "name")
                     <*> (o .:? "displayName"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaAccountSummary
         where
        toJSON GoogleAnalyticsAdminV1alphaAccountSummary'{..}
          = object
              (catMaybes
                 [("propertySummaries" .=) <$>
                    _gaavasPropertySummaries,
                  ("account" .=) <$> _gaavasAccount,
                  ("name" .=) <$> _gaavasName,
                  ("displayName" .=) <$> _gaavasDisplayName])

-- | Response message for ListAccountSummaries RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaListAccountSummariesResponse' smart constructor.
data GoogleAnalyticsAdminV1alphaListAccountSummariesResponse =
  GoogleAnalyticsAdminV1alphaListAccountSummariesResponse'
    { _gaavlasrNextPageToken :: !(Maybe Text)
    , _gaavlasrAccountSummaries :: !(Maybe [GoogleAnalyticsAdminV1alphaAccountSummary])
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaListAccountSummariesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavlasrNextPageToken'
--
-- * 'gaavlasrAccountSummaries'
googleAnalyticsAdminV1alphaListAccountSummariesResponse
    :: GoogleAnalyticsAdminV1alphaListAccountSummariesResponse
googleAnalyticsAdminV1alphaListAccountSummariesResponse =
  GoogleAnalyticsAdminV1alphaListAccountSummariesResponse'
    {_gaavlasrNextPageToken = Nothing, _gaavlasrAccountSummaries = Nothing}


-- | A token, which can be sent as \`page_token\` to retrieve the next page.
-- If this field is omitted, there are no subsequent pages.
gaavlasrNextPageToken :: Lens' GoogleAnalyticsAdminV1alphaListAccountSummariesResponse (Maybe Text)
gaavlasrNextPageToken
  = lens _gaavlasrNextPageToken
      (\ s a -> s{_gaavlasrNextPageToken = a})

-- | Account summaries of all accounts the caller has access to.
gaavlasrAccountSummaries :: Lens' GoogleAnalyticsAdminV1alphaListAccountSummariesResponse [GoogleAnalyticsAdminV1alphaAccountSummary]
gaavlasrAccountSummaries
  = lens _gaavlasrAccountSummaries
      (\ s a -> s{_gaavlasrAccountSummaries = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaListAccountSummariesResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaListAccountSummariesResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaListAccountSummariesResponse'
                   <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "accountSummaries" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaListAccountSummariesResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaListAccountSummariesResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _gaavlasrNextPageToken,
                  ("accountSummaries" .=) <$>
                    _gaavlasrAccountSummaries])

-- | Response message for ProvisionAccountTicket RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaProvisionAccountTicketResponse' smart constructor.
newtype GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse =
  GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse'
    { _gaavpatrAccountTicketId :: Maybe Text
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavpatrAccountTicketId'
googleAnalyticsAdminV1alphaProvisionAccountTicketResponse
    :: GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse
googleAnalyticsAdminV1alphaProvisionAccountTicketResponse =
  GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse'
    {_gaavpatrAccountTicketId = Nothing}


-- | The param to be passed in the ToS link.
gaavpatrAccountTicketId :: Lens' GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse (Maybe Text)
gaavpatrAccountTicketId
  = lens _gaavpatrAccountTicketId
      (\ s a -> s{_gaavpatrAccountTicketId = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse'
                   <$> (o .:? "accountTicketId"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaProvisionAccountTicketResponse'{..}
          = object
              (catMaybes
                 [("accountTicketId" .=) <$>
                    _gaavpatrAccountTicketId])

-- | A link between an GA4 property and a Google Ads account.
--
-- /See:/ 'googleAnalyticsAdminV1alphaGoogleAdsLink' smart constructor.
data GoogleAnalyticsAdminV1alphaGoogleAdsLink =
  GoogleAnalyticsAdminV1alphaGoogleAdsLink'
    { _gaavgalParent :: !(Maybe Text)
    , _gaavgalCustomerId :: !(Maybe Text)
    , _gaavgalUpdateTime :: !(Maybe DateTime')
    , _gaavgalName :: !(Maybe Text)
    , _gaavgalCanManageClients :: !(Maybe Bool)
    , _gaavgalEmailAddress :: !(Maybe Text)
    , _gaavgalAdsPersonalizationEnabled :: !(Maybe Bool)
    , _gaavgalCreateTime :: !(Maybe DateTime')
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaGoogleAdsLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavgalParent'
--
-- * 'gaavgalCustomerId'
--
-- * 'gaavgalUpdateTime'
--
-- * 'gaavgalName'
--
-- * 'gaavgalCanManageClients'
--
-- * 'gaavgalEmailAddress'
--
-- * 'gaavgalAdsPersonalizationEnabled'
--
-- * 'gaavgalCreateTime'
googleAnalyticsAdminV1alphaGoogleAdsLink
    :: GoogleAnalyticsAdminV1alphaGoogleAdsLink
googleAnalyticsAdminV1alphaGoogleAdsLink =
  GoogleAnalyticsAdminV1alphaGoogleAdsLink'
    { _gaavgalParent = Nothing
    , _gaavgalCustomerId = Nothing
    , _gaavgalUpdateTime = Nothing
    , _gaavgalName = Nothing
    , _gaavgalCanManageClients = Nothing
    , _gaavgalEmailAddress = Nothing
    , _gaavgalAdsPersonalizationEnabled = Nothing
    , _gaavgalCreateTime = Nothing
    }


-- | Immutable. Format: properties\/{propertyId}
gaavgalParent :: Lens' GoogleAnalyticsAdminV1alphaGoogleAdsLink (Maybe Text)
gaavgalParent
  = lens _gaavgalParent
      (\ s a -> s{_gaavgalParent = a})

-- | Immutable. Google Ads customer ID.
gaavgalCustomerId :: Lens' GoogleAnalyticsAdminV1alphaGoogleAdsLink (Maybe Text)
gaavgalCustomerId
  = lens _gaavgalCustomerId
      (\ s a -> s{_gaavgalCustomerId = a})

-- | Output only. Time when this link was last updated.
gaavgalUpdateTime :: Lens' GoogleAnalyticsAdminV1alphaGoogleAdsLink (Maybe UTCTime)
gaavgalUpdateTime
  = lens _gaavgalUpdateTime
      (\ s a -> s{_gaavgalUpdateTime = a})
      . mapping _DateTime

-- | Output only. Format:
-- properties\/{propertyId}\/googleAdsLinks\/{googleAdsLinkId} Note:
-- googleAdsLinkId is not the Google Ads customer ID.
gaavgalName :: Lens' GoogleAnalyticsAdminV1alphaGoogleAdsLink (Maybe Text)
gaavgalName
  = lens _gaavgalName (\ s a -> s{_gaavgalName = a})

-- | Output only. If true, this link is for a Google Ads manager account.
gaavgalCanManageClients :: Lens' GoogleAnalyticsAdminV1alphaGoogleAdsLink (Maybe Bool)
gaavgalCanManageClients
  = lens _gaavgalCanManageClients
      (\ s a -> s{_gaavgalCanManageClients = a})

-- | Output only. Email address of the user that created the link. An empty
-- string will be returned if the email address can\'t be retrieved.
gaavgalEmailAddress :: Lens' GoogleAnalyticsAdminV1alphaGoogleAdsLink (Maybe Text)
gaavgalEmailAddress
  = lens _gaavgalEmailAddress
      (\ s a -> s{_gaavgalEmailAddress = a})

-- | Enable personalized advertising features with this integration.
-- Automatically publish my Google Analytics audience lists and Google
-- Analytics remarketing events\/parameters to the linked Google Ads
-- account. If this field is not set on create\/update it will be defaulted
-- to true.
gaavgalAdsPersonalizationEnabled :: Lens' GoogleAnalyticsAdminV1alphaGoogleAdsLink (Maybe Bool)
gaavgalAdsPersonalizationEnabled
  = lens _gaavgalAdsPersonalizationEnabled
      (\ s a -> s{_gaavgalAdsPersonalizationEnabled = a})

-- | Output only. Time when this link was originally created.
gaavgalCreateTime :: Lens' GoogleAnalyticsAdminV1alphaGoogleAdsLink (Maybe UTCTime)
gaavgalCreateTime
  = lens _gaavgalCreateTime
      (\ s a -> s{_gaavgalCreateTime = a})
      . mapping _DateTime

instance FromJSON
           GoogleAnalyticsAdminV1alphaGoogleAdsLink
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaGoogleAdsLink"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaGoogleAdsLink' <$>
                   (o .:? "parent") <*> (o .:? "customerId") <*>
                     (o .:? "updateTime")
                     <*> (o .:? "name")
                     <*> (o .:? "canManageClients")
                     <*> (o .:? "emailAddress")
                     <*> (o .:? "adsPersonalizationEnabled")
                     <*> (o .:? "createTime"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaGoogleAdsLink
         where
        toJSON GoogleAnalyticsAdminV1alphaGoogleAdsLink'{..}
          = object
              (catMaybes
                 [("parent" .=) <$> _gaavgalParent,
                  ("customerId" .=) <$> _gaavgalCustomerId,
                  ("updateTime" .=) <$> _gaavgalUpdateTime,
                  ("name" .=) <$> _gaavgalName,
                  ("canManageClients" .=) <$> _gaavgalCanManageClients,
                  ("emailAddress" .=) <$> _gaavgalEmailAddress,
                  ("adsPersonalizationEnabled" .=) <$>
                    _gaavgalAdsPersonalizationEnabled,
                  ("createTime" .=) <$> _gaavgalCreateTime])

-- | Read-only resource used to summarize a principal\'s effective roles.
--
-- /See:/ 'googleAnalyticsAdminV1alphaAuditUserLink' smart constructor.
data GoogleAnalyticsAdminV1alphaAuditUserLink =
  GoogleAnalyticsAdminV1alphaAuditUserLink'
    { _gaavaulDirectRoles :: !(Maybe [Text])
    , _gaavaulEffectiveRoles :: !(Maybe [Text])
    , _gaavaulName :: !(Maybe Text)
    , _gaavaulEmailAddress :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaAuditUserLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavaulDirectRoles'
--
-- * 'gaavaulEffectiveRoles'
--
-- * 'gaavaulName'
--
-- * 'gaavaulEmailAddress'
googleAnalyticsAdminV1alphaAuditUserLink
    :: GoogleAnalyticsAdminV1alphaAuditUserLink
googleAnalyticsAdminV1alphaAuditUserLink =
  GoogleAnalyticsAdminV1alphaAuditUserLink'
    { _gaavaulDirectRoles = Nothing
    , _gaavaulEffectiveRoles = Nothing
    , _gaavaulName = Nothing
    , _gaavaulEmailAddress = Nothing
    }


-- | Roles directly assigned to this user for this entity. Format:
-- predefinedRoles\/read Excludes roles that are inherited from an account
-- (if this is for a property), group, or organization admin role.
gaavaulDirectRoles :: Lens' GoogleAnalyticsAdminV1alphaAuditUserLink [Text]
gaavaulDirectRoles
  = lens _gaavaulDirectRoles
      (\ s a -> s{_gaavaulDirectRoles = a})
      . _Default
      . _Coerce

-- | Union of all permissions a user has at this account or property
-- (includes direct permissions, group-inherited permissions, etc.).
-- Format: predefinedRoles\/read
gaavaulEffectiveRoles :: Lens' GoogleAnalyticsAdminV1alphaAuditUserLink [Text]
gaavaulEffectiveRoles
  = lens _gaavaulEffectiveRoles
      (\ s a -> s{_gaavaulEffectiveRoles = a})
      . _Default
      . _Coerce

-- | Example format: properties\/1234\/userLinks\/5678
gaavaulName :: Lens' GoogleAnalyticsAdminV1alphaAuditUserLink (Maybe Text)
gaavaulName
  = lens _gaavaulName (\ s a -> s{_gaavaulName = a})

-- | Email address of the linked user
gaavaulEmailAddress :: Lens' GoogleAnalyticsAdminV1alphaAuditUserLink (Maybe Text)
gaavaulEmailAddress
  = lens _gaavaulEmailAddress
      (\ s a -> s{_gaavaulEmailAddress = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaAuditUserLink
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaAuditUserLink"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaAuditUserLink' <$>
                   (o .:? "directRoles" .!= mempty) <*>
                     (o .:? "effectiveRoles" .!= mempty)
                     <*> (o .:? "name")
                     <*> (o .:? "emailAddress"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaAuditUserLink
         where
        toJSON GoogleAnalyticsAdminV1alphaAuditUserLink'{..}
          = object
              (catMaybes
                 [("directRoles" .=) <$> _gaavaulDirectRoles,
                  ("effectiveRoles" .=) <$> _gaavaulEffectiveRoles,
                  ("name" .=) <$> _gaavaulName,
                  ("emailAddress" .=) <$> _gaavaulEmailAddress])

-- | Response message for AuditUserLinks RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaAuditUserLinksResponse' smart constructor.
data GoogleAnalyticsAdminV1alphaAuditUserLinksResponse =
  GoogleAnalyticsAdminV1alphaAuditUserLinksResponse'
    { _gaavaulrNextPageToken :: !(Maybe Text)
    , _gaavaulrUserLinks :: !(Maybe [GoogleAnalyticsAdminV1alphaAuditUserLink])
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaAuditUserLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavaulrNextPageToken'
--
-- * 'gaavaulrUserLinks'
googleAnalyticsAdminV1alphaAuditUserLinksResponse
    :: GoogleAnalyticsAdminV1alphaAuditUserLinksResponse
googleAnalyticsAdminV1alphaAuditUserLinksResponse =
  GoogleAnalyticsAdminV1alphaAuditUserLinksResponse'
    {_gaavaulrNextPageToken = Nothing, _gaavaulrUserLinks = Nothing}


-- | A token, which can be sent as \`page_token\` to retrieve the next page.
-- If this field is omitted, there are no subsequent pages.
gaavaulrNextPageToken :: Lens' GoogleAnalyticsAdminV1alphaAuditUserLinksResponse (Maybe Text)
gaavaulrNextPageToken
  = lens _gaavaulrNextPageToken
      (\ s a -> s{_gaavaulrNextPageToken = a})

-- | List of AuditUserLinks. These will be ordered stably, but in an
-- arbitrary order.
gaavaulrUserLinks :: Lens' GoogleAnalyticsAdminV1alphaAuditUserLinksResponse [GoogleAnalyticsAdminV1alphaAuditUserLink]
gaavaulrUserLinks
  = lens _gaavaulrUserLinks
      (\ s a -> s{_gaavaulrUserLinks = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaAuditUserLinksResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaAuditUserLinksResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaAuditUserLinksResponse'
                   <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "userLinks" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaAuditUserLinksResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaAuditUserLinksResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _gaavaulrNextPageToken,
                  ("userLinks" .=) <$> _gaavaulrUserLinks])

-- | Response message for BatchCreateUserLinks RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaBatchCreateUserLinksResponse' smart constructor.
newtype GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse =
  GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse'
    { _gaavbculrUserLinks :: Maybe [GoogleAnalyticsAdminV1alphaUserLink]
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavbculrUserLinks'
googleAnalyticsAdminV1alphaBatchCreateUserLinksResponse
    :: GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse
googleAnalyticsAdminV1alphaBatchCreateUserLinksResponse =
  GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse'
    {_gaavbculrUserLinks = Nothing}


-- | The user links created.
gaavbculrUserLinks :: Lens' GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse [GoogleAnalyticsAdminV1alphaUserLink]
gaavbculrUserLinks
  = lens _gaavbculrUserLinks
      (\ s a -> s{_gaavbculrUserLinks = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse'
                   <$> (o .:? "userLinks" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaBatchCreateUserLinksResponse'{..}
          = object
              (catMaybes
                 [("userLinks" .=) <$> _gaavbculrUserLinks])

-- | A resource message representing a Google Analytics account.
--
-- /See:/ 'googleAnalyticsAdminV1alphaAccount' smart constructor.
data GoogleAnalyticsAdminV1alphaAccount =
  GoogleAnalyticsAdminV1alphaAccount'
    { _gaavaUpdateTime :: !(Maybe DateTime')
    , _gaavaName :: !(Maybe Text)
    , _gaavaDisplayName :: !(Maybe Text)
    , _gaavaCountryCode :: !(Maybe Text)
    , _gaavaDeleted :: !(Maybe Bool)
    , _gaavaCreateTime :: !(Maybe DateTime')
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavaUpdateTime'
--
-- * 'gaavaName'
--
-- * 'gaavaDisplayName'
--
-- * 'gaavaCountryCode'
--
-- * 'gaavaDeleted'
--
-- * 'gaavaCreateTime'
googleAnalyticsAdminV1alphaAccount
    :: GoogleAnalyticsAdminV1alphaAccount
googleAnalyticsAdminV1alphaAccount =
  GoogleAnalyticsAdminV1alphaAccount'
    { _gaavaUpdateTime = Nothing
    , _gaavaName = Nothing
    , _gaavaDisplayName = Nothing
    , _gaavaCountryCode = Nothing
    , _gaavaDeleted = Nothing
    , _gaavaCreateTime = Nothing
    }


-- | Output only. Time when account payload fields were last updated.
gaavaUpdateTime :: Lens' GoogleAnalyticsAdminV1alphaAccount (Maybe UTCTime)
gaavaUpdateTime
  = lens _gaavaUpdateTime
      (\ s a -> s{_gaavaUpdateTime = a})
      . mapping _DateTime

-- | Output only. Resource name of this account. Format: accounts\/{account}
-- Example: \"accounts\/100\"
gaavaName :: Lens' GoogleAnalyticsAdminV1alphaAccount (Maybe Text)
gaavaName
  = lens _gaavaName (\ s a -> s{_gaavaName = a})

-- | Required. Human-readable display name for this account.
gaavaDisplayName :: Lens' GoogleAnalyticsAdminV1alphaAccount (Maybe Text)
gaavaDisplayName
  = lens _gaavaDisplayName
      (\ s a -> s{_gaavaDisplayName = a})

-- | Country of business. Must be a non-deprecated code for a UN M.49 region.
-- https:\/\/unicode.org\/cldr\/charts\/latest\/supplemental\/territory_containment_un_m_49.html
gaavaCountryCode :: Lens' GoogleAnalyticsAdminV1alphaAccount (Maybe Text)
gaavaCountryCode
  = lens _gaavaCountryCode
      (\ s a -> s{_gaavaCountryCode = a})

-- | Output only. Indicates whether this Account is soft-deleted or not.
-- Deleted accounts are excluded from List results unless specifically
-- requested.
gaavaDeleted :: Lens' GoogleAnalyticsAdminV1alphaAccount (Maybe Bool)
gaavaDeleted
  = lens _gaavaDeleted (\ s a -> s{_gaavaDeleted = a})

-- | Output only. Time when this account was originally created.
gaavaCreateTime :: Lens' GoogleAnalyticsAdminV1alphaAccount (Maybe UTCTime)
gaavaCreateTime
  = lens _gaavaCreateTime
      (\ s a -> s{_gaavaCreateTime = a})
      . mapping _DateTime

instance FromJSON GoogleAnalyticsAdminV1alphaAccount
         where
        parseJSON
          = withObject "GoogleAnalyticsAdminV1alphaAccount"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaAccount' <$>
                   (o .:? "updateTime") <*> (o .:? "name") <*>
                     (o .:? "displayName")
                     <*> (o .:? "countryCode")
                     <*> (o .:? "deleted")
                     <*> (o .:? "createTime"))

instance ToJSON GoogleAnalyticsAdminV1alphaAccount
         where
        toJSON GoogleAnalyticsAdminV1alphaAccount'{..}
          = object
              (catMaybes
                 [("updateTime" .=) <$> _gaavaUpdateTime,
                  ("name" .=) <$> _gaavaName,
                  ("displayName" .=) <$> _gaavaDisplayName,
                  ("countryCode" .=) <$> _gaavaCountryCode,
                  ("deleted" .=) <$> _gaavaDeleted,
                  ("createTime" .=) <$> _gaavaCreateTime])

-- | Request message for UpdateUserLink RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaUpdateUserLinkRequest' smart constructor.
newtype GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest =
  GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest'
    { _gaavuulrUserLink :: Maybe GoogleAnalyticsAdminV1alphaUserLink
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavuulrUserLink'
googleAnalyticsAdminV1alphaUpdateUserLinkRequest
    :: GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest
googleAnalyticsAdminV1alphaUpdateUserLinkRequest =
  GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest'
    {_gaavuulrUserLink = Nothing}


-- | Required. The user link to update.
gaavuulrUserLink :: Lens' GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest (Maybe GoogleAnalyticsAdminV1alphaUserLink)
gaavuulrUserLink
  = lens _gaavuulrUserLink
      (\ s a -> s{_gaavuulrUserLink = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest' <$>
                   (o .:? "userLink"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest
         where
        toJSON
          GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest'{..}
          = object
              (catMaybes [("userLink" .=) <$> _gaavuulrUserLink])

-- | Request message for DeleteUserLink RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaDeleteUserLinkRequest' smart constructor.
newtype GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest =
  GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest'
    { _gaavdulrName :: Maybe Text
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavdulrName'
googleAnalyticsAdminV1alphaDeleteUserLinkRequest
    :: GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest
googleAnalyticsAdminV1alphaDeleteUserLinkRequest =
  GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest' {_gaavdulrName = Nothing}


-- | Required. Example format: accounts\/1234\/userLinks\/5678
gaavdulrName :: Lens' GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest (Maybe Text)
gaavdulrName
  = lens _gaavdulrName (\ s a -> s{_gaavdulrName = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest' <$>
                   (o .:? "name"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest
         where
        toJSON
          GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest'{..}
          = object (catMaybes [("name" .=) <$> _gaavdulrName])

-- | Singleton resource under a WebDataStream, configuring measurement of
-- additional site interactions and content.
--
-- /See:/ 'googleAnalyticsAdminV1alphaEnhancedMeasurementSettings' smart constructor.
data GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings =
  GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings'
    { _gaavemsOutboundClicksEnabled :: !(Maybe Bool)
    , _gaavemsSearchQueryParameter :: !(Maybe Text)
    , _gaavemsURLQueryParameter :: !(Maybe Text)
    , _gaavemsSiteSearchEnabled :: !(Maybe Bool)
    , _gaavemsVideoEngagementEnabled :: !(Maybe Bool)
    , _gaavemsStreamEnabled :: !(Maybe Bool)
    , _gaavemsExcludedDomains :: !(Maybe Text)
    , _gaavemsArticlesAndBlogsEnabled :: !(Maybe Bool)
    , _gaavemsFileDownloadsEnabled :: !(Maybe Bool)
    , _gaavemsName :: !(Maybe Text)
    , _gaavemsFormInteractionsEnabled :: !(Maybe Bool)
    , _gaavemsContentViewsEnabled :: !(Maybe Bool)
    , _gaavemsDataTaggedElementClicksEnabled :: !(Maybe Bool)
    , _gaavemsPageViewsEnabled :: !(Maybe Bool)
    , _gaavemsPageChangesEnabled :: !(Maybe Bool)
    , _gaavemsPageLoadsEnabled :: !(Maybe Bool)
    , _gaavemsScrollsEnabled :: !(Maybe Bool)
    , _gaavemsProductsAndEcommerceEnabled :: !(Maybe Bool)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavemsOutboundClicksEnabled'
--
-- * 'gaavemsSearchQueryParameter'
--
-- * 'gaavemsURLQueryParameter'
--
-- * 'gaavemsSiteSearchEnabled'
--
-- * 'gaavemsVideoEngagementEnabled'
--
-- * 'gaavemsStreamEnabled'
--
-- * 'gaavemsExcludedDomains'
--
-- * 'gaavemsArticlesAndBlogsEnabled'
--
-- * 'gaavemsFileDownloadsEnabled'
--
-- * 'gaavemsName'
--
-- * 'gaavemsFormInteractionsEnabled'
--
-- * 'gaavemsContentViewsEnabled'
--
-- * 'gaavemsDataTaggedElementClicksEnabled'
--
-- * 'gaavemsPageViewsEnabled'
--
-- * 'gaavemsPageChangesEnabled'
--
-- * 'gaavemsPageLoadsEnabled'
--
-- * 'gaavemsScrollsEnabled'
--
-- * 'gaavemsProductsAndEcommerceEnabled'
googleAnalyticsAdminV1alphaEnhancedMeasurementSettings
    :: GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings
googleAnalyticsAdminV1alphaEnhancedMeasurementSettings =
  GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings'
    { _gaavemsOutboundClicksEnabled = Nothing
    , _gaavemsSearchQueryParameter = Nothing
    , _gaavemsURLQueryParameter = Nothing
    , _gaavemsSiteSearchEnabled = Nothing
    , _gaavemsVideoEngagementEnabled = Nothing
    , _gaavemsStreamEnabled = Nothing
    , _gaavemsExcludedDomains = Nothing
    , _gaavemsArticlesAndBlogsEnabled = Nothing
    , _gaavemsFileDownloadsEnabled = Nothing
    , _gaavemsName = Nothing
    , _gaavemsFormInteractionsEnabled = Nothing
    , _gaavemsContentViewsEnabled = Nothing
    , _gaavemsDataTaggedElementClicksEnabled = Nothing
    , _gaavemsPageViewsEnabled = Nothing
    , _gaavemsPageChangesEnabled = Nothing
    , _gaavemsPageLoadsEnabled = Nothing
    , _gaavemsScrollsEnabled = Nothing
    , _gaavemsProductsAndEcommerceEnabled = Nothing
    }


-- | If enabled, capture an outbound click event each time a visitor clicks a
-- link that leads them away from your domain.
gaavemsOutboundClicksEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsOutboundClicksEnabled
  = lens _gaavemsOutboundClicksEnabled
      (\ s a -> s{_gaavemsOutboundClicksEnabled = a})

-- | Required. URL query parameters to interpret as site search parameters.
-- Max length is 1024 characters. Must not be empty.
gaavemsSearchQueryParameter :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Text)
gaavemsSearchQueryParameter
  = lens _gaavemsSearchQueryParameter
      (\ s a -> s{_gaavemsSearchQueryParameter = a})

-- | Additional URL query parameters. Max length is 1024 characters.
gaavemsURLQueryParameter :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Text)
gaavemsURLQueryParameter
  = lens _gaavemsURLQueryParameter
      (\ s a -> s{_gaavemsURLQueryParameter = a})

-- | If enabled, capture a view search results event each time a visitor
-- performs a search on your site (based on a query parameter).
gaavemsSiteSearchEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsSiteSearchEnabled
  = lens _gaavemsSiteSearchEnabled
      (\ s a -> s{_gaavemsSiteSearchEnabled = a})

-- | If enabled, capture video play, progress, and complete events as
-- visitors view embedded videos on your site.
gaavemsVideoEngagementEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsVideoEngagementEnabled
  = lens _gaavemsVideoEngagementEnabled
      (\ s a -> s{_gaavemsVideoEngagementEnabled = a})

-- | Indicates whether Enhanced Measurement Settings will be used to
-- automatically measure interactions and content on this web stream.
-- Changing this value does not affect the settings themselves, but
-- determines whether they are respected.
gaavemsStreamEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsStreamEnabled
  = lens _gaavemsStreamEnabled
      (\ s a -> s{_gaavemsStreamEnabled = a})

-- | Domains to exclude from measurement. Max length is 1024 characters.
gaavemsExcludedDomains :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Text)
gaavemsExcludedDomains
  = lens _gaavemsExcludedDomains
      (\ s a -> s{_gaavemsExcludedDomains = a})

-- | Capture events when your visitors view content on your site that has
-- articles or blog posts.
gaavemsArticlesAndBlogsEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsArticlesAndBlogsEnabled
  = lens _gaavemsArticlesAndBlogsEnabled
      (\ s a -> s{_gaavemsArticlesAndBlogsEnabled = a})

-- | If enabled, capture a file download event each time a link is clicked
-- with a common document, compressed file, application, video, or audio
-- extension.
gaavemsFileDownloadsEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsFileDownloadsEnabled
  = lens _gaavemsFileDownloadsEnabled
      (\ s a -> s{_gaavemsFileDownloadsEnabled = a})

-- | Output only. Resource name of this Data Stream. Format:
-- properties\/{property_id}\/webDataStreams\/{stream_id}\/enhancedMeasurementSettings
-- Example:
-- \"properties\/1000\/webDataStreams\/2000\/enhancedMeasurementSettings\"
gaavemsName :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Text)
gaavemsName
  = lens _gaavemsName (\ s a -> s{_gaavemsName = a})

-- | If enabled, capture a view search results event each time a visitor
-- interacts with a form on your site.
gaavemsFormInteractionsEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsFormInteractionsEnabled
  = lens _gaavemsFormInteractionsEnabled
      (\ s a -> s{_gaavemsFormInteractionsEnabled = a})

-- | Capture events when your visitors view content on your site that has
-- structured data (eg, articles, blog posts, product details screens,
-- etc.).
gaavemsContentViewsEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsContentViewsEnabled
  = lens _gaavemsContentViewsEnabled
      (\ s a -> s{_gaavemsContentViewsEnabled = a})

-- | If enabled, capture a click event each time a visitor clicks a link or
-- element that has data attributes beginning with \"data-ga\".
gaavemsDataTaggedElementClicksEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsDataTaggedElementClicksEnabled
  = lens _gaavemsDataTaggedElementClicksEnabled
      (\ s a ->
         s{_gaavemsDataTaggedElementClicksEnabled = a})

-- | Output only. If enabled, capture a page view event each time a page
-- loads or the website changes the browser history state.
gaavemsPageViewsEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsPageViewsEnabled
  = lens _gaavemsPageViewsEnabled
      (\ s a -> s{_gaavemsPageViewsEnabled = a})

-- | If enabled, capture a page view event each time the website changes the
-- browser history state.
gaavemsPageChangesEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsPageChangesEnabled
  = lens _gaavemsPageChangesEnabled
      (\ s a -> s{_gaavemsPageChangesEnabled = a})

-- | If enabled, capture a page view event each time a page loads.
gaavemsPageLoadsEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsPageLoadsEnabled
  = lens _gaavemsPageLoadsEnabled
      (\ s a -> s{_gaavemsPageLoadsEnabled = a})

-- | If enabled, capture scroll events each time a visitor gets to the bottom
-- of a page.
gaavemsScrollsEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsScrollsEnabled
  = lens _gaavemsScrollsEnabled
      (\ s a -> s{_gaavemsScrollsEnabled = a})

-- | Capture events when your visitors view content on your site that has
-- product details screens, etc.
gaavemsProductsAndEcommerceEnabled :: Lens' GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings (Maybe Bool)
gaavemsProductsAndEcommerceEnabled
  = lens _gaavemsProductsAndEcommerceEnabled
      (\ s a -> s{_gaavemsProductsAndEcommerceEnabled = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings'
                   <$>
                   (o .:? "outboundClicksEnabled") <*>
                     (o .:? "searchQueryParameter")
                     <*> (o .:? "urlQueryParameter")
                     <*> (o .:? "siteSearchEnabled")
                     <*> (o .:? "videoEngagementEnabled")
                     <*> (o .:? "streamEnabled")
                     <*> (o .:? "excludedDomains")
                     <*> (o .:? "articlesAndBlogsEnabled")
                     <*> (o .:? "fileDownloadsEnabled")
                     <*> (o .:? "name")
                     <*> (o .:? "formInteractionsEnabled")
                     <*> (o .:? "contentViewsEnabled")
                     <*> (o .:? "dataTaggedElementClicksEnabled")
                     <*> (o .:? "pageViewsEnabled")
                     <*> (o .:? "pageChangesEnabled")
                     <*> (o .:? "pageLoadsEnabled")
                     <*> (o .:? "scrollsEnabled")
                     <*> (o .:? "productsAndEcommerceEnabled"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings
         where
        toJSON
          GoogleAnalyticsAdminV1alphaEnhancedMeasurementSettings'{..}
          = object
              (catMaybes
                 [("outboundClicksEnabled" .=) <$>
                    _gaavemsOutboundClicksEnabled,
                  ("searchQueryParameter" .=) <$>
                    _gaavemsSearchQueryParameter,
                  ("urlQueryParameter" .=) <$>
                    _gaavemsURLQueryParameter,
                  ("siteSearchEnabled" .=) <$>
                    _gaavemsSiteSearchEnabled,
                  ("videoEngagementEnabled" .=) <$>
                    _gaavemsVideoEngagementEnabled,
                  ("streamEnabled" .=) <$> _gaavemsStreamEnabled,
                  ("excludedDomains" .=) <$> _gaavemsExcludedDomains,
                  ("articlesAndBlogsEnabled" .=) <$>
                    _gaavemsArticlesAndBlogsEnabled,
                  ("fileDownloadsEnabled" .=) <$>
                    _gaavemsFileDownloadsEnabled,
                  ("name" .=) <$> _gaavemsName,
                  ("formInteractionsEnabled" .=) <$>
                    _gaavemsFormInteractionsEnabled,
                  ("contentViewsEnabled" .=) <$>
                    _gaavemsContentViewsEnabled,
                  ("dataTaggedElementClicksEnabled" .=) <$>
                    _gaavemsDataTaggedElementClicksEnabled,
                  ("pageViewsEnabled" .=) <$> _gaavemsPageViewsEnabled,
                  ("pageChangesEnabled" .=) <$>
                    _gaavemsPageChangesEnabled,
                  ("pageLoadsEnabled" .=) <$> _gaavemsPageLoadsEnabled,
                  ("scrollsEnabled" .=) <$> _gaavemsScrollsEnabled,
                  ("productsAndEcommerceEnabled" .=) <$>
                    _gaavemsProductsAndEcommerceEnabled])

-- | Request message for BatchDeleteUserLinks RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest' smart constructor.
newtype GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest =
  GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest'
    { _gaavbdulrRequests :: Maybe [GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest]
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavbdulrRequests'
googleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
    :: GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
googleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest =
  GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest'
    {_gaavbdulrRequests = Nothing}


-- | Required. The requests specifying the user links to update. A maximum of
-- 1000 user links can be updated in a batch.
gaavbdulrRequests :: Lens' GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest [GoogleAnalyticsAdminV1alphaDeleteUserLinkRequest]
gaavbdulrRequests
  = lens _gaavbdulrRequests
      (\ s a -> s{_gaavbdulrRequests = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest'
                   <$> (o .:? "requests" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest
         where
        toJSON
          GoogleAnalyticsAdminV1alphaBatchDeleteUserLinksRequest'{..}
          = object
              (catMaybes [("requests" .=) <$> _gaavbdulrRequests])

-- | Request message for BatchUpdateUserLinks RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest' smart constructor.
newtype GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest =
  GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest'
    { _gaavbuulrRequests :: Maybe [GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest]
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavbuulrRequests'
googleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
    :: GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
googleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest =
  GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest'
    {_gaavbuulrRequests = Nothing}


-- | Required. The requests specifying the user links to update. A maximum of
-- 1000 user links can be updated in a batch.
gaavbuulrRequests :: Lens' GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest [GoogleAnalyticsAdminV1alphaUpdateUserLinkRequest]
gaavbuulrRequests
  = lens _gaavbuulrRequests
      (\ s a -> s{_gaavbuulrRequests = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest'
                   <$> (o .:? "requests" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest
         where
        toJSON
          GoogleAnalyticsAdminV1alphaBatchUpdateUserLinksRequest'{..}
          = object
              (catMaybes [("requests" .=) <$> _gaavbuulrRequests])

-- | Request message for BatchCreateUserLinks RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaBatchCreateUserLinksRequest' smart constructor.
data GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest =
  GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest'
    { _gaavbculrNotifyNewUsers :: !(Maybe Bool)
    , _gaavbculrRequests :: !(Maybe [GoogleAnalyticsAdminV1alphaCreateUserLinkRequest])
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavbculrNotifyNewUsers'
--
-- * 'gaavbculrRequests'
googleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
    :: GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
googleAnalyticsAdminV1alphaBatchCreateUserLinksRequest =
  GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest'
    {_gaavbculrNotifyNewUsers = Nothing, _gaavbculrRequests = Nothing}


-- | Optional. If set, then email the new users notifying them that they\'ve
-- been granted permissions to the resource. Regardless of whether this is
-- set or not, notify_new_user field inside each individual request is
-- ignored.
gaavbculrNotifyNewUsers :: Lens' GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest (Maybe Bool)
gaavbculrNotifyNewUsers
  = lens _gaavbculrNotifyNewUsers
      (\ s a -> s{_gaavbculrNotifyNewUsers = a})

-- | Required. The requests specifying the user links to create. A maximum of
-- 1000 user links can be created in a batch.
gaavbculrRequests :: Lens' GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest [GoogleAnalyticsAdminV1alphaCreateUserLinkRequest]
gaavbculrRequests
  = lens _gaavbculrRequests
      (\ s a -> s{_gaavbculrRequests = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest'
                   <$>
                   (o .:? "notifyNewUsers") <*>
                     (o .:? "requests" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest
         where
        toJSON
          GoogleAnalyticsAdminV1alphaBatchCreateUserLinksRequest'{..}
          = object
              (catMaybes
                 [("notifyNewUsers" .=) <$> _gaavbculrNotifyNewUsers,
                  ("requests" .=) <$> _gaavbculrRequests])

-- | Response message for ListUserLinks RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaListUserLinksResponse' smart constructor.
data GoogleAnalyticsAdminV1alphaListUserLinksResponse =
  GoogleAnalyticsAdminV1alphaListUserLinksResponse'
    { _gaavlulrNextPageToken :: !(Maybe Text)
    , _gaavlulrUserLinks :: !(Maybe [GoogleAnalyticsAdminV1alphaUserLink])
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaListUserLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavlulrNextPageToken'
--
-- * 'gaavlulrUserLinks'
googleAnalyticsAdminV1alphaListUserLinksResponse
    :: GoogleAnalyticsAdminV1alphaListUserLinksResponse
googleAnalyticsAdminV1alphaListUserLinksResponse =
  GoogleAnalyticsAdminV1alphaListUserLinksResponse'
    {_gaavlulrNextPageToken = Nothing, _gaavlulrUserLinks = Nothing}


-- | A token, which can be sent as \`page_token\` to retrieve the next page.
-- If this field is omitted, there are no subsequent pages.
gaavlulrNextPageToken :: Lens' GoogleAnalyticsAdminV1alphaListUserLinksResponse (Maybe Text)
gaavlulrNextPageToken
  = lens _gaavlulrNextPageToken
      (\ s a -> s{_gaavlulrNextPageToken = a})

-- | List of UserLinks. These will be ordered stably, but in an arbitrary
-- order.
gaavlulrUserLinks :: Lens' GoogleAnalyticsAdminV1alphaListUserLinksResponse [GoogleAnalyticsAdminV1alphaUserLink]
gaavlulrUserLinks
  = lens _gaavlulrUserLinks
      (\ s a -> s{_gaavlulrUserLinks = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaListUserLinksResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaListUserLinksResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaListUserLinksResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "userLinks" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaListUserLinksResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaListUserLinksResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _gaavlulrNextPageToken,
                  ("userLinks" .=) <$> _gaavlulrUserLinks])

-- | A resource message representing a Google Analytics IOS app stream.
--
-- /See:/ 'googleAnalyticsAdminV1alphaIosAppDataStream' smart constructor.
data GoogleAnalyticsAdminV1alphaIosAppDataStream =
  GoogleAnalyticsAdminV1alphaIosAppDataStream'
    { _gaaviadsBundleId :: !(Maybe Text)
    , _gaaviadsUpdateTime :: !(Maybe DateTime')
    , _gaaviadsName :: !(Maybe Text)
    , _gaaviadsDisplayName :: !(Maybe Text)
    , _gaaviadsCreateTime :: !(Maybe DateTime')
    , _gaaviadsFirebaseAppId :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaIosAppDataStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaaviadsBundleId'
--
-- * 'gaaviadsUpdateTime'
--
-- * 'gaaviadsName'
--
-- * 'gaaviadsDisplayName'
--
-- * 'gaaviadsCreateTime'
--
-- * 'gaaviadsFirebaseAppId'
googleAnalyticsAdminV1alphaIosAppDataStream
    :: GoogleAnalyticsAdminV1alphaIosAppDataStream
googleAnalyticsAdminV1alphaIosAppDataStream =
  GoogleAnalyticsAdminV1alphaIosAppDataStream'
    { _gaaviadsBundleId = Nothing
    , _gaaviadsUpdateTime = Nothing
    , _gaaviadsName = Nothing
    , _gaaviadsDisplayName = Nothing
    , _gaaviadsCreateTime = Nothing
    , _gaaviadsFirebaseAppId = Nothing
    }


-- | Required. Immutable. The Apple App Store Bundle ID for the app Example:
-- \"com.example.myiosapp\"
gaaviadsBundleId :: Lens' GoogleAnalyticsAdminV1alphaIosAppDataStream (Maybe Text)
gaaviadsBundleId
  = lens _gaaviadsBundleId
      (\ s a -> s{_gaaviadsBundleId = a})

-- | Output only. Time when stream payload fields were last updated.
gaaviadsUpdateTime :: Lens' GoogleAnalyticsAdminV1alphaIosAppDataStream (Maybe UTCTime)
gaaviadsUpdateTime
  = lens _gaaviadsUpdateTime
      (\ s a -> s{_gaaviadsUpdateTime = a})
      . mapping _DateTime

-- | Output only. Resource name of this Data Stream. Format:
-- properties\/{property_id}\/iosAppDataStreams\/{stream_id} Example:
-- \"properties\/1000\/iosAppDataStreams\/2000\"
gaaviadsName :: Lens' GoogleAnalyticsAdminV1alphaIosAppDataStream (Maybe Text)
gaaviadsName
  = lens _gaaviadsName (\ s a -> s{_gaaviadsName = a})

-- | Human-readable display name for the Data Stream. The max allowed display
-- name length is 255 UTF-16 code units.
gaaviadsDisplayName :: Lens' GoogleAnalyticsAdminV1alphaIosAppDataStream (Maybe Text)
gaaviadsDisplayName
  = lens _gaaviadsDisplayName
      (\ s a -> s{_gaaviadsDisplayName = a})

-- | Output only. Time when this stream was originally created.
gaaviadsCreateTime :: Lens' GoogleAnalyticsAdminV1alphaIosAppDataStream (Maybe UTCTime)
gaaviadsCreateTime
  = lens _gaaviadsCreateTime
      (\ s a -> s{_gaaviadsCreateTime = a})
      . mapping _DateTime

-- | Output only. ID of the corresponding iOS app in Firebase, if any. This
-- ID can change if the iOS app is deleted and recreated.
gaaviadsFirebaseAppId :: Lens' GoogleAnalyticsAdminV1alphaIosAppDataStream (Maybe Text)
gaaviadsFirebaseAppId
  = lens _gaaviadsFirebaseAppId
      (\ s a -> s{_gaaviadsFirebaseAppId = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaIosAppDataStream
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaIosAppDataStream"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaIosAppDataStream' <$>
                   (o .:? "bundleId") <*> (o .:? "updateTime") <*>
                     (o .:? "name")
                     <*> (o .:? "displayName")
                     <*> (o .:? "createTime")
                     <*> (o .:? "firebaseAppId"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaIosAppDataStream
         where
        toJSON
          GoogleAnalyticsAdminV1alphaIosAppDataStream'{..}
          = object
              (catMaybes
                 [("bundleId" .=) <$> _gaaviadsBundleId,
                  ("updateTime" .=) <$> _gaaviadsUpdateTime,
                  ("name" .=) <$> _gaaviadsName,
                  ("displayName" .=) <$> _gaaviadsDisplayName,
                  ("createTime" .=) <$> _gaaviadsCreateTime,
                  ("firebaseAppId" .=) <$> _gaaviadsFirebaseAppId])

-- | Response message for ListProperties RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaListPropertiesResponse' smart constructor.
data GoogleAnalyticsAdminV1alphaListPropertiesResponse =
  GoogleAnalyticsAdminV1alphaListPropertiesResponse'
    { _gaavlprNextPageToken :: !(Maybe Text)
    , _gaavlprProperties :: !(Maybe [GoogleAnalyticsAdminV1alphaProperty])
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaListPropertiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavlprNextPageToken'
--
-- * 'gaavlprProperties'
googleAnalyticsAdminV1alphaListPropertiesResponse
    :: GoogleAnalyticsAdminV1alphaListPropertiesResponse
googleAnalyticsAdminV1alphaListPropertiesResponse =
  GoogleAnalyticsAdminV1alphaListPropertiesResponse'
    {_gaavlprNextPageToken = Nothing, _gaavlprProperties = Nothing}


-- | A token, which can be sent as \`page_token\` to retrieve the next page.
-- If this field is omitted, there are no subsequent pages.
gaavlprNextPageToken :: Lens' GoogleAnalyticsAdminV1alphaListPropertiesResponse (Maybe Text)
gaavlprNextPageToken
  = lens _gaavlprNextPageToken
      (\ s a -> s{_gaavlprNextPageToken = a})

-- | Results that matched the filter criteria and were accessible to the
-- caller.
gaavlprProperties :: Lens' GoogleAnalyticsAdminV1alphaListPropertiesResponse [GoogleAnalyticsAdminV1alphaProperty]
gaavlprProperties
  = lens _gaavlprProperties
      (\ s a -> s{_gaavlprProperties = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaListPropertiesResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaListPropertiesResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaListPropertiesResponse'
                   <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "properties" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaListPropertiesResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaListPropertiesResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _gaavlprNextPageToken,
                  ("properties" .=) <$> _gaavlprProperties])

-- | Response message for ListGoogleAdsLinks RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaListGoogleAdsLinksResponse' smart constructor.
data GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse =
  GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse'
    { _gaavlgalrGoogleAdsLinks :: !(Maybe [GoogleAnalyticsAdminV1alphaGoogleAdsLink])
    , _gaavlgalrNextPageToken :: !(Maybe Text)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavlgalrGoogleAdsLinks'
--
-- * 'gaavlgalrNextPageToken'
googleAnalyticsAdminV1alphaListGoogleAdsLinksResponse
    :: GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse
googleAnalyticsAdminV1alphaListGoogleAdsLinksResponse =
  GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse'
    {_gaavlgalrGoogleAdsLinks = Nothing, _gaavlgalrNextPageToken = Nothing}


-- | List of GoogleAdsLinks.
gaavlgalrGoogleAdsLinks :: Lens' GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse [GoogleAnalyticsAdminV1alphaGoogleAdsLink]
gaavlgalrGoogleAdsLinks
  = lens _gaavlgalrGoogleAdsLinks
      (\ s a -> s{_gaavlgalrGoogleAdsLinks = a})
      . _Default
      . _Coerce

-- | A token, which can be sent as \`page_token\` to retrieve the next page.
-- If this field is omitted, there are no subsequent pages.
gaavlgalrNextPageToken :: Lens' GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse (Maybe Text)
gaavlgalrNextPageToken
  = lens _gaavlgalrNextPageToken
      (\ s a -> s{_gaavlgalrNextPageToken = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse'
                   <$>
                   (o .:? "googleAdsLinks" .!= mempty) <*>
                     (o .:? "nextPageToken"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaListGoogleAdsLinksResponse'{..}
          = object
              (catMaybes
                 [("googleAdsLinks" .=) <$> _gaavlgalrGoogleAdsLinks,
                  ("nextPageToken" .=) <$> _gaavlgalrNextPageToken])

-- | A resource message representing data sharing settings of a Google
-- Analytics account.
--
-- /See:/ 'googleAnalyticsAdminV1alphaDataSharingSettings' smart constructor.
data GoogleAnalyticsAdminV1alphaDataSharingSettings =
  GoogleAnalyticsAdminV1alphaDataSharingSettings'
    { _gaavdssSharingWithGoogleAnySalesEnabled :: !(Maybe Bool)
    , _gaavdssSharingWithGoogleAssignedSalesEnabled :: !(Maybe Bool)
    , _gaavdssName :: !(Maybe Text)
    , _gaavdssSharingWithOthersEnabled :: !(Maybe Bool)
    , _gaavdssSharingWithGoogleProductsEnabled :: !(Maybe Bool)
    , _gaavdssSharingWithGoogleSupportEnabled :: !(Maybe Bool)
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaDataSharingSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavdssSharingWithGoogleAnySalesEnabled'
--
-- * 'gaavdssSharingWithGoogleAssignedSalesEnabled'
--
-- * 'gaavdssName'
--
-- * 'gaavdssSharingWithOthersEnabled'
--
-- * 'gaavdssSharingWithGoogleProductsEnabled'
--
-- * 'gaavdssSharingWithGoogleSupportEnabled'
googleAnalyticsAdminV1alphaDataSharingSettings
    :: GoogleAnalyticsAdminV1alphaDataSharingSettings
googleAnalyticsAdminV1alphaDataSharingSettings =
  GoogleAnalyticsAdminV1alphaDataSharingSettings'
    { _gaavdssSharingWithGoogleAnySalesEnabled = Nothing
    , _gaavdssSharingWithGoogleAssignedSalesEnabled = Nothing
    , _gaavdssName = Nothing
    , _gaavdssSharingWithOthersEnabled = Nothing
    , _gaavdssSharingWithGoogleProductsEnabled = Nothing
    , _gaavdssSharingWithGoogleSupportEnabled = Nothing
    }


-- | Allows any of Google sales to access the data in order to suggest
-- configuration changes to improve results.
gaavdssSharingWithGoogleAnySalesEnabled :: Lens' GoogleAnalyticsAdminV1alphaDataSharingSettings (Maybe Bool)
gaavdssSharingWithGoogleAnySalesEnabled
  = lens _gaavdssSharingWithGoogleAnySalesEnabled
      (\ s a ->
         s{_gaavdssSharingWithGoogleAnySalesEnabled = a})

-- | Allows Google sales teams that are assigned to the customer to access
-- the data in order to suggest configuration changes to improve results.
-- Sales team restrictions still apply when enabled.
gaavdssSharingWithGoogleAssignedSalesEnabled :: Lens' GoogleAnalyticsAdminV1alphaDataSharingSettings (Maybe Bool)
gaavdssSharingWithGoogleAssignedSalesEnabled
  = lens _gaavdssSharingWithGoogleAssignedSalesEnabled
      (\ s a ->
         s{_gaavdssSharingWithGoogleAssignedSalesEnabled = a})

-- | Output only. Resource name. Format:
-- accounts\/{account}\/dataSharingSettings Example:
-- \"accounts\/1000\/dataSharingSettings\"
gaavdssName :: Lens' GoogleAnalyticsAdminV1alphaDataSharingSettings (Maybe Text)
gaavdssName
  = lens _gaavdssName (\ s a -> s{_gaavdssName = a})

-- | Allows Google to share the data anonymously in aggregate form with
-- others.
gaavdssSharingWithOthersEnabled :: Lens' GoogleAnalyticsAdminV1alphaDataSharingSettings (Maybe Bool)
gaavdssSharingWithOthersEnabled
  = lens _gaavdssSharingWithOthersEnabled
      (\ s a -> s{_gaavdssSharingWithOthersEnabled = a})

-- | Allows Google to use the data to improve other Google products or
-- services.
gaavdssSharingWithGoogleProductsEnabled :: Lens' GoogleAnalyticsAdminV1alphaDataSharingSettings (Maybe Bool)
gaavdssSharingWithGoogleProductsEnabled
  = lens _gaavdssSharingWithGoogleProductsEnabled
      (\ s a ->
         s{_gaavdssSharingWithGoogleProductsEnabled = a})

-- | Allows Google support to access the data in order to help troubleshoot
-- issues.
gaavdssSharingWithGoogleSupportEnabled :: Lens' GoogleAnalyticsAdminV1alphaDataSharingSettings (Maybe Bool)
gaavdssSharingWithGoogleSupportEnabled
  = lens _gaavdssSharingWithGoogleSupportEnabled
      (\ s a ->
         s{_gaavdssSharingWithGoogleSupportEnabled = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaDataSharingSettings
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaDataSharingSettings"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaDataSharingSettings' <$>
                   (o .:? "sharingWithGoogleAnySalesEnabled") <*>
                     (o .:? "sharingWithGoogleAssignedSalesEnabled")
                     <*> (o .:? "name")
                     <*> (o .:? "sharingWithOthersEnabled")
                     <*> (o .:? "sharingWithGoogleProductsEnabled")
                     <*> (o .:? "sharingWithGoogleSupportEnabled"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaDataSharingSettings
         where
        toJSON
          GoogleAnalyticsAdminV1alphaDataSharingSettings'{..}
          = object
              (catMaybes
                 [("sharingWithGoogleAnySalesEnabled" .=) <$>
                    _gaavdssSharingWithGoogleAnySalesEnabled,
                  ("sharingWithGoogleAssignedSalesEnabled" .=) <$>
                    _gaavdssSharingWithGoogleAssignedSalesEnabled,
                  ("name" .=) <$> _gaavdssName,
                  ("sharingWithOthersEnabled" .=) <$>
                    _gaavdssSharingWithOthersEnabled,
                  ("sharingWithGoogleProductsEnabled" .=) <$>
                    _gaavdssSharingWithGoogleProductsEnabled,
                  ("sharingWithGoogleSupportEnabled" .=) <$>
                    _gaavdssSharingWithGoogleSupportEnabled])

-- | Read-only resource with the tag for sending data from a website to a
-- WebDataStream.
--
-- /See:/ 'googleAnalyticsAdminV1alphaGlobalSiteTag' smart constructor.
newtype GoogleAnalyticsAdminV1alphaGlobalSiteTag =
  GoogleAnalyticsAdminV1alphaGlobalSiteTag'
    { _gaavgstSnippet :: Maybe Text
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaGlobalSiteTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavgstSnippet'
googleAnalyticsAdminV1alphaGlobalSiteTag
    :: GoogleAnalyticsAdminV1alphaGlobalSiteTag
googleAnalyticsAdminV1alphaGlobalSiteTag =
  GoogleAnalyticsAdminV1alphaGlobalSiteTag' {_gaavgstSnippet = Nothing}


-- | Immutable. JavaScript code snippet to be pasted as the first item into
-- the head tag of every webpage to measure.
gaavgstSnippet :: Lens' GoogleAnalyticsAdminV1alphaGlobalSiteTag (Maybe Text)
gaavgstSnippet
  = lens _gaavgstSnippet
      (\ s a -> s{_gaavgstSnippet = a})

instance FromJSON
           GoogleAnalyticsAdminV1alphaGlobalSiteTag
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaGlobalSiteTag"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaGlobalSiteTag' <$>
                   (o .:? "snippet"))

instance ToJSON
           GoogleAnalyticsAdminV1alphaGlobalSiteTag
         where
        toJSON GoogleAnalyticsAdminV1alphaGlobalSiteTag'{..}
          = object
              (catMaybes [("snippet" .=) <$> _gaavgstSnippet])

-- | Request message for ListIosAppDataStreams RPC.
--
-- /See:/ 'googleAnalyticsAdminV1alphaListIosAppDataStreamsResponse' smart constructor.
data GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse =
  GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse'
    { _gaavliadsrNextPageToken :: !(Maybe Text)
    , _gaavliadsrIosAppDataStreams :: !(Maybe [GoogleAnalyticsAdminV1alphaIosAppDataStream])
    }
  deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaavliadsrNextPageToken'
--
-- * 'gaavliadsrIosAppDataStreams'
googleAnalyticsAdminV1alphaListIosAppDataStreamsResponse
    :: GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse
googleAnalyticsAdminV1alphaListIosAppDataStreamsResponse =
  GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse'
    {_gaavliadsrNextPageToken = Nothing, _gaavliadsrIosAppDataStreams = Nothing}


-- | A token, which can be sent as \`page_token\` to retrieve the next page.
-- If this field is omitted, there are no subsequent pages.
gaavliadsrNextPageToken :: Lens' GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse (Maybe Text)
gaavliadsrNextPageToken
  = lens _gaavliadsrNextPageToken
      (\ s a -> s{_gaavliadsrNextPageToken = a})

-- | Results that matched the filter criteria and were accessible to the
-- caller.
gaavliadsrIosAppDataStreams :: Lens' GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse [GoogleAnalyticsAdminV1alphaIosAppDataStream]
gaavliadsrIosAppDataStreams
  = lens _gaavliadsrIosAppDataStreams
      (\ s a -> s{_gaavliadsrIosAppDataStreams = a})
      . _Default
      . _Coerce

instance FromJSON
           GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse
         where
        parseJSON
          = withObject
              "GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse"
              (\ o ->
                 GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse'
                   <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "iosAppDataStreams" .!= mempty))

instance ToJSON
           GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse
         where
        toJSON
          GoogleAnalyticsAdminV1alphaListIosAppDataStreamsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _gaavliadsrNextPageToken,
                  ("iosAppDataStreams" .=) <$>
                    _gaavliadsrIosAppDataStreams])
