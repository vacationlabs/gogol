{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.AnalyticsAdmin.Types.Sum
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.AnalyticsAdmin.Types.Sum where

import Network.Google.Prelude hiding (Bytes)

-- | V1 error format.
data Xgafv
    = X1
      -- ^ @1@
      -- v1 error format
    | X2
      -- ^ @2@
      -- v2 error format
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable Xgafv

instance FromHttpApiData Xgafv where
    parseQueryParam = \case
        "1" -> Right X1
        "2" -> Right X2
        x -> Left ("Unable to parse Xgafv from: " <> x)

instance ToHttpApiData Xgafv where
    toQueryParam = \case
        X1 -> "1"
        X2 -> "2"

instance FromJSON Xgafv where
    parseJSON = parseJSONText "Xgafv"

instance ToJSON Xgafv where
    toJSON = toJSONText

-- | Maximum user access to the GA4 property allowed to admins of the linked
-- Firebase project.
data GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess
    = MaximumUserAccessUnspecified
      -- ^ @MAXIMUM_USER_ACCESS_UNSPECIFIED@
      -- Unspecified maximum user access.
    | NoAccess
      -- ^ @NO_ACCESS@
      -- Firebase users have no access to the Analytics property.
    | ReadAndAnalyze
      -- ^ @READ_AND_ANALYZE@
      -- Firebase users have Read & Analyze access to the Analytics property.
    | EditorWithoutLinkManagement
      -- ^ @EDITOR_WITHOUT_LINK_MANAGEMENT@
      -- Firebase users have edit access to the Analytics property, but may not
      -- manage the Firebase link.
    | EditorIncludingLinkManagement
      -- ^ @EDITOR_INCLUDING_LINK_MANAGEMENT@
      -- Firebase users have edit access to the Analytics property and may manage
      -- the Firebase link.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess

instance FromHttpApiData GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess where
    parseQueryParam = \case
        "MAXIMUM_USER_ACCESS_UNSPECIFIED" -> Right MaximumUserAccessUnspecified
        "NO_ACCESS" -> Right NoAccess
        "READ_AND_ANALYZE" -> Right ReadAndAnalyze
        "EDITOR_WITHOUT_LINK_MANAGEMENT" -> Right EditorWithoutLinkManagement
        "EDITOR_INCLUDING_LINK_MANAGEMENT" -> Right EditorIncludingLinkManagement
        x -> Left ("Unable to parse GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess from: " <> x)

instance ToHttpApiData GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess where
    toQueryParam = \case
        MaximumUserAccessUnspecified -> "MAXIMUM_USER_ACCESS_UNSPECIFIED"
        NoAccess -> "NO_ACCESS"
        ReadAndAnalyze -> "READ_AND_ANALYZE"
        EditorWithoutLinkManagement -> "EDITOR_WITHOUT_LINK_MANAGEMENT"
        EditorIncludingLinkManagement -> "EDITOR_INCLUDING_LINK_MANAGEMENT"

instance FromJSON GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess where
    parseJSON = parseJSONText "GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess"

instance ToJSON GoogleAnalyticsAdminV1alphaFirebaseLinkMaximumUserAccess where
    toJSON = toJSONText

-- | Industry associated with this property Example: AUTOMOTIVE,
-- FOOD_AND_DRINK
data GoogleAnalyticsAdminV1alphaPropertyIndustryCategory
    = IndustryCategoryUnspecified
      -- ^ @INDUSTRY_CATEGORY_UNSPECIFIED@
      -- Industry category unspecified
    | Automotive
      -- ^ @AUTOMOTIVE@
      -- Automotive
    | BusinessAndIndustrialMarkets
      -- ^ @BUSINESS_AND_INDUSTRIAL_MARKETS@
      -- Business and industrial markets
    | Finance
      -- ^ @FINANCE@
      -- Finance
    | Healthcare
      -- ^ @HEALTHCARE@
      -- Healthcare
    | Technology
      -- ^ @TECHNOLOGY@
      -- Technology
    | Travel
      -- ^ @TRAVEL@
      -- Travel
    | Other
      -- ^ @OTHER@
      -- Other
    | ArtsAndEntertainment
      -- ^ @ARTS_AND_ENTERTAINMENT@
      -- Arts and entertainment
    | BeautyAndFitness
      -- ^ @BEAUTY_AND_FITNESS@
      -- Beauty and fitness
    | BooksAndLiterature
      -- ^ @BOOKS_AND_LITERATURE@
      -- Books and literature
    | FoodAndDrink
      -- ^ @FOOD_AND_DRINK@
      -- Food and drink
    | Games
      -- ^ @GAMES@
      -- Games
    | HobbiesAndLeisure
      -- ^ @HOBBIES_AND_LEISURE@
      -- Hobbies and leisure
    | HomeAndGarden
      -- ^ @HOME_AND_GARDEN@
      -- Home and garden
    | InternetAndTelecom
      -- ^ @INTERNET_AND_TELECOM@
      -- Internet and telecom
    | LawAndGovernment
      -- ^ @LAW_AND_GOVERNMENT@
      -- Law and government
    | News
      -- ^ @NEWS@
      -- News
    | OnlineCommUnities
      -- ^ @ONLINE_COMMUNITIES@
      -- Online communities
    | PeopleAndSociety
      -- ^ @PEOPLE_AND_SOCIETY@
      -- People and society
    | PetsAndAnimals
      -- ^ @PETS_AND_ANIMALS@
      -- Pets and animals
    | RealEState
      -- ^ @REAL_ESTATE@
      -- Real estate
    | Reference
      -- ^ @REFERENCE@
      -- Reference
    | Science
      -- ^ @SCIENCE@
      -- Science
    | Sports
      -- ^ @SPORTS@
      -- Sports
    | JobsAndEducation
      -- ^ @JOBS_AND_EDUCATION@
      -- Jobs and education
    | Shopping
      -- ^ @SHOPPING@
      -- Shopping
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable GoogleAnalyticsAdminV1alphaPropertyIndustryCategory

instance FromHttpApiData GoogleAnalyticsAdminV1alphaPropertyIndustryCategory where
    parseQueryParam = \case
        "INDUSTRY_CATEGORY_UNSPECIFIED" -> Right IndustryCategoryUnspecified
        "AUTOMOTIVE" -> Right Automotive
        "BUSINESS_AND_INDUSTRIAL_MARKETS" -> Right BusinessAndIndustrialMarkets
        "FINANCE" -> Right Finance
        "HEALTHCARE" -> Right Healthcare
        "TECHNOLOGY" -> Right Technology
        "TRAVEL" -> Right Travel
        "OTHER" -> Right Other
        "ARTS_AND_ENTERTAINMENT" -> Right ArtsAndEntertainment
        "BEAUTY_AND_FITNESS" -> Right BeautyAndFitness
        "BOOKS_AND_LITERATURE" -> Right BooksAndLiterature
        "FOOD_AND_DRINK" -> Right FoodAndDrink
        "GAMES" -> Right Games
        "HOBBIES_AND_LEISURE" -> Right HobbiesAndLeisure
        "HOME_AND_GARDEN" -> Right HomeAndGarden
        "INTERNET_AND_TELECOM" -> Right InternetAndTelecom
        "LAW_AND_GOVERNMENT" -> Right LawAndGovernment
        "NEWS" -> Right News
        "ONLINE_COMMUNITIES" -> Right OnlineCommUnities
        "PEOPLE_AND_SOCIETY" -> Right PeopleAndSociety
        "PETS_AND_ANIMALS" -> Right PetsAndAnimals
        "REAL_ESTATE" -> Right RealEState
        "REFERENCE" -> Right Reference
        "SCIENCE" -> Right Science
        "SPORTS" -> Right Sports
        "JOBS_AND_EDUCATION" -> Right JobsAndEducation
        "SHOPPING" -> Right Shopping
        x -> Left ("Unable to parse GoogleAnalyticsAdminV1alphaPropertyIndustryCategory from: " <> x)

instance ToHttpApiData GoogleAnalyticsAdminV1alphaPropertyIndustryCategory where
    toQueryParam = \case
        IndustryCategoryUnspecified -> "INDUSTRY_CATEGORY_UNSPECIFIED"
        Automotive -> "AUTOMOTIVE"
        BusinessAndIndustrialMarkets -> "BUSINESS_AND_INDUSTRIAL_MARKETS"
        Finance -> "FINANCE"
        Healthcare -> "HEALTHCARE"
        Technology -> "TECHNOLOGY"
        Travel -> "TRAVEL"
        Other -> "OTHER"
        ArtsAndEntertainment -> "ARTS_AND_ENTERTAINMENT"
        BeautyAndFitness -> "BEAUTY_AND_FITNESS"
        BooksAndLiterature -> "BOOKS_AND_LITERATURE"
        FoodAndDrink -> "FOOD_AND_DRINK"
        Games -> "GAMES"
        HobbiesAndLeisure -> "HOBBIES_AND_LEISURE"
        HomeAndGarden -> "HOME_AND_GARDEN"
        InternetAndTelecom -> "INTERNET_AND_TELECOM"
        LawAndGovernment -> "LAW_AND_GOVERNMENT"
        News -> "NEWS"
        OnlineCommUnities -> "ONLINE_COMMUNITIES"
        PeopleAndSociety -> "PEOPLE_AND_SOCIETY"
        PetsAndAnimals -> "PETS_AND_ANIMALS"
        RealEState -> "REAL_ESTATE"
        Reference -> "REFERENCE"
        Science -> "SCIENCE"
        Sports -> "SPORTS"
        JobsAndEducation -> "JOBS_AND_EDUCATION"
        Shopping -> "SHOPPING"

instance FromJSON GoogleAnalyticsAdminV1alphaPropertyIndustryCategory where
    parseJSON = parseJSONText "GoogleAnalyticsAdminV1alphaPropertyIndustryCategory"

instance ToJSON GoogleAnalyticsAdminV1alphaPropertyIndustryCategory where
    toJSON = toJSONText
