{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Resource.DFAReporting.AdvertiserGroups.Update
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- | Updates an existing advertiser group.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/reporting/ DCM/DFA Reporting And Trafficking API Reference> for @DfareportingAdvertiserGroupsUpdate@.
module Network.Google.Resource.DFAReporting.AdvertiserGroups.Update
    (
    -- * REST Resource
      AdvertiserGroupsUpdateResource

    -- * Creating a Request
    , advertiserGroupsUpdate'
    , AdvertiserGroupsUpdate'

    -- * Request Lenses
    , aguQuotaUser
    , aguPrettyPrint
    , aguAdvertiserGroup
    , aguUserIP
    , aguProfileId
    , aguKey
    , aguOAuthToken
    , aguFields
    ) where

import           Network.Google.DFAReporting.Types
import           Network.Google.Prelude

-- | A resource alias for @DfareportingAdvertiserGroupsUpdate@ which the
-- 'AdvertiserGroupsUpdate'' request conforms to.
type AdvertiserGroupsUpdateResource =
     "userprofiles" :>
       Capture "profileId" Int64 :>
         "advertiserGroups" :>
           QueryParam "quotaUser" Text :>
             QueryParam "prettyPrint" Bool :>
               QueryParam "userIp" Text :>
                 QueryParam "key" Key :>
                   QueryParam "oauth_token" OAuthToken :>
                     QueryParam "fields" Text :>
                       QueryParam "alt" AltJSON :>
                         ReqBody '[JSON] AdvertiserGroup :>
                           Put '[JSON] AdvertiserGroup

-- | Updates an existing advertiser group.
--
-- /See:/ 'advertiserGroupsUpdate'' smart constructor.
data AdvertiserGroupsUpdate' = AdvertiserGroupsUpdate'
    { _aguQuotaUser       :: !(Maybe Text)
    , _aguPrettyPrint     :: !Bool
    , _aguAdvertiserGroup :: !AdvertiserGroup
    , _aguUserIP          :: !(Maybe Text)
    , _aguProfileId       :: !Int64
    , _aguKey             :: !(Maybe Key)
    , _aguOAuthToken      :: !(Maybe OAuthToken)
    , _aguFields          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AdvertiserGroupsUpdate'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aguQuotaUser'
--
-- * 'aguPrettyPrint'
--
-- * 'aguAdvertiserGroup'
--
-- * 'aguUserIP'
--
-- * 'aguProfileId'
--
-- * 'aguKey'
--
-- * 'aguOAuthToken'
--
-- * 'aguFields'
advertiserGroupsUpdate'
    :: AdvertiserGroup -- ^ 'AdvertiserGroup'
    -> Int64 -- ^ 'profileId'
    -> AdvertiserGroupsUpdate'
advertiserGroupsUpdate' pAguAdvertiserGroup_ pAguProfileId_ =
    AdvertiserGroupsUpdate'
    { _aguQuotaUser = Nothing
    , _aguPrettyPrint = True
    , _aguAdvertiserGroup = pAguAdvertiserGroup_
    , _aguUserIP = Nothing
    , _aguProfileId = pAguProfileId_
    , _aguKey = Nothing
    , _aguOAuthToken = Nothing
    , _aguFields = Nothing
    }

-- | Available to use for quota purposes for server-side applications. Can be
-- any arbitrary string assigned to a user, but should not exceed 40
-- characters. Overrides userIp if both are provided.
aguQuotaUser :: Lens' AdvertiserGroupsUpdate' (Maybe Text)
aguQuotaUser
  = lens _aguQuotaUser (\ s a -> s{_aguQuotaUser = a})

-- | Returns response with indentations and line breaks.
aguPrettyPrint :: Lens' AdvertiserGroupsUpdate' Bool
aguPrettyPrint
  = lens _aguPrettyPrint
      (\ s a -> s{_aguPrettyPrint = a})

-- | Multipart request metadata.
aguAdvertiserGroup :: Lens' AdvertiserGroupsUpdate' AdvertiserGroup
aguAdvertiserGroup
  = lens _aguAdvertiserGroup
      (\ s a -> s{_aguAdvertiserGroup = a})

-- | IP address of the site where the request originates. Use this if you
-- want to enforce per-user limits.
aguUserIP :: Lens' AdvertiserGroupsUpdate' (Maybe Text)
aguUserIP
  = lens _aguUserIP (\ s a -> s{_aguUserIP = a})

-- | User profile ID associated with this request.
aguProfileId :: Lens' AdvertiserGroupsUpdate' Int64
aguProfileId
  = lens _aguProfileId (\ s a -> s{_aguProfileId = a})

-- | API key. Your API key identifies your project and provides you with API
-- access, quota, and reports. Required unless you provide an OAuth 2.0
-- token.
aguKey :: Lens' AdvertiserGroupsUpdate' (Maybe Key)
aguKey = lens _aguKey (\ s a -> s{_aguKey = a})

-- | OAuth 2.0 token for the current user.
aguOAuthToken :: Lens' AdvertiserGroupsUpdate' (Maybe OAuthToken)
aguOAuthToken
  = lens _aguOAuthToken
      (\ s a -> s{_aguOAuthToken = a})

-- | Selector specifying which fields to include in a partial response.
aguFields :: Lens' AdvertiserGroupsUpdate' (Maybe Text)
aguFields
  = lens _aguFields (\ s a -> s{_aguFields = a})

instance GoogleAuth AdvertiserGroupsUpdate' where
        authKey = aguKey . _Just
        authToken = aguOAuthToken . _Just

instance GoogleRequest AdvertiserGroupsUpdate' where
        type Rs AdvertiserGroupsUpdate' = AdvertiserGroup
        request = requestWithRoute defReq dFAReportingURL
        requestWithRoute r u AdvertiserGroupsUpdate'{..}
          = go _aguQuotaUser (Just _aguPrettyPrint) _aguUserIP
              _aguProfileId
              _aguKey
              _aguOAuthToken
              _aguFields
              (Just AltJSON)
              _aguAdvertiserGroup
          where go
                  = clientWithRoute
                      (Proxy :: Proxy AdvertiserGroupsUpdateResource)
                      r
                      u