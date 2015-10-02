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
-- Module      : Network.Google.Resource.Fitness.Users.DataSources.Get
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- | Returns a data source identified by a data stream ID.
--
-- /See:/ <https://developers.google.com/fit/rest/ Fitness Reference> for @FitnessUsersDataSourcesGet@.
module Network.Google.Resource.Fitness.Users.DataSources.Get
    (
    -- * REST Resource
      UsersDataSourcesGetResource

    -- * Creating a Request
    , usersDataSourcesGet'
    , UsersDataSourcesGet'

    -- * Request Lenses
    , udsgQuotaUser
    , udsgPrettyPrint
    , udsgUserIP
    , udsgDataSourceId
    , udsgUserId
    , udsgKey
    , udsgOAuthToken
    , udsgFields
    ) where

import           Network.Google.Fitness.Types
import           Network.Google.Prelude

-- | A resource alias for @FitnessUsersDataSourcesGet@ which the
-- 'UsersDataSourcesGet'' request conforms to.
type UsersDataSourcesGetResource =
     Capture "userId" Text :>
       "dataSources" :>
         Capture "dataSourceId" Text :>
           QueryParam "quotaUser" Text :>
             QueryParam "prettyPrint" Bool :>
               QueryParam "userIp" Text :>
                 QueryParam "key" Key :>
                   QueryParam "oauth_token" OAuthToken :>
                     QueryParam "fields" Text :>
                       QueryParam "alt" AltJSON :> Get '[JSON] DataSource

-- | Returns a data source identified by a data stream ID.
--
-- /See:/ 'usersDataSourcesGet'' smart constructor.
data UsersDataSourcesGet' = UsersDataSourcesGet'
    { _udsgQuotaUser    :: !(Maybe Text)
    , _udsgPrettyPrint  :: !Bool
    , _udsgUserIP       :: !(Maybe Text)
    , _udsgDataSourceId :: !Text
    , _udsgUserId       :: !Text
    , _udsgKey          :: !(Maybe Key)
    , _udsgOAuthToken   :: !(Maybe OAuthToken)
    , _udsgFields       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersDataSourcesGet'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udsgQuotaUser'
--
-- * 'udsgPrettyPrint'
--
-- * 'udsgUserIP'
--
-- * 'udsgDataSourceId'
--
-- * 'udsgUserId'
--
-- * 'udsgKey'
--
-- * 'udsgOAuthToken'
--
-- * 'udsgFields'
usersDataSourcesGet'
    :: Text -- ^ 'dataSourceId'
    -> Text -- ^ 'userId'
    -> UsersDataSourcesGet'
usersDataSourcesGet' pUdsgDataSourceId_ pUdsgUserId_ =
    UsersDataSourcesGet'
    { _udsgQuotaUser = Nothing
    , _udsgPrettyPrint = True
    , _udsgUserIP = Nothing
    , _udsgDataSourceId = pUdsgDataSourceId_
    , _udsgUserId = pUdsgUserId_
    , _udsgKey = Nothing
    , _udsgOAuthToken = Nothing
    , _udsgFields = Nothing
    }

-- | Available to use for quota purposes for server-side applications. Can be
-- any arbitrary string assigned to a user, but should not exceed 40
-- characters. Overrides userIp if both are provided.
udsgQuotaUser :: Lens' UsersDataSourcesGet' (Maybe Text)
udsgQuotaUser
  = lens _udsgQuotaUser
      (\ s a -> s{_udsgQuotaUser = a})

-- | Returns response with indentations and line breaks.
udsgPrettyPrint :: Lens' UsersDataSourcesGet' Bool
udsgPrettyPrint
  = lens _udsgPrettyPrint
      (\ s a -> s{_udsgPrettyPrint = a})

-- | IP address of the site where the request originates. Use this if you
-- want to enforce per-user limits.
udsgUserIP :: Lens' UsersDataSourcesGet' (Maybe Text)
udsgUserIP
  = lens _udsgUserIP (\ s a -> s{_udsgUserIP = a})

-- | The data stream ID of the data source to retrieve.
udsgDataSourceId :: Lens' UsersDataSourcesGet' Text
udsgDataSourceId
  = lens _udsgDataSourceId
      (\ s a -> s{_udsgDataSourceId = a})

-- | Retrieve a data source for the person identified. Use me to indicate the
-- authenticated user. Only me is supported at this time.
udsgUserId :: Lens' UsersDataSourcesGet' Text
udsgUserId
  = lens _udsgUserId (\ s a -> s{_udsgUserId = a})

-- | API key. Your API key identifies your project and provides you with API
-- access, quota, and reports. Required unless you provide an OAuth 2.0
-- token.
udsgKey :: Lens' UsersDataSourcesGet' (Maybe Key)
udsgKey = lens _udsgKey (\ s a -> s{_udsgKey = a})

-- | OAuth 2.0 token for the current user.
udsgOAuthToken :: Lens' UsersDataSourcesGet' (Maybe OAuthToken)
udsgOAuthToken
  = lens _udsgOAuthToken
      (\ s a -> s{_udsgOAuthToken = a})

-- | Selector specifying which fields to include in a partial response.
udsgFields :: Lens' UsersDataSourcesGet' (Maybe Text)
udsgFields
  = lens _udsgFields (\ s a -> s{_udsgFields = a})

instance GoogleAuth UsersDataSourcesGet' where
        authKey = udsgKey . _Just
        authToken = udsgOAuthToken . _Just

instance GoogleRequest UsersDataSourcesGet' where
        type Rs UsersDataSourcesGet' = DataSource
        request = requestWithRoute defReq fitnessURL
        requestWithRoute r u UsersDataSourcesGet'{..}
          = go _udsgQuotaUser (Just _udsgPrettyPrint)
              _udsgUserIP
              _udsgDataSourceId
              _udsgUserId
              _udsgKey
              _udsgOAuthToken
              _udsgFields
              (Just AltJSON)
          where go
                  = clientWithRoute
                      (Proxy :: Proxy UsersDataSourcesGetResource)
                      r
                      u