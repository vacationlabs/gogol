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
-- Module      : Network.Google.Resource.Calendar.Channels.Stop
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- | Stop watching resources through this channel
--
-- /See:/ <https://developers.google.com/google-apps/calendar/firstapp Calendar API Reference> for @CalendarChannelsStop@.
module Network.Google.Resource.Calendar.Channels.Stop
    (
    -- * REST Resource
      ChannelsStopResource

    -- * Creating a Request
    , channelsStop'
    , ChannelsStop'

    -- * Request Lenses
    , csQuotaUser
    , csPrettyPrint
    , csUserIP
    , csChannel
    , csKey
    , csOAuthToken
    , csFields
    ) where

import           Network.Google.AppsCalendar.Types
import           Network.Google.Prelude

-- | A resource alias for @CalendarChannelsStop@ which the
-- 'ChannelsStop'' request conforms to.
type ChannelsStopResource =
     "channels" :>
       "stop" :>
         QueryParam "quotaUser" Text :>
           QueryParam "prettyPrint" Bool :>
             QueryParam "userIp" Text :>
               QueryParam "key" Key :>
                 QueryParam "oauth_token" OAuthToken :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON] Channel :> Post '[JSON] ()

-- | Stop watching resources through this channel
--
-- /See:/ 'channelsStop'' smart constructor.
data ChannelsStop' = ChannelsStop'
    { _csQuotaUser   :: !(Maybe Text)
    , _csPrettyPrint :: !Bool
    , _csUserIP      :: !(Maybe Text)
    , _csChannel     :: !Channel
    , _csKey         :: !(Maybe Key)
    , _csOAuthToken  :: !(Maybe OAuthToken)
    , _csFields      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChannelsStop'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csQuotaUser'
--
-- * 'csPrettyPrint'
--
-- * 'csUserIP'
--
-- * 'csChannel'
--
-- * 'csKey'
--
-- * 'csOAuthToken'
--
-- * 'csFields'
channelsStop'
    :: Channel -- ^ 'Channel'
    -> ChannelsStop'
channelsStop' pCsChannel_ =
    ChannelsStop'
    { _csQuotaUser = Nothing
    , _csPrettyPrint = True
    , _csUserIP = Nothing
    , _csChannel = pCsChannel_
    , _csKey = Nothing
    , _csOAuthToken = Nothing
    , _csFields = Nothing
    }

-- | Available to use for quota purposes for server-side applications. Can be
-- any arbitrary string assigned to a user, but should not exceed 40
-- characters. Overrides userIp if both are provided.
csQuotaUser :: Lens' ChannelsStop' (Maybe Text)
csQuotaUser
  = lens _csQuotaUser (\ s a -> s{_csQuotaUser = a})

-- | Returns response with indentations and line breaks.
csPrettyPrint :: Lens' ChannelsStop' Bool
csPrettyPrint
  = lens _csPrettyPrint
      (\ s a -> s{_csPrettyPrint = a})

-- | IP address of the site where the request originates. Use this if you
-- want to enforce per-user limits.
csUserIP :: Lens' ChannelsStop' (Maybe Text)
csUserIP = lens _csUserIP (\ s a -> s{_csUserIP = a})

-- | Multipart request metadata.
csChannel :: Lens' ChannelsStop' Channel
csChannel
  = lens _csChannel (\ s a -> s{_csChannel = a})

-- | API key. Your API key identifies your project and provides you with API
-- access, quota, and reports. Required unless you provide an OAuth 2.0
-- token.
csKey :: Lens' ChannelsStop' (Maybe Key)
csKey = lens _csKey (\ s a -> s{_csKey = a})

-- | OAuth 2.0 token for the current user.
csOAuthToken :: Lens' ChannelsStop' (Maybe OAuthToken)
csOAuthToken
  = lens _csOAuthToken (\ s a -> s{_csOAuthToken = a})

-- | Selector specifying which fields to include in a partial response.
csFields :: Lens' ChannelsStop' (Maybe Text)
csFields = lens _csFields (\ s a -> s{_csFields = a})

instance GoogleAuth ChannelsStop' where
        authKey = csKey . _Just
        authToken = csOAuthToken . _Just

instance GoogleRequest ChannelsStop' where
        type Rs ChannelsStop' = ()
        request = requestWithRoute defReq appsCalendarURL
        requestWithRoute r u ChannelsStop'{..}
          = go _csQuotaUser (Just _csPrettyPrint) _csUserIP
              _csKey
              _csOAuthToken
              _csFields
              (Just AltJSON)
              _csChannel
          where go
                  = clientWithRoute
                      (Proxy :: Proxy ChannelsStopResource)
                      r
                      u