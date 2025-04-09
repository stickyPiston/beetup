{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Presentation.Meeting where

import Utils.Datatypes (Sessions, Meeting (..), UserId, Availability (Availability), MeetingId)
import Web.Twain (ResponderM, send, fromBody, status, status200, json, text, status400, param)
import Presentation.Authentication (requireSession)
import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)
import Data.Text (pack, Text)
import Data.Aeson ((.=), (.:), FromJSON (parseJSON), withObject, object, ToJSON)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Integration.MeetingStore (storeMeeting, findMeetingById, updateAvailabilities)
import Data.Aeson.Types (ToJSON(toJSON))
import Utils.Endpoint (DBPool, withDB)
import Database.Persist (selectList, (==.), Entity (..))
import Integration.Init (EntityField(MeetingEntityUserId), MeetingEntity(..), AvailabilityEntity(..))
import Database.Persist.Sql (toSqlKey)
import Data.List (nub)

data MeetingParams = MeetingParams {
    title       :: Text,
    start       :: UTCTime,
    end         :: UTCTime,
    days        :: [UTCTime],
    description :: Text
}

newtype MeetingIdParams = MeetingIdParams {
  meetingId :: Text
}

instance FromJSON MeetingIdParams where
  parseJSON = withObject "MeetingIdParams" $ \o -> MeetingIdParams
    <$> o .: "meetingId"

instance FromJSON MeetingParams where
  parseJSON = withObject "MeetingParams" $ \ o -> MeetingParams
    <$> o .: "title"
    <*> o .: "start"
    <*> o .: "end"
    <*> o .: "days"
    <*> o .: "description"

data AvailabilityParams = AvailabilityParams {
  start :: UTCTime,
  end   :: UTCTime
}

data MeetingResponse = MeetingResponse {
  meetingId :: MeetingId,
  title :: Text,
  start :: UTCTime,
  end :: UTCTime,
  days :: [UTCTime],
  userId :: UserId,
  description :: Text,
  availabilities :: [AvailabilityResponse]
}

data AvailabilityResponse = AvailabilityResponse {
  start :: UTCTime,
  end :: UTCTime,
  userId :: UserId
}

instance ToJSON AvailabilityResponse where
  toJSON (AvailabilityResponse s e id) = object ["start" .= s, "end" .= e, "userId" .= id]

instance ToJSON MeetingResponse where
  toJSON (MeetingResponse mId t s e days uId desc as) = object
    [ "id" .= mId
    , "title" .= t
    , "start" .= formatTime defaultTimeLocale "%T" s
    , "end" .= formatTime defaultTimeLocale "%T" e
    , "days" .= map (formatTime defaultTimeLocale "%F") days
    , "userId" .= uId
    , "availabilities" .= as
    , "description" .= desc
    ]

instance FromJSON AvailabilityParams where
  parseJSON = withObject "AvailabilityParams" $ \ o -> AvailabilityParams
    <$> o .: "start"
    <*> o .: "end"

meetingToResponse :: Meeting -> MeetingResponse
meetingToResponse (Meeting mId t s e days uId desc as) = MeetingResponse 
  mId 
  t 
  s
  e
  days
  uId 
  desc
  (map availabilityToResponse as)

availabilityToResponse :: Availability -> AvailabilityResponse
availabilityToResponse (Availability s e uId) = AvailabilityResponse s e uId

createMeeting :: Sessions -> DBPool -> ResponderM a
createMeeting sessions pool = do
  MeetingParams title start end days desc <- fromBody

  uId <- requireSession sessions
  meetingId <- liftIO nextRandom

  let meeting = Meeting (pack $ show meetingId) title start end days uId desc []
  
  _ <- withDB pool $ storeMeeting meeting

  send $ status status200 $ json $ object ["meetingId" .= meetingId]

addAvailabilitiesToMeeting :: Sessions -> DBPool -> ResponderM a
addAvailabilitiesToMeeting sessions pool = do
  uId <- requireSession sessions
  mId <- param "mId"
  (availabilityParamsToAvailabilities uId -> bodyAps) <- fromBody

  withDB pool (findMeetingById mId) >>= \case
    Nothing -> send $ status status400 $ text "No such meeting"
    Just Meeting { mAvailabilities } ->
      withDB pool (updateAvailabilities mId (mAvailabilities ++ bodyAps)) >>= \case
        Nothing -> send $ status status400 $ text "Couldn't fetch updated meeting"  
        Just _ -> send $ status status200 $ text "Done."

getMeetingWithId :: DBPool -> ResponderM a
getMeetingWithId pool = do
  mId <- param "mId"
  
  withDB pool (findMeetingById mId) >>= \case
    Nothing -> send $ status status400 $ text "No such meeting"
    Just meeting -> send $ status status200 $ json (meetingToResponse meeting)

availabilityParamsToAvailabilities :: UserId -> [AvailabilityParams] -> [Availability]
availabilityParamsToAvailabilities uId = map (\ (AvailabilityParams s e) -> Availability s e uId)

getOwnMeetings :: Sessions -> DBPool -> ResponderM a
getOwnMeetings sessions pool = do
  userId <- toSqlKey . fromIntegral <$> requireSession sessions
  meetings <- withDB pool (selectList [MeetingEntityUserId ==. userId] [])
  send $ status status200 $ json
    [ object ["id" .= m.meetingEntityMeetingId, "noOfUsers" .= noOfUsers m, "title" .= m.meetingEntityTitle]
    | Entity { entityVal = m } <- meetings
    ]
  where
    noOfUsers :: MeetingEntity -> Int
    noOfUsers m = length $ nub [a.availabilityEntityUserId | a <- m.meetingEntityAvailabilities]