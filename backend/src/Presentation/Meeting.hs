{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}

module Presentation.Meeting where

import Utils.Datatypes (Sessions, Meeting (..), UserId, Availability (Availability), MeetingId)
import Web.Twain (ResponderM, send, fromBody, status, status200, json, text, status400, param)
import Presentation.Authentication (requireSession)
import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)
import Data.Text (pack, Text)
import Data.Aeson ((.=), (.:), FromJSON (parseJSON), withObject, object, ToJSON)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Integration.MeetingStore (storeMeeting, findMeetingById, updateAvailabilities, updateUsers)
import Data.Aeson.Types (ToJSON(toJSON))
import Utils.Endpoint (DBPool, withDB)
import Database.Persist (selectList, Entity (..))
import Database.Persist.Sql (fromSqlKey)
import Utils.DbInit (MeetingEntity(..), AvailabilityEntity(..))
import Data.List (nub, sortOn, delete, maximumBy)
import Data.Function (on)

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
  userIds :: [UserId],
  description :: Text,
  availabilities :: [AvailabilityResponse],
  maximumAttendancy :: Maybe Attendancy
}

data AvailabilityResponse = AvailabilityResponse {
  start :: UTCTime,
  end :: UTCTime,
  userId :: UserId
}

instance ToJSON AvailabilityResponse where
  toJSON (AvailabilityResponse s e id) = object ["start" .= s, "end" .= e, "userId" .= id]

instance ToJSON MeetingResponse where
  toJSON (MeetingResponse mId t s e days uIds desc as attendancy) = object
    [ "id" .= mId
    , "title" .= t
    , "start" .= formatTime defaultTimeLocale "%T" s
    , "end" .= formatTime defaultTimeLocale "%T" e
    , "days" .= map (formatTime defaultTimeLocale "%F") days
    , "userIds" .= uIds
    , "availabilities" .= as
    , "description" .= desc
    , "maximumAttendancy" .= attendancy
    ]

instance FromJSON AvailabilityParams where
  parseJSON = withObject "AvailabilityParams" $ \ o -> AvailabilityParams
    <$> o .: "start"
    <*> o .: "end"

meetingToResponse :: Meeting -> Maybe Attendancy -> MeetingResponse
meetingToResponse (Meeting mId t s e days uIds desc as) = MeetingResponse 
  mId 
  t 
  s
  e
  days
  uIds 
  desc
  (map availabilityToResponse as)
 

availabilityToResponse :: Availability -> AvailabilityResponse
availabilityToResponse (Availability s e uId) = AvailabilityResponse s e uId

createMeeting :: Sessions -> DBPool -> ResponderM a
createMeeting sessions pool = do
  MeetingParams title start end days desc <- fromBody

  uId <- requireSession sessions
  meetingId <- liftIO nextRandom

  let meeting = Meeting (pack $ show meetingId) title start end days [uId] desc []
  
  _ <- withDB pool $ storeMeeting meeting

  send $ status status200 $ json $ object ["meetingId" .= meetingId]

addAvailabilitiesToMeeting :: Sessions -> DBPool -> ResponderM a
addAvailabilitiesToMeeting sessions pool = do
  uId <- requireSession sessions
  mId <- param "mId"
  (availabilityParamsToAvailabilities uId -> bodyAps) <- fromBody

  withDB pool (updateAvailabilities mId uId bodyAps)
  withDB pool (updateUsers mId uId)

  send $ status status200 $ text "Done."

data ScanStep
  = Join { stepDate :: UTCTime, stepUser :: UserId }
  | Leave { stepDate :: UTCTime, stepUser :: UserId }

data Attendancy = Attendancy
  { users :: [UserId]
  , start :: UTCTime
  , end   :: UTCTime
  } deriving Show

instance ToJSON Attendancy where
  toJSON (Attendancy users start end) = object
    [ "users" .= users
    , "start" .= start
    , "end" .= end
    ]

getMeetingWithId :: Sessions -> DBPool -> ResponderM a
getMeetingWithId sessions pool = do
  uId <- requireSession sessions
  mId <- param "mId"
  
  withDB pool (findMeetingById mId) >>= \case
    Nothing -> send $ status status400 $ text "No such meeting"
    Just meeting@Meeting { mAvailabilities } ->
      let ownAvailabilities = filter (\(Availability _ _ id) -> id == uId) mAvailabilities
          response = meetingToResponse meeting { mAvailabilities = ownAvailabilities } (findMaximumAttendancy mAvailabilities)
       in send $ status status200 $ json response

findMaximumAttendancy :: [Availability] -> Maybe Attendancy
findMaximumAttendancy avails =
  let scanSteps = sortOn stepDate $ concatMap toScanStep avails
      scanList = scanl trackUsers [] scanSteps
      dates = map stepDate scanSteps
      attendencies = zipWith3 Attendancy (tail scanList) dates (tail dates)
   in case attendencies of
    [] -> Nothing
    _ -> Just $ maximumBy (compare `on` (length . users)) attendencies
  where
    trackUsers :: [UserId] -> ScanStep -> [UserId]
    trackUsers ids (Join _ id)  = id : ids
    trackUsers ids (Leave _ id) = delete id ids

    toScanStep :: Availability -> [ScanStep]
    toScanStep (Availability start end uId) = [Join start uId, Leave end uId]

availabilityParamsToAvailabilities :: UserId -> [AvailabilityParams] -> [Availability]
availabilityParamsToAvailabilities uId = map (\ (AvailabilityParams s e) -> Availability s e uId)

getOwnMeetings :: Sessions -> DBPool -> ResponderM a
getOwnMeetings sessions pool = do
  userId <- requireSession sessions
  meetings <- filter (ownMeeting userId) <$> withDB pool (selectList [] [])
  send $ status status200 $ json
    [ object
      [ "id" .= m.meetingEntityMeetingId
      , "stats" .= object
        [ "added" .= length m.meetingEntityUserIds
        , "availabilities" .= noOfUsers m
        ]
      , "title" .= m.meetingEntityTitle
      ]
    | Entity { entityVal = m } <- meetings
    ]
  where
    noOfUsers :: MeetingEntity -> Int
    noOfUsers m = length $ nub [a.availabilityEntityUserId | a <- m.meetingEntityAvailabilities]

    ownMeeting :: UserId -> Entity MeetingEntity -> Bool
    ownMeeting uid (Entity { entityVal = meeting }) =
      fromIntegral uid `elem` map fromSqlKey meeting.meetingEntityUserIds

addUserToMeeting :: Sessions -> DBPool -> ResponderM a
addUserToMeeting sessions pool = do
  uId <- requireSession sessions
  mId <- param "mId"

  withDB pool (updateUsers mId uId)

  send $ status status200 $ text "Done."
