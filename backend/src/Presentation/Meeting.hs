{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}

module Presentation.Meeting where

import Utils.Datatypes
    ( Sessions,
      Meeting(..),
      UserId,
      Availability(Availability),
      MeetingId,
      Availability(..) )
import Web.Twain (ResponderM, send, fromBody, status, status200, json, text, status400, param, status500)
import Presentation.Authentication (requireSession)
import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)
import Data.Text (pack, Text)
import Data.Aeson ((.=), (.:), FromJSON (parseJSON), withObject, object, ToJSON)
import Data.Time (UTCTime (UTCTime, utctDay, utctDayTime), formatTime, defaultTimeLocale)
import Integration.MeetingStore (storeMeeting, findMeetingById, updateAvailabilities, updateUsers)
import Data.Aeson.Types (ToJSON(toJSON))
import Utils.Endpoint (DBPool, withDB)
import Database.Persist (selectList, Entity(..))
import Database.Persist.Sql (fromSqlKey)
import Utils.DbInit (MeetingEntity(..), AvailabilityEntity(..))
import Data.List (nub, sortOn, delete, maximumBy)
import Data.Function (on)
import Availability (determineAvailabilites, splitAvailabilities)
import Integration.OccupancyStore (findUserOccupancies)

-- | Params for the create meeting request
data MeetingParams = MeetingParams {
    title       :: Text,
    start       :: UTCTime,
    end         :: UTCTime,
    days        :: [UTCTime],
    description :: Text
}

-- | Params for the get meeting request
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

-- | Params for the add availabilities to meeting request
data AvailabilityParams = AvailabilityParams {
  start :: UTCTime,
  end   :: UTCTime
}

-- | Params for the get meeting request response
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

-- | Params for the get availabilities request response
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

-- | Function that transforms a meeting into a request response
meetingToResponse :: Meeting -- ^ Meeting to be transformed
                  -> Maybe Attendancy -- ^ Calculated best meeting time
                  -> MeetingResponse
meetingToResponse (Meeting mId t s e days uIds desc as) = MeetingResponse
  mId
  t
  s
  e
  days
  uIds
  desc
  (map availabilityToResponse as)

-- | Function that transforms an availability to a request response
availabilityToResponse :: Availability -> AvailabilityResponse
availabilityToResponse (Availability s e uId) = AvailabilityResponse s e uId

-- | HTTP Post endpoint for creating meetings
createMeeting :: Sessions -> DBPool -> ResponderM a
createMeeting sessions pool = do
  MeetingParams title start end days desc <- fromBody

  -- Grab the id of the logged in meeting
  uId <- requireSession sessions

  -- Generate a meeting id
  meetingId <- liftIO nextRandom

  let meeting = Meeting (pack $ show meetingId) title start end days [uId] desc []

  -- Store the meeting
  _ <- withDB pool $ storeMeeting meeting

  send $ status status200 $ json $ object ["meetingId" .= meetingId]

-- | HTTP Put endpoint for adding availabilities to a meeting
addAvailabilitiesToMeeting :: Sessions -> DBPool -> ResponderM a
addAvailabilitiesToMeeting sessions pool = do
  -- Grab user and meeting id
  uId <- requireSession sessions
  mId <- param "mId"
  (availabilityParamsToAvailabilities uId -> bodyAps) <- fromBody

  -- Update the meeting and responed users in the database
  withDB pool (updateAvailabilities mId uId bodyAps)
  withDB pool (updateUsers mId uId)

  send $ status status200 $ text "Done."

-- | Datatype that hods information on when the user is available for the meeting
data ScanStep
  = Join { stepDate :: UTCTime, stepUser :: UserId } -- ^ Available for the meeting
  | Leave { stepDate :: UTCTime, stepUser :: UserId } -- ^ Not available for the meeting

-- | Datatype that holds information on which users are attending the meeting at the decided time.
data Attendancy = Attendancy
  { users :: [UserId] -- ^ Users that attend the meeting
  , start :: UTCTime -- ^ Start time of the meeting
  , end   :: UTCTime -- ^ End time of the meeting
  } deriving Show

instance ToJSON Attendancy where
  toJSON (Attendancy users start end) = object
    [ "users" .= users
    , "start" .= start
    , "end" .= end
    ]

-- | HTTP Get endpoint to find a meeting with an Id
getMeetingWithId :: Sessions -> DBPool -> ResponderM a
getMeetingWithId sessions pool = do
  -- Grab user & meeting id
  uId <- requireSession sessions
  mId <- param "mId"

  -- | Query the meeting from the database
  withDB pool (findMeetingById mId) >>= \case
    -- No meeting exists
    Nothing -> send $ status status400 $ text "No such meeting"

    -- Meeting exists, return it
    Just meeting@Meeting { mAvailabilities } ->
      let ownAvailabilities = filter (\(Availability _ _ id) -> id == uId) mAvailabilities
          response = meetingToResponse meeting { mAvailabilities = ownAvailabilities } (findMaximumAttendancy mAvailabilities)
       in send $ status status200 $ json response

-- | Function that finds the best time to host the meeting based on the availabilities of the responded users
findMaximumAttendancy :: [Availability] -> Maybe Attendancy
findMaximumAttendancy avails =
  -- Find possible times to host the meeting
  let scanSteps = sortOn stepDate $ concatMap toScanStep avails
      scanList = scanl trackUsers [] scanSteps
      dates = map stepDate scanSteps
      attendencies = zipWith3 Attendancy (tail scanList) dates (tail dates)

   -- If there are not attendies, return nothing
   in case attendencies of
    [] -> Nothing
    -- If there are, return the first time step with the largest attendence
    _ -> Just $ maximumBy (compare `on` length . users) attendencies
  where
    trackUsers :: [UserId] -> ScanStep -> [UserId]
    trackUsers ids (Join _ id)  = id : ids
    trackUsers ids (Leave _ id) = delete id ids

    toScanStep :: Availability -> [ScanStep]
    toScanStep (Availability start end uId) = [Join start uId, Leave end uId]

-- | Function that transforms a request body into a list of availabilities
availabilityParamsToAvailabilities :: UserId -> [AvailabilityParams] -> [Availability]
availabilityParamsToAvailabilities uId = map (\ (AvailabilityParams s e) -> Availability s e uId)

-- | HTTP Get endpoint to query the users meetings
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

-- | HTTP Put endpoint to add a user to a specific meeting
addUserToMeeting :: Sessions -> DBPool -> ResponderM a
addUserToMeeting sessions pool = do
  uId <- requireSession sessions
  mId <- param "mId"

  withDB pool (updateUsers mId uId)

  send $ status status200 $ text "Done."

importOccupancies :: Sessions -> DBPool -> ResponderM a
importOccupancies sessions pool = do
  uId <- requireSession sessions
  mId <- param "mId"

  withDB pool (findMeetingById mId) >>= \case
    Just meeting -> do
      let firstDay = UTCTime (utctDay $ minimum meeting.mDays) (utctDayTime meeting.mStart)
      let lastDay = UTCTime (utctDay $ maximum meeting.mDays) (utctDayTime meeting.mEnd)

      occupancies <- withDB pool (findUserOccupancies uId)
      case determineAvailabilites firstDay lastDay occupancies uId of
        Just availabilities -> do
          let ownAvails = filter (\a -> a.aUserId == uId) meeting.mAvailabilities
          let splittedAvails = splitAvailabilities availabilities
          withDB pool (updateAvailabilities mId uId (splittedAvails ++ ownAvails))
          send $ status status200 $ text "Done."
        Nothing -> send $ status status500 $ text "problem"
    Nothing -> send $ status status400 $ text "Not a meeting"
