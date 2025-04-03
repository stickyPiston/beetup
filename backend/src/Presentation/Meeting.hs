module Presentation.Meeting where
import Utils.Datatypes (Sessions, Meeting (Meeting, mTitle, mStart, mEnd, mUserId, mAvailabilities), UserId, Availability (Availability), MeetingId)
import Web.Twain (ResponderM, send, fromBody, status, status200, json, text, status400, param)
import Presentation.Authentication (requireSession)
import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)
import Data.Text (pack, Text)
import Data.Aeson ((.=), (.:), FromJSON (parseJSON), withObject, object, ToJSON)
import Data.Time (UTCTime)
import Integration.MeetingStore (storeMeeting, findMeetingById, updateAvailabilities)
import Utils.Functions (whenNothing)
import Data.Maybe (fromJust)
import Web.Twain.Types (ResponderM(ResponderM))
import Data.Aeson.Types (ToJSON(toJSON))

data MeetingParams = MeetingParams {
    title :: Text,
    start :: UTCTime,
    end   :: UTCTime
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

data AvailabilityParams = AvailabilityParams {
  start :: UTCTime,
  end   :: UTCTime
}

data MeetingResponse = MeetingResponse {
  meetingId :: MeetingId,
  title :: Text,
  start :: Text,
  end :: Text,
  userId :: UserId,
  availabilities :: [AvailabilityResponse]
}

data AvailabilityResponse = AvailabilityResponse {
  start :: Text,
  end :: Text,
  userId :: UserId
}

instance ToJSON AvailabilityResponse where
  toJSON (AvailabilityResponse s e id) = object ["start" .= s, "end" .= e, "userId" .= id]

instance ToJSON MeetingResponse where
  toJSON (MeetingResponse mId t s e uId as) = object ["meetingId" .= mId, "title" .= t, "start" .= s, "end" .= e, "userId" .= uId, "availabilities" .= as]

instance FromJSON AvailabilityParams where
  parseJSON = withObject "AvailabilityParams" $ \ o -> AvailabilityParams
    <$> o .: "start"
    <*> o .: "end"

meetingToResponse :: Meeting -> MeetingResponse
meetingToResponse (Meeting mId t s e uId as) = MeetingResponse 
  mId 
  t 
  (pack $ show s) 
  (pack $ show e)
  uId 
  (map availabilityToResponse as)

availabilityToResponse :: Availability -> AvailabilityResponse
availabilityToResponse (Availability s e uId) = AvailabilityResponse (pack $ show s) (pack $ show e) uId

createMeeting :: Sessions -> ResponderM a
createMeeting sessions = do
  MeetingParams title start end <- fromBody

  uId <- requireSession sessions
  meetingId <- liftIO nextRandom

  let meeting = Meeting (pack $ show meetingId) title start end uId []
  
  _ <- liftIO $ storeMeeting meeting

  send $ status status200 $ json $ object ["meetingId" .= meetingId]

addAvailabilitiesToMeeting :: Sessions -> ResponderM a
addAvailabilitiesToMeeting sessions = do
  uId <- requireSession sessions
  mId <- param "mId"
  aps <- fromBody :: ResponderM [AvailabilityParams]

  maybeMeeting <- liftIO $ findMeetingById mId
  whenNothing maybeMeeting (send $ status status400 $ text "No such meeting")

  let m = fromJust maybeMeeting

  mNewMeeting <- liftIO $ updateAvailabilities mId (mAvailabilities m ++ availabilityParamsToAvailabilities uId aps)
  whenNothing mNewMeeting (send $ status status400 $ text "Couldn't fetch updated meeting")  

  send $ status status200 $ text "Done."

getMeetingWithId :: Sessions -> ResponderM a
getMeetingWithId _ = do
  mId <- param "mId"
  
  maybeMeeting <- liftIO $ findMeetingById mId
  whenNothing maybeMeeting (send $ status status400 $ text "No such meeting")

  send $ status status200 $ json (meetingToResponse $ fromJust maybeMeeting)


availabilityParamsToAvailabilities :: UserId -> [AvailabilityParams] -> [Availability]
availabilityParamsToAvailabilities uId = map (\ (AvailabilityParams s e) -> Availability s e uId)
