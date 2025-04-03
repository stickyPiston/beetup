module Integration.MeetingStore where
import Utils.Datatypes (Meeting, MeetingId, Availability)
import Database.Persist.Sqlite (runSqlite, PersistStoreWrite (insert, update), PersistQueryRead (selectFirst), toSqlKey, PersistQueryWrite (updateWhere), Entity (entityKey))
import Database.Persist ((==.), (=.))
import Utils.Functions (meetingToEntity, entityToMeeting, availabilityToEntity, whenNothing)
import Integration.Init (MeetingEntityId, EntityField(MeetingEntityMeetingId, MeetingEntityAvailabilities), MeetingEntity (meetingEntityMeetingId))
import Control.Monad.IO.Class (MonadIO(liftIO))

storeMeeting :: Meeting -> IO ()
storeMeeting m = runSqlite "main.db" $ do

  let entity = meetingToEntity m
  
  _ <- insert entity
  return ()

findMeetingById :: MeetingId -> IO (Maybe Meeting)
findMeetingById id = runSqlite "main.db" $ do
  
  meeting <- selectFirst [MeetingEntityMeetingId ==. id] []

  return $ fmap entityToMeeting meeting

updateAvailabilities :: MeetingId -> [Availability] -> IO (Maybe Meeting)
updateAvailabilities mId as = runSqlite "main.db" $ do  
  mMeeting <- selectFirst [MeetingEntityMeetingId ==. mId] []

  case mMeeting of  
    Nothing -> return Nothing
    Just meeting ->do
      update (entityKey meeting) [MeetingEntityAvailabilities =. map availabilityToEntity as]
  
      liftIO $ findMeetingById mId