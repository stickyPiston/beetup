module Utils.Functions where
import Utils.Datatypes (User (password, username, name, User), Occupancy (oTitle, oStart, oEnd, Occupancy), Meeting (Meeting, mTitle, mStart, mEnd, mAvailabilities, mUserId), Availability (Availability, aStart, aEnd, aUserId))
import Data.Time (Day, NominalDiffTime)
import Text.ICalendar (Date (Date))
import Database.Persist (Entity (entityKey, entityVal))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Integration.Init

whenNothing :: Monad m => Maybe a -> m () -> m ()
whenNothing Nothing f = f
whenNothing (Just _) _ = return ()

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()

-- | Given a @NominalDiffTime@, returns a multiple of the @NominalDiffTime@
times :: NominalDiffTime -> Int -> NominalDiffTime
times dt n = iterate (+ dt) dt !! n

dateToDay :: Date -> Day
dateToDay (Date d) = d

userToEntity :: User -> UserEntity
userToEntity u = UserEntity (name u) (username u) (password u)

occupancyToEntity :: Occupancy -> UserEntityId -> OccupancyEntity
occupancyToEntity o = OccupancyEntity (oTitle o) (oStart o) (oEnd o)

entityToUser :: Entity UserEntity -> User
entityToUser e = do
   let id  = fromSqlKey $ entityKey e
   let val = entityVal e
   User 
    (fromIntegral id) 
    (userEntityName val) 
    (userEntityUsername val) 
    (userEntityPassword val)

entityToOccupancy :: Entity OccupancyEntity -> Occupancy
entityToOccupancy e =do 
  let val = entityVal e
  
  Occupancy 
    (occupancyEntityTitle val) 
    (occupancyEntityStart val) 
    (occupancyEntityEnd val)

availabilityToEntity :: Availability -> AvailabilityEntity
availabilityToEntity a = AvailabilityEntity (aStart a) (aEnd a) (toSqlKey $ fromIntegral $ aUserId a)

entityToAvailability :: AvailabilityEntity -> Availability
entityToAvailability e =do
  let val = e
  let uId = fromSqlKey (availabilityEntityUserId val)

  Availability
    (availabilityEntityStart val)
    (availabilityEntityEnd val)
    (fromIntegral uId)

meetingToEntity :: Meeting -> MeetingEntity
meetingToEntity m = MeetingEntity 
  (mTitle m) 
  (mStart m) 
  (mEnd m) 
  (toSqlKey $ fromIntegral $ mUserId m) 
  (map availabilityToEntity $ mAvailabilities m)

entityToMeeting :: Entity MeetingEntity -> Meeting
entityToMeeting e =do
  let val = entityVal e
  let uId = fromSqlKey (meetingEntityUserId val)
  Meeting
    (meetingEntityTitle val)
    (meetingEntityStart val)
    (meetingEntityEnd val)
    (fromIntegral uId)
    (map entityToAvailability $ meetingEntityAvailabilities val)