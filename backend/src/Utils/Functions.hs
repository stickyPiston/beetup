module Utils.Functions where
import Integration.Init (UserEntity (UserEntity, userEntityName, userEntityUsername, userEntityPassword), UserEntityId, OccupancyEntity (OccupancyEntity, occupancyEntityTitle, occupancyEntityDate, occupancyEntityStartTime))
import Utils.Datatypes (User (password, username, name, User), Occupancy (title, date, startTime, endTime, Occupancy))
import Data.Text (pack)
import Text.ICalendar (Date (Date))
import Data.Time (Day, NominalDiffTime)
import Database.Persist (Entity (entityKey, entityVal))
import Database.Persist.Sql (fromSqlKey)

whenNothing :: Monad m => Maybe a -> m () -> m ()
whenNothing Nothing f = f
whenNothing (Just _) _ = return ()

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()

dateToDay :: Date -> Day
dateToDay (Date d) = d

userToEntity :: User -> UserEntity
userToEntity u = UserEntity (name u) (username u) (password u)

occupancyToEntity :: Occupancy -> UserEntityId -> OccupancyEntity
occupancyToEntity o = OccupancyEntity (pack $ title o) (dateToDay $ date o) (startTime o) (endTime o)

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
    (show $ occupancyEntityTitle val) 
    (Date $ occupancyEntityDate val) 
    (occupancyEntityStartTime val) 
    (occupancyEntityStartTime val)

-- | Given a @NominalDiffTime@, returns a multiple of the @NominalDiffTime@
times :: NominalDiffTime -> Int -> NominalDiffTime
times dt n = iterate (+ dt) dt !! n

