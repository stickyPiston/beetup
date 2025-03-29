module Utils.Functions where
import Utils.Datatypes (User (password, username, name, User), Occupancy (oTitle, oStart, oEnd, Occupancy))
import Data.Text (pack)
import Data.Time (Day, NominalDiffTime)
import Text.ICalendar (Date (Date))
import Database.Persist (Entity (entityKey, entityVal))
import Database.Persist.Sql (fromSqlKey)
import Integration.Init
import Data.Time (NominalDiffTime)

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
occupancyToEntity o = OccupancyEntity (pack $ oTitle o) (oStart o) (oEnd o)

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
    (occupancyEntityStart val) 
    (occupancyEntityEnd val)
