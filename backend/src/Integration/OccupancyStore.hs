module Integration.OccupancyStore where

import Database.Persist (selectList, (==.), PersistStoreWrite (insert))
import Integration.Init (UserEntityId, EntityField (OccupancyEntityUserId))
import Database.Persist.Sql (toSqlKey)
import Database.Persist.Sqlite (runSqlite)
import Utils.Datatypes (UserId, Occupancy)
import Utils.Functions (entityToOccupancy, occupancyToEntity)

-- Function to find user occupancies by user ID
findUserOccupancies :: UserId -> IO (UserId, [Occupancy])
findUserOccupancies uId = runSqlite "main.db" $ do
  
  -- Convert int to UserEntityId
  let userId = toSqlKey (fromIntegral uId) :: UserEntityId
  
  os <- selectList [OccupancyEntityUserId ==. userId] []

  return (uId, map entityToOccupancy os)

storeUserOccupancy :: UserId -> Occupancy -> IO ()
storeUserOccupancy uId o = runSqlite "main.db" $ do
  
  -- Convert int to UserEntityId
  let userId = toSqlKey (fromIntegral uId) :: UserEntityId

  let entity = occupancyToEntity o userId
  
  _ <- insert entity
  return ()
