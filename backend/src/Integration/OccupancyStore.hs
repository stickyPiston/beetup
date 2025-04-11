module Integration.OccupancyStore where

import Database.Persist (selectList, (==.), PersistStoreWrite (insert))
import Utils.DbInit (UserEntityId, EntityField (OccupancyEntityUserId))
import Database.Persist.Sql (toSqlKey)
import Control.Monad (void)
import Utils.Datatypes (UserId, Occupancy)
import Utils.Functions (entityToOccupancy, occupancyToEntity)
import Utils.Endpoint (SqlQuery)

-- Function to find user occupancies by user ID
findUserOccupancies :: UserId -> SqlQuery [Occupancy]
findUserOccupancies uId =
  -- Convert int to UserEntityId
  let userId = toSqlKey (fromIntegral uId) :: UserEntityId
      entitiesQuery = selectList [OccupancyEntityUserId ==. userId] []
   in map entityToOccupancy <$> entitiesQuery

storeUserOccupancy :: UserId -> Occupancy -> SqlQuery ()
storeUserOccupancy uId o =
  -- Convert int to UserEntityId
  let userId = toSqlKey (fromIntegral uId) :: UserEntityId
      entity = occupancyToEntity o userId
   in void $ insert entity

storeUserOccupancies :: UserId -> [Occupancy] -> SqlQuery ()
storeUserOccupancies uId = mapM_ (storeUserOccupancy uId) 
