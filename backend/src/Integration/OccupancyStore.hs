module Integration.OccupancyStore where

import Database.Persist (selectList, (==.), PersistStoreWrite (insert))
import Utils.DbInit (UserEntityId, EntityField (OccupancyEntityUserId))
import Database.Persist.Sql (toSqlKey)
import Control.Monad (void)
import Utils.Datatypes (UserId, Occupancy)
import Utils.Functions (entityToOccupancy, occupancyToEntity)
import Utils.Endpoint (SqlQuery)

-- Function to find user occupancies by user ID
findUserOccupancies :: UserId -- ^ UserId of the user whos occupancies are queried 
                    -> SqlQuery [Occupancy]
findUserOccupancies uId =
  -- Convert int to UserEntityId
  let userId = toSqlKey (fromIntegral uId) :: UserEntityId
      entitiesQuery = selectList [OccupancyEntityUserId ==. userId] []
   in map entityToOccupancy <$> entitiesQuery

-- | Adds an occupancy to the given user
storeUserOccupancy :: UserId -- ^ Id of the user
                   -> Occupancy -- ^ Occupancy that needs to be added
                   -> SqlQuery ()
storeUserOccupancy uId o =
  -- Convert int to UserEntityId
  let userId = toSqlKey (fromIntegral uId) :: UserEntityId
      entity = occupancyToEntity o userId
   in void $ insert entity

-- | Stores multiple occupancies at the same time
--   Maps over storeUserOccupancy.
storeUserOccupancies :: UserId -- ^ Id of the user
                     -> [Occupancy] -- ^ List of the occupancies that need to be added
                     -> SqlQuery ()
storeUserOccupancies uId = mapM_ (storeUserOccupancy uId) 
