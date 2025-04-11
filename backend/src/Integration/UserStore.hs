module Integration.UserStore where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite (toSqlKey, fromSqlKey)
import Utils.DbInit
import Utils.Datatypes (User, UserId)
import Utils.Functions (entityToUser)
import Utils.Endpoint (SqlQuery)

-- | Finds a user by their username
findUserByUsername :: Text -- ^ Username of the user that is queried 
                   -> SqlQuery (Maybe User)
findUserByUsername uname = fmap entityToUser <$> selectFirst [UserEntityUsername ==. uname] []

-- | Finds a user by their Id
findUserById :: UserId -- Id of the user that is queried
             -> SqlQuery (Maybe User)
findUserById id = fmap entityToUser <$> selectFirst [UserEntityId ==. toSqlKey (fromIntegral id)] []

-- | Adds a user to the database
-- | Returns the generated Id of that user
insertUser :: UserEntity -- Entity of the user that is stored
           -> SqlQuery UserId
insertUser u = fromIntegral . fromSqlKey <$> insert u
