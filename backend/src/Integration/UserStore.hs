module Integration.UserStore where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite (toSqlKey, fromSqlKey)
import Integration.Init
import Utils.Datatypes (User, UserId)
import Utils.Functions (entityToUser)
import Utils.Endpoint (SqlQuery)

findUserByUsername :: Text -> SqlQuery (Maybe User)
findUserByUsername uname = fmap entityToUser <$> selectFirst [UserEntityUsername ==. uname] []

findUserById :: Int -> SqlQuery (Maybe User)
findUserById id = fmap entityToUser <$> selectFirst [UserEntityId ==. toSqlKey (fromIntegral id)] []

insertUser :: UserEntity -> SqlQuery UserId
insertUser u = fromIntegral . fromSqlKey <$> insert u
