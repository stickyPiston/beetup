module Integration.UserStore where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite (runSqlite, toSqlKey)
import Integration.Init
import Utils.Datatypes (User)
import Utils.Functions (entityToUser)

findUserByUsername :: Text -> IO (Maybe User)
findUserByUsername uname = do
  e <- runSqlite "main.db" $ selectFirst [UserEntityUsername ==. uname] []
  
  return $ fmap entityToUser e

findUserById :: Int -> IO (Maybe User)
findUserById id = do
  e <- runSqlite "main.db" $ selectFirst [UserEntityId ==. toSqlKey (fromIntegral id)] []
  
  return $ fmap entityToUser e

insertUser :: UserEntity -> IO UserEntityId
insertUser u = runSqlite "main.db" $ insert u
