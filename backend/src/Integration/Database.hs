module Integration.Database (initDB) where

import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Database.HDBC (IConnection(commit, runRaw), quickQuery', toSql, fromSql, run)
import Data.UUID (UUID)
import Control.Monad (when)
import Data.Password.Bcrypt
import Domain.User

initDB :: Connection -> IO ()
initDB conn = do
  runRaw conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, username TEXT, password TEXT, name TEXT)"
  [[fromSql -> userCount]] <- quickQuery' conn "SELECT COUNT(*) FROM users WHERE username = \"robin\"" []
  when (userCount == (0 :: Int)) $ do
    hash <- hashPassword "hello"
    _ <- run
      conn
      "INSERT INTO users (username, password, name) VALUES (\"robin\", ?, \"Robin-Lynn\")"
      [toSql $ unPasswordHash hash]
    return ()
  commit conn

selectUser :: Connection  -- database connection
           -> String      -- username
           -> Maybe User  -- return user if found
selectUser = undefined

selectAllUsers :: Connection -> Maybe [User]
selectAllUsers = undefined

insertUser :: Connection -> User -> Maybe User
insertUser = undefined