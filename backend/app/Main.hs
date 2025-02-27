module Main where

import Network.Wai.Handler.Warp (run)
import Web.Twain
import Data.Aeson
import Data.Text (Text, pack)
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Database.HDBC (IConnection(commit, runRaw), quickQuery', toSql, fromSql)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  conn <- connectSqlite3 "main.db"
  initDB conn

  run 8001 $
    foldr ($)
      (notFound missing)
      [ post "/login" (login conn) ]

initDB :: Connection -> IO ()
initDB conn = do
  runRaw conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, username TEXT, password TEXT, name TEXT)"
  [[fromSql -> userCount]] <- quickQuery' conn "SELECT COUNT(*) FROM users WHERE username = \"robin\"" []
  when (userCount == (0 :: Int)) $ do
    runRaw conn "INSERT INTO users (username, password, name) VALUES (\"robin\", \"hello\", \"Robin-Lynn\")"
  commit conn

data LoginParams = LoginParams
  { username :: Text
  , password :: Text
  }

instance FromJSON LoginParams where
  parseJSON = withObject "LoginParams" $ \ o -> LoginParams
    <$> o .: "username"
    <*> o .: "password"

data LoginResponse = LoginResponse
  { name :: Text
  , id :: Text
  }

instance ToJSON LoginResponse where
  toJSON (LoginResponse name id) = object ["name" .= name, "id" .= id]

login :: Connection -> ResponderM a
login conn = do
  LoginParams username password <- fromBody
  foundUsers <- liftIO $ quickQuery'
    conn
    "SELECT id, name FROM users WHERE username = ? AND password = ?"
    [toSql username, toSql password]
  case foundUsers of
    [map fromSql -> [id, name]] -> respond $ json $ LoginResponse name (pack $ show id)
    _ -> respond $ status unauthorized401 $ html ""
  where respond = send . withHeader ("Access-Control-Allow-Origin", "*")

missing :: ResponderM a
missing = send $ html "Not found..."