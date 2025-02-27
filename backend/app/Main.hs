module Main where

import qualified Network.Wai.Handler.Warp as Warp
import Web.Twain
import Data.Aeson
import Data.Text (Text, pack)
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Database.HDBC (IConnection(commit, runRaw), quickQuery', toSql, fromSql, run)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Password.Bcrypt

main :: IO ()
main = do
  conn <- connectSqlite3 "main.db"
  initDB conn

  Warp.run 8001 $
    foldr ($)
      (notFound missing)
      [ post "/login" (login conn) ]

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
  LoginParams username (mkPassword -> password) <- fromBody
  foundUsers <- liftIO $ quickQuery'
    conn
    "SELECT id, name, password FROM users WHERE username = ?"
    [toSql username]
  case foundUsers of
    [map fromSql -> [id, name, PasswordHash -> pwHash]]
      | checkPassword password pwHash == PasswordCheckSuccess ->
        respond $ json $ LoginResponse name (pack $ show id)
    _ -> respond $ status unauthorized401 $ html ""
  where respond = send . withHeader ("Access-Control-Allow-Origin", "*")

missing :: ResponderM a
missing = send $ html "Not found..."