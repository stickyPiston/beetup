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
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import qualified Data.Map as M
import Data.UUID (UUID, fromText, toText)
import Data.UUID.V4 (nextRandom)

type Sessions = IORef (M.Map UUID Text)

main :: IO ()
main = do
  conn <- connectSqlite3 "main.db"
  sessions <- newIORef M.empty
  initDB conn

  Warp.run 8001 $
    foldr ($)
      (notFound missing)
      [ post "/login" (login sessions conn)
      , get "/user" (user sessions conn)
      , get "/logout" (logout sessions)
      ]

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

login :: Sessions -> Connection -> ResponderM a
login sessions conn = do
  LoginParams username (mkPassword -> password) <- fromBody
  foundUsers <- liftIO $ quickQuery'
    conn
    "SELECT id, name, password FROM users WHERE username = ?"
    [toSql username]
  case foundUsers of
    [map fromSql -> [id, name, PasswordHash -> pwHash]]
      | checkPassword password pwHash == PasswordCheckSuccess -> do
        sessionID <- liftIO nextRandom
        liftIO $ modifyIORef' sessions $ M.insert sessionID id
        respond $ withCookie "SESSION" (toText sessionID) $ json $ LoginResponse name (pack $ show id)
    _ -> respond $ status unauthorized401 $ html ""
  where respond = send . withHeader ("Access-Control-Allow-Origin", "*")

requireSession :: Sessions -> ResponderM Text
requireSession sessions = do
  sessionCookie <- cookieParamMaybe "SESSION"
  sessionMap <- liftIO $ readIORef sessions
  case sessionCookie >>= fromText >>= (sessionMap M.!?) of
    Just userID -> return userID
    Nothing -> send $ status unauthorized401 $ html ""

user :: Sessions -> Connection -> ResponderM a
user sessions conn = do
  userID <- requireSession sessions
  result <- liftIO $ quickQuery' conn "SELECT name FROM users WHERE id = ?" [toSql userID]
  case result of
    [map fromSql -> [name]] -> send $ json $ LoginResponse name userID
    _ -> next

logout :: Sessions -> ResponderM a
logout sessions = do
  cookie <- cookieParamMaybe "SESSION"
  case cookie >>= fromText of
    Just sessionID ->
      liftIO $ modifyIORef' sessions (M.delete sessionID)
    _ -> return ()
  send $ raw status200 [] ""

missing :: ResponderM a
missing = send $ html "Not found..."