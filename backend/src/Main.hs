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
import Data.UUID (UUID, fromText, toText, toASCIIBytes)
import Data.UUID.V4 (nextRandom)

import Presentation.Login
import Integration.Database
import Utils.Datatypes

main :: IO ()
main = do
  conn <- connectSqlite3 "main.db"
  sessions <- newIORef M.empty
  initDB conn

  Warp.run 8001 $
    foldr ($)
      (notFound missing)
      [ post "/login" (login sessions conn)
      , post "/register" (register sessions conn)
      , get "/user" (user sessions conn)
      , get "/logout" (logout sessions)
      ]

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

data RegisterParams = RegisterParams
  { username :: Text
  , password :: Text
  , name :: Text
  }

instance FromJSON RegisterParams where
  parseJSON = withObject "RegisterParams" $ \ o -> RegisterParams
    <$> o .: "username"
    <*> o .: "password"
    <*> o .: "name"

register :: Sessions -> Connection -> ResponderM a
register sessions conn = do
  RegisterParams username (mkPassword -> password) name <- fromBody
  -- TODO: Validation
  hashedPassword <- liftIO $ hashPassword password

  _ <- liftIO $ run
    conn
    "INSERT INTO users (username, password, name) VALUES (?, ?, ?)"
    [toSql username, toSql (unPasswordHash hashedPassword), toSql name]
  
  liftIO $ commit conn

  users <- liftIO $
    quickQuery' conn "SELECT id, name FROM users WHERE username = ?" [toSql username]

  case users of
    [map fromSql -> [id, name]] -> do
      sessionID <- liftIO nextRandom
      liftIO $ modifyIORef' sessions $ M.insert sessionID id
      send $ withHeader ("token", toASCIIBytes sessionID) $ withCookie "SESSION" (toText sessionID) $ json $ LoginResponse name (pack $ show id)
    _ -> send $ status status400 $ text "User already exists"

missing :: ResponderM a
missing = send $ html "Not found..."
