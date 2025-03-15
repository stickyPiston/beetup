module Presentation.Login where

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
import Utils.Datatypes

import Integration.Database

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

login2 :: Sessions     -- Session information
       -> Connection   -- Database connection
       -> ResponderM a -- Response
login2 sessions connection = do
  -- Find login inforamtion inside request body
  LoginParams username (mkPassword -> password) <- fromBody

  -- Find user inside database
  selectUser 
  -- If not found, error

  -- If found, return session token inside header & cookie


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
        respond $ withHeader ("token", toASCIIBytes sessionID) $ withCookie "SESSION" (toText sessionID) $ json $ LoginResponse name (pack $ show id)
    _ -> respond $ status unauthorized401 $ html ""
  where respond = send . withHeader ("Access-Control-Allow-Origin", "*")