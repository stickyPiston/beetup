module Presentation.Authentication where

import Web.Twain
import Data.Aeson
import Data.Text (Text)
import Utils.Datatypes
import Integration.Datastore (findUserByUsername, insertUser, User (User))
import Data.Password.Bcrypt
import Control.Monad.Cont (MonadIO(liftIO))


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

register :: Sessions
         -> ResponderM a
register sessions = do
  RegisterParams uname (mkPassword -> password) name <- fromBody

  -- Check if user exists
  maybeUser <- liftIO $ findUserByUsername uname

  case maybeUser of
    Just u -> send $ status status400 $ text "User already exists."
    Nothing -> do
      hashedPassword <- liftIO $ hashPassword password

      userId <- liftIO $ insertUser $ User name uname (unPasswordHash hashedPassword)
      
      send $ status status201 $ json $ userId

  -- Hash password for security reasons


  



login :: Sessions     -- Session information
       -> ResponderM a -- Response
login = undefined
-- login2 sessions = do
--   -- Find login inforamtion inside request body
--   LoginParams username (mkPassword -> password) <- fromBody

--   -- Find user inside database
--   selectUser 
--   -- If not found, error

--   -- If found, return session token inside header & cookie