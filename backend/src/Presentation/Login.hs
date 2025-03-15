module Presentation.Login where

import Web.Twain
import Data.Aeson
import Data.Text (Text)
import Utils.Datatypes


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