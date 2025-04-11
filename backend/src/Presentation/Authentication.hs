{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Presentation.Authentication where

import Web.Twain
import Data.Aeson
import Data.Text (Text, pack)
import Utils.Datatypes
import Data.Map as M
import Data.Password.Bcrypt
    ( checkPassword,
      hashPassword,
      mkPassword,
      PasswordCheck(PasswordCheckSuccess),
      PasswordHash(PasswordHash, unPasswordHash) )
import Control.Monad.IO.Class (liftIO)
import Data.UUID (toASCIIBytes, toText, fromText)
import Data.UUID.V4 (nextRandom)
import Data.IORef (modifyIORef', readIORef)
import Utils.Functions
import Integration.UserStore (findUserByUsername, insertUser)
import Utils.DbInit (UserEntity(UserEntity))
import Utils.Endpoint (withDB, DBPool)

-- | Parameters of the login request
data LoginParams = LoginParams
  { username :: Text
  , password :: Text
  }

instance FromJSON LoginParams where
  parseJSON = withObject "LoginParams" $ \ o -> LoginParams
    <$> o .: "username"
    <*> o .: "password"

-- | Response for the login request
data LoginResponse = LoginResponse
  { name :: Text
  , id :: Text
  }

instance ToJSON LoginResponse where
  toJSON (LoginResponse name id) = object ["name" .= name, "id" .= id]

-- | Parameters for the register request
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

-- | Function that checks if there is a session token inside the cookies of the client
--   If no cookie is found, status 401 is returned.
--   Otherwise the Id of the logged in user is returned.
requireSession :: Sessions -- ^ Map of currently logged in sessions
               -> ResponderM UserId
requireSession sessions = do
  -- Grab the session from the cookie
  sessionCookie <- cookieParamMaybe "SESSION"

  -- Grab the session map
  sessionMap <- liftIO $ readIORef sessions
  
  -- Check if the cookie is recognised
  case sessionCookie >>= fromText >>= (sessionMap M.!?) of
    -- If so return the user id
    Just userID -> return userID
    -- If not, send status 401
    Nothing -> send $ status status401 $ text "Unauthorized."

-- | HTTP Post endpoint to register a new user
register :: DBPool -> ResponderM a
register pool = do
  RegisterParams uname (mkPassword -> password) name <- fromBody

  -- Check if user exists
  maybeUser <- withDB pool $ findUserByUsername uname
  whenJust maybeUser (\_ -> send $ status status400 $ text "User already exists." )

  -- Hash password for security reasons
  hashedPassword <- liftIO $ hashPassword password

  -- Register user in database
  userId <- withDB pool $ insertUser $ UserEntity name uname (unPasswordHash hashedPassword)
  
  sessionID <- liftIO nextRandom

  -- Send 201 Created
  send 
    $ status status201
    $ withCookie "SESSION" (toText sessionID) 
    $ withHeader ("token", toASCIIBytes sessionID) 
    $ json userId

-- | HTTP POST endpoint for logging out
logout :: Sessions -> ResponderM a
logout sessions = do
  -- Grab the session cookie
  cookie <- cookieParamMaybe "SESSION"
  
  -- Remove the cookie from the sessions map
  case cookie >>= fromText of
    Just sessionID ->
      liftIO $ modifyIORef' sessions (M.delete sessionID)
    _ -> return ()
  -- Return status 200
  send $ raw status200 [] ""

-- | HTTP Post endpoint for logging in
login :: Sessions -> DBPool -> ResponderM a
login sessions pool = do
  LoginParams uname (mkPassword -> password) <- fromBody

  -- Grab the user
  withDB pool (findUserByUsername uname) >>= \case
    -- If the user doesn't exist, return 400 bad request
    Nothing -> send $ status status400 $ text "User does not exist."

    Just User { id, password = userPassword, name }
      | checkPassword password (PasswordHash userPassword) == PasswordCheckSuccess -> do
        -- If the passwords mach, return a new session ID
        sessionID <- liftIO nextRandom
        liftIO $ modifyIORef' sessions $ M.insert sessionID id

        send 
          $ withCookie "SESSION" (toText sessionID) 
          $ json $ LoginResponse name (pack $ show id)
      -- If not, return 401
      | otherwise -> send $ status status401 $ text "Incorrect password"