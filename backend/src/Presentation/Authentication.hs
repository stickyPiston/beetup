module Presentation.Authentication where

import Web.Twain
import Data.Aeson
import Data.Text (Text, pack)
import Utils.Datatypes as DT
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
import Data.Maybe (fromJust)
import Integration.UserStore (findUserByUsername, insertUser)
import Integration.Init (UserEntity(UserEntity))

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

requireSession :: Sessions -> ResponderM UserId
requireSession sessions = do
  sessionCookie <- cookieParamMaybe "SESSION"
  sessionMap <- liftIO $ readIORef sessions
  case sessionCookie >>= fromText >>= (sessionMap M.!?) of
    Just userID -> return userID
    Nothing -> send $ status status401 $ text "Unauthorized."

register :: ResponderM a
register = do
  RegisterParams uname (mkPassword -> password) name <- fromBody

  -- Check if user exists
  maybeUser <- liftIO $ findUserByUsername uname
  whenJust maybeUser (\_ -> send $ status status400 $ text "User already exists." )

  -- Hash password for security reasons
  hashedPassword <- liftIO $ hashPassword password

  -- Register user in database
  userId <- liftIO $ insertUser $ UserEntity name uname (unPasswordHash hashedPassword)
  
  sessionID <- liftIO nextRandom

  -- Send 201 Created
  send 
    $ status status201
    $ withCookie "SESSION" (toText sessionID) 
    $ withHeader ("token", toASCIIBytes sessionID) 
    $ json userId

logout :: Sessions -> ResponderM a
logout sessions = do
  cookie <- cookieParamMaybe "SESSION"
  case cookie >>= fromText of
    Just sessionID ->
      liftIO $ modifyIORef' sessions (M.delete sessionID)
    _ -> return ()
  send $ raw status200 [] ""

login :: Sessions     -- Session information
       -> ResponderM a -- Response
login sessions = do
  LoginParams uname (mkPassword -> password) <- fromBody

  -- Grab the user
  maybeUser <- liftIO $ findUserByUsername uname

  -- If the user doesn't exist, return 400 bad request
  whenNothing maybeUser (send $ status status400 $ text "User does not exist.")

  let user = fromJust maybeUser
      userId = DT.id user
      hash = DT.password user

  -- Do a password check
  if checkPassword password (PasswordHash hash) == PasswordCheckSuccess
    then do
      -- If the passwords mach, return a new session ID
      sessionID <- liftIO nextRandom
      liftIO $ modifyIORef' sessions $ M.insert sessionID userId

      send 
        $ withCookie "SESSION" (toText sessionID) 
        $ json $ LoginResponse (DT.name user) (pack $ show userId)
    else do
      -- If they don't match, return 401 unauthorized
      send 
        $ status status401
        $ text "Incorrect password"
