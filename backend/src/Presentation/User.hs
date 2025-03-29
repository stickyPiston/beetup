module Presentation.User where

import Web.Twain (ResponderM, send, text, status, status400, json)
import Utils.Datatypes (User (name), Sessions)
import Presentation.Authentication (requireSession, LoginResponse (LoginResponse))
import Integration.UserStore (findUserById)
import Control.Monad.IO.Class (liftIO)
import Utils.Functions (whenNothing)
import Data.Maybe (fromJust)
import Data.Text (pack)

getUserMe :: Sessions -> ResponderM a
getUserMe sessions = do
  -- Find ID from the session cookie
  uId <- requireSession sessions

  -- Grab associated user
  maybeUser <- liftIO $ findUserById uId

  -- If there is no user, return 400 bad request (Shouldn't be able to happen)
  whenNothing maybeUser (send $ status status400 $ text "User does not exist.")
  
  let user = fromJust maybeUser
  -- Return the user
  send $ json $ LoginResponse (name user) (pack $ show uId)
