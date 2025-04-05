{-# LANGUAGE LambdaCase #-}

module Presentation.User where

import Web.Twain (ResponderM, send, text, status, status400, json)
import Utils.Datatypes (User (name), Sessions)
import Presentation.Authentication (requireSession, LoginResponse (LoginResponse))
import Integration.UserStore (findUserById)
import Data.Text (pack)
import Utils.Endpoint (DBPool, withDB)

getUserMe :: Sessions -> DBPool -> ResponderM a
getUserMe sessions pool = do
  -- Find ID from the session cookie
  uId <- requireSession sessions

  -- Grab associated user
  withDB pool (findUserById uId) >>= \case
    -- If there is no user, return 400 bad request (Shouldn't be able to happen)
    Nothing -> send $ status status400 $ text "User does not exist."
    Just user -> send $ json $ LoginResponse (name user) (pack $ show uId)
