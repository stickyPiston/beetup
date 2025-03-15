module Main where

import qualified Network.Wai.Handler.Warp as Warp
import Web.Twain
import Data.IORef (newIORef)
import qualified Data.Map as M

import Integration.Datastore (initDB)
import Presentation.Authentication (register, logout, login)
import Presentation.User (getUserMe)

main :: IO ()
main = do
  sessions <- newIORef M.empty
  initDB

  Warp.run 8001 $
    foldr ($)
      (notFound missing)
      [ 
        post "/login" (login sessions)
      , post "/register" register
      , get "/user" (getUserMe sessions)
      , get "/logout" (logout sessions)
      ]

missing :: ResponderM a
missing = send $ html "Not found..."
