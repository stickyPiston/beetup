module Main where

import qualified Network.Wai.Handler.Warp as Warp
import Web.Twain
import Data.IORef (newIORef)
import qualified Data.Map as M

import Integration.Init (initDB)
import Presentation.Authentication (register, logout, login)
import Presentation.User (getUserMe)
import Presentation.Calendar (importUserCalendar)
import Presentation.Occupancies (getUserOccupancies)
import Presentation.Meeting (createMeeting, addAvailabilitiesToMeeting, getMeetingWithId)

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
      , post "/calendar" (importUserCalendar sessions)
      , post "/meeting" (createMeeting sessions)
      , put  "/meeting/:mId" (addAvailabilitiesToMeeting sessions)
      , get "/user" (getUserMe sessions)
      , get "/logout" (logout sessions)
      , get "/occupancies" (getUserOccupancies sessions)
      , get "/meeting/:mId" (getMeetingWithId sessions)
      ]

missing :: ResponderM a
missing = send $ html "Not found..."
