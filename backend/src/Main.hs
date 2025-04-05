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

import Database.Persist.Sqlite (createSqlitePool)
import Control.Monad.Logger (runNoLoggingT)

main :: IO ()
main = do
  sessions <- newIORef M.empty
  pool <- runNoLoggingT $ createSqlitePool "main.db" 10

  initDB pool

  Warp.run 8001 $
    foldr ($)
      (notFound missing)
      [ post "/login" (login sessions pool)
      , post "/register" (register pool)
      , post "/calendar" (importUserCalendar sessions pool)
      , post "/meeting" (createMeeting sessions pool)
      , put  "/meeting/:mId" (addAvailabilitiesToMeeting sessions pool)
      , get "/user" (getUserMe sessions pool)
      , get "/logout" (logout sessions)
      , get "/occupancies" (getUserOccupancies sessions pool)
      , get "/meeting/:mId" (getMeetingWithId pool)
      ]

missing :: ResponderM a
missing = send $ html "Not found..."
