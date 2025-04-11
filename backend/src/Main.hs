module Main where

import qualified Network.Wai.Handler.Warp as Warp
import Web.Twain
import Data.IORef (newIORef)
import qualified Data.Map as M

import Utils.DbInit (initDB)
import Presentation.Authentication (register, logout, login)
import Presentation.User (getUserMe)
import Presentation.Calendar (importUserCalendar)
import Presentation.Occupancies (getUserOccupancies)
import Presentation.Meeting (createMeeting, addAvailabilitiesToMeeting, getMeetingWithId, getOwnMeetings, addUserToMeeting)

import Database.Persist.Sqlite (createSqlitePool)
import Control.Monad.Logger (runNoLoggingT)

-- | Main function that starts up the web server
main :: IO ()
main = do
  -- Create new sessions map
  sessions <- newIORef M.empty

  -- Start up the database
  pool <- runNoLoggingT $ createSqlitePool "main.db" 10

  initDB pool

  -- Serve the specified endpoints on port 8001
  Warp.run 8001 $
    foldr ($)
      (notFound missing)
      [ post "/login" (login sessions pool)
      , post "/register" (register pool)
      , post "/calendar" (importUserCalendar sessions pool)
      , post "/meeting" (createMeeting sessions pool)
      , post "/meeting/:mId" (addAvailabilitiesToMeeting sessions pool)
      , get "/user" (getUserMe sessions pool)
      , get "/logout" (logout sessions)
      , get "/occupancies" (getUserOccupancies sessions pool)
      , get "/meeting/:mId" (getMeetingWithId sessions pool)
      , get "/meeting" (getOwnMeetings sessions pool)
      , post "/meeting/:mId/addUser" (addUserToMeeting sessions pool)
      ]

-- | Backup endpoint if there is no endpoint found
missing :: ResponderM a
missing = send $ html "Not found..."
