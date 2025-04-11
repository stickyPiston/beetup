module Presentation.Calendar where

import Utils.Datatypes (Sessions)
import Presentation.Authentication (requireSession)
import Availability.Parse (parseOccupancies)
import qualified Data.Text as T
import Integration.OccupancyStore (storeUserOccupancies)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Twain (ResponderM, send, status, status400, text, status200, request, strictRequestBody)
import Utils.Endpoint (DBPool, withDB)

importUserCalendar :: Sessions -> DBPool -> ResponderM a
importUserCalendar sessions pool = do
  -- Find ID from the session cookie
  uId <- requireSession sessions

  req <- request
  body <- liftIO $ strictRequestBody req

  case parseOccupancies body of
    -- File cannot be parsed correcly, return 400 with error
    Left e        -> send $ status status400 $ text $ T.pack e
    -- File can be parsed correctly, store it in the database
    Right (w, os) -> do
      _ <- withDB pool $ storeUserOccupancies uId os
      
      send $ status status200 $ text $ T.concat $ map T.pack w
