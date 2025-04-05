module Presentation.Occupancies where

import Utils.Datatypes (Sessions, Occupancy (Occupancy))
import Web.Twain (ResponderM, send, json)
import Presentation.Authentication (requireSession)
import Integration.OccupancyStore (findUserOccupancies)
import Data.Aeson (ToJSON (toJSON), object, (.=))
import Utils.Endpoint (withDB, DBPool)

instance ToJSON Occupancy where
  toJSON (Occupancy t s e) = object ["title" .= t, "start" .= s, "end" .= e]

getUserOccupancies :: Sessions -> DBPool -> ResponderM a
getUserOccupancies sessions pool = do
  uId <- requireSession sessions
  os <- withDB pool $ findUserOccupancies uId
  send $ json os