module Presentation.Occupancies where
import Utils.Datatypes (Sessions, Occupancy (Occupancy))
import Web.Twain (ResponderM, send, json)
import Presentation.Authentication (requireSession)
import Integration.OccupancyStore (findUserOccupancies)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (ToJSON (toJSON), object, (.=))

data OccupancyResponse = OccupancyResponse 
                       { title :: Text
                       , start :: Text
                       , end :: Text
                       }

instance ToJSON Occupancy where
  toJSON (Occupancy t s e) = object ["title" .= t, "start" .= s, "end" .= e]

getUserOccupancies :: Sessions -> ResponderM m
getUserOccupancies sessions =do
  -- Find ID from the session cookie
  uId <- requireSession sessions

  os <- liftIO $ findUserOccupancies uId

  send $ json os