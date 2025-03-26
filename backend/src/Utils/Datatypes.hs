module Utils.Datatypes where

import Data.IORef (IORef)
import qualified Data.Map as M
import Data.UUID (UUID)
import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Clock (UTCTime)
import Text.ICalendar (Date)
import Data.Text

type Sessions = IORef (M.Map UUID Int)

-- | Represents a user in the system
data User = User { id       :: Int -- ^ unique id
                 , name     :: Text -- ^ Real name
                 , username :: Text -- ^ unique username
                 , password :: Text -- ^ Hashed password
                 }

-- | Represents a time slot in which a user is available.
--
-- Note that it does not store for which user this availability is.
--
-- Also note that on a single day, multiple availabilities can exist,
-- typically interspersed by a user's occupancy.
data Availability = Availability { aStart :: UTCTime -- ^ When does this availabiliy start?
                                 , aEnd   :: UTCTime -- ^ When does this availabiliy end?
                                 } deriving (Show, Eq, Ord)

-- | Represents a time slot in which a user is /not/ available.
--
-- Note that it does not store for which user this occupancy is.
--
-- Also note that on a single day, multiple occupancies can exist,
-- typically interspersed by a user's availabilities.
data Occupancy = Occupancy { oTitle :: String -- ^ The title of the event, intended to be only visible to the user
                           , oStart :: UTCTime -- ^ When does this occupancy start?
                           , oEnd   :: UTCTime -- ^ When does this occupancy end?
                           } deriving (Show)

-- | Some temporary identifier for a user
type UserId = Int

-- | Represents a single timeslot of half an hour
data TimeSlot = TimeSlot Date TimeOfDay deriving (Show)

instance Eq Occupancy where
  -- All fields but title considered
  a == b = oStart a == oStart b && oEnd a == oEnd b

instance Ord Occupancy where
  -- All fields but title considered
  a <= b = oStart a < oStart b || oStart a == oStart b && oEnd a <= oEnd b
