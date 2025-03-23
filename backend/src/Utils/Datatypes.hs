module Utils.Datatypes where

import Data.IORef (IORef)
import qualified Data.Map as M
import Data.UUID (UUID)
import Data.Time.LocalTime (TimeOfDay)
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
-- typically interspersed by a user's appointment.
data Availability = Availability { date      :: Date -- ^ The date for which the availability holds
                                 , startTime :: TimeOfDay -- ^ When does this time slot start?
                                 , endTime   :: TimeOfDay -- ^ When does this time slot end?
                                 }

-- | Represents a time slot in which a user is /not/ available.
--
-- Note that it does not store for which user this occupancy is.
--
-- Also note that on a single day, multiple occupancies can exist,
-- typically interspersed by a user's availabilities.
data Occupancy = Occupancy { title     :: String -- ^ The title of the event, intended to be only visible to the user
                           , date      :: Date -- ^ The date for which the availability holds
                           , startTime :: TimeOfDay -- ^ When does this time slot start?
                           , endTime   :: TimeOfDay -- ^ When does this time slot end?
                           } deriving (Show)

-- | Some temporary identifier for a user
type UserId = Int

-- | Represents a single timeslot of half an hour
data TimeSlot = TimeSlot Date TimeOfDay TimeOfDay

-- | Represents a single range of time bound to a single day
data TimeRange = TimeRange Date TimeOfDay TimeOfDay

-- | Contains related availabilities of a single user.
-- Typically used to represent all availabilities of the user for a specific meeting.
data UserAvailabilities = UserAvailabilities UserId [Availability]

-- | Contains all calendar events which the user uploaded, trimmed to just the basics.
-- Represents all time slots where the user is not available.
data UserOccupancies = UserOccupancies UserId [Occupancy]
  deriving (Show)
