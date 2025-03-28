{-# LANGUAGE InstanceSigs #-}
module Utils.Datatypes where

import Data.IORef (IORef)
import qualified Data.Map as M
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime, NominalDiffTime, secondsToNominalDiffTime, addUTCTime, diffUTCTime)
import Control.Monad (liftM2)

type Sessions = IORef (M.Map UUID Int)

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

-- | Represents a single timeslot of arbitrary length
data TimeSlot = TimeSlot { tStart :: UTCTime -- ^ Start timestamp of the @TimeSlot@
                         , duration :: NominalDiffTime -- ^ Duration of the @TimeSlot@
                         } deriving (Show, Eq, Ord)

instance Eq Occupancy where
  -- All fields but title considered
  (==) :: Occupancy -> Occupancy -> Bool
  a == b = oStart a == oStart b && oEnd a == oEnd b

instance Ord Occupancy where
  -- All fields but title considered
  (<=) :: Occupancy -> Occupancy -> Bool
  a <= b = oStart a < oStart b || oStart a == oStart b && oEnd a <= oEnd b


-- | Represents datatypes which have a start and end time.
-- Is accompanied with many helpful functions on those times.
class (Ord a, Eq a) => TimeSlice a where
  start :: a -> UTCTime
  end :: a -> UTCTime
  setStart :: a -> UTCTime -> a
  setEnd :: a -> UTCTime -> a

  range :: a -> (UTCTime, UTCTime)
  range a = (start a, end a)

  overlaps, contains :: (TimeSlice b) => a -> b -> Bool
  contains a b = start a <= start b && end a >= end b
  overlaps a b = end a > start b && end a <= end b
                 || start b < end a && start b >= start a
  earlier, later :: (TimeSlice b) => a -> b -> Bool
  a `earlier` b = start a < start b
  a `later` b = start a > start b

  valid :: a -> Bool -- TODO can we prevent needing these cases, agda style?
  valid a = start a < end a
  length :: a -> NominalDiffTime
  length = uncurry (flip diffUTCTime) . range


-- | Determines the disjunction of two @TimeSlice@s.
-- Should be interpreted as the second timeslice being subtracted
-- from the first @TimeSlice@. The result is thus alterations of the first
-- @TimeSlice@.
disj :: (TimeSlice a, TimeSlice b) => a -> b -> Maybe [a]
disj original substr | not $ valid original = Nothing
                     | not $ valid substr   = Nothing
                     | not $ original `overlaps` substr = Just [original]
                     | substr `contains` original = Just []
                     | original `contains` substr = Just [setEnd original $ start substr, setStart original $ end substr]
                     | original `earlier` substr = Just [setStart original $ end substr]
                     | otherwise = Just [setEnd original $ start substr]

-- | Determines the conjunction of two @TimeSlice@s.
-- Should be interpreted as the second timeslice being appended
-- to the first @TimeSlice@. The result is thus alterations of the first
-- @TimeSlice@.
conj :: (TimeSlice a, TimeSlice b) => a -> b -> Maybe a
conj left right | not $ left `overlaps` right = Nothing
                | left `contains` right = Just left
                | left `earlier` right = Just $ setEnd left (end right)
                | otherwise = Just $ setStart left (start right)

instance TimeSlice Availability where
  start = aStart
  end = aEnd
  setStart a t = a { aStart = t }
  setEnd a t = a { aEnd = t  }

instance TimeSlice Occupancy where
  start = oStart
  end = oEnd
  setStart a t = a { oStart = t }
  setEnd a t = a { oEnd = t  }

instance TimeSlice TimeSlot where
  start = tStart
  end = liftM2 addUTCTime duration tStart
  setStart ts t = ts { tStart = t }
  setEnd t e = t { duration = diffUTCTime e (tStart t) }


hour, minute, second :: NominalDiffTime
hour   = secondsToNominalDiffTime 60 * 60
minute = secondsToNominalDiffTime 60
second = secondsToNominalDiffTime 1
