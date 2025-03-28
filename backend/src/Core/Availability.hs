module Core.Availability (determineAvailabilites, substractOccupancy, timeSlots) where

import Data.Time (UTCTime, NominalDiffTime, addUTCTime)
import Data.List (sort)
import Utils.Datatypes (Availability (Availability), Occupancy (Occupancy), TimeSlot (TimeSlot))

-- | Creates an availability from the given timestamps, and then removes all slices
-- of that availability where an occupancy overlaps. The result is zero or more
-- availabilities.
--
-- Note: assumes occupancies do not overlap.
determineAvailabilites :: UTCTime -- ^ Start of timeslice to be considered
                       -> UTCTime -- ^ End of timeslice to be considered
                       -> [Occupancy] -- ^ Occupancies expressing no availability in the timeslice to be considered
                       -> [Availability] -- ^ Resulting availabilities (zero or more)
determineAvailabilites from til = foldl f [Availability from til] . sort
  where
    f :: [Availability] -> Occupancy -> [Availability]
    f as = (init as ++) . substractOccupancy (last as)

-- | Shrinks, removes or splits an availability if it overlaps with an occupancy.
--
-- Note that result is still sorted.
-- TODO move this to typeclass
substractOccupancy :: Availability -> Occupancy -> [Availability]
substractOccupancy (Availability a b) (Occupancy _ x y) | x >= y = error "Occupancy is invalid" -- TODO can we prevent these cases agda style?
                                                        | a >= b = error "Availability is invalid"
                                                        | x <= a && y >= b = []
                                                        | x <= a && y <  b && y >  a = [Availability y b]
                                                        | x >  a && x <  b && y >= b = [Availability a x]
                                                        | x >  a && y <  b = [Availability a x, Availability y b]
                                                        | otherwise = [Availability a b]

-- | Given two timestamps, returns @TimeSlot@s such that all time between timestamps are covered
-- (and thus potentially a little more, since the final e.g. 15 minutes will be covered by an e.g. 30 min timeslot).
timeSlots :: NominalDiffTime -- ^ Time difference between the start of each @TimeSlot@
          -> UTCTime -- ^ The start timestamp (should align with half hour intervals)
          -> UTCTime -- ^ The end timestamp
          -> [TimeSlot] -- ^ All timeslots covering time between start and end timestamps
timeSlots s t1 t2 | t1 >= t2  = []
                  | otherwise = TimeSlot t1 s : timeSlots s (addUTCTime s t1) t2

-- TODO:
-- Upload ICS file
-- > Convert to occupancies: title, datetime range
-- Store occupancies
-- Visualize occupancies, edit them -> global occupancy
-- > Given a range, determine availability
-- store copy of availability of meeting: per meeting or per user?
