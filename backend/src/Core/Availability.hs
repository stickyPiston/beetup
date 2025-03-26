module Core.Availability () where

import Data.Time (UTCTime)
import Data.List (sort)
import Utils.Datatypes (Availability (Availability), Occupancy (Occupancy))

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

-- TODO:
-- Upload ICS file
-- > Convert to occupancies: title, datetime range
-- Store occupancies
-- Visualize occupancies, edit them -> global occupancy
-- > Given a range, determine availability
-- store copy of availability of meeting: per meeting or per user?
