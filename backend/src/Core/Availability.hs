module Core.Availability () where

import Core.Availability.Parse
import Data.Time (TimeOfDay (TimeOfDay), midnight)
import Text.ICalendar (Date (Date))
import Data.List (sort, unfoldr, groupBy)
import Utils.Datatypes (UserAvailabilities, UserOccupancies (UserOccupancies), Availability (Availability, aDate, aStartTime, aEndTime), Occupancy (oEndTime, oStartTime, Occupancy, oDate))

determineAvailabilites :: UserOccupancies
                       -> Date
                       -> Date
                       -> UserAvailabilities
determineAvailabilites (UserOccupancies userId occupancies) = undefined

-- Note: assumes that no occupancies overlap.
determineAvail :: [Occupancy]
                      -> Date
                      -> Date
                      -> [Availability]
determineAvail os from til = concatMap splitAvailabilityWithOccupancy $ groupBy (\x y -> oDate x == oDate y) os


availabilitiesBetween :: Date
                      -> Date
                      -> [Availability]
availabilitiesBetween day1 day2 | day1 > day2 = []
                                | day1 == day2 = [Availability day1 midnight (TimeOfDay 23 59 59)]
                                | otherwise = [Availability day1 midnight (TimeOfDay 23 59 59)] : availabilitiesBetween (Date (addUt day day1) day2

-- Note that result is still sorted
splitBy :: Availability -> Occupancy -> [Availability]
splitBy a o | aDate a /= oDate o = error "Cannot split availabiliy by an occupancy on another date"
            | otherwise = [Availability (aDate a) (aStartTime a) (oStartTime o),
                           Availability (aDate a) (oEndTime   o) (aEndTime   a)]

-- Note: assumes sorted list of occupancies. Result is sorted.
splitAvailabilityWithOccupancy :: Availability -> [Occupancy] -> [Availability]
splitAvailabilityWithOccupancy a = foldr (\o as -> init as ++ splitBy (last as) o) [a]

-- Group by date
-- concatMap over all groups an unfoldr:
--    unfoldr f [Availability] group
--      where f occupancy availability = split availabiliy by occupancy


-- TODO:
-- Upload ICS file
-- > Convert to occupancies: title, datetime range
-- Store occupancies
-- Visualize occupancies, edit them -> global occupancy
-- > Given a range, determine availability
-- store copy of availability of meeting: per meeting or per user?
