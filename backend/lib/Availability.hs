{-# LANGUAGE OverloadedRecordDot #-}

module Availability (determineAvailabilites, splitAvailabilities, timeSlots) where

import Data.Time (UTCTime (..), TimeOfDay(..), midnight, timeOfDayToTime, NominalDiffTime, addUTCTime)
import Data.List (sort)
import Utils.Datatypes (Availability (..), Occupancy, TimeSlot (TimeSlot), disj, UserId)

-- | Creates an availability from the given timestamps, and then removes all slices
-- of that availability where an occupancy overlaps. The result is zero or more
-- availabilities.
--
-- Note: assumes occupancies do not overlap.
determineAvailabilites :: UTCTime -- ^ Start of timeslice to be considered
                       -> UTCTime -- ^ End of timeslice to be considered
                       -> [Occupancy] -- ^ Occupancies expressing no availability in the timeslice to be considered
                       -> UserId -- ^ UserId of the availability
                       -> Maybe [Availability] -- ^ Resulting availabilities (zero or more)
determineAvailabilites from til os uid | from >= til = Nothing
                                       | otherwise   = foldl substrOccupancy (Just [Availability from til uid]) $ sort os
  where
    -- Remember that the availabilities are sorted from earliest to latest
    substrOccupancy :: Maybe [Availability] -> Occupancy -> Maybe [Availability]
    substrOccupancy Nothing       = const Nothing -- We have no availability left
    substrOccupancy (Just [])     = const $ Just []
    substrOccupancy (Just [x])    = disj x -- We split the latest availability with earliest occupancy
    substrOccupancy (Just (x:xs)) = fmap (x :) . substrOccupancy (Just xs)

-- | Given a list of availabilites, split them up such that they do not span across day boundaries.
splitAvailabilities :: [Availability] -> [Availability]
splitAvailabilities = 
  concatMap $ \a ->
    if a.aStart.utctDay /= a.aEnd.utctDay then
      let days = [a.aStart.utctDay .. a.aEnd.utctDay]
          firstDay = UTCTime (head days) (utctDayTime a.aStart)
          lastDay  = UTCTime (last days) (utctDayTime a.aEnd)
          middle = init $ tail days
          endOfDay = timeOfDayToTime $ TimeOfDay 23 59 0
          startOfDay = timeOfDayToTime midnight
        in Availability firstDay (UTCTime (head days) endOfDay) a.aUserId
          : map (\ d -> Availability (UTCTime d startOfDay) (UTCTime d endOfDay) a.aUserId) middle
          ++ [Availability (UTCTime (last days) startOfDay) lastDay a.aUserId]
      else
        [a]

-- | Given two timestamps, returns @TimeSlot@s such that all time between timestamps are covered
-- (and thus potentially a little more, since the final e.g. 15 minutes will be covered by an e.g. 30 min timeslot).
timeSlots :: NominalDiffTime -- ^ Time difference between the start of each @TimeSlot@
          -> UTCTime -- ^ The start timestamp (should align with half hour intervals)
          -> UTCTime -- ^ The end timestamp
          -> [TimeSlot] -- ^ All timeslots covering time between start and end timestamps
timeSlots s t1 t2 | t1 >= t2  = []
                  | otherwise = TimeSlot t1 s : timeSlots s (addUTCTime s t1) t2
