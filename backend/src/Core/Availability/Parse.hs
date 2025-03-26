module Core.Availability.Parse (parseOccupancies) where

import Data.ByteString.Lazy (ByteString)
import Data.Tuple (swap)
import Data.Default (def)
import Data.Map.Internal (elems)
import Text.ICalendar
import Data.Time
import Data.Maybe (mapMaybe)
import Data.Text.Lazy (unpack)

import Utils.Datatypes (Occupancy (Occupancy))

type ImportError = String
type ImportWarning = String

-- | First parses a @ByteString@ into a @VCalendar@, and then interprets the @VCalendar@ into
-- @UserOccupancies@. It does so by calling @vcalendarToOccupancies@ and combining the resulting
-- @Occupancy@ values with the user id, creating @UserOccupancies@.
parseOccupancies :: ByteString -> Either ImportError ([ImportWarning], [Occupancy])
parseOccupancies bytes = fmap (concatMap vcalendarToOccupancies) . swap
                           <$> parseICalendar def "Uploaded vcalendar" bytes

-- | Grabs all occupancies as described by an iCalendar.
-- Note: events which we failed to fully parse as an occupancy are not included
-- Note: as explained in @grabOccupancies@, one event might be represented by multiple occupancies.
vcalendarToOccupancies :: VCalendar -> [Occupancy]
vcalendarToOccupancies = mapMaybe grabOccupancy . elems . vcEvents

-- | Interprets a @VEvent@ into multiple @Occupancy@ values.
-- It is in the definition of a @Occupancy@ that one is bound to a single day.
-- We thus might find multiple occupancies here (one for each day).
grabOccupancy :: VEvent -> Maybe Occupancy
grabOccupancy e = do
  title <- unpack . summaryValue <$> veSummary e
  start <- dateTimeToUTC . dtStartDateTimeValue <$> veDTStart e
  end   <- dateTimeToUTC . dtEndDateTimeValue <$> fmap (fromDTEndDurationToDTEnd start) (veDTEndDuration e)
  return $ Occupancy title start end

-- TODO this function assumes timezones if not present, can be determined by global timezone calendar element.
dateTimeToUTC :: DateTime -> UTCTime
dateTimeToUTC (FloatingDateTime lt) = localTimeToUTC utc1 lt
dateTimeToUTC (UTCDateTime t) = t -- Currently only correct implementation
dateTimeToUTC (ZonedDateTime lt _) = localTimeToUTC utc1 lt
utc1 :: TimeZone
utc1 = TimeZone 60 False "Amsterdam" -- TODO summertime?

-- | Given two timestamps, returns @TimeSlot@s such that all time between timestamps are covered
-- (and thus potentially a little more, depending on definition on a @TimeSlot@)
-- toTimeSlices :: TimeZone -- ^ The timezone for which to determine end of days
--              -> UTCTime -- ^ The start timestamp
--              -> UTCTime -- ^ The end timestamp
--              -> [TimeSlot] -- ^ All timeslots covering time between start and end timestamps
-- toTimeSlices tz = go (round t1 down to start slot)
--   where go t1 t2 | t1 > t2   = []
--         go t1 t2 | otherwise = TimeSlot date t1 : go (t1 + half hour) t2
-- -- toTimeSlices t1 t2 = if day1 < day2
-- --                          then TimeSlice day1 time1 (TimeOfDay 23 59 59)
-- --                                : toTimeSlices
-- --                                    (addUTCTime nominalDay $ UTCTime (utctDay t1) (picosecondsToDiffTime 0)) -- Begin day 2
-- --                                    t2
-- --                          else [TimeSlice day1 (takeTime t1) (takeTime t2)
-- --                                 | time2 > time1] -- Covers edge cases


-- | An event may potentially indicate its end with a duration and not a @UTCTime@. If so, convert it to a @UTCTime@.
fromDTEndDurationToDTEnd :: UTCTime -- ^ The event starting date
                         -> Either DTEnd DurationProp -- ^ Contains either the absolute end date or the event duration
                         -> DTEnd -- ^ The absolute event ending date and time
fromDTEndDurationToDTEnd _            (Left  d) = d
fromDTEndDurationToDTEnd startTimeUTC (Right d) = flip DTEndDateTime def $ UTCDateTime $
  case durationValue d of
    (DurationDate _ days hours minutes seconds) -> addUTCTime (nominalDay `times` days + hour `times` hours + minute `times` minutes + second `times` seconds) startTimeUTC
    (DurationTime _ hours minutes seconds)      -> addUTCTime (                          hour `times` hours + minute `times` minutes + second `times` seconds) startTimeUTC
    (DurationWeek _ weeks)                      -> addUTCTime (nominalDay `times` weeks * 7) startTimeUTC -- FIXME Remove offset of startTime to end at midnight and not somewhere the day after



{- Some small helper methods follow below -}

-- | Given a @NominalDiffTime@, returns a multiple of the @NominalDiffTime@
times :: NominalDiffTime -> Int -> NominalDiffTime
times dt n = iterate (+ dt) dt !! n

hour, minute, second :: NominalDiffTime
hour   = secondsToNominalDiffTime 60 * 60
minute = secondsToNominalDiffTime 60
second = secondsToNominalDiffTime 1
