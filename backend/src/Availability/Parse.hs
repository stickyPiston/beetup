module Availability.Parse (parseOccupancies, icalendarToOccupancies) where

import Data.ByteString.Lazy (ByteString)
import Data.Tuple (swap)
import Data.Default (def)
import Data.Map.Internal (elems)
import Text.ICalendar
import Data.Time
import Data.Maybe (mapMaybe)
import Data.Text.Lazy (unpack)

import Utils.Datatypes (UserId, UserOccupancies (..), Occupancy (..), TimeRange (..))

type ImportError = String
type ImportWarning = String

-- | First parses a @ByteString@ into a @VCalendar@, and then interprets the @VCalendar@ into
-- @UserOccupancies@. It does so by calling @icalendarToOccupancies@ and combining the resulting
-- @Occupancy@ values with the user id, creating @UserOccupancies@.
parseOccupancies :: UserId -> ByteString -> Either ImportError ([ImportWarning], UserOccupancies)
parseOccupancies user bytes = fmap (UserOccupancies user . concatMap icalendarToOccupancies) . swap
                             <$> parseICalendar def "test-path" bytes

-- | Grabs all occupancies as described by an iCalendar.
-- Note: events which we failed to fully parse as an occupancy are not included
-- Note: as explained in @grabOccupancies@, one event might be represented by multiple occupancies.
icalendarToOccupancies :: VCalendar -> [Occupancy]
icalendarToOccupancies cal = let events = elems $ vcEvents cal
                      in concat $ mapMaybe grabOccupancies events

-- | Interprets a @VEvent@ into multiple @Occupancy@ values.
-- It is in the definition of a @Occupancy@ that one is bound to a single day.
-- We thus might find multiple occupancies here (one for each day).
grabOccupancies :: VEvent -> Maybe [Occupancy]
grabOccupancies e = do
  title <- grabTitle e
  occupancies <- grabOccupancyTimeRanges e
  return $ map (\(TimeRange date start end) -> Occupancy title date start end) occupancies

-- | Grabs a title from a @VEvent@, if present.
grabTitle :: VEvent -> Maybe String
grabTitle e = unpack . summaryValue <$> veSummary e

-- | Determines the raw timewise occupancy described by the given @VEvent@, and returns it in the shape of concurrent @TimeRange@ values.
-- It is in the definition of a @TimeRange@ that one is bound to a single day. We thus might need to return multiple ranges here.
grabOccupancyTimeRanges :: VEvent -> Maybe [TimeRange]
grabOccupancyTimeRanges e = do
  utcStart <- dateTimeUTC . dtStartDateTimeValue <$> veDTStart e
  utcEnd   <- dateTimeUTC . dtEndDateTimeValue <$> fmap (fromDTEndDurationToDTEnd utcStart) (veDTEndDuration e)
  return $ toTimeRanges utcStart utcEnd

-- | Given two timestamps, divides the time inbetween into time ranges restricted to whole days.
toTimeRanges :: UTCTime -- ^ The start timestamp
             -> UTCTime -- ^ The end timestamp
             -> [TimeRange] -- ^ All time ranges inbetween start and end timestamps, each covering at most one fully day.
toTimeRanges t1 t2 = let day1 = takeDate t1
                         day2 = takeDate t2
                    in if day1 < day2
                         then TimeRange day1 (takeTime t1) (TimeOfDay 12 59 59)
                               : toTimeRanges (addUTCTime nominalDay t1) t2 -- Add timerange of first day to results of other days
                         else [TimeRange day1 (takeTime t1) (takeTime t2)]


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

takeDate :: UTCTime -> Date
takeDate = Date . utctDay

takeTime :: UTCTime -> TimeOfDay
takeTime = timeToTimeOfDay . utctDayTime

-- | Given a @NominalDiffTime@, returns a multiple of the @NominalDiffTime@
times :: NominalDiffTime -> Int -> NominalDiffTime
times dt n = iterate (+ dt) dt !! n

hour, minute, second :: NominalDiffTime
hour   = secondsToNominalDiffTime $ 60 * 60
minute = secondsToNominalDiffTime 60
second = secondsToNominalDiffTime 1
