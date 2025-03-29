module Core.Availability.Parse (parseOccupancies) where

import Data.ByteString.Lazy (ByteString)
import Data.Tuple (swap)
import Data.Default (def)
import Data.Map.Internal (elems)
import Text.ICalendar
import Data.Time
import Data.Maybe (mapMaybe)
import Data.Text.Lazy (unpack)

import Utils.Datatypes (Occupancy (Occupancy), hour, minute, second)
import Utils.Functions (times)

type ImportError = String
type ImportWarning = String

-- | First parses a @ByteString@ into a @VCalendar@, and then interprets the @VCalendar@ into
-- @Occupancy@s.
parseOccupancies :: ByteString -> Either ImportError ([ImportWarning], [Occupancy])
parseOccupancies bytes = fmap (concatMap vcalendarToOccupancies) . swap
                           <$> parseICalendar def "Uploaded vcalendar" bytes

-- | Grabs all occupancies as described by an iCalendar.
-- Note: events which we failed to fully parse as an occupancy are not included
vcalendarToOccupancies :: VCalendar -> [Occupancy]
vcalendarToOccupancies = mapMaybe grabOccupancy . elems . vcEvents

-- | Interprets a @VEvent@ into an @Occupancy@.
grabOccupancy :: VEvent -> Maybe Occupancy
grabOccupancy e = do
  title <- unpack . summaryValue <$> veSummary e
  start <- dateTimeToUTC . dtStartDateTimeValue <$> veDTStart e
  end   <- dateTimeToUTC . dtEndDateTimeValue <$> fmap (fromDTEndDurationToDTEnd start) (veDTEndDuration e)
  return $ Occupancy title start end

-- TODO it is possible to parse the timezone specified globally, makes this complete
-- | Converts a datetime to UTC, assuming the utc + 1 timezone if no timezone is specified.
dateTimeToUTC :: DateTime -> UTCTime
dateTimeToUTC (FloatingDateTime lt) = localTimeToUTC utc1 lt
dateTimeToUTC (UTCDateTime t) = t -- Currently only correct implementation
dateTimeToUTC (ZonedDateTime lt _) = localTimeToUTC utc1 lt
utc1 :: TimeZone
utc1 = TimeZone 60 False "Amsterdam"

-- | An event may potentially indicate its end with a duration and not a @UTCTime@. If so, convert it to a @UTCTime@.
fromDTEndDurationToDTEnd :: UTCTime -- ^ The event starting date
                         -> Either DTEnd DurationProp -- ^ Contains either the absolute end date or the event duration
                         -> DTEnd -- ^ The absolute event ending date and time
fromDTEndDurationToDTEnd _            (Left  d) = d
fromDTEndDurationToDTEnd startTimeUTC (Right d) = flip DTEndDateTime def $ UTCDateTime $
  case durationValue d of
    (DurationDate _ days hours minutes seconds) -> addUTCTime (nominalDay `times` days + hour `times` hours + minute `times` minutes + second `times` seconds) startTimeUTC
    (DurationTime _ hours minutes seconds)      -> addUTCTime (                          hour `times` hours + minute `times` minutes + second `times` seconds) startTimeUTC
    (DurationWeek _ weeks)                      -> addUTCTime (nominalDay `times` weeks * 7) startTimeUTC
