module Utils.DateTime exposing
    ( Date, Time, DateTime
    , parseDate, parseTime
    , dateDecoder, timeDecoder
    , utcTimeParser, utcTimeDecoder
    , dateEncoder, timeEncoder, datetimeEncoder
    , formatDate, formatTime, formatDateTime
    , getWeekStart
    , incrementDays, decrementDays
    , millisToDate
    , weekdays
    , compareDate, compareDateTime
    )

import Calendar exposing (Date, months, getDay, getMonth, monthToInt, getYear, getWeekday, decrementDay, incrementDay)
import Clock exposing (Time, getHours, getMinutes, getSeconds, midnight)
import Time exposing (millisToPosix, Weekday(..))
import DateTime

import Parser exposing (Parser, succeed, chompIf, symbol, (|=), (|.), problem, chompWhile, oneOf)

import Maybe.Extra as Maybe
import List.Extra as List
import Result.Extra as Result
import Array

import Json.Decode as Decode exposing (Decoder, string)
import Json.Encode as Encode

-- RE-EXPORTED TYPE ALIASES

type alias Date = Calendar.Date
type alias Time = Clock.Time
type alias DateTime = DateTime.DateTime

-- PARSING

parseDate : String -> Maybe Date
parseDate input = case String.split "-" input |> Maybe.traverse String.toInt of
    -- The date field returns the date in yyyy-mm-dd format
    Just [year, month, day] -> 
        Array.get (month - 1) months
        |> Maybe.andThen (\ parsedMonth -> Calendar.fromRawParts { year = year, month = parsedMonth, day = day })
    _ -> Nothing

parseTime : String -> Maybe Time
parseTime input = case String.split ":" input |> Maybe.traverse String.toInt of
    -- The time field either returns a time in HH:mm or HH:mm:ss format
    Just [hours, minutes] -> Clock.fromRawParts { hours = hours, minutes = minutes, seconds = 0, milliseconds = 0 }
    Just [hours, minutes, seconds] -> Clock.fromRawParts { hours = hours, minutes = minutes, seconds = seconds, milliseconds = 0 }
    _ -> Nothing

dateDecoder : Decoder Date
dateDecoder = string
    |> Decode.andThen (parseDate >> Maybe.unwrap (Decode.fail "") Decode.succeed)

timeDecoder : Decoder Time
timeDecoder = string
    |> Decode.andThen (parseTime >> Maybe.unwrap (Decode.fail "") Decode.succeed)

utcTimeParser : Parser DateTime
utcTimeParser =
    let constructDateTime year month day hours minutes seconds = 
            Array.get (month - 1) months
            |> Maybe.andThen (\ parsedMonth ->
                DateTime.fromRawParts
                    { day = day, month = parsedMonth, year = year }
                    { seconds = seconds, minutes = minutes, hours = hours, milliseconds = 0 })
        intWithLeadingZeroes = succeed (\ x -> x)
            |. chompWhile (\ c -> c == '0')
            |= oneOf [Parser.int, succeed 0]
     in succeed constructDateTime
        |= intWithLeadingZeroes |. symbol "-"
        |= intWithLeadingZeroes |. symbol "-"
        |= intWithLeadingZeroes |. symbol "T"
        |= intWithLeadingZeroes |. symbol ":"
        |= intWithLeadingZeroes |. symbol ":"
        |= intWithLeadingZeroes |. chompIf (\ c -> c == 'Z')
        |> Parser.andThen (Maybe.unwrap (problem "Invalid date") succeed)

utcTimeDecoder : Decoder DateTime
utcTimeDecoder =
    let parseUtcTime = Parser.run utcTimeParser >> Result.unpack (Debug.toString >> Decode.fail) Decode.succeed
     in Decode.string |> Decode.andThen parseUtcTime

dateEncoder : Date -> Encode.Value
dateEncoder = formatDate >> Encode.string

timeEncoder : Time -> Encode.Value
timeEncoder = formatTime >> Encode.string

datetimeEncoder : DateTime -> Encode.Value
datetimeEncoder datetime =
    let encodedDate = formatDate (DateTime.getDate datetime)
        encodedTime = formatTime (DateTime.getTime datetime)
     in Encode.string (encodedDate ++ "T" ++ encodedTime ++ "Z")

-- FORMATTING

formatTime : Time -> String
formatTime time =
    [getHours time, getMinutes time, getSeconds time]
    |> List.map (String.fromInt >> String.padLeft 2 '0')
    |> List.intersperse ":"
    |> String.concat

formatDate : Date -> String
formatDate date =
    [getYear date, getMonth date |> monthToInt, getDay date]
    |> List.map (String.fromInt >> String.padLeft 2 '0')
    |> List.intersperse "-"
    |> String.concat

formatDateTime : DateTime -> String
formatDateTime dt = formatDate (DateTime.getDate dt) ++ " " ++ formatTime (DateTime.getTime dt)

-- DATE CONTEXT UTILS

times : Int -> (a -> a) -> a -> a
times n f a = case n of
    0 -> a
    1 -> f a
    _ -> if n < 0 then a else f (times (n - 1) f a)

getWeekStart : Date -> Date
getWeekStart date =
    List.elemIndex (getWeekday date) weekdays
    |> Maybe.withDefault 0
    |> \ n -> times n decrementDay date

incrementDays : Int -> Date -> Date
incrementDays n d = case n of
    0 -> d
    _ -> if n > 0 then times n incrementDay d else times -n decrementDay d

decrementDays : Int -> Date -> Date
decrementDays n = incrementDays -n

-- ELM TIME UTILS

millisToDate : Int -> Date
millisToDate = millisToPosix >> Calendar.fromPosix

weekdays : List Weekday
weekdays = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]

compareDate : Date -> Date -> Order
compareDate a b = DateTime.compareDates (DateTime.fromDateAndTime a midnight) (DateTime.fromDateAndTime b midnight)

compareDateTime : DateTime -> DateTime -> Order
compareDateTime = DateTime.compareDates