module Utils.DateTime exposing
    ( Date, Time
    , parseDate, parseTime
    , dateDecoder, timeDecoder
    , formatDate, formatTime
    , getWeekStart
    , incrementDays, decrementDays
    , millisToDate
    , weekdays
    )

import Calendar exposing (Date, months, getDay, getMonth, monthToInt, getYear, getWeekday, decrementDay, incrementDay)
import Clock exposing (Time, getHours, getMinutes)
import Time exposing (millisToPosix, Weekday(..))

import Maybe.Extra as Maybe
import Array
import List.Extra

import Json.Decode as Decode exposing (Decoder, int)

-- RE-EXPORTED TYPE ALIASES

type alias Date = Calendar.Date
type alias Time = Clock.Time

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
dateDecoder = int |> Decode.map millisToDate

timeDecoder : Decoder Time
timeDecoder = int |> Decode.map (millisToPosix >> Clock.fromPosix)

-- FORMATTING

formatTime : Time -> String
formatTime time =
    let formatNumber s = if String.length s == 1 then "0" ++ s else s
     in [getHours time, getMinutes time]
        |> List.map (String.fromInt >> formatNumber)
        |> List.intersperse ":"
        |> String.concat

formatDate : Date -> String
formatDate date =
    [getDay date, getMonth date |> monthToInt, getYear date]
    |> List.map String.fromInt
    |> List.intersperse "-"
    |> String.concat

-- DATE CONTEXT UTILS

times : Int -> (a -> a) -> a -> a
times n f a = case n of
    0 -> a
    1 -> f a
    _ -> if n < 0 then a else f (times (n - 1) f a)

getWeekStart : Date -> Date
getWeekStart date =
    List.Extra.elemIndex (getWeekday date) weekdays
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