module Utils.DateTime exposing
    ( Date, Time
    , parseDate
    , parseTime
    , formatDate
    , formatTime
    , millisToDate
    )

import Calendar exposing (Date, months, getDay, getMonth, monthToInt, getYear)
import Clock exposing (Time, getHours, getMinutes)
import Time exposing (millisToPosix)

import Maybe.Extra as Maybe
import Array

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

-- ELM TIME UTILS

millisToDate : Int -> Date
millisToDate = millisToPosix >> Calendar.fromPosix