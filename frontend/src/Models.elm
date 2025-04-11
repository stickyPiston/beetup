module Models exposing (..)

import Calendar exposing (Date)
import Clock exposing (Time)

import Json.Decode as Decode exposing (Decoder, string, list, int)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode

import Utils.DateTime exposing (dateDecoder, timeDecoder, compareDateTime, utcTimeDecoder, DateTime)
import DateTime exposing (getDate, getTime)
import Utils.DateTime exposing (datetimeEncoder)
import Json.Decode as Decode

-- DATA TYPES

type alias Availability =
    { date : Date
    , startTime : Time
    , endTime : Time
    , userId : String
    }

type alias Occupancy = Availability

type AvailabilityTimeType
    = StartTime
    | EndTime

type alias Meeting =
    { id : String
    , availabilities : List Availability
    , start : Time
    , end : Time
    , days : List Date
    , title : String
    , description : String
    , userIds : List String
    , maximumAttendancy : Maybe MaximumAttendancy
    }

type alias MaximumAttendancy =
    { users : List String
    , start : DateTime
    , end : DateTime
    }

type alias User =
    { name : String
    , id : String
    }

-- DECODERS

meetingDecoder : Decoder Meeting
meetingDecoder = Decode.succeed Meeting
    |> required "id" string
    |> required "availabilities" (list availabilityDecoder)
    |> required "start" timeDecoder
    |> required "end" timeDecoder
    |> required "days" (list dateDecoder)
    |> required "title" string
    |> required "description" string
    |> required "userIds" (list int |> Decode.map (List.map String.fromInt))
    |> required "maximumAttendancy" (Decode.nullable maximumAttendancyDecoder)

maximumAttendancyDecoder : Decoder MaximumAttendancy
maximumAttendancyDecoder = Decode.succeed MaximumAttendancy
    |> required "users" (list int |> Decode.map (List.map String.fromInt))
    |> required "start" utcTimeDecoder
    |> required "end" utcTimeDecoder

availabilityDecoder : Decoder Availability
availabilityDecoder =
    let createAvailability ({ start, end, userId }) = case compareDateTime start end of
            EQ -> Decode.succeed
                      { startTime = getTime start
                      , endTime = getTime end
                      , userId = String.fromInt userId
                      , date = getDate start
                      }
            _  -> Decode.fail "Dates of start and end time not equal"
     in Decode.succeed (\ start end userId -> { start = start, end = end, userId = userId })
        |> required "start" utcTimeDecoder
        |> required "end" utcTimeDecoder
        |> required "userId" int
        |> Decode.andThen createAvailability

availabilityEncoder : Availability -> Encode.Value
availabilityEncoder { date, startTime, endTime } =
    let startDatetime = DateTime.fromDateAndTime date startTime
        endDatetime = DateTime.fromDateAndTime date endTime
     in Encode.object
        [ ("start", datetimeEncoder startDatetime)
        , ("end", datetimeEncoder endDatetime)
        ]

userDecoder : Decoder User
userDecoder = Decode.succeed User
    |> required "name" string
    |> required "id" string
