module Models exposing (..)

import Dict exposing (Dict)
import Calendar exposing (Date)
import Clock exposing (Time)

import Json.Decode as Decode exposing (Decoder, string, list, dict)
import Json.Decode.Pipeline exposing (required)

import Utils.DateTime exposing (dateDecoder, timeDecoder)

-- DATA TYPES

type alias Availability =
    { id : String
    , date : Date
    , startTime : Time
    , endTime : Time
    }

type alias Occupancy = Availability

type AvailabilityTimeType
    = StartTime
    | EndTime

type alias Meeting =
    { id : String
    , availabilities : Dict String (List Availability)
    , startTime : Time
    , endTime : Time
    , days : List Date
    , title : String
    , description : String
    }

type alias User =
    { name : String
    , id : String
    }

-- DECODERS

meetingDecoder : Decoder Meeting
meetingDecoder = Decode.succeed Meeting
    |> required "id" string
    |> required "availabilities" (dict (list availabilityDecoder))
    |> required "startTime" timeDecoder
    |> required "endTime" timeDecoder
    |> required "days" (list dateDecoder)
    |> required "title" string
    |> required "description" string

availabilityDecoder : Decoder Availability
availabilityDecoder = Decode.succeed Availability
    |> required "id" string
    |> required "date" dateDecoder
    |> required "startTime" timeDecoder
    |> required "endTime" timeDecoder

userDecoder : Decoder User
userDecoder = Decode.succeed User
    |> required "name" string
    |> required "id" string
