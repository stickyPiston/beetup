module Models exposing (..)

import Dict exposing (Dict)
import Calendar exposing (Date)
import Clock exposing (Time)

type alias Availability =
    { date : Date
    , startTime : Time
    , endTime : Time
    }

type AvailabilityTimeType
    = StartTime
    | EndTime


type alias Meeting =
    { availabilities : Dict String (List Availability)
    , startTime : Time
    , endTime : Time
    , days : List Date
    }