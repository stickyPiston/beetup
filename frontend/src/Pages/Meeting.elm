module Pages.Meeting exposing (..)

import Html exposing (Html, form, label, text, input, ul, button, li)
import Html.Attributes exposing (value, type_)
import Html.Events exposing (onInput, onClick)

import Clock exposing (midnight)

import Utils.DateTime exposing (Time, Date, formatDate, formatTime)

-- MODEL

type alias Model =
    { dateFields : List Date
    , startTime : Time
    , endTime : Time
    }

init : Model
init =
    let startTime = Maybe.withDefault (Debug.todo "") (Clock.setHours 9 midnight)
        endTime   = Maybe.withDefault (Debug.todo "") (Clock.setHours 17 midnight)
     in { dateFields = [], startTime = startTime, endTime = endTime }

-- UDPATE

type Msg
    = DateChanged Int String
    | DateRemoved Int
    | CreatingDraftAvailability

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    DateChanged idx date -> Debug.todo ""
    DateRemoved idx -> Debug.todo ""
    CreatingDraftAvailability -> Debug.todo ""

-- VIEW

view : Model -> Html Msg
view model = form []
    [ label [] [text "Dates:"]
    , viewDates model.dateFields
    , label [] [text "Start time: "]
    , input [type_ "time", value <| formatTime model.startTime] []
    , label [] [text "End time: "]
    , input [type_ "time", value <| formatTime model.endTime] []
    ]

viewDates : List Date -> Html Msg
viewDates dates = ul [] <| List.indexedMap viewDate dates

viewDate : Int -> Date -> Html Msg
viewDate idx date = li []
    [ input [value (formatDate date), type_ "date", onInput (DateChanged idx)] []
    , button [onClick (DateRemoved idx)] [text "Delete"]
    ]