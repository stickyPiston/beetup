module Pages.Availability exposing (..)

import Html exposing (Html, div, text, h2, input, button, form, label)
import Html.Attributes as A exposing (type_, value)
import Html.Events exposing (onClick, onSubmit, onInput)

import Dict exposing (Dict)

import Clock exposing (midnight)
import Calendar
import Utils.DateTime exposing (Time, Date, parseDate, parseTime, formatDate, formatTime, millisToDate)
import Models exposing (AvailabilityTimeType(..), User)

import Maybe.Extra as Maybe
import List.Extra as List

-- MODEL

type alias AvailabilityDraft =
    { date : Int
    , startTime : Maybe Time
    , endTime : Maybe Time
    }

type alias Model =
    { days : Dict Int (List AvailabilityDraft)
    , startTime : Time
    , endTime : Time
    , dateField : Maybe Date
    , attendeeName : String
    , attendeeEmail : String
    }

init : Maybe User -> Model
init user =
    { days = Dict.empty
    , startTime = midnight
    , endTime = midnight
    , dateField = Nothing
    , attendeeName = Maybe.unwrap "" (\ u -> u.name) user 
    , attendeeEmail = Maybe.unwrap "" (\ u -> u.id) user 
    }

-- UPDATE

type AttendeeInfoType
    = Name
    | Email

type Msg
    = AddAvailability Int
    | AddDay
    | DateChanged String
    -- AvailabilityTimeChanged type_ day idx value will update the availability on day at index idx
    -- to the time represented by value if it parses to a valid time.
    | AvailabilityTimeChanged AvailabilityTimeType Int Int String
    | DeleteAvailability Int Int
    | AttendeeInfoChanged AttendeeInfoType String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let unchanged = (model, Cmd.none)
        newAvailability day = AvailabilityDraft day Nothing Nothing
     in case msg of
        AddAvailability day ->
            let updateDay availability dictEntry =
                    dictEntry
                    |> Maybe.unwrap (Just [availability]) (\ availabilities -> Just (availability :: availabilities))
                newDict = Dict.update day (updateDay <| newAvailability day) model.days
             in ({ model | days = newDict }, Cmd.none)
        DateChanged value ->
            parseDate value
            |> Maybe.unwrap unchanged (\ parsedDate -> ({ model | dateField = Just parsedDate }, Cmd.none))
        AddDay ->
            Maybe.map Calendar.toMillis model.dateField
            |> Maybe.unwrap unchanged (\ posix -> ({ model | days = Dict.insert posix [] model.days }, Cmd.none))
        AvailabilityTimeChanged timeType day idx value ->
            let updateAvailability newTime avail = case timeType of
                    StartTime -> { avail | startTime = Just newTime }
                    EndTime   -> { avail | endTime = Just newTime }
                
                updateAvails newTime = List.updateAt idx (updateAvailability newTime)
             in parseTime value
                |> Maybe.map (\ time -> Dict.update day (Maybe.map (updateAvails time)) model.days)
                |> Maybe.unwrap unchanged (\ newDays -> ({ model | days = newDays }, Cmd.none))
        DeleteAvailability day idx ->
            let newDays = Dict.update day (Maybe.map (List.removeAt idx)) model.days
             in ({ model | days = newDays }, Cmd.none)
        AttendeeInfoChanged attendeeInfoType value ->
            case attendeeInfoType of
                Name -> ({ model | attendeeName = value }, Cmd.none)
                Email -> ({ model | attendeeEmail = value }, Cmd.none)

-- VIEW

view : Model -> List (Html Msg)
view model =
    let viewedDays   = model.days |> Dict.map (viewDay model.startTime model.endTime) |> Dict.values
        attendeeInfo = viewAttendeeInfo model.attendeeName model.attendeeEmail
     in attendeeInfo :: addDayForm :: viewedDays ++ [text <| Debug.toString model.days]

viewAttendeeInfo : String -> String -> Html Msg
viewAttendeeInfo name email = div []
    [ label [] [text "Name: "]
    , input [type_ "text", value name, onInput (AttendeeInfoChanged Name)] []
    , label [] [text "Email: "]
    , input [type_ "email", value email, onInput (AttendeeInfoChanged Email)] []
    ]

addDayForm : Html Msg
addDayForm = form [onSubmit AddDay]
    [ input [type_ "date", onInput DateChanged] []
    , input [type_ "submit"] []
    ]

viewDay : Time -> Time -> Int -> List AvailabilityDraft -> Html Msg
viewDay startTime endTime day availabilities =
    let formattedDate = day |> millisToDate |> formatDate
     in div []
        [ h2 [] [text formattedDate]
        , div [] <| List.indexedMap (viewAvailabilities startTime endTime day) availabilities
        , button [onClick <| AddAvailability day] [text "New availability"]
        ]

viewAvailabilities : Time -> Time -> Int -> Int -> AvailabilityDraft -> Html Msg
viewAvailabilities startTime endTime day idx availability =
    let startTimeValue = Maybe.unwrap "" formatTime availability.startTime
        endTimeValue   = Maybe.unwrap "" formatTime availability.endTime
     in div []
        [ input -- TODO: Refactor
            [ type_ "time"
            , A.min <| formatTime <| startTime
            , A.max <| formatTime <| endTime
            , value <| startTimeValue
            , onInput (AvailabilityTimeChanged StartTime day idx)
            ] []
        , input
            [ type_ "time"
            , A.min <| formatTime <| startTime
            , A.max <| formatTime <| endTime
            , value <| endTimeValue
            , onInput (AvailabilityTimeChanged EndTime day idx)
            ] []
        , button [onClick (DeleteAvailability day idx)] [text "Delete"]
        ]
