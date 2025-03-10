module Pages.Meeting exposing (..)

import Html exposing (Html, form, label, text, input, div, span, ruby, rp, rt, textarea)
import Html.Attributes exposing (value, type_, class)
import Html.Events exposing (onInput, onClick)

import Calendar exposing (getDay, getMonth, getDateRange)
import Clock exposing (midnight)
import Utils.DateTime exposing (Time, Date, formatTime, weekdays, getWeekStart, incrementDays, parseTime)
import Time exposing (Weekday(..), Month(..))
import Models exposing (AvailabilityTimeType(..))

import Task
import List.Extra as List
import Maybe.Extra as Maybe

-- MODEL

type alias Model =
    { selectedDates : List Date
    , startTime : Time
    , endTime : Time
    , currentDay : Maybe Date
    , title : String
    , description : String
    }

init : (Model, Cmd Msg)
init =
    let startTime     = Maybe.withDefault midnight (Clock.setHours 9 midnight)
        endTime       = Maybe.withDefault midnight (Clock.setHours 17 midnight)
        getCurrentDay = Time.now
            |> Task.map Calendar.fromPosix
            |> Task.perform GotDate
     in ( { selectedDates = []
          , startTime = startTime
          , endTime = endTime
          , currentDay = Nothing
          , title = ""
          , description = ""
          }
        , getCurrentDay
        )

-- UDPATE

type TextInputType
    = Title
    | Description

type Msg
    = CreatingDraftAvailability
    | GotDate Date
    | NextMonth
    | PrevMonth
    | DateClicked Date
    | TimeUpdated AvailabilityTimeType String
    | TextInputUpdated TextInputType String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let incrementCurrentDay n = { model | currentDay = Maybe.map (incrementDays n) model.currentDay }
     in case msg of
        CreatingDraftAvailability -> (model, Cmd.none)
        GotDate date -> ({ model | currentDay = Just date }, Cmd.none)
        NextMonth -> (incrementCurrentDay 28, Cmd.none)
        PrevMonth -> (incrementCurrentDay -28, Cmd.none)
        DateClicked date ->
            let compareDate a b = Calendar.compare a b == EQ
                updateModel dates = ({ model | selectedDates = dates}, Cmd.none) 
             in case List.findIndex (compareDate date) model.selectedDates of
                Just idx -> updateModel (List.removeAt idx model.selectedDates)
                Nothing  -> updateModel (date :: model.selectedDates)
        TimeUpdated timeType value ->
            let updateModel with = case timeType of
                    StartTime -> { model | startTime = with }
                    EndTime   -> { model | endTime = with }
                unchanged = (model, Cmd.none)
             in parseTime value |> Maybe.unwrap unchanged (\ t -> (updateModel t, Cmd.none))
        TextInputUpdated textType value -> case textType of
            Title -> ({ model | title = value }, Cmd.none)
            Description -> ({ model | description = value }, Cmd.none)

-- VIEW

view : Model -> List (Html Msg)
view model = 
    [ form []
        [ label [] [text "Start time: "]
        , input [type_ "time", value <| formatTime model.startTime, onInput (TimeUpdated StartTime)] []
        , label [] [text "End time: "]
        , input [type_ "time", value <| formatTime model.endTime, onInput (TimeUpdated EndTime)] []
        , label [] [text "Title: "]
        , input [type_ "text", value model.title] []
        , label [] [text "Description: "]
        , textarea [value model.description] []
        , div [class "calendar"]
            [ span [class "arrow", onClick PrevMonth] [text "<"]
            , div [class "weeks"] (viewCalendar model)
            , span [class "arrow", onClick NextMonth] [text ">"]
            ]
        ]
    ]

toShortWeekday : Weekday -> String 
toShortWeekday weekday = case weekday of
    Mon -> "Mon"
    Tue -> "Tue"
    Wed -> "Wed"
    Thu -> "Thu"
    Fri -> "Fri"
    Sat -> "Sat"
    Sun -> "Sun"

toShortMonth : Month -> String
toShortMonth month = case month of
    Jan -> "Jan"
    Feb -> "Feb"
    Mar -> "Mar"
    Apr -> "Apr"
    May -> "May"
    Jun -> "Jun"
    Jul -> "Jul"
    Aug -> "Aug"
    Sep -> "Sep"
    Oct -> "Oct"
    Nov -> "Nov"
    Dec -> "Dec"

viewCalendar : Model -> List (Html Msg)
viewCalendar model = case model.currentDay of
    Nothing -> []
    Just currentDay -> viewWeekdays :: viewDays model.selectedDates currentDay

viewWeekdays : Html a
viewWeekdays = div [] <| List.map (\ weekday -> span [] [text <| toShortWeekday weekday]) weekdays

viewDays : List Date -> Date -> List (Html Msg)
viewDays selectedDates currentDay =
    let calendarStart  = getWeekStart currentDay
        calendarEnd    = incrementDays 27 calendarStart
        dates          = getDateRange calendarStart calendarEnd
        (week1, rest1) = List.splitAt 7 dates
        (week2, rest2) = List.splitAt 7 rest1
        (week3, week4) = List.splitAt 7 rest2
     in List.indexedMap (\ idx week -> viewWeek (idx == 0) selectedDates week) [week1, week2, week3, week4]

viewWeek : Bool -> List Date -> List Date -> Html Msg
viewWeek isFirstWeek selectedDates days =
    let viewedDays = List.indexedMap (\ idx day -> viewDay (isFirstWeek && idx == 0) selectedDates day) days
     in div [class "week"] viewedDays 

viewDay : Bool -> List Date -> Date -> Html Msg
viewDay isFirstDay selectedDates date =
    let isClicked = List.findIndex (\ d -> Calendar.compare d date == EQ) selectedDates |> Maybe.isJust
        dayElement = getDay date |> String.fromInt |> text
     in span
        ( onClick (DateClicked date)
            :: if isClicked then [class "active"] else [])
        [ if isFirstDay || getDay date == 1 then 
            ruby []
                [ dayElement 
                , rp [] [text "("]
                , rt [] [getMonth date |> toShortMonth |> text]
                , rp [] [text ")"]
                ]
          else
            dayElement
        ]