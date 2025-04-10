module Pages.Availability exposing (..)

import Html exposing (Html, div, text, h2, input, button)
import Html.Attributes as A exposing (type_, value)
import Html.Events exposing (onClick, onInput)

import Clock exposing (midnight)
import Utils.DateTime exposing (Time, Date, parseTime, formatDate, formatTime, compareDate)
import Models exposing (AvailabilityTimeType(..), User, Meeting, meetingDecoder, availabilityEncoder)

import Maybe.Extra as Maybe
import List.Extra as List
import Http
import Json.Encode as Encode

-- MODEL

type alias AvailabilityDraft =
    { startTime : Maybe Time
    , endTime : Maybe Time
    }

type alias Model =
    { days : List (Date, List AvailabilityDraft)
    , startTime : Time
    , endTime : Time
    , id : String
    }

init : String -> (Model, Cmd Msg)
init id =
    ( { days = []
      , startTime = midnight
      , endTime = midnight
      , id = id
      }
    , Http.get
        { url = "http://localhost:8001/meeting/" ++ id
        , expect = Http.expectJson GotMeeting meetingDecoder
        }
    )

-- UPDATE

type AttendeeInfoType
    = Name
    | Email

type Msg
    = AddAvailability Int
    -- AvailabilityTimeChanged type_ day idx value will update the availability on day at index idx
    -- to the time represented by value if it parses to a valid time.
    | AvailabilityTimeChanged AvailabilityTimeType Int Int String
    | DeleteAvailability Int Int
    | GotMeeting (Result Http.Error Meeting)
    | SubmitAvailabilities
    | SubmittedAvailabilities (Result Http.Error ())

update : User -> Msg -> Model -> (Model, Cmd Msg)
update user msg model =
    let unchanged = (model, Cmd.none)
        newAvailability = AvailabilityDraft Nothing Nothing
     in case msg of
        AddAvailability dayidx ->
            ( { model
              | days = model.days
                    |> List.updateAt dayidx (Tuple.mapSecond (\ avails -> newAvailability :: avails))
              }
            , Cmd.none
            )
        AvailabilityTimeChanged timeType dayidx availidx value ->
            let updateAvailability newTime avail = case timeType of
                    StartTime -> { avail | startTime = Just newTime }
                    EndTime   -> { avail | endTime = Just newTime }
                updateAvails newTime = List.updateAt availidx (updateAvailability newTime)
             in parseTime value
                |> Maybe.map (\ time -> List.updateAt dayidx (updateAvails time |> Tuple.mapSecond) model.days)
                |> Maybe.unwrap unchanged (\ newDays -> ({ model | days = newDays }, Cmd.none))
        DeleteAvailability dayidx availidx ->
            model.days
            |> List.updateAt dayidx (List.removeAt availidx |> Tuple.mapSecond)
            |> \ d -> Tuple.pair { model | days = d } Cmd.none
        GotMeeting (Err _) -> (model, Cmd.none)
        GotMeeting (Ok meeting) ->
            let compareDates a b = compareDate (a.date) (b.date)
                toDraft { startTime, endTime } =
                    { startTime = Just startTime
                    , endTime = Just endTime
                    }
                daysWithAvails = meeting.availabilities
                    |> List.map .date
                    |> List.unique
                daysWithoutAvails = meeting.days
                    |> List.filterNot (\ date -> List.member date daysWithAvails)
                parsedDaysDict = meeting.availabilities
                    |> List.filter (\ { userId } -> userId == user.id)
                    |> List.sortWith compareDates
                    |> List.groupWhile (\ a b -> compareDates a b == EQ)
                    |> List.map (\ ({ date } as head, values) -> (date, List.map toDraft (head :: values)))
                    |> List.append (daysWithoutAvails |> List.map (\ date -> (date, [])))
                    |> List.sortWith (\ (a, _) (b, _) -> compareDate a b)
             in ( { model
                  | startTime = meeting.start
                  , endTime = meeting.end
                  , days = parsedDaysDict
                  }
                , Cmd.none
                )
        SubmitAvailabilities ->
            let toAvailability day { startTime, endTime } = case (startTime, endTime) of
                    (Just start, Just end) ->
                        Just { startTime = start , endTime = end , date = day , userId = user.id }
                    _ -> Nothing
                encodeDay (day, values) = List.map (toAvailability day) values
                avails = List.concatMap encodeDay model.days |> Maybe.combine
             in case avails of
                Just availabilities ->
                    ( model
                    , Http.post
                        { url = "http://localhost:8001/meeting/" ++ model.id
                        , expect = Http.expectWhatever SubmittedAvailabilities
                        , body = Http.jsonBody (Encode.list availabilityEncoder availabilities)
                        }
                    )
                Nothing -> Debug.todo ""
        SubmittedAvailabilities (Err _) -> (Debug.todo "", Cmd.none)
        SubmittedAvailabilities (Ok _) -> (model, Cmd.none)

-- VIEW

view : Model -> List (Html Msg)
view model =
    let viewedDays   = model.days |> List.indexedMap (viewDay model.startTime model.endTime)
     in button [onClick SubmitAvailabilities] [text "Submit"] :: viewedDays

viewDay : Time -> Time -> Int -> (Date, List AvailabilityDraft) -> Html Msg
viewDay startTime endTime dayidx (day, availabilities) =
    let formattedDate = formatDate day
     in div []
        [ h2 [] [text formattedDate]
        , div [] <| List.indexedMap (viewAvailabilities startTime endTime dayidx) availabilities
        , button [onClick <| AddAvailability dayidx] [text "New availability"]
        ]

viewAvailabilities : Time -> Time -> Int -> Int -> AvailabilityDraft -> Html Msg
viewAvailabilities startTime endTime dayidx availidx availability =
    let startTimeValue = Maybe.unwrap "" formatTime availability.startTime
        endTimeValue   = Maybe.unwrap "" formatTime availability.endTime
     in div []
        [ input -- TODO: Refactor
            [ type_ "time"
            , A.min (startTime |> formatTime |> String.dropRight 3)
            , A.max (endTime |> formatTime |> String.dropRight 3)
            , value <| startTimeValue
            , onInput (AvailabilityTimeChanged StartTime dayidx availidx)
            ] []
        , input
            [ type_ "time"
            , A.min (startTime |> formatTime |> String.dropRight 3)
            , A.max (endTime |> formatTime |> String.dropRight 3)
            , value <| endTimeValue
            , onInput (AvailabilityTimeChanged EndTime dayidx availidx)
            ] []
        , button [onClick (DeleteAvailability dayidx availidx)] [text "Delete"]
        ]
