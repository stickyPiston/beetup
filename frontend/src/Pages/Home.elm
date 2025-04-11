module Pages.Home exposing (..)

import Html exposing (Html, text, button, a, div, h2, ul, li, strong, input, p)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (href, multiple, type_)

import Http
import File exposing (File)

import Models exposing (User, Occupancy, User)
import Utils.DateTime exposing (DateTime, formatDateTime, utcTimeDecoder)
import List.Extra as List
import Json.Decode as Decode exposing (Decoder, list)
import Json.Decode.Pipeline exposing (required, requiredAt)

-- MODEL

type FetchRequest a
    = Loading
    | Got a
    | Error Http.Error

type alias Occupancy =
    { title : String
    , start : DateTime
    , end : DateTime
    }

type alias Meeting =
    { id : String
    , added : Int
    , availabilities : Int
    , title : String
    }

type alias Model =
    { meetings : FetchRequest (List Meeting)
    , selectedFile : Maybe File
    , error : Maybe String
    , occupancies : List Occupancy
    }

init : (Model, Cmd Msg)
init =
    ( { meetings = Loading
      , selectedFile = Nothing
      , error = Nothing
      , occupancies = []
      }
    , Cmd.batch
        [ Http.get
            { url = "http://localhost:8001/meeting"
            , expect = Http.expectJson GotMeetings meetingsDecoder
            }
        , Http.get
            { url = "http://localhost:8001/occupancies"
            , expect = Http.expectJson GotOccupancies occupanciesDecoder
            }
        ]
    )

-- UPDATE

type Msg
    = Logout
    | LoggedOut (Result Http.Error ())
    | GotMeetings (Result Http.Error (List Meeting))
    | SelectedFile File
    | UploadFile
    | UploadedFile (Result Http.Error ())
    -- | DeleteOccupancy Int
    -- | DeletedOccupancy Int (Result Http.Error ())
    | GotOccupancies (Result Http.Error (List Occupancy))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Logout ->
        ( model
        , Http.get
            { url = "http://localhost:8001/logout"
            , expect = Http.expectWhatever LoggedOut
            }
        )
    LoggedOut _ -> (model, Cmd.none)
    GotMeetings (Ok meetings) -> ({ model | meetings = Got meetings }, Cmd.none)
    GotMeetings (Err error) -> ({ model | meetings = Error error }, Cmd.none)
    SelectedFile file -> ({ model | selectedFile = Just file }, Cmd.none)
    UploadFile ->
        let uploadCmd file = Http.post
                { url = "http://localhost:8001/calendar"
                , body = Http.fileBody file
                , expect = Http.expectWhatever UploadedFile
                }
         in case model.selectedFile of
            Just file -> ({ model | error = Nothing }, uploadCmd file)
            Nothing -> ({ model | error = Just "You did not selected a file" }, Cmd.none)
    UploadedFile (Ok _) -> ({ model | selectedFile = Nothing, error = Nothing }, Cmd.none)
    UploadedFile _ -> ({ model | error = Just "Could not upload file" }, Cmd.none)
    -- DeleteOccupancy idx ->
    --     case List.getAt idx model.occupancies of
    --         Just occupancy -> (model, Http.request
    --             { method = "DELETE"
    --             , headers = []
    --             , url = "http://localhost:8001/occupancies/" ++ occupancy.id
    --             , body = Http.emptyBody
    --             , expect = Http.expectWhatever (DeletedOccupancy idx)
    --             , timeout = Nothing
    --             , tracker = Nothing
    --             })
    --         Nothing -> (model, Cmd.none) -- Cannot happen
    -- DeletedOccupancy _ (Err _) -> ({ model | error = Just "Could not delete occupancy" }, Cmd.none)
    -- DeletedOccupancy idx (Ok _) ->
    --     let updatedOccupancies = List.removeAt idx model.occupancies 
    --      in ({ model | occupancies = updatedOccupancies }, Cmd.none)
    GotOccupancies (Err _) -> ({ model | error = Just "Could not retrieve occupancies" }, Cmd.none)
    GotOccupancies (Ok occupancies) -> ({ model | occupancies = occupancies }, Cmd.none)

fileDecoder : Decoder Msg
fileDecoder =
    let decodeFirst files = case files of
            f :: _ -> Decode.succeed f
            _ -> Decode.fail "No files selected"
     in list File.decoder
        |> Decode.at ["target", "files"]
        |> Decode.andThen decodeFirst
        |> Decode.map SelectedFile

meetingsDecoder : Decoder (List Meeting)
meetingsDecoder =
    Decode.succeed Meeting
    |> required "id" Decode.string
    |> requiredAt ["stats", "added"] Decode.int
    |> requiredAt ["stats", "availabilities"] Decode.int
    |> required "title" Decode.string
    |> Decode.list

occupanciesDecoder : Decoder (List Occupancy)
occupanciesDecoder =
    Decode.succeed Occupancy
    |> required "title" Decode.string
    |> required "start" utcTimeDecoder
    |> required "end" utcTimeDecoder
    |> Decode.list

-- VIEW

view : Maybe User -> Model -> List (Html Msg)
view user model = case user of
    Just { name } ->
        let prependErrorTo body = case model.error of
                Nothing -> body
                Just error -> p [] [text error] :: body
         in prependErrorTo
            [ p [] [text ("Welcome " ++ name ++ "!")]
            , p [] [button [onClick Logout] [text "Log out"]]
            , p [] [viewMeetings model]
            , viewOccupanciesPanel model.occupancies
            ] 
    Nothing ->
        [ text "Please login first!"
        , a [href "/login"] [text "To the login page"]
        ]

viewMeetings : Model -> Html Msg
viewMeetings model = case model.meetings of
    Loading -> strong [] [text "Loading meetings..."]
    Error _ -> strong [] [text "Error loading meetings"] 
    Got meetings ->
        let viewNewMeeting = li [] [a [href "/meeting"] [text "Plan a new meeting"]]
         in div []
            [ h2 [] [text "My meetings"]
            , ul [] <| List.map viewMeeting meetings ++ [viewNewMeeting]
            ]

viewMeeting : Meeting -> Html Msg
viewMeeting meeting = li []
    [ a
        [href <| "/availability/" ++ meeting.id]
        [text <| meeting.title ++ " (" ++ String.fromInt meeting.added ++ " joined, "
            ++ String.fromInt meeting.availabilities ++ " answered)"]
    ]

viewOccupanciesPanel : List Occupancy -> Html Msg
viewOccupanciesPanel occupancies = div []
    [ h2 [] [text "Occupancies"]
    , viewCalendarUpload
    , viewOccupancies occupancies
    ]

viewCalendarUpload : Html Msg
viewCalendarUpload =
    p []
        [ input
            [ type_ "file"
            , multiple False
            , on "change" (fileDecoder)
            ]
            []
        , button [onClick UploadFile] [text "Upload calendar"]
        ]

viewOccupancies : List Occupancy -> Html Msg
viewOccupancies occupancies = ul [] (List.indexedMap viewOccupancy occupancies)

viewOccupancy : Int -> Occupancy -> Html Msg
viewOccupancy idx { title, start, end } =
    li []
        [ text <| title ++ " from " ++ formatDateTime start ++ " to " ++ formatDateTime end
        -- , button [onClick (DeleteOccupancy idx)] [text "Delete"]
        ]