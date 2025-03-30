module Pages.Home exposing (..)

import Html exposing (Html, text, button, a, div, h2, ul, li, strong, input)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (href, multiple, type_)

import Http
import File exposing (File)

import Models exposing (Meeting, meetingDecoder, User)
import Json.Decode as Decode exposing (Decoder, list)
import Maybe.Extra as Maybe

-- MODEL

type FetchRequest a
    = Loading
    | Got a
    | Error Http.Error

type alias Model =
    { meetings : FetchRequest (List Meeting)
    , selectedFile : Maybe File
    }

init : (Model, Cmd Msg)
init =
    ( { meetings = Loading
      , selectedFile = Nothing
      }
    , Http.get
        { url = "http://localhost:8001/meetings"
        , expect = Http.expectJson GotMeetings (list meetingDecoder)
        }
    )

-- UPDATE

type Msg
    = Logout
    | LoggedOut (Result Http.Error ())
    | GotMeetings (Result Http.Error (List Meeting))
    | SelectedFile File
    | UploadFile
    | UploadedFile (Result Http.Error ())

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
         in (model, Maybe.unwrap Cmd.none uploadCmd model.selectedFile)
    UploadedFile (Ok _) -> ({ model | selectedFile = Nothing }, Cmd.none)
    UploadedFile _ -> (model, Cmd.none)

fileDecoder : Decoder Msg
fileDecoder =
    let decodeFirst files = case files of
            f :: _ -> Decode.succeed f
            _ -> Decode.fail "No files selected"
     in list File.decoder
        |> Decode.at ["target", "files"]
        |> Decode.andThen decodeFirst
        |> Decode.map SelectedFile

-- VIEW

view : Maybe User -> Model -> List (Html Msg)
view user model = case user of
    Just { name } ->
        [ text ("Welcome " ++ name ++ "!")
        , button [onClick Logout] [text "Log out"]
        , viewMeetings model
        ] ++ viewCalendarUpload
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
viewMeeting meeting = li [] [a [href <| "/availability/" ++ meeting.id] [text meeting.title]]

viewCalendarUpload : List (Html Msg)
viewCalendarUpload =
    [ input
        [ type_ "file"
        , multiple False
        , on "change" (fileDecoder)
        ]
        []
    , button [onClick UploadFile] [text "Upload calendar"]
    ]