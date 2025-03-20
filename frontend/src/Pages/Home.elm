module Pages.Home exposing (..)

import Html exposing (Html, text, button, a, div, h2, ul, li, strong)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href)

import Http

import Pages.Login as LoginPage
import Models exposing (Meeting, meetingDecoder)
import Json.Decode exposing (list)

-- MODEL

type FetchRequest a
    = Loading
    | Got a
    | Error Http.Error

type alias Model =
    { login : LoginPage.Model
    , meetings : FetchRequest (List Meeting)
    }

init : LoginPage.Model -> (Model, Cmd Msg)
init login =
    ( { login = login
      , meetings = Loading
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Logout ->
        ( model
        , Http.get
            { url = "http://localhost:8001/logout"
            , expect = Http.expectWhatever LoggedOut
            }
        )
    LoggedOut response ->
        let updateModel newLogin = { model | login = newLogin }
            login = model.login
         in ( response
                |> Result.map  (\ _ -> updateModel { login | user = Nothing })
                |> Result.withDefault model
            , Cmd.none
            )
    GotMeetings (Ok meetings) -> ({ model | meetings = Got meetings }, Cmd.none)
    GotMeetings (Err error) -> ({ model | meetings = Error error }, Cmd.none)

-- VIEW

view : Model -> List (Html Msg)
view model = case model.login.user of
    Just { name } ->
        [ text ("Welcome " ++ name ++ "!")
        , button [onClick Logout] [text "Log out"]
        , viewMeetings model
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
viewMeeting meeting = li [] [a [href <| "/availability/" ++ meeting.id] [text meeting.title]]