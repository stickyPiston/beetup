module Main exposing (main)

import Browser
import Browser exposing (UrlRequest, Document)
import Browser.Navigation exposing (Key)

import Html

import Url exposing (Url)

import Http

import Pages.Login as LoginPage
import Pages.Home as HomePage
import Pages.CreateMeeting as CreateMeetingPage

main : Program () Model Msg
main = Browser.application
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    , onUrlChange = OnUrlChange
    , onUrlRequest = OnUrlRequest
    }

type alias Model =
    { currentUrl : Url
    , loginPage : LoginPage.Model
    , createMeetingPage : CreateMeetingPage.Model
    }

init : () -> Url -> Key -> (Model, Cmd Msg)
init () url _ = 
    let model =
            { currentUrl = url
            , loginPage = LoginPage.init
            , createMeetingPage = CreateMeetingPage.init
            }
     in ( model
        , Http.get
            { url = "http://localhost:8001/user"
            , expect = Http.expectJson
                (LoginPageMsg << LoginPage.GotUser)
                LoginPage.userDecoder
            }
        )

type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | LoginPageMsg LoginPage.Msg
    | HomePageMsg HomePage.Msg
    | CreateMeetingMsg CreateMeetingPage.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    OnUrlChange url ->
        ( { model | currentUrl = url }
        , Cmd.none
        )
    OnUrlRequest _ -> (model, Cmd.none)
    LoginPageMsg lmsg ->
        let (newModel, cmd) = LoginPage.update lmsg model.loginPage
         in ({ model | loginPage = newModel }, Cmd.map LoginPageMsg cmd)
    HomePageMsg hmsg ->
        let (newModel, cmd) = HomePage.update hmsg model.loginPage
         in ({ model | loginPage = newModel }, Cmd.map HomePageMsg cmd)
    CreateMeetingMsg cmsg ->
        let (newModel, cmd) = CreateMeetingPage.update cmsg model.createMeetingPage
         in ({ model | createMeetingPage = newModel }, Cmd.map CreateMeetingMsg cmd)

view : Model -> Document Msg
view model =
    { title = "Elm"
    -- , body = case model.loginPage.user of
    --     Just user ->
    --         HomePage.view user
    --         |> List.map (Html.map HomePageMsg)
    --     Nothing ->
    --         LoginPage.view model.loginPage
    --         |> List.map (Html.map LoginPageMsg)
    , body = CreateMeetingPage.view model.createMeetingPage
        |> List.map (Html.map CreateMeetingMsg)
    }
