module Main exposing (main)

import Browser
import Browser exposing (UrlRequest(..), Document)
import Browser.Navigation as Nav exposing (Key)

import Html exposing (text)

import Url exposing (Url)
import Url.Parser exposing (Parser, s, top, int, (</>), map, oneOf, parse)

import Http

import Pages.Login as Login
import Pages.Home as Home
import Pages.Availability as Availability
import Pages.Meeting as Meeting

main : Program () Model Msg
main = Browser.application
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    , onUrlChange = OnUrlChange
    , onUrlRequest = OnUrlRequest
    }

-- MODEL

type alias Model =
    { navKey : Key
    , currentRoute : Route
    , loginModel : Login.Model
    , meetingModel : Meeting.Model
    , availabilityModel : Availability.Model
    }

init : () -> Url -> Key -> (Model, Cmd Msg)
init () url key =
    let model =
            { navKey = key
            , currentRoute = routeFromUrl url
            , loginModel = Login.init
            , availabilityModel = Availability.init
            , meetingModel = Meeting.init
            }
     in ( model
        , Http.get
            { url = "http://localhost:8001/user"
            , expect = Http.expectJson (LoginMsg << Login.GotUser) Login.userDecoder
            }
        )

-- UPDATE

type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | LoginMsg Login.Msg
    | HomeMsg Home.Msg
    | AvailabilityMsg Availability.Msg
    | MeetingMsg Meeting.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    OnUrlChange url -> ({ model | currentRoute = routeFromUrl url } , Cmd.none)
    OnUrlRequest request -> case request of
        Internal url -> (model, Nav.pushUrl model.navKey (Url.toString url))
        External url -> (model, Nav.load url)
    LoginMsg lmsg ->
        let (newModel, cmd) = Login.update lmsg model.loginModel
         in ({ model | loginModel = newModel }, Cmd.map LoginMsg cmd)
    HomeMsg hmsg ->
        let (newModel, cmd) = Home.update hmsg model.loginModel
         in ({ model | loginModel = newModel }, Cmd.map HomeMsg cmd)
    AvailabilityMsg cmsg ->
        let (newModel, cmd) = Availability.update cmsg model.availabilityModel
         in ({ model | availabilityModel = newModel }, Cmd.map AvailabilityMsg cmd)
    MeetingMsg mmsg ->
        let (newModel, cmd) = Meeting.update mmsg model.meetingModel
         in ({ model | meetingModel = newModel }, Cmd.map MeetingMsg cmd)

view : Model -> Document Msg
view model =
    { title = "Elm"
    , body = case model.currentRoute of
        Home -> Home.view model.loginModel |> List.map (Html.map HomeMsg)
        Meeting -> Meeting.view model.meetingModel |> List.map (Html.map MeetingMsg)
        Availability id -> Availability.view model.availabilityModel |> List.map (Html.map AvailabilityMsg)
        Login -> Login.view model.loginModel |> List.map (Html.map LoginMsg)
        NotFound _ -> [text "Not found"]
    }

-- ROUTES

type Route
    = Home
    | Meeting
    | Availability Int
    | Login
    | NotFound Url

routeParser : Parser (Route -> a) a
routeParser = oneOf
    [ map Home top
    , map Meeting (s "meeting")
    , map Availability (s "availability" </> int)
    , map Login (s "login")
    ]

routeFromUrl : Url -> Route
routeFromUrl url = 
    let parsedRoute = parse routeParser url
     in Maybe.withDefault (NotFound url) parsedRoute