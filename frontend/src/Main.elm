module Main exposing (main)

import Browser
import Browser exposing (UrlRequest(..), Document)
import Browser.Navigation as Nav exposing (Key)

import Html exposing (text)

import Url exposing (Url)
import Url.Parser exposing (Parser, s, top, int, (</>), map, oneOf, parse)

import Pages.Login as Login
import Pages.Home as Home
import Pages.Availability as Availability
import Pages.Meeting as Meeting

import Models exposing (User, userDecoder)
import Json.Decode as Decode

main : Program Decode.Value Model Msg
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
    , homeModel : Home.Model
    , user : Maybe User
    }

init : Decode.Value -> Url -> Key -> (Model, Cmd Msg)
init flag url key =
    let user = Decode.decodeValue userDecoder flag
            |> Result.toMaybe
        (meetingModel, meetingCmd) = Meeting.init
        (homeModel, homeCmd) = Home.init
        model =
            { navKey = key
            , currentRoute = routeFromUrl url
            , loginModel = Login.init
            , availabilityModel = Availability.init user
            , meetingModel = meetingModel
            , homeModel = homeModel
            , user = user
            }
     in ( model
        , Cmd.batch
            [ Cmd.map MeetingMsg meetingCmd
            , Cmd.map HomeMsg homeCmd
            ]
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
    LoginMsg (Login.GotUser response as lmsg) ->
        let (newLoginModel, cmd) = Login.update model.navKey lmsg model.loginModel
            loginResponse = ({ model | loginModel = newLoginModel }, Cmd.map LoginMsg cmd)
         in case response of
            Err _ -> loginResponse
            Ok user -> Tuple.mapFirst (\ newModel -> { newModel | user = Just user }) loginResponse
    LoginMsg lmsg ->
        let (newModel, cmd) = Login.update model.navKey lmsg model.loginModel
         in ({ model | loginModel = newModel }, Cmd.map LoginMsg cmd)
    HomeMsg hmsg ->
        let (newHomeModel, cmd) = Home.update hmsg model.homeModel
            updateResponse = ({ model | homeModel = newHomeModel }, Cmd.map HomeMsg cmd)
         in case hmsg of
            Home.LoggedOut _ -> Tuple.mapFirst (\ newModel -> { newModel | user = Nothing }) updateResponse
            _ -> updateResponse
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
        Home -> Home.view model.user model.homeModel |> List.map (Html.map HomeMsg)
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