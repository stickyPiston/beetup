module Main exposing (main)

import Browser
import Browser exposing (UrlRequest(..), Document)
import Browser.Navigation as Nav exposing (Key)

import Html exposing (text)

import Url exposing (Url)
import Url.Parser exposing (Parser, s, top, string, (</>), map, oneOf, parse)

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

type Page
    = LoginPage Login.Model
    | MeetingPage Meeting.Model
    | AvailabilityPage Availability.Model
    | HomePage Home.Model
    | NotFoundPage

type alias Model =
    { navKey : Key
    , currentRoute : Route
    , currentPage : Page
    , user : Maybe User
    }

init : Decode.Value -> Url -> Key -> (Model, Cmd Msg)
init flag url key =
    let user = Decode.decodeValue userDecoder flag |> Result.toMaybe
        currentRoute = routeFromUrl url
        (currentPage, cmd) = newPageFromRoute currentRoute
        model = { navKey = key , currentRoute = currentRoute , currentPage = currentPage , user = user }
     in (model, cmd)

-- UPDATE

newPageFromRoute : Route -> (Page, Cmd Msg)
newPageFromRoute route = case route of
        Home ->
            let (model, cmd) = Home.init
             in (HomePage model, Cmd.map HomeMsg cmd)
        Meeting ->
            let (model, cmd) = Meeting.init
             in (MeetingPage model, Cmd.map MeetingMsg cmd)
        Availability id ->
            let (model, cmd) = Availability.init id
             in (AvailabilityPage model, Cmd.map AvailabilityMsg cmd)
        Login -> (LoginPage Login.init, Cmd.none)
        NotFound _ -> (NotFoundPage, Cmd.none)

type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | LoginMsg Login.Msg
    | HomeMsg Home.Msg
    | AvailabilityMsg Availability.Msg
    | MeetingMsg Meeting.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (msg, model.currentPage) of
    (OnUrlChange url, _) ->
        let currentRoute = routeFromUrl url
            (currentPage, cmd) = newPageFromRoute currentRoute
         in ( { model
            | currentRoute = currentRoute
            , currentPage = currentPage
            }
        , cmd)
    (OnUrlRequest request, _) -> case request of
        Internal url -> (model, Nav.pushUrl model.navKey (Url.toString url))
        External url -> (model, Nav.load url)
    (LoginMsg (Login.GotUser response as lmsg), page) ->
        let loginResponse = case page of
                LoginPage loginModel -> Login.update model.navKey lmsg loginModel 
                    |> Tuple.mapBoth LoginPage (Cmd.map LoginMsg)
                _ -> (page, Cmd.none)
            returned = Tuple.mapFirst (\ p -> { model | currentPage = p }) loginResponse
         in case response of
            Err _ -> returned
            Ok user -> Tuple.mapFirst (\ m -> { m | user = Just user }) returned
    (LoginMsg lmsg, LoginPage loginModel) ->
        let (newPage, cmd) = Login.update model.navKey lmsg loginModel
         in ({ model | currentPage = LoginPage newPage }, Cmd.map LoginMsg cmd)
    (HomeMsg hmsg, HomePage homeModel) ->
        let (newHomeModel, cmd) = Home.update hmsg homeModel
            updateResponse = ({ model | currentPage = HomePage newHomeModel }, Cmd.map HomeMsg cmd)
         in case hmsg of
            Home.LoggedOut _ -> Tuple.mapFirst (\ newModel -> { newModel | user = Nothing }) updateResponse
            _ -> updateResponse
    (AvailabilityMsg cmsg, AvailabilityPage availModel) -> case model.user of
        Just user ->
            let (newModel, cmd) = Availability.update user cmsg availModel
            in ({ model | currentPage = AvailabilityPage newModel }, Cmd.map AvailabilityMsg cmd)
        Nothing -> (model, Cmd.none) -- Impossible
    (MeetingMsg mmsg, MeetingPage meetingModel) ->
        let (newModel, cmd) = Meeting.update mmsg meetingModel
         in ({ model | currentPage = MeetingPage newModel }, Cmd.map MeetingMsg cmd)
    -- If a message is received but the page that requested it is already swapped out,
    -- then just ignore the message.
    (_, _) -> (model, Cmd.none)

view : Model -> Document Msg
view model =
    { title = "Elm"
    , body = case model.currentPage of
        HomePage homeModel -> Home.view model.user homeModel |> List.map (Html.map HomeMsg)
        MeetingPage meetingModel -> Meeting.view meetingModel |> List.map (Html.map MeetingMsg)
        AvailabilityPage availModel -> Availability.view availModel |> List.map (Html.map AvailabilityMsg)
        LoginPage loginModel -> Login.view loginModel |> List.map (Html.map LoginMsg)
        NotFoundPage -> [text "Not found"]
    }

-- ROUTES

type Route
    = Home
    | Meeting
    | Availability String
    | Login
    | NotFound Url

routeParser : Parser (Route -> a) a
routeParser = oneOf
    [ map Home top
    , map Meeting (s "meeting")
    , map Availability (s "availability" </> string)
    , map Login (s "login")
    ]

routeFromUrl : Url -> Route
routeFromUrl url = 
    let parsedRoute = parse routeParser url
     in Maybe.withDefault (NotFound url) parsedRoute