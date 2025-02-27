module Main exposing (..)

import Browser
import Browser exposing (UrlRequest, Document)
import Browser.Navigation exposing (Key)

import Html exposing (Html, form, input, label, text, br, button)
import Html.Events exposing (onSubmit, onInput, onClick)
import Html.Attributes exposing (name, type_, value)

import Url exposing (Url)

import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode

main : Program () Model Msg
main = Browser.application
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    , onUrlChange = OnUrlChange
    , onUrlRequest = OnUrlRequest
    }

type alias LoginForm =
    { username : String
    , password : String
    }

type alias Model =
    { count : Int
    , currentUrl : Url
    , loginForm : LoginForm
    , user : Maybe User
    }

type alias User =
    { name : String
    , id : String
    }

init : () -> Url -> Key -> (Model, Cmd Msg)
init () url _ = 
    let model =
            { count = 0
            , currentUrl = url
            , loginForm =
                { password = ""
                , username = ""
                }
            , user = Nothing
            }
     in ( model
        , Http.get
            { url = "http://localhost:8001/user"
            , expect = Http.expectJson GotUser userDecoder
            }
        )

type Msg
    = Increment
    | Decrement
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | LoginFormSubmit
    | UsernameChanged String
    | PasswordChanged String
    | GotUser (Result Http.Error User)
    | Logout
    | LoggedOut (Result Http.Error ())

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )
        Decrement ->
            ( { model | count = model.count - 1 }
            , Cmd.none
            )
        OnUrlChange url ->
            ( { model | currentUrl = url }
            , Cmd.none
            )
        OnUrlRequest _ -> (model, Cmd.none)
        UsernameChanged new ->
            let loginForm = model.loginForm
                updatedLoginForm = { loginForm | username = new }
             in ({ model | loginForm = updatedLoginForm }, Cmd.none)
        PasswordChanged new ->
            let loginForm = model.loginForm
                updatedLoginForm = { loginForm | password = new }
             in ({ model | loginForm = updatedLoginForm }, Cmd.none)
        LoginFormSubmit ->
            (model, Http.post
                { body = Http.jsonBody (loginEncoder model.loginForm)
                , url = "http://localhost:8001/login"
                , expect = Http.expectJson GotUser userDecoder
                })
        GotUser response ->
            ( response
                |> Result.map (\user -> { model | user = Just user })
                |> Result.withDefault model
            , Cmd.none
            )
        Logout ->
            ( model
            , Http.get
                { url = "http://localhost:8001/logout"
                , expect = Http.expectWhatever LoggedOut
                }
            )
        LoggedOut response ->
            ( response
                |> Result.map (\_ -> { model | user = Nothing })
                |> Result.withDefault model
            , Cmd.none
            )

loginEncoder : LoginForm -> Encode.Value
loginEncoder login = Encode.object 
    [ ("username", Encode.string login.username)
    , ("password", Encode.string login.password)
    ]

userDecoder : Decoder User
userDecoder = Decode.succeed User
    |> required "name" string
    |> required "id" string

view : Model -> Document Msg
view model =
    { title = "Elm"
    , body = case model.user of
        Just user -> viewLoggedIn user
        Nothing -> viewLoginForm model.loginForm
    }

viewLoggedIn : User -> List (Html Msg)
viewLoggedIn user =
    [ text ("Welcome " ++ user.name ++ "!")
    , button [onClick Logout] [text "Log out"]
    ]

viewLoginForm : LoginForm -> List (Html Msg)
viewLoginForm loginForm =
    [ form [onSubmit LoginFormSubmit]
        [ label [] [text "Username: "]
        , input [value loginForm.username, type_ "text", name "username", onInput UsernameChanged] []
        , br [] []
        , label [] [text "Password: "]
        , input [value loginForm.password, type_ "password", name "password", onInput PasswordChanged] []
        , br [] []
        , input [type_ "submit"] []
        ]
    ]
