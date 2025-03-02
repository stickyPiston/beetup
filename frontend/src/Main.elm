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

type alias RegisterForm =
    { username : String
    , password : String
    , name : String
    }

type alias Model =
    { count : Int
    , currentUrl : Url
    , loginForm : LoginForm
    , registerForm : RegisterForm
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
            , registerForm =
                { password = ""
                , username = ""
                , name = ""
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
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | LoginFormSubmit
    | LoginFormMsg (LoginForm -> LoginForm)
    | RegisterFormSubmit
    | RegisterFormMsg (RegisterForm -> RegisterForm)
    | GotUser (Result Http.Error User)
    | Logout
    | LoggedOut (Result Http.Error ())

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    OnUrlChange url ->
        ( { model | currentUrl = url }
        , Cmd.none
        )
    OnUrlRequest _ -> (model, Cmd.none)
    LoginFormSubmit ->
        ( model
        , Http.post
            { body = Http.jsonBody (loginEncoder model.loginForm)
            , url = "http://localhost:8001/login"
            , expect = Http.expectJson GotUser userDecoder
            }
        )
    LoginFormMsg updateFn -> ({ model | loginForm = updateFn model.loginForm }, Cmd.none)
    RegisterFormSubmit ->
        ( model
        , Http.post
            { body = Http.jsonBody (registerEncoder model.registerForm)
            , url = "http://localhost:8001/register"
            , expect = Http.expectJson GotUser userDecoder
            }
        )
    RegisterFormMsg updateFn -> ({ model | registerForm = updateFn model.registerForm }, Cmd.none)
    GotUser response ->
        ( response
            |> (Result.map <| \ user -> { model | user = Just user })
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
            |> (Result.map <| \ _ -> { model | user = Nothing })
            |> Result.withDefault model
        , Cmd.none
        )

loginEncoder : LoginForm -> Encode.Value
loginEncoder login = Encode.object 
    [ ("username", Encode.string login.username)
    , ("password", Encode.string login.password)
    ]

registerEncoder : RegisterForm -> Encode.Value
registerEncoder register = Encode.object
    [ ("username", Encode.string register.username)
    , ("password", Encode.string register.password)
    , ("name", Encode.string register.name)
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
        Nothing -> viewForms model
    }

viewLoggedIn : User -> List (Html Msg)
viewLoggedIn user =
    [ text ("Welcome " ++ user.name ++ "!")
    , button [onClick Logout] [text "Log out"]
    ]

viewForms : Model -> List (Html Msg)
viewForms model = [viewLoginForm model.loginForm, viewRegisterForm model.registerForm]

viewLoginForm : LoginForm -> Html Msg
viewLoginForm loginForm =
    let updateUsername val = LoginFormMsg <| \ old -> { old | username = val }
        updatePassword val = LoginFormMsg <| \ old -> { old | password = val }
     in form [onSubmit LoginFormSubmit]
        [ label [] [text "Username: "]
        , input [value loginForm.username, type_ "text", name "username", onInput updateUsername] []
        , br [] []
        , label [] [text "Password: "]
        , input [value loginForm.password, type_ "password", name "password", onInput updatePassword] []
        , br [] []
        , input [type_ "submit"] []
        ]

viewRegisterForm : RegisterForm -> Html Msg
viewRegisterForm registerForm =
    let updateUsername val = RegisterFormMsg <| \ old -> { old | username = val }
        updatePassword val = RegisterFormMsg <| \ old -> { old | password = val }
        updateName     val = RegisterFormMsg <| \ old -> { old | name = val }
    in form [onSubmit RegisterFormSubmit]
        [ label [] [text "Username: "]
        , input [value registerForm.username, type_ "text", name "username", onInput updateUsername] []
        , br [] []
        , label [] [text "Password: "]
        , input [value registerForm.password, type_ "password", name "password", onInput updatePassword] []
        , br [] []
        , label [] [text "Name: "]
        , input [value registerForm.name, type_ "text", name "name", onInput updateName] []
        , br [] []
        , input [type_ "submit"] []
        ]
