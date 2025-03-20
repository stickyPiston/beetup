module Pages.Login exposing (..)

import Html exposing (Html, form, input, label, text, br)
import Html.Events exposing (onSubmit, onInput)
import Html.Attributes exposing (name, type_, value)

import Browser.Navigation as Nav
import Http

import Json.Encode as Encode
import Json.Decode exposing (int)
import Models exposing (User, userDecoder)

-- MODEL

type alias Model =
    { loginForm : LoginForm
    , registerForm : RegisterForm
    , user : Maybe User
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

init : Maybe User -> Model
init user =
    { loginForm =
        { username = ""
        , password = ""
        }
    , registerForm =
        { username = ""
        , password = ""
        , name = ""
        }
    , user = user
    }

-- UPDATE

type Msg
    = LoginFormSubmit
    | LoginFormMsg (LoginForm -> LoginForm)
    | RegisterFormSubmit
    | RegisterFormMsg (RegisterForm -> RegisterForm)
    | GotUser (Result Http.Error User)
    | GotUserId (Result Http.Error Int)

update : Nav.Key -> Msg -> Model -> (Model, Cmd Msg)
update key msg model = case msg of
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
            , expect = Http.expectJson GotUserId int
            }
        )
    RegisterFormMsg updateFn -> ({ model | registerForm = updateFn model.registerForm }, Cmd.none)
    GotUserId response -> case response of
        Ok _  ->
            let request = Http.get
                    { url = "http://localhost:8001/user"
                    , expect = Http.expectJson GotUser userDecoder
                    }
             in (model, request)
        Err _ -> (model, Cmd.none)
    GotUser response -> case response of
        Ok user ->
            ( { model | user = Just user }
            , Nav.pushUrl key "/"
            )
        Err _ -> (model, Cmd.none)

-- DECODERS / ENCODERS

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

-- VIEWS

view : Model -> List (Html Msg)
view model = [viewLoginForm model.loginForm, viewRegisterForm model.registerForm]

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