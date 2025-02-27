module Main exposing (..)

import Browser
import Browser exposing (UrlRequest, Document)
import Browser.Navigation exposing (Key)

import Html exposing (form, input, label, text, br)
import Html.Events exposing (onSubmit, onInput)
import Html.Attributes exposing (name, type_, value)

import Url exposing (Url)

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
    { count : Int
    , currentUrl : Url
    , username : String
    , password : String
    }

init : () -> Url -> Key -> (Model, Cmd Msg)
init () url _ = 
    let model = { count = 0, currentUrl = url, password = "", username = "" }
     in (model, Cmd.none)

type Msg
    = Increment
    | Decrement
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | LoginFormSubmit
    | UsernameChanged String
    | PasswordChanged String

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
        UsernameChanged new -> ({ model | username = new }, Cmd.none)
        PasswordChanged new -> ({ model | password = new }, Cmd.none)
        LoginFormSubmit -> Debug.log (Debug.toString model) (model, Cmd.none)

view : Model -> Document Msg
view model =
    { title = "Elm"
    , body = 
        [ form [onSubmit LoginFormSubmit]
            [ label [] [text "Username: "]
            , input [value model.username, type_ "text", name "username", onInput UsernameChanged] []
            , br [] []
            , label [] [text "Password: "]
            , input [value model.password, type_ "password", name "password", onInput PasswordChanged] []
            , br [] []
            , input [type_ "submit"] []
            ]
        ]
    }
