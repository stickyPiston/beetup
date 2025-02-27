module Main exposing (..)

import Browser
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Browser exposing (UrlRequest)

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
    }

init : () -> Url -> Key -> (Model, Cmd Msg)
init () url _ = 
    let model = { count = 0, currentUrl = url }
     in (model, Cmd.none)

type Msg
    = Increment
    | Decrement
    | OnUrlChange Url
    | OnUrlRequest UrlRequest

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

view : Model -> Document Msg
view model =
    { title = "Elm"
    , body = 
        [ div []
            [ button [ onClick Decrement ] [ text "-" ]
            , div [] [ text (String.fromInt model.count) ]
            , button [ onClick Increment ] [ text "+" ]
            ]
        ]
    }
