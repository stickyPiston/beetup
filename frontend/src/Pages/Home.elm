module Pages.Home exposing (..)

import Html exposing (Html, text, button)
import Html.Events exposing (onClick)

import Http

import Pages.Login as LoginPage

-- MODEL

type alias Model = LoginPage.Model

-- UPDATE

type Msg
    = Logout
    | LoggedOut (Result Http.Error ())

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
        ( response
            |> Result.map  (\ _ -> { model | user = Nothing })
            |> Result.withDefault model
        , Cmd.none
        )

-- VIEW

view : LoginPage.User -> List (Html Msg)
view user =
    [ text ("Welcome " ++ user.name ++ "!")
    , button [onClick Logout] [text "Log out"]
    ]