module Main exposing (..)

import Date
import Html exposing (Html, div, button, text, program)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import ControlsView exposing (view)
import MainModel exposing (..)
import MainMessages exposing (..)
import MainUpdate exposing (update)
import Stylesheet exposing (view)
import TableView exposing (view)
import Time


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Html.h2 [] [ text "Program builder" ]
        , Stylesheet.view
        , ControlsView.view model
        , TableView.view model.dates model.sessions model.columns
        ]



-- UPDATE
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
