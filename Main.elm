module Main exposing (..)

import Html exposing (Html, div, button, text, program)
import Html.Attributes exposing (class)
import ControlsView exposing (view)
import MainModel exposing (..)
import MainMessages exposing (..)
import MainUpdate exposing (update)
import Stylesheet exposing (view)
import TableView exposing (view)
import Api


-- import Json.Decode as Json exposing (int, string, float, Decoder)
-- import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


init : ( Model, Cmd Msg )
init =
    ( initialModel, Api.getModelFromDb )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Html.h2 [] [ text "Program builder" ]
        , Stylesheet.view
        , ControlsView.view model
        , TableView.view model
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
