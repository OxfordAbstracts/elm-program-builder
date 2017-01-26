module Main exposing (..)

import Html exposing (Html, div, button, text, program)
import Html.Attributes exposing (..)
import ControlsView exposing (view)
import MainModel exposing (..)
import MainMessages exposing (..)
import MainUpdate exposing (update)
import Stylesheet exposing (view)
import TableView exposing (view)
import Json.Decode as Json
import Http


init : ( Model, Cmd Msg )
init =
    ( initialModel, getModelFromDb )


decodeUrl : Json.Decoder String
decodeUrl =
    Json.at [ "model" ] Json.string


getModelFromDb : Cmd Msg
getModelFromDb =
    let
        url =
            "/get-model-from-db"

        requestString =
            Http.get url decodeUrl

        requestModel =
            requestString

        -- convert requet string to model
    in
        Http.send UpdateModel requestModel



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
