module Main exposing (..)

import Html exposing (Html, div, button, text, program)
import Html.Attributes exposing (..)
import ControlsView exposing (view)
import MainModel exposing (..)
import MainMessages exposing (..)
import MainUpdate exposing (update)
import Stylesheet exposing (view)
import TableView exposing (view)
import Json.Decode as Json exposing (int, string, float, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Http


init : ( Model, Cmd Msg )
init =
    ( initialModel, getModelFromDb )


decodeUrl : Json.Decoder String
decodeUrl =
    Json.at [ "model" ] Json.string



--     -- Json.at [ "model" ] Json.string
--     Json.decodeString (field "Session" Json.)
-- decodeUrl : Json.Decoder Model
-- http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode
-- http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest
-- decodeUrl : Json.Decoder Model
-- decodeUrl =
--     let
--         required =
--             Json.Decode.Pipeline.required
--     in
--         decode Model
--             |> required "sessions" (Json.list Session)
--             |> required "tracks" Json.list
--             |> required "columns" Json.list
--             |> required "dates" Json.list
--             |> required "showNewSessionUi" Json.bool
--             |> required "showNewTrackUi" Json.bool
--             |> required "showNewColumnUi" Json.bool
--             |> required "newSession" Session
--             |> required "newColumn" Column
--             |> required "newTrack" Track
--             |> required "idOfSessionBeingEdited" Json.int
-- Json.decode Model
--     (field "days" Json.int)
--     (field "avgPerDay" Json.float)
--     (field "reports" Json.int)
--     (field "people" Json.int)
--     (field "locations" Json.int)
--     ("totalCoffees" := Json.int)
--     ("coffeesPerDay" (:=) Json.float)
-- (field "session" Json.list)


getModelFromDb : Cmd Msg
getModelFromDb =
    let
        url =
            "localhost:5000/get-model-from-db"

        requestString =
            Http.get url decodeUrl

        -- requestModel =
        --     requestString
        -- convert request string to model
    in
        Http.send UpdateModel requestString



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
