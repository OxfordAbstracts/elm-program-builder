module Api exposing (getModelFromDb)

import MainModel exposing (..)
import MainMessages exposing (..)
import Json.Decode as Json
import Json.Decode.Pipeline exposing (required, decode)
import Http


decodeUrl : Json.Decoder ApiUpdate
decodeUrl =
    decode ApiUpdate
        |> required "sessions" (Json.list sessionDecoder)
        |> required "tracks" (Json.list trackDecoder)
        |> required "columns" (Json.list columnDecoder)
        |> required "dates" (Json.list dateDecoder)


sessionDecoder : Json.Decoder Session
sessionDecoder =
    decode Session
        |> required "id" Json.int
        |> required "name" Json.string
        |> required "description" Json.string
        |> required "date" dateDecoder
        |> required "startTime" timeDecoder
        |> required "endTime" timeDecoder
        |> required "columnId" Json.int
        |> required "trackId" Json.int
        |> required "location" Json.string
        |> required "submissionIds" (Json.list Json.int)
        |> required "chair" Json.string


trackDecoder : Json.Decoder Track
trackDecoder =
    decode Track
        |> required "id" Json.int
        |> required "name" Json.string


columnDecoder : Json.Decoder Column
columnDecoder =
    decode Column
        |> required "id" Json.int
        |> required "name" Json.string


dateDecoder : Json.Decoder DateWithoutTime
dateDecoder =
    decode DateWithoutTime
        |> required "year" Json.int
        |> required "month" Json.int
        |> required "day" Json.int


timeDecoder : Json.Decoder TimeOfDay
timeDecoder =
    decode TimeOfDay
        |> required "hour" Json.int
        |> required "minute" Json.int


getModelFromDb : Cmd Msg
getModelFromDb =
    let
        url =
            "http://localhost:5000/get-model-from-db"

        requestString =
            Http.get url decodeUrl
    in
        Http.send UpdateModel requestString
