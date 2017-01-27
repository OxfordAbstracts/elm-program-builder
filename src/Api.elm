module Api exposing (getModelFromDb, postModelToDb)

import MainModel exposing (..)
import MainMessages exposing (..)
import Json.Decode as Json
import Json.Decode.Pipeline exposing (required, decode)
import Http
import Json.Encode


decodeUrl : Json.Decoder ApiUpdate
decodeUrl =
    decode ApiUpdate
        |> required "sessions" (Json.list sessionDecoder)



-- |> required "tracks" (Json.list trackDecoder)
-- |> required "columns" (Json.list columnDecoder)
-- |> required "dates" (Json.list dateDecoder)


encodeModel : ApiUpdate -> Json.Encode.Value
encodeModel record =
    Json.Encode.object
        [ ( "sessions", Json.Encode.list <| List.map sessionEncoder <| record.sessions )
          -- , ( "tracks", Json.Encode.list <| List.map encodeComplexType <| record.tracks )
          -- , ( "columns", Json.Encode.list <| List.map encodeComplexType <| record.columns )
          -- , ( "dates", Json.Encode.list <| List.map encodeComplexType <| record.dates )
        ]


sessionEncoder : Session -> Json.Encode.Value
sessionEncoder record =
    Json.Encode.object
        [ ( "id", Json.Encode.int <| record.id )
        , ( "name", Json.Encode.string <| record.name )
        , ( "description", Json.Encode.string <| record.description )
        , ( "date", dateEncoder <| record.date )
        , ( "startTime", timeEncoder <| record.startTime )
        , ( "endTime", timeEncoder <| record.endTime )
        , ( "columnId", Json.Encode.int <| record.columnId )
        , ( "trackId", Json.Encode.int <| record.trackId )
        , ( "location", Json.Encode.string <| record.location )
        , ( "submissionIds", Json.Encode.list <| List.map Json.Encode.int <| record.submissionIds )
        , ( "chair", Json.Encode.string <| record.chair )
        ]


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


dateEncoder : DateWithoutTime -> Json.Encode.Value
dateEncoder record =
    Json.Encode.object
        [ ( "year", Json.Encode.int <| record.year )
        , ( "month", Json.Encode.int <| record.month )
        , ( "day", Json.Encode.int <| record.day )
        ]


timeDecoder : Json.Decoder TimeOfDay
timeDecoder =
    decode TimeOfDay
        |> required "hour" Json.int
        |> required "minute" Json.int


timeEncoder : TimeOfDay -> Json.Encode.Value
timeEncoder record =
    Json.Encode.object
        [ ( "hour", Json.Encode.int <| record.hour )
        , ( "minute", Json.Encode.int <| record.minute )
        ]


getModelFromDb : Cmd Msg
getModelFromDb =
    let
        url =
            "/get-model-from-db"

        request =
            Http.get url decodeUrl
    in
        Http.send UpdateModel request


postModelToDb : ApiUpdate -> Cmd Msg
postModelToDb apiUpdateModel =
    let
        -- url =
        --     "http://localhost:5000/post-model-from-db"
        request =
            Http.post "/post-model-to-db" (Http.jsonBody (encodeModel apiUpdateModel)) decodeUrl
    in
        Http.send SaveModel request



-- Http.stringBody (encodeModel)
-- in
--     Http.send SaveModel MainModel.Model request
