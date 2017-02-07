module Api exposing (..)

import MainModel exposing (..)
import MainMessages exposing (..)
import Json.Decode
import Json.Decode.Pipeline exposing (required, decode)
import Http
import Json.Encode


apiUpdateDecoder : Json.Decode.Decoder ApiUpdate
apiUpdateDecoder =
    decode ApiUpdate
        |> required "sessions" (Json.Decode.list sessionDecoder)
        |> required "tracks" (Json.Decode.list trackDecoder)
        |> required "columns" (Json.Decode.list columnDecoder)
        |> required "dates" (Json.Decode.list dateDecoder)


encodeApiUpdate : ApiUpdate -> Json.Encode.Value
encodeApiUpdate record =
    Json.Encode.object
        [ ( "sessions", Json.Encode.list <| List.map sessionEncoder record.sessions )
        , ( "tracks", Json.Encode.list <| List.map trackEncoder record.tracks )
        , ( "columns", Json.Encode.list <| List.map columnEncoder record.columns )
        , ( "dates", Json.Encode.list <| List.map dateEncoder record.dates )
        ]


sessionEncoder : Session -> Json.Encode.Value
sessionEncoder record =
    Json.Encode.object
        [ ( "id", Json.Encode.int record.id )
        , ( "name", Json.Encode.string record.name )
        , ( "description", Json.Encode.string record.description )
        , ( "date", dateEncoder record.date )
        , ( "startTime", timeEncoder record.startTime )
        , ( "endTime", timeEncoder record.endTime )
        , ( "columnId", Json.Encode.int record.columnId )
        , ( "trackId", Json.Encode.int record.trackId )
        , ( "location", Json.Encode.string record.location )
        , ( "submissionIds", Json.Encode.list <| List.map Json.Encode.int record.submissionIds )
        , ( "chair", Json.Encode.string record.chair )
        ]


sessionDecoder : Json.Decode.Decoder Session
sessionDecoder =
    decode Session
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string
        |> required "description" Json.Decode.string
        |> required "date" dateDecoder
        |> required "startTime" timeDecoder
        |> required "endTime" timeDecoder
        |> required "columnId" Json.Decode.int
        |> required "trackId" Json.Decode.int
        |> required "location" Json.Decode.string
        |> required "submissionIds" (Json.Decode.list Json.Decode.int)
        |> required "chair" Json.Decode.string


trackEncoder : Track -> Json.Encode.Value
trackEncoder record =
    Json.Encode.object
        [ ( "id", Json.Encode.int record.id )
        , ( "name", Json.Encode.string record.name )
        ]


trackDecoder : Json.Decode.Decoder Track
trackDecoder =
    decode Track
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string


columnEncoder : Column -> Json.Encode.Value
columnEncoder record =
    Json.Encode.object
        [ ( "id", Json.Encode.int record.id )
        , ( "name", Json.Encode.string record.name )
        ]


columnDecoder : Json.Decode.Decoder Column
columnDecoder =
    decode Column
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string


dateDecoder : Json.Decode.Decoder DateWithoutTime
dateDecoder =
    decode DateWithoutTime
        |> required "year" Json.Decode.int
        |> required "month" Json.Decode.int
        |> required "day" Json.Decode.int


dateEncoder : DateWithoutTime -> Json.Encode.Value
dateEncoder record =
    Json.Encode.object
        [ ( "year", Json.Encode.int record.year )
        , ( "month", Json.Encode.int record.month )
        , ( "day", Json.Encode.int record.day )
        ]


timeDecoder : Json.Decode.Decoder TimeOfDay
timeDecoder =
    decode TimeOfDay
        |> required "hour" Json.Decode.int
        |> required "minute" Json.Decode.int


timeEncoder : TimeOfDay -> Json.Encode.Value
timeEncoder record =
    Json.Encode.object
        [ ( "hour", Json.Encode.int record.hour )
        , ( "minute", Json.Encode.int record.minute )
        ]


getModelFromDb : String -> Cmd Msg
getModelFromDb eventId =
    let
        url =
            "/events/" ++ eventId ++ "/programme-builder-model"

        request =
            Http.get url apiUpdateDecoder
    in
        Http.send UpdateModel request


postModelToDb : ApiUpdate -> String -> Cmd Msg
postModelToDb apiUpdateModel eventId =
    let
        requestUrl =
            "/events/" ++ eventId ++ "/programme-builder-model"

        request =
            Http.post requestUrl (Http.jsonBody (encodeApiUpdate apiUpdateModel)) apiUpdateDecoder
    in
        Http.send SaveModel request
