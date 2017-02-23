module Api exposing (..)

import MainModel exposing (..)
import MainMessages exposing (..)
import Json.Decode
import Json.Decode.Pipeline exposing (required, decode)
import Http
import Json.Encode


apiUpdateGetDecoder : Json.Decode.Decoder ApiUpdateGet
apiUpdateGetDecoder =
    decode ApiUpdateGet
        |> required "datesWithSessions" (Json.Decode.list dateWithSessionsDecoder)
        |> required "tracks" (Json.Decode.list trackDecoder)
        |> required "columns" (Json.Decode.list columnDecoder)
        |> required "submissions" (Json.Decode.list submissionDecoder)


apiUpdatePostDecoder : Json.Decode.Decoder ApiUpdatePost
apiUpdatePostDecoder =
    decode ApiUpdatePost
        |> required "datesWithSessions" (Json.Decode.list dateWithSessionsDecoder)
        |> required "tracks" (Json.Decode.list trackDecoder)
        |> required "columns" (Json.Decode.list columnDecoder)


encodeApiUpdatePost : ApiUpdatePost -> Json.Encode.Value
encodeApiUpdatePost record =
    Json.Encode.object
        [ ( "datesWithSessions", Json.Encode.list <| List.map dateWithSessionsEncoder record.datesWithSessions )
        , ( "tracks", Json.Encode.list <| List.map trackEncoder record.tracks )
        , ( "columns", Json.Encode.list <| List.map columnEncoder record.columns )
        ]


sessionEncoder : Session -> Json.Encode.Value
sessionEncoder session =
    Json.Encode.object
        [ ( "id", Json.Encode.int session.id )
        , ( "name", Json.Encode.string session.name )
        , ( "description", Json.Encode.string session.description )
        , ( "startTime", timeEncoder session.startTime )
        , ( "endTime", timeEncoder session.endTime )
        , ( "sessionColumn", sessionColumnEncoder session.sessionColumn )
        , ( "trackId", Json.Encode.int session.trackId )
        , ( "location", Json.Encode.string session.location )
        , ( "submissionIds", Json.Encode.list <| List.map Json.Encode.int session.submissionIds )
        , ( "chair", Json.Encode.string session.chair )
        ]


sessionColumnEncoder : SessionColumn -> Json.Encode.Value
sessionColumnEncoder record =
    case record of
        ColumnId int ->
            Json.Encode.int int

        AllColumns ->
            Json.Encode.string "All columns"

        NoColumns ->
            Json.Encode.null


dateWithSessionsEncoder : DateWithSessions -> Json.Encode.Value
dateWithSessionsEncoder record =
    Json.Encode.object
        [ ( "date", dateEncoder record.date )
        , ( "sessions", Json.Encode.list <| List.map sessionEncoder record.sessions )
        ]


dateWithSessionsDecoder : Json.Decode.Decoder DateWithSessions
dateWithSessionsDecoder =
    decode DateWithSessions
        |> required "date" dateDecoder
        |> required "sessions" (Json.Decode.list sessionDecoder)


sessionDecoder : Json.Decode.Decoder Session
sessionDecoder =
    decode Session
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string
        |> required "description" Json.Decode.string
        |> required "startTime" timeDecoder
        |> required "endTime" timeDecoder
        |> required "columnId" sessionColumnDecoder
        |> required "trackId" Json.Decode.int
        |> required "location" Json.Decode.string
        |> required "submissionIds" (Json.Decode.list Json.Decode.int)
        |> required "chair" Json.Decode.string


sessionColumnDecoder : Json.Decode.Decoder SessionColumn
sessionColumnDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map ColumnId Json.Decode.int
        , Json.Decode.map stringColumnDecoder Json.Decode.string
        , Json.Decode.null NoColumns
        ]


stringColumnDecoder string =
    case string of
        "All columns" ->
            AllColumns

        _ ->
            Debug.log "ahhhh what do we do!!!!!" NoColumns


trackEncoder : Track -> Json.Encode.Value
trackEncoder record =
    Json.Encode.object
        [ ( "id", Json.Encode.int record.id )
        , ( "name", Json.Encode.string record.name )
        , ( "description", Json.Encode.string record.description )
        ]


trackDecoder : Json.Decode.Decoder Track
trackDecoder =
    decode Track
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string
        |> required "description" Json.Decode.string


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


submissionDecoder : Json.Decode.Decoder Submission
submissionDecoder =
    decode Submission
        |> required "id" Json.Decode.int


getModelFromDb : String -> Cmd Msg
getModelFromDb eventId =
    let
        url =
            "/events/" ++ eventId ++ "/programme-builder-model"

        request =
            Http.get url apiUpdateGetDecoder
    in
        Http.send UpdateModel request


postModelToDb : ApiUpdatePost -> String -> Cmd Msg
postModelToDb apiUpdateModel eventId =
    let
        requestUrl =
            "/events/" ++ eventId ++ "/programme-builder-model"

        request =
            Http.post requestUrl (Http.jsonBody (encodeApiUpdatePost apiUpdateModel)) apiUpdatePostDecoder
    in
        Http.send SaveModel request
