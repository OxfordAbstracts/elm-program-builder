module Api exposing (..)

import MainModel exposing (..)
import MainMessages exposing (..)
import Json.Decode exposing (nullable)
import Json.Decode.Pipeline exposing (required, optional, decode)
import Http
import Json.Encode as Encode


apiUpdateGetDecoder : Json.Decode.Decoder ApiUpdateGet
apiUpdateGetDecoder =
    decode ApiUpdateGet
        |> required "datesWithSessions" (Json.Decode.list dateWithSessionsDecoder)
        |> required "tracks" (Json.Decode.list trackDecoder)
        |> required "columns" (Json.Decode.list columnDecoder)
        |> required "submissions" (Json.Decode.list submissionDecoder)
        |> required "published" (Json.Decode.bool)


apiUpdatePostDecoder : Json.Decode.Decoder ApiUpdatePost
apiUpdatePostDecoder =
    decode ApiUpdatePost
        |> required "datesWithSessions" (Json.Decode.list dateWithSessionsDecoder)
        |> required "tracks" (Json.Decode.list trackDecoder)
        |> required "columns" (Json.Decode.list columnDecoder)
        |> required "published" (Json.Decode.bool)


encodeApiUpdatePost : ApiUpdatePost -> Encode.Value
encodeApiUpdatePost record =
    Encode.object
        [ ( "datesWithSessions", Encode.list <| List.map dateWithSessionsEncoder record.datesWithSessions )
        , ( "tracks", Encode.list <| List.map trackEncoder record.tracks )
        , ( "columns", Encode.list <| List.map columnEncoder record.columns )
        , ( "published", Encode.bool record.published )
        ]


sessionEncoder : Session -> Encode.Value
sessionEncoder session =
    Encode.object
        [ ( "id", Encode.int session.id )
        , ( "name", Encode.string session.name )
        , ( "description", Encode.string session.description )
        , ( "startTime", timeEncoder session.startTime )
        , ( "endTime", timeEncoder session.endTime )
        , ( "sessionColumn", sessionColumnEncoder session.sessionColumn )
        , ( "trackId", sessionTrackIdEncoder session.trackId )
        , ( "location", Encode.string session.location )
        , ( "submissions", Encode.list <| List.map sessionSubmissionEncoder session.submissions )
        , ( "chair", Encode.string session.chair )
        ]


sessionSubmissionEncoder : SessionSubmission -> Encode.Value
sessionSubmissionEncoder submission =
    Encode.object
        [ ( "id", Encode.int submission.id )
        , ( "startTime", Maybe.withDefault Encode.null (Maybe.map timeEncoder submission.startTime) )
        , ( "endTime", Maybe.withDefault Encode.null (Maybe.map timeEncoder submission.endTime) )
        ]


sessionTrackIdEncoder : Maybe TrackId -> Encode.Value
sessionTrackIdEncoder record =
    case record of
        Just int ->
            Encode.int int

        Nothing ->
            Encode.null


sessionColumnEncoder : SessionColumn -> Encode.Value
sessionColumnEncoder record =
    case record of
        ColumnId int ->
            Encode.int int

        AllColumns ->
            Encode.string "All columns"

        NoColumns ->
            Encode.null


dateWithSessionsEncoder : DateWithSessions -> Encode.Value
dateWithSessionsEncoder record =
    Encode.object
        [ ( "date", dateEncoder record.date )
        , ( "sessions", Encode.list <| List.map sessionEncoder record.sessions )
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
        |> required "sessionColumn" sessionColumnDecoder
        |> required "trackId" sessionTrackIdDecoder
        |> required "location" Json.Decode.string
        |> required "submissions" (Json.Decode.list sessionSubmissionDecoder)
        |> required "chair" Json.Decode.string


sessionSubmissionDecoder : Json.Decode.Decoder SessionSubmission
sessionSubmissionDecoder =
    decode SessionSubmission
        |> required "id" Json.Decode.int
        |> optional "startTime" (nullable timeDecoder) Nothing
        |> optional "endTime" (nullable timeDecoder) Nothing


sessionTrackIdDecoder : Json.Decode.Decoder (Maybe TrackId)
sessionTrackIdDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map Just Json.Decode.int
        , Json.Decode.null Nothing
        ]


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
            NoColumns


trackEncoder : Track -> Encode.Value
trackEncoder record =
    Encode.object
        [ ( "id", Encode.int record.id )
        , ( "name", Encode.string record.name )
        , ( "description", Encode.string record.description )
        ]


trackDecoder : Json.Decode.Decoder Track
trackDecoder =
    decode Track
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string
        |> required "description" Json.Decode.string


columnEncoder : Column -> Encode.Value
columnEncoder record =
    Encode.object
        [ ( "id", Encode.int record.id )
        , ( "name", Encode.string record.name )
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


dateEncoder : DateWithoutTime -> Encode.Value
dateEncoder record =
    Encode.object
        [ ( "year", Encode.int record.year )
        , ( "month", Encode.int record.month )
        , ( "day", Encode.int record.day )
        ]


timeDecoder : Json.Decode.Decoder TimeOfDay
timeDecoder =
    decode TimeOfDay
        |> required "hour" Json.Decode.int
        |> required "minute" Json.Decode.int


timeEncoder : TimeOfDay -> Encode.Value
timeEncoder record =
    Encode.object
        [ ( "hour", Encode.int record.hour )
        , ( "minute", Encode.int record.minute )
        ]


submissionDecoder : Json.Decode.Decoder Submission
submissionDecoder =
    decode Submission
        |> required "id" Json.Decode.int



-- |> optional "startTime" (nullable timeDecoder) Nothing
-- |> optional "endTime" (nullable timeDecoder) Nothing


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
