module ApiTests exposing (..)

import Test exposing (..)
import Expect
import MainModel
import DummyTypes exposing (..)
import Api
import Json.Decode


-- showNewTrackUi is true for initial model


dummyApiUpdateJson =
    """{"sessions":[{"id":1,"name":"Conceptualising diabetes self-management as an occupation",
    "description":"This a description of the inital session","date":{"year":2017,"month":1,"day":1},
    "startTime":{"hour":9,"minute":0},"endTime":{"hour":9,"minute":1},"columnId":1,"trackId":1,
    "location":"The aquariam","submissionIds":[],"chair":"Chairman Dave"},"tracks":[{"id":1, "name":"Test track"]},
    "columns":[{"id":1, "name":"Test column"]},"dates":[{"year":2017, "month":"1", "day": "1"]} """


decodedApiUpdate =
    Json.Decode.decodeString Api.apiUpdateDecoder dummyApiUpdateJson
        |> Result.withDefault dummyApiUpdate


all : Test
all =
    describe "Api functions"
        [ test "apiUpdateDecoder decodes apiUpdate from a JSON to elm format for sessions" <|
            \() ->
                Expect.equal (decodedApiUpdate.sessions) (dummySessions)
        , test "apiUpdateDecoder decodes model from a JSON to elm format for tracks" <|
            \() ->
                Expect.equal (decodedApiUpdate.tracks) (dummyTracks)
        , test "apiUpdateDecoder decodes model from a JSON to elm format for columns" <|
            \() ->
                Expect.equal (decodedApiUpdate.columns) (dummyColumn)
        , test "apiUpdateDecoder decodes model from a JSON to elm format for dates" <|
            \() ->
                Expect.equal (decodedApiUpdate.dates) (dummyDates)
        ]
