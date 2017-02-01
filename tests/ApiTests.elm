module ApiTests exposing (..)

import Test exposing (..)
import Expect
import DummyTypes exposing (..)
import Api
import Json.Decode
import Json.Encode


dummyApiUpdateJson =
    """{
  "sessions": [
    {
      "id": 1,
      "name": "Conceptualising diabetes self-management as an occupation",
      "description": "This a description of the inital session",
      "date": {
        "year": 2017,
        "month": 1,
        "day": 1
      },
      "startTime": {
        "hour": 9,
        "minute": 0
      },
      "endTime": {
        "hour": 9,
        "minute": 1
      },
      "columnId": 1,
      "trackId": 1,
      "location": "The aquariam",
      "submissionIds": [],
      "chair": "Chairman Dave"
    },
    {
      "id": 2,
      "name": "Computers n stuff sesh 2",
      "description": "This a description of the second inital session",
      "date": {
        "year": 2017,
        "month": 1,
        "day": 1
      },
      "startTime": {
        "hour": 10,
        "minute": 30
      },
      "endTime": {
        "hour": 11,
        "minute": 0
      },
      "columnId": 1,
      "trackId": 1,
      "location": "The observatory",
      "submissionIds": [],
      "chair": "Chairwoman Sue"
    }
  ],
  "tracks": [
    {
      "id": 1,
      "name": "Test track"
    }
  ],
  "columns": [
    {
      "id": 1,
      "name": "Test column"
    }
  ],
  "dates": [
    {
      "year": 2017,
      "month": 1,
      "day": 1
    }
  ]
}"""


decodedApiUpdate =
    Json.Decode.decodeString Api.apiUpdateDecoder dummyApiUpdateJson
        |> Result.withDefault dummyApiUpdate


all : Test
all =
    describe "Api functions"
        [ test "apiUpdateDecoder decodes apiUpdate from a JSON to elm format for sessions" <|
            \() ->
                Expect.equal (decodedApiUpdate.sessions) (dummySessions)
        , test "apiUpdateDecoder decodes apiUpdate from a JSON to elm format for tracks" <|
            \() ->
                Expect.equal (decodedApiUpdate.tracks) (dummyTracks)
        , test "apiUpdateDecoder decodes apiUpdate from a JSON to elm format for columns" <|
            \() ->
                Expect.equal (decodedApiUpdate.columns) (dummyColumn)
        , test "apiUpdateDecoder decodes apiUpdate from a JSON to elm format for dates" <|
            \() ->
                Expect.equal (decodedApiUpdate.dates) (dummyDates)
        , test "encodeApiUpdate encodes apiUpdate from elm format to a JSON" <|
            \() ->
                Expect.equal (Json.Encode.encode 2 (Api.encodeApiUpdate dummyApiUpdate)) (dummyApiUpdateJson)
        ]
