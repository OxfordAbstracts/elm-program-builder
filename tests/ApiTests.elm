module ApiTests exposing (..)

import Test exposing (..)
import Expect
import Api
import Json.Decode
import MainModel exposing (ApiUpdateGet, Session, Submission, Track, Column, DateWithoutTime)
import Fuzz exposing (int, intRange, string)


all : Test
all =
    describe "Api functions"
        [ fuzz4 string
            string
            int
            (intRange 1 12)
            "apiUpdateDecoder should decode JSON from server into an ApiUpdate"
          <|
            \sessionName sessionDescription sessionYear sessionMonth ->
                let
                    apiJson =
                        createApiJson sessionName sessionDescription sessionYear sessionMonth

                    decodedApiJson =
                        case Json.Decode.decodeString Api.apiUpdateGetDecoder apiJson of
                            Err str ->
                                Debug.crash str

                            Ok decodedUpdate ->
                                decodedUpdate

                    apiUpdate =
                        createApiUpdate sessionName sessionDescription sessionYear sessionMonth
                in
                    Expect.equal (apiUpdate) (decodedApiJson)
        ]


createApiJson : String -> String -> Int -> Int -> String
createApiJson sessionName sessionDescription sessionYear sessionMonth =
    """{
    "datesWithSessions": [
        {"date": {"year":2017, "month": 1, "day": 1},
        "sessions": [
          {
            "id": 1,
            "name": """ ++ (toString sessionName) ++ """,
            "description": """ ++ (toString sessionDescription) ++ """,
            "startTime": {
              "hour": 9,
              "minute": 0
            },
            "endTime": {
              "hour": 9,
              "minute": 30
            },
            "sessionColumn": 1,
            "trackId": 1,
            "location": "This is the location",
            "submissionIds": [],
            "chair": "This is the chair"
          }
        ]
      }
  ],
  "tracks": [
    {
      "id": 1,
      "name": "track 1",
      "description": "track 1 description"
    }
  ],
  "columns": [
    {
      "id": 1,
      "name": "column 1"
    }
  ],
"submissions": [
  {"id": 1}
]
}"""


createApiUpdate : String -> String -> Int -> Int -> ApiUpdateGet
createApiUpdate sessionName sessionDescription sessionYear sessionMonth =
    ApiUpdateGet
        [ { date = { year = 2017, month = 1, day = 1 }
          , sessions = [ createSession sessionName sessionDescription sessionYear sessionMonth ]
          }
        ]
        [ Track 1 "track 1" "track 1 description" ]
        [ Column 1 "column 1" ]
        [ Submission 1 ]


createSession : String -> String -> Int -> Int -> Session
createSession sessionName sessionDescription sessionYear sessionMonth =
    Session
        1
        sessionName
        sessionDescription
        { hour = 9
        , minute = 0
        }
        { hour = 9
        , minute = 30
        }
        (MainModel.ColumnId 1)
        1
        "This is the location"
        []
        "This is the chair"
