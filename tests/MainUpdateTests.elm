module MainUpdateTests exposing (..)

import Test exposing (..)
import Expect
import MainModel
import MainUpdate


dummyModel : MainModel.Model
dummyModel =
  MainModel.initialModel

dummySessions : List MainModel.Session
dummySessions =
    [ MainModel.Session
        1
        "Conceptualising diabetes self-management as an occupation"
        "This a description of the inital session"
        (MainModel.DateWithoutTime 2017 1 1)
        (MainModel.TimeOfDay 9 0)
        (MainModel.TimeOfDay 9 1)
        1
        1
        "The aquariam"
        []
        "Chairman Dave"
    , MainModel.Session
        2
        "Computers n stuff sesh 2"
        "This a description of the second inital session"
        (MainModel.DateWithoutTime 2017 1 1)
        (MainModel.TimeOfDay 10 30)
        (MainModel.TimeOfDay 11 0)
        1
        1
        "The observatory"
        []
        "Chairwoman Sue"
    ]

all: Test
all =
    describe "Main update tests"
      [
        test "update new column" <|
                \() ->
                    Expect.equal (MainUpdate.updateNewColumn dummyModel (\ns -> { ns | name = "new column" })).newColumn.name "new column"
      , test "update new session" <|
              \() ->
                  Expect.equal (MainUpdate.updateNewSession dummyModel (\ns -> { ns | name = "new session" })).newSession.name "new session"
      , test "update new track" <|
              \() ->
                  Expect.equal (MainUpdate.updateNewTrack dummyModel (\ns -> { ns | name = "new track" })).newTrack.name "new track"
    , test "update new session start time" <|
            \() ->
                Expect.equal (MainUpdate.updateNewSessionStartTime dummyModel (\st -> { st | hour = 4, minute = 59 })).newSession.startTime { hour = 4, minute = 59 }
    , test "update new session end time" <|
            \() ->
                Expect.equal (MainUpdate.updateNewSessionEndTime dummyModel (\st -> { st | hour = 19, minute = 00 })).newSession.endTime { hour = 19, minute = 00 }
    , test "toInt" <|
            \() ->
                Expect.equal (MainUpdate.toInt dummyModel "5") 5
      ]
