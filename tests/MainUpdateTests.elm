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


all : Test
all =
    describe "Main update tests"
        [ test "updateNewColumn updates the model" <|
            \() ->
                Expect.equal (MainUpdate.updateNewColumn dummyModel (\ns -> { ns | name = "new column" })).newColumn.name "new column"
        , test "updateNewSession updates the model" <|
            \() ->
                Expect.equal (MainUpdate.updateNewSession dummyModel (\ns -> { ns | name = "new session" })).newSession.name "new session"
        , test "updateNewTrack updates the model" <|
            \() ->
                Expect.equal (MainUpdate.updateNewTrack dummyModel (\ns -> { ns | name = "new track" })).newTrack.name "new track"
        , test "updateNewSessionStartTime updates the model" <|
            \() ->
                let
                    actualTimeRecord =
                        (MainUpdate.updateNewSessionStartTime
                            dummyModel
                            (\st -> { st | hour = 4, minute = 59 })
                        ).newSession.startTime
                in
                    Expect.equal actualTimeRecord { hour = 4, minute = 59 }
        , test "updateNewSessionEndTime updates the model" <|
            \() ->
                let
                    actualTimeRecord =
                        (MainUpdate.updateNewSessionEndTime
                            dummyModel
                            (\st -> { st | hour = 19, minute = 0 })
                        ).newSession.endTime
                in
                    Expect.equal actualTimeRecord { hour = 19, minute = 0 }
        , test "toInt updates the model" <|
            \() ->
                Expect.equal (MainUpdate.toInt dummyModel "5") 5
        ]
