module MainUpdateTests exposing (..)

import Test exposing (..)
import Expect
import DummyTypes exposing (..)
import MainUpdate


all : Test
all =
    describe "Main update tests"
        [ test "updateNewColumn updates the model with the new column name" <|
            \() ->
                Expect.equal (MainUpdate.updateNewColumn dummyModel (\ns -> { ns | name = "new column" })).newColumn.name "new column"
        , test "updateNewSession updates the model with the new session name" <|
            \() ->
                Expect.equal (MainUpdate.updateNewSession dummyModel (\ns -> { ns | name = "new session" })).newSession.name "new session"
        , test "updateNewTrack updates the model with the new track name" <|
            \() ->
                Expect.equal (MainUpdate.updateNewTrack dummyModel (\ns -> { ns | name = "new track" })).newTrack.name "new track"
        , test "updateNewSessionStartTime updates the model with the new session start time" <|
            \() ->
                let
                    actualTimeRecord =
                        (MainUpdate.updateNewSessionStartTime
                            dummyModel
                            (\st -> { st | hour = 4, minute = 59 })
                        ).newSession.startTime
                in
                    Expect.equal actualTimeRecord { hour = 4, minute = 59 }
        , test "updateNewSessionEndTime updates the model with the new session end time" <|
            \() ->
                let
                    actualTimeRecord =
                        (MainUpdate.updateNewSessionEndTime
                            dummyModel
                            (\st -> { st | hour = 19, minute = 0 })
                        ).newSession.endTime
                in
                    Expect.equal actualTimeRecord { hour = 19, minute = 0 }
        , test "toInt converts a stringified integer to an integer" <|
            \() ->
                Expect.equal (MainUpdate.toInt dummyModel "5") 5
        , test "updateModel updates model with the tracks in apiUpdate" <|
            \() ->
                Expect.equal (updatedModel.tracks) dummyTracks
        , test "updateModel updates model with the columns in apiUpdate" <|
            \() ->
                Expect.equal (updatedModel.columns)
                    dummyColumn
        , test "updateModel updates model with the dates in apiUpdate" <|
            \() ->
                Expect.equal (updatedModel.dates) dummyDates
        , test "updateModel updates model with the sessions in apiUpdate" <|
            \() ->
                Expect.equal (updatedModel.sessions) dummySessions
        ]
