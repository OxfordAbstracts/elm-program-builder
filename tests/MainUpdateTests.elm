module MainUpdateTests exposing (..)

import Test exposing (..)
import Expect
import DummyTypes exposing (..)
import MainUpdate
import MainMessages
import Tuple
import MainModel


all : Test
all =
    describe "Main update tests"
        [ test "updateNewColumn updates the model with the new column name" <|
            \() ->
                let
                    updatedModel =
                        MainUpdate.updateNewColumn dummyModel
                            (\ns -> { ns | name = "new column" })
                in
                    Expect.equal updatedModel.newColumn.name "new column"
        , test "updateNewSession updates the model with the new session name" <|
            \() ->
                let
                    updatedModel =
                        MainUpdate.updateNewSession dummyModel
                            (\ns -> { ns | name = "new session" })
                in
                    Expect.equal updatedModel.newSession.name "new session"
        , test "updateNewTrack updates the model with the new track name" <|
            \() ->
                let
                    updatedModel =
                        MainUpdate.updateNewTrack dummyModel
                            (\ns -> { ns | name = "new track", description = "new track description" })
                in
                    updatedModel.newTrack
                        |> Expect.all
                            [ .name >> Expect.equal "new track"
                            , .description >> Expect.equal "new track description"
                            ]
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
        , test "updateModel updates model with the sessions in apiUpdate" <|
            \() ->
                Expect.equal (updatedModel.datesWithSessions) dummyDatesWithSessions
        , test """EditSession should remove the edited session and adds a new session with the new data
                and the invalid submission ids are not added to submissionIds from the submissionIdInput""" <|
            \() ->
                let
                    editSession =
                        { id = 1
                        , name = "new test name"
                        , description = "new test description"
                        , startTime = { hour = 12, minute = 0 }
                        , endTime = { hour = 9, minute = 0 }
                        , sessionColumn = MainModel.ColumnId 1
                        , chair = "test chair"
                        , location = "test location"
                        , trackId = (Just 1)
                        , submissionIds = [ 1 ]
                        }

                    modelWithEditingId =
                        { dummyModel
                            | idOfSessionBeingEdited = Just 1
                            , editSession = editSession
                            , submissionIdsInput =
                                "1 , 4,5"
                                -- numbers with whitespace should be parsed
                        }

                    modelAfterEdit =
                        modelWithEditingId
                            |> MainUpdate.update MainMessages.EditSession
                            |> Tuple.first

                    editedSession modelAfterEdit =
                        modelAfterEdit
                            |> .datesWithSessions
                            |> List.concatMap .sessions
                            |> List.filter (\s -> s.id == 1)
                            |> List.head
                            |> Maybe.withDefault (MainModel.blankSession -1)
                in
                    modelAfterEdit
                        |> Expect.all
                            [ .idOfSessionBeingEdited >> Expect.equal (Nothing)
                            , editedSession >> Expect.equal (editSession)
                            ]
        , test """CreateNewSession should add the new session to sessions and the invalid submission
            ids should not be added to submissionIds from the submissionIdInput""" <|
            \() ->
                let
                    newSession =
                        { id = 6
                        , name = "new test name"
                        , description = "new test description"
                        , startTime = { hour = 12, minute = 0 }
                        , endTime = { hour = 9, minute = 0 }
                        , sessionColumn = MainModel.ColumnId 1
                        , chair = "test chair"
                        , location = "test location"
                        , trackId = (Just 1)
                        , submissionIds = [ 1 ]
                        }

                    modelWithNewSession =
                        { dummyModel
                            | newSession = newSession
                            , submissionIdsInput = "1,4,5"
                        }

                    modelAfterUpdate =
                        modelWithNewSession
                            |> MainUpdate.update MainMessages.CreateNewSession
                            |> Tuple.first

                    addedSession =
                        modelAfterUpdate
                            |> .datesWithSessions
                            |> List.concatMap .sessions
                            |> List.filter (\s -> s.id == 6)
                            |> List.head
                            |> Maybe.withDefault (MainModel.blankSession -1)
                in
                    Expect.equal (Just addedSession) (Just newSession)
        ]
