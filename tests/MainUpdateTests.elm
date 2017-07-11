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
                        , submissions = [ { id = 1, startTime = Nothing, endTime = Nothing } ]
                        }

                    modelWithEditingId =
                        { dummyModel
                            | idOfSessionBeingEdited = Just 1
                            , editSession = editSession
                            , submissionIdsInputs =
                                [ { submissionIds = "1 , 4,5"
                                  , startTime = Nothing
                                  , endTime = Nothing
                                  , id = 1
                                  }
                                ]
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
                        { id = 1
                        , name = "new test name"
                        , description = "new test description"
                        , startTime = { hour = 12, minute = 0 }
                        , endTime = { hour = 9, minute = 0 }
                        , sessionColumn = MainModel.ColumnId 1
                        , chair = "test chair"
                        , location = "test location"
                        , trackId = (Just 1)
                        , submissions = [ { id = 1, startTime = Nothing, endTime = Nothing } ]
                        }

                    modelWithNewSession =
                        { dummyModel
                            | newSession = newSession
                            , submissionIdsInputs =
                                [ { submissionIds = "1,4,5"
                                  , startTime = Nothing
                                  , endTime = Nothing
                                  , id = 1
                                  }
                                ]
                        }

                    modelAfterUpdate =
                        modelWithNewSession
                            |> MainUpdate.update MainMessages.CreateNewSession
                            |> Tuple.first

                    addedSession =
                        modelAfterUpdate
                            |> .datesWithSessions
                            |> List.concatMap .sessions
                            |> List.filter (\s -> s.id == 1)
                            |> List.head
                            |> Maybe.withDefault (MainModel.blankSession -1)
                in
                    Expect.equal (Just addedSession) (Just newSession)
        , test """SetSessionSubmissionStartTimes should set the startTime
            of the submissions with the specified ids in the session with the specified id""" <|
            \() ->
                let
                    result =
                        MainUpdate.update
                            (MainMessages.SetSessionSubmissionStartTimes 1 [ 3, 4 ] "18:15")
                            dummyModelWithSessions

                    expectedModel =
                        makeDummyModel
                            [ MainModel.Session
                                1
                                "Conceptualising diabetes self-management as an occupation"
                                "This a description of the inital session"
                                (MainModel.TimeOfDay 9 0)
                                (MainModel.TimeOfDay 9 1)
                                (MainModel.ColumnId 1)
                                (Just 1)
                                "The aquariam"
                                [ { id = 1, startTime = Just { hour = 11, minute = 0 }, endTime = Just { hour = 11, minute = 45 } }
                                , { id = 2, startTime = Just { hour = 12, minute = 0 }, endTime = Just { hour = 12, minute = 30 } }
                                , { id = 3, startTime = Just { hour = 18, minute = 15 }, endTime = Just { hour = 12, minute = 30 } }
                                , { id = 4, startTime = Just { hour = 18, minute = 15 }, endTime = Just { hour = 12, minute = 30 } }
                                ]
                                "Chairman Dave"
                            , MainModel.Session
                                2
                                "Computers n stuff sesh 2"
                                "This a description of the second inital session"
                                (MainModel.TimeOfDay 10 30)
                                (MainModel.TimeOfDay 11 0)
                                (MainModel.ColumnId 1)
                                (Just 1)
                                "The observatory"
                                [ { id = 5, startTime = Nothing, endTime = Nothing }
                                , { id = 2, startTime = Nothing, endTime = Nothing }
                                ]
                                "Chairwoman Sue"
                            ]
                in
                    Expect.equal result ( expectedModel, Cmd.none )
        , test """SetSessionSubmissionEndTimes should set the endTime
              of the submissions with the specified ids in the session with the specified id""" <|
            \() ->
                let
                    result =
                        MainUpdate.update
                            (MainMessages.SetSessionSubmissionEndTimes 1 [ 3, 4 ] "18:00")
                            dummyModelWithSessions

                    expectedModel =
                        makeDummyModel
                            [ MainModel.Session
                                1
                                "Conceptualising diabetes self-management as an occupation"
                                "This a description of the inital session"
                                (MainModel.TimeOfDay 9 0)
                                (MainModel.TimeOfDay 9 1)
                                (MainModel.ColumnId 1)
                                (Just 1)
                                "The aquariam"
                                [ { id = 1, startTime = Just { hour = 11, minute = 0 }, endTime = Just { hour = 11, minute = 45 } }
                                , { id = 2, startTime = Just { hour = 12, minute = 0 }, endTime = Just { hour = 12, minute = 30 } }
                                , { id = 3, startTime = Just { hour = 12, minute = 0 }, endTime = Just { hour = 18, minute = 0 } }
                                , { id = 4, startTime = Just { hour = 12, minute = 0 }, endTime = Just { hour = 18, minute = 0 } }
                                ]
                                "Chairman Dave"
                            , MainModel.Session
                                2
                                "Computers n stuff sesh 2"
                                "This a description of the second inital session"
                                (MainModel.TimeOfDay 10 30)
                                (MainModel.TimeOfDay 11 0)
                                (MainModel.ColumnId 1)
                                (Just 1)
                                "The observatory"
                                [ { id = 5, startTime = Nothing, endTime = Nothing }
                                , { id = 2, startTime = Nothing, endTime = Nothing }
                                ]
                                "Chairwoman Sue"
                            ]
                in
                    Expect.equal result ( expectedModel, Cmd.none )
        , test """CreateSubmissionInput should add a new submissionIdsInput
                  with a unique id""" <|
            \() ->
                let
                    result =
                        MainUpdate.update
                            MainMessages.CreateSubmissionInput
                            dummyModel

                    expectedModel =
                        { dummyModel
                            | submissionIdsInputs =
                                [ { submissionIds = ""
                                  , startTime = Nothing
                                  , endTime = Nothing
                                  , id = 1
                                  }
                                , { submissionIds = ""
                                  , startTime = Nothing
                                  , endTime = Nothing
                                  , id = 2
                                  }
                                ]
                        }
                in
                    Expect.equal result ( expectedModel, Cmd.none )
        ]
