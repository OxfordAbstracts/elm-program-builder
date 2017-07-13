module MainUpdateTests exposing (..)

import Test exposing (..)
import Expect
import DummyTypes exposing (..)
import MainUpdate
import MainMessages
import Tuple
import MainModel exposing (Submission)


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
                        , startTime = { hour = 9, minute = 0 }
                        , endTime = { hour = 12, minute = 0 }
                        , sessionColumn = MainModel.ColumnId 1
                        , chair = "test chair"
                        , location = "test location"
                        , trackId = (Just 1)
                        , submissions = [ { id = 1, startTime = Nothing, endTime = Nothing } ]
                        }

                    modelWithEditingId =
                        { dummyModelWithSessions
                            | idOfSessionBeingEdited = Just 1
                            , editSession = editSession
                            , submissions = [ Submission 1, Submission 2, Submission 3, Submission 4 ]
                            , submissionIdsInputs =
                                [ { submissionIds = "1"
                                  , startTime = Nothing
                                  , endTime = Nothing
                                  , id = 1
                                  }
                                  -- numbers with whitespace should be parsed
                                , { submissionIds = " 4, 5"
                                  , startTime = Just { hour = 11, minute = 30 }
                                  , endTime = Just { hour = 12, minute = 0 }
                                  , id = 2
                                  }
                                ]
                        }

                    modelAfterEdit =
                        modelWithEditingId
                            |> MainUpdate.update MainMessages.EditSession
                            |> Tuple.first

                    expectedDatesWithSessions =
                        [ { date =
                                { year = 2017
                                , month = 1
                                , day = 1
                                }
                          , sessions =
                                [ { id = 1
                                  , name = "new test name"
                                  , description = "new test description"
                                  , startTime =
                                        { hour = 9
                                        , minute = 0
                                        }
                                  , endTime =
                                        { hour = 12
                                        , minute = 0
                                        }
                                  , sessionColumn = MainModel.ColumnId 1
                                  , trackId = Just 1
                                  , location = "test location"
                                  , submissions =
                                        [ { id = 1
                                          , startTime = Nothing
                                          , endTime = Nothing
                                          }
                                        , { id = 4
                                          , startTime =
                                                Just
                                                    { hour = 11
                                                    , minute = 30
                                                    }
                                          , endTime =
                                                Just
                                                    { hour = 12
                                                    , minute = 0
                                                    }
                                          }
                                          -- no submission 5 as not in model.submissions
                                        ]
                                  , chair = "test chair"
                                  }
                                , { id = 2
                                  , name = "Computers n stuff sesh 2"
                                  , description = "This a description of the second inital session"
                                  , startTime =
                                        { hour = 10
                                        , minute = 30
                                        }
                                  , endTime = { hour = 11, minute = 0 }
                                  , sessionColumn = MainModel.ColumnId 1
                                  , trackId = Just 1
                                  , location = "The observatory"
                                  , submissions = [ { id = 5, startTime = Nothing, endTime = Nothing }, { id = 2, startTime = Nothing, endTime = Nothing } ]
                                  , chair = "Chairwoman Sue"
                                  }
                                ]
                          }
                        ]
                in
                    Expect.equal modelAfterEdit.datesWithSessions expectedDatesWithSessions
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
                            (MainMessages.SetSessionSubmissionStartTimes 1 "18:15")
                            dummyModel

                    expectedModel =
                        { dummyModel
                            | submissionIdsInputs =
                                [ { submissionIds = ""
                                  , startTime = Just { hour = 18, minute = 15 }
                                  , endTime = Nothing
                                  , id = 1
                                  }
                                ]
                        }
                in
                    Expect.equal result ( expectedModel, Cmd.none )
        , test """SetSessionSubmissionEndTimes should set the endTime
              of the submissions with the specified ids in the session with the specified id""" <|
            \() ->
                let
                    result =
                        MainUpdate.update
                            (MainMessages.SetSessionSubmissionEndTimes 2 "18:00")
                            { dummyModel
                                | submissionIdsInputs =
                                    [ { submissionIds = "4"
                                      , startTime = Nothing
                                      , endTime = Just { hour = 12, minute = 0 }
                                      , id = 1
                                      }
                                    , { submissionIds = "1,2 ,3"
                                      , startTime = Just { hour = 11, minute = 5 }
                                      , endTime = Just { hour = 12, minute = 0 }
                                      , id = 2
                                      }
                                    ]
                            }

                    expectedModel =
                        { dummyModel
                            | submissionIdsInputs =
                                [ { submissionIds = "4"
                                  , startTime = Nothing
                                  , endTime = Just { hour = 12, minute = 0 }
                                  , id = 1
                                  }
                                , { submissionIds = "1,2 ,3"
                                  , startTime = Just { hour = 11, minute = 5 }
                                  , endTime = Just { hour = 18, minute = 0 }
                                  , id = 2
                                  }
                                ]
                        }
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
        , test """DeleteSubmissionInput should delete the submissionIdsInput
                    with the given id""" <|
            \() ->
                let
                    result =
                        MainUpdate.update
                            (MainMessages.DeleteSubmissionInput 1)
                            model

                    model =
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

                    expectedModel =
                        { dummyModel
                            | submissionIdsInputs =
                                [ { submissionIds = ""
                                  , startTime = Nothing
                                  , endTime = Nothing
                                  , id = 2
                                  }
                                ]
                        }
                in
                    Expect.equal result ( expectedModel, Cmd.none )
        , test """submissionsToInputText should convert a list of session submissions to
    a list of submissionIdInputs""" <|
            \() ->
                let
                    sessionSubmissions =
                        [ { id = 1
                          , startTime = Nothing
                          , endTime = Nothing
                          }
                        , { id = 2
                          , startTime = Just { hour = 11, minute = 0 }
                          , endTime = Just { hour = 11, minute = 30 }
                          }
                        , { id = 3
                          , startTime = Just { hour = 11, minute = 0 }
                          , endTime = Just { hour = 11, minute = 30 }
                          }
                        , { id = 6
                          , startTime = Just { hour = 11, minute = 0 }
                          , endTime = Just { hour = 12, minute = 0 }
                          }
                        ]

                    result =
                        MainUpdate.submissionsToInputText sessionSubmissions

                    expectedResult =
                        [ { submissionIds = "1"
                          , startTime = Nothing
                          , endTime = Nothing
                          , id = 1
                          }
                        , { submissionIds = "2, 3"
                          , startTime = Just { hour = 11, minute = 0 }
                          , endTime = Just { hour = 11, minute = 30 }
                          , id = 2
                          }
                        , { submissionIds = "6"
                          , startTime = Just { hour = 11, minute = 0 }
                          , endTime = Just { hour = 12, minute = 0 }
                          , id = 3
                          }
                        ]
                in
                    Expect.equal result expectedResult
        , test """ToogleScheduleSubmissionsIndividually should toggle the scheduleSubmissionsIndividually property
        and condense all submissionIdsInputs to a single submissionIdsInput and with no start or end time""" <|
            \() ->
                let
                    model =
                        { dummyModel
                            | scheduleSubmissionsIndividually = True
                            , submissionIdsInputs =
                                [ { submissionIds = "1"
                                  , startTime = Nothing
                                  , endTime = Nothing
                                  , id = 1
                                  }
                                , { submissionIds = "2,3, 6, 7"
                                  , startTime = Just { hour = 11, minute = 0 }
                                  , endTime = Just { hour = 11, minute = 30 }
                                  , id = 2
                                  }
                                , { submissionIds = "6"
                                  , startTime = Just { hour = 11, minute = 0 }
                                  , endTime = Just { hour = 12, minute = 0 }
                                  , id = 3
                                  }
                                ]
                        }

                    result =
                        MainUpdate.update
                            MainMessages.ToogleScheduleSubmissionsIndividually
                            model

                    expectedModel =
                        { dummyModel
                            | scheduleSubmissionsIndividually = False
                            , submissionIdsInputs =
                                [ { submissionIds = "1, 2, 3, 6, 7"
                                  , startTime = Nothing
                                  , endTime = Nothing
                                  , id = 1
                                  }
                                ]
                        }
                in
                    Expect.equal result ( expectedModel, Cmd.none )
        ]
