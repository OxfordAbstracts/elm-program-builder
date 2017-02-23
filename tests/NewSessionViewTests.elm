module NewSessionViewTests exposing (..)

import Test exposing (..)
import Expect
import NewSessionView exposing (..)
import MainMessages exposing (..)
import MainModel


-- showNewTrackUi is true for initial model


model : MainModel.Model
model =
    MainModel.initialModel


all : Test
all =
    describe "newSessionView functions"
        [ test "newSessionViewWarning returns message when there is no session name" <|
            \() ->
                let
                    newModel =
                        { model | showNewTrackUi = False, showNewSessionUi = True }

                    context =
                        NewSessionContext "Edit session" EditSession model.editSession model.editSessionDate
                in
                    Expect.equal (NewSessionView.newSessionViewWarning context newModel) ("Cannot create: Session name field is empty")
        , test "newSessionViewWarning shows when end time is less than the start time" <|
            \() ->
                let
                    newModel =
                        { model
                            | showNewTrackUi = False
                            , showNewSessionUi = True
                            , newSession = MainModel.blankSession -1
                            , editSession =
                                { id = 1
                                , name = "jj"
                                , description = "jj"
                                , startTime = { hour = 12, minute = 0 }
                                , endTime = { hour = 9, minute = 0 }
                                , sessionColumn = MainModel.ColumnId 1
                                , chair = "test chair"
                                , location = "test location"
                                , trackId = 1
                                , submissionIds = []
                                }
                            , editSessionDate = { year = 2017, month = 1, day = 1 }
                        }

                    context =
                        NewSessionContext "Edit session" EditSession newModel.editSession newModel.editSessionDate
                in
                    Expect.equal (NewSessionView.newSessionViewWarning context newModel) "Cannot create: Session end time must be greater than start time"
        , test "newSessionViewWarning shows message when another session overlaps with when editting a session" <|
            \() ->
                let
                    newModel =
                        { model
                            | showNewTrackUi = False
                            , showNewSessionUi = True
                            , newSession =
                                { id = 1
                                , name = "jj"
                                , description = "jj"
                                , startTime = { hour = 12, minute = 0 }
                                , endTime = { hour = 14, minute = 0 }
                                , sessionColumn = MainModel.ColumnId 1
                                , chair = "test chair"
                                , location = "test location"
                                , trackId = 1
                                , submissionIds = []
                                }
                            , newSessionDate = { year = 2017, month = 1, day = 1 }
                        }

                    context =
                        NewSessionContext "Create session" CreateNewSession newModel.newSession newModel.newSessionDate
                in
                    Expect.equal (NewSessionView.newSessionViewWarning context newModel) ("Cannot create: Session times overlap another session in the same column")
        ]
