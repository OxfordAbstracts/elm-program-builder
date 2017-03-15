module NewSessionViewTests exposing (..)

import Test exposing (..)
import Expect
import NewSessionView exposing (..)
import MainMessages exposing (..)
import MainModel exposing (..)


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
                                , trackId = (Just 1)
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
                    initialSessions =
                        [ Session
                            1
                            "Conceptualising diabetes self-management as an occupation"
                            "This a description of the inital session"
                            (TimeOfDay 9 0)
                            (TimeOfDay 9 1)
                            (ColumnId 1)
                            (Just 1)
                            "The aquariam"
                            []
                            "Chairman Dave"
                        , Session
                            2
                            "Computers n stuff sesh 2"
                            "This a description of the second inital session"
                            (TimeOfDay 10 30)
                            (TimeOfDay 11 0)
                            (ColumnId 1)
                            (Just 1)
                            "The observatory"
                            []
                            "Chairwoman Sue"
                        , Session
                            3
                            "Sessioning hard 3"
                            "This a description of the third inital session"
                            (TimeOfDay 13 30)
                            (TimeOfDay 15 0)
                            (ColumnId 1)
                            (Just 1)
                            "The games room"
                            []
                            ""
                        , Session
                            4
                            "Other column sesh 4"
                            "This a description of the fourth inital session"
                            (TimeOfDay 13 0)
                            (TimeOfDay 13 30)
                            (ColumnId 1)
                            (Just 1)
                            "The mystery room"
                            []
                            ""
                        , Session
                            5
                            "first column 1 day 2 sesh 5"
                            "This a description of the fifth inital session"
                            (TimeOfDay 11 0)
                            (TimeOfDay 11 30)
                            AllColumns
                            (Just 1)
                            "The mystery room 4"
                            []
                            ""
                        ]

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
                                , trackId = (Just 1)
                                , submissionIds = []
                                }
                            , datesWithSessions = [ { date = MainModel.DateWithoutTime 2017 1 1, sessions = initialSessions } ]
                            , newSessionDate = { year = 2017, month = 1, day = 1 }
                            , tracks = [ Track 1 "track 1" "track 1 description", Track 2 "track 2" "track 2 description" ]
                            , columns = [ Column 1 "Pediatric Sessions", Column 2 "Other Sessions" ]
                        }

                    context =
                        NewSessionContext "Create session" CreateNewSession newModel.newSession newModel.newSessionDate
                in
                    Expect.equal (NewSessionView.newSessionViewWarning context newModel) ("Cannot create: Session times overlap another session in the same column")
        ]
