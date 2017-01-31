module NewSessionViewTests exposing (..)

import Test exposing (..)
import Expect
import NewSessionView
import MainModel


-- showNewTrackUi is true for initial model


dummyModel : MainModel.Model
dummyModel =
    MainModel.initialModel


all : Test
all =
    describe "newSessionView functions"
        [ test "newSessionWarning returns message when there is no session name" <|
            \() ->
                let
                    newModel =
                        { dummyModel | showNewTrackUi = False, showNewSessionUi = True }
                in
                    Expect.equal (NewSessionView.newSessionWarning newModel) ("Cannot create: Session name field is empty")
        , test "newSessionWarning shows when end time is less than the start time" <|
            \() ->
                let
                    newModel =
                        { dummyModel
                            | showNewTrackUi = False
                            , showNewSessionUi = True
                            , newSession =
                                { id = 1
                                , name = "jj"
                                , description = "jj"
                                , date = { year = 2017, month = 1, day = 1 }
                                , startTime = { hour = 12, minute = 0 }
                                , endTime = { hour = 9, minute = 0 }
                                , columnId = 1
                                }
                        }
                in
                    Expect.equal (NewSessionView.newSessionWarning newModel) ("Cannot create: Session end time must be greater than start time")
        , test "newSessionWarning shows message when another session overlaps" <|
            \() ->
                let
                    newModel =
                        { dummyModel
                            | showNewTrackUi = False
                            , showNewSessionUi = True
                            , newSession =
                                { id = 1
                                , name = "jj"
                                , description = "jj"
                                , date = { year = 2017, month = 1, day = 1 }
                                , startTime = { hour = 9, minute = 0 }
                                , endTime = { hour = 12, minute = 0 }
                                , columnId = 1
                                }
                        }
                in
                    Expect.equal (NewSessionView.newSessionWarning newModel) ("Cannot create: Session times overlap another session in the same column")
        ]
