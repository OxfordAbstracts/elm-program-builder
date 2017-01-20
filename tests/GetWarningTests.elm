module GetWarningTests exposing (..)

import Test exposing (..)
import Expect
import DateUtils
import Date
import GetWarning
import MainModel
import Debug exposing (log)


-- showNewTrackUi is true for initial model


dummyModel =
    MainModel.initialModel


all : Test
all =
    describe "GetWarning functions work correctly"
        [ test "GetWarning shows correct message when there is no track name" <|
            \() ->
                Expect.equal (GetWarning.getWarning dummyModel) ("Cannot create: Track name field is empty")
        , test "GetWarning shows correct message when there is no session name" <|
            \() ->
                let
                    newModel =
                        { dummyModel | showNewTrackUi = False, showNewSessionUi = True }
                in
                    Expect.equal (GetWarning.getWarning newModel) ("Cannot create: Session name field is empty")
        , test "GetWarning shows correct message when there is no column name" <|
            \() ->
                let
                    newModel =
                        { dummyModel | showNewTrackUi = False, showNewColumnUi = True }
                in
                    Expect.equal (GetWarning.getWarning newModel) ("Cannot create: Column name field is empty")
        , test "GetWarning shows correct message when another session overlaps" <|
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
                    Expect.equal (GetWarning.getWarning newModel) ("Cannot create: Session times overlap another session in the same column")
        , test "GetWarning shows when end time is less than the start time" <|
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
                    Expect.equal (GetWarning.getWarning newModel) ("Cannot create: Session end time must be greater than start time")
        ]
