port module Main exposing (..)

import ApiTests
import DateUtilsTests
import GetWarningTests
import MainUpdateTests
import UtilsTests
import NewColumnViewTests
import NewSessionViewTests
import NewTrackViewTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test exposing (..)


allTests : Test
allTests =
    describe "all tests"
        [ ApiTests.all
        , DateUtilsTests.all
        , GetWarningTests.all
        , MainUpdateTests.all
        , NewColumnViewTests.all
        , NewSessionViewTests.all
        , NewTrackViewTests.all
        , UtilsTests.all
        ]


main : TestProgram
main =
    run emit allTests


port emit : ( String, Value ) -> Cmd msg
