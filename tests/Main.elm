port module Main exposing (..)

import DateUtilsTests
import GetWarningTests
import MainUpdateTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test exposing (..)


allTests : Test
allTests =
    describe "all tests"
        [ DateUtilsTests.all
        , MainUpdateTests.all
        , GetWarningTests.all
        ]


main : TestProgram
main =
    run emit allTests


port emit : ( String, Value ) -> Cmd msg
