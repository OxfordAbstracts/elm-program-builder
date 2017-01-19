port module Main exposing (..)

import DateUtilsTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)

-- this files lists all our tests

main : TestProgram
main =
    run emit DateUtilsTests.all


port emit : ( String, Value ) -> Cmd msg
