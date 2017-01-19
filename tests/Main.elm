port module Main exposing (..)

import DateUtilsTests
import MainUpdateTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)

-- this files lists all our tests

main : TestProgram
main =
  -- UNSURE HOW TO GET BOTH OF THESE TO RUN ONE AFTER THE OTHER :(
    -- run emit DateUtilsTests.all
    run emit MainUpdateTests.all


port emit : ( String, Value ) -> Cmd msg
