module DateUtilsTests exposing (..)

import Test exposing (..)
import Expect
import DateUtils
import Date


all: Test
all =
    describe "Date Utils"
        [
        test "parse default" <|
                \() ->
                    Expect.equal (DateUtils.parse "not Date") (Date.fromTime 0)
        , test "parse" <|
                \() ->
                    Expect.equal (DateUtils.parse "1970-01-01T00:00:00.100+00:00") (Date.fromTime 100)
        , test "dateWithoutTimeToDate" <|
                \() ->
                    Expect.equal (DateUtils.dateWithoutTimeToDate { year = 1970, month = 1, day = 1 }) (Date.fromTime 0)
        , test "dateToDateWithoutTime" <|
                \() ->
                    Expect.equal (DateUtils.dateToDateWithoutTime (Date.fromTime 0)) ({ year = 1970, month = 1, day = 1 })

        , test "timeOfDayToTime"  <|
              \() ->
                    Expect.equal (DateUtils.timeOfDayToTime { year = 1970, month = 1, day = 1 } { hour = 0, minute = 0 }) 0

        , test "dateWithoutTimeToValueString"  <|
                \() ->
                      Expect.equal (DateUtils.dateWithoutTimeToValueString { year = 1970, month = 1, day = 1 }) "1970-1-1"
        , test "valueStringToDateWithoutTime"  <|
                \() ->
                      Expect.equal (DateUtils.valueStringToDateWithoutTime "1970-1-1") { year = 1970, month = 1, day = 1 }
        , test "displayDateWithoutTime"  <|
                \() ->
                      Expect.equal (DateUtils.displayDateWithoutTime { year = 1970, month = 1, day = 1 }) "1 Jan"
        , test "displayTimeOfDay"  <|
                \() ->
                      Expect.equal (DateUtils.displayTimeOfDay { hour = 0, minute = 0 }) "00:00"
        , test "getDateMonthInt"  <|
                \() ->
                  -- 2764800000 should be february
                      Expect.equal (DateUtils.getDateMonthInt (Date.fromTime 2764800000)) 2
        ]
