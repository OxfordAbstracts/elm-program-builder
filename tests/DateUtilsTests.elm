module DateUtilsTests exposing (..)

import Test exposing (..)
import Expect
import DateUtils
import Date


all : Test
all =
    -- 1497744000 is 18 june 2017 00:00 gmt
    describe "Date Utils"
        [ test "parse function defaults to 0 if it is passed an invalid date" <|
            \() ->
                Expect.equal (DateUtils.parse "not Date") (Date.fromTime 0)
        , test "parse appears to correctly parse dates" <|
            \() ->
                Expect.equal
                    (DateUtils.parse "2017-06-18T00:00:00.000+00:00")
                    (Date.fromTime 1497744000000)
        , test "dateWithoutTimeToDate correctly converts a date record without a time to the correct date" <|
            \() ->
                let
                    expectedDate =
                        case Date.fromString "2017-6-18" of
                            Err str ->
                                Debug.crash str

                            Ok date ->
                                date
                in
                    Expect.equal (DateUtils.dateWithoutTimeToDate { year = 2017, month = 6, day = 18 }) expectedDate
        , test "dateToDateWithoutTime correctly converts a date to our date record without a time" <|
            \() ->
                Expect.equal (DateUtils.dateToDateWithoutTime (Date.fromTime 1497740400000)) ({ year = 2017, month = 6, day = 18 })
        , test "timeOfDayToTime correct converts our time our time of day record to the correct time (ms)" <|
            \() ->
                Expect.equal (DateUtils.timeOfDayToTime { year = 2017, month = 6, day = 18 } { hour = 0, minute = 0 }) 1497740400000
        , test "dateWithoutTimeToValueString correctly changes format of dateWithoutTime record to readable numerical string" <|
            \() ->
                Expect.equal (DateUtils.dateWithoutTimeToValueString { year = 2017, month = 6, day = 18 }) "2017-6-18"
        , test "valueStringToDateWithoutTime correct converts readable date string to dateWithoutTime record" <|
            \() ->
                Expect.equal (DateUtils.valueStringToDateWithoutTime "2017-6-18") { year = 2017, month = 6, day = 18 }
        , test "displayDateWithoutTime converts dateWithoutTime records to readable date format with 3 letter word for the month" <|
            \() ->
                Expect.equal (DateUtils.displayDateWithoutTime { year = 2017, month = 6, day = 18 }) "18 Jun"
        , test "displayTimeOfDay correctly converts our time record to a readable string" <|
            \() ->
                Expect.equal (DateUtils.displayTimeOfDay { hour = 9, minute = 32 }) "09:32"
        , test "getDateMonthInt correctly gets the numerical month from the date" <|
            \() ->
                Expect.equal (DateUtils.getDateMonthInt (Date.fromTime 1497744000000)) 6
        ]
