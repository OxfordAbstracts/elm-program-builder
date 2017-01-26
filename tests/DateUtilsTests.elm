module DateUtilsTests exposing (..)

import Test exposing (..)
import Expect
import DateUtils
import Date
import Fuzz exposing (intRange)


all : Test
all =
    describe "DateUtils Module"
        [ test "fromStringWithDefault function defaults to 0 if it is passed an invalid date" <|
            \() ->
                Expect.equal (DateUtils.fromStringWithDefault "not Date") (Date.fromTime 0)
        , test "fromStringWithDefault appears to convert string into a date" <|
            \() ->
                Expect.equal
                    (Ok (DateUtils.fromStringWithDefault "2017-06-18T00:00:00.000+00:00"))
                    (Date.fromString "2017-06-18T00:00:00.000+00:00")
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
                let
                    result =
                        "2017-06-18T00:00:00.000+00:00"
                            |> DateUtils.fromStringWithDefault
                            |> DateUtils.dateToDateWithoutTime
                in
                    Expect.equal result { year = 2017, month = 6, day = 18 }
        , test "dateWithoutTimeToValueString correctly changes format of dateWithoutTime record to readable numerical string" <|
            \() ->
                Expect.equal (DateUtils.dateWithoutTimeToValueString { year = 2017, month = 6, day = 18 }) "2017-6-18"
        , test "valueStringToDateWithoutTime correct converts readable date string to dateWithoutTime record" <|
            \() ->
                Expect.equal (DateUtils.valueStringToDateWithoutTime "2017-6-18") { year = 2017, month = 6, day = 18 }
        , test "displayDateWithoutTime converts dateWithoutTime records to readable date format with 3 letter word for the month" <|
            \() ->
                Expect.equal (DateUtils.displayDateWithoutTime { year = 2017, month = 6, day = 18 }) "18 Jun"
        , fuzz2 (intRange 0 11) (intRange 0 59) "displayTimeOfDay correctly converts our time record to a readable string" <|
            \h m ->
                let
                    paddedHour =
                        DateUtils.add0Padding (toString h)

                    paddedMins =
                        DateUtils.add0Padding (toString m)
                in
                    Expect.equal (DateUtils.displayTimeOfDay { hour = h, minute = m }) (paddedHour ++ ":" ++ paddedMins)
        , test "getDateMonthInt correctly gets the numerical month from the date" <|
            \() ->
                Expect.equal (DateUtils.getDateMonthInt (Date.fromTime 1497744000000)) 6
        , test "displayTime displays a timeStamp as a readable time" <|
            \() ->
                let
                    dateWithoutTime =
                        { year = 2017, month = 12, day = 20 }

                    timeofDay =
                        { hour = 9, minute = 45 }

                    time =
                        --  DateWithoutTime -> TimeOfDay -> Time.Time
                        DateUtils.timeOfDayToTime dateWithoutTime timeofDay
                in
                    -- use display time of day
                    Expect.equal (DateUtils.displayTime time) "09:45"
        ]
