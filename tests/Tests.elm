module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import DateUtils
import Date


all : Test
all =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "Addition" <|
                \() ->
                    Expect.equal (3 + 7) 10
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
            ]
        , describe "Date Utils"
            [ test "parse default" <|
                \() ->
                    Expect.equal (DateUtils.parse "not Date") (Date.fromTime 0)
            , test "parse" <|
                \() ->
                    Expect.equal (DateUtils.parse "1970-01-01T00:00:00.100+00:00") (Date.fromTime 100)
            ]
        , describe "Date Utils"
            [ test "dateWithoutTimeToDate" <|
                \() ->
                    Expect.equal (DateUtils.dateWithoutTimeToDate { year = 1970, month = 1, day = 1 }) (Date.fromTime 0)
            ]
        , describe "Date Utils"
            [ test "dateToDateWithoutTime" <|
                \() ->
                    Expect.equal (DateUtils.dateToDateWithoutTime (Date.fromTime 0)) ({ year = 1970, month = 1, day = 1 })
            ]
        , describe "Fuzz test examples, using randomly generated input"
            [ fuzz (list int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
            , fuzz (list int) "Sorting a list does not change its length" <|
                \aList ->
                    List.sort aList |> List.length |> Expect.equal (List.length aList)
            , fuzzWith { runs = 1000 } int "List.member will find an integer in a list containing it" <|
                \i ->
                    List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
            , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                \s1 s2 ->
                    s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
            ]
        ]
