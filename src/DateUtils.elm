module DateUtils
    exposing
        ( add0Padding
        , dateWithoutTimeToDate
        , dateToDateWithoutTime
        , dateWithoutTimeToDay
        , displayDateWithoutTime
        , displayTime
        , displayTimeOfDay
        , intToMonth
        , intToMonthString
        , parseTimeOfDay
        , dateWithoutTimeToValueString
        , getDateMonthInt
        , fromStringWithDefault
        , timeIsBetween
        , timeOfDayToTime
        , valueStringToDateWithoutTime
        , pikadayValueToDate
        )

import Date
import MainModel exposing (..)
import Time
import List.Extra
import Date.Extra exposing (calendarDate)


add0Padding : String -> String
add0Padding hour =
    if String.length hour == 1 then
        "0" ++ hour
    else
        hour


fromStringWithDefault : String -> Date.Date
fromStringWithDefault string =
    string
        |> Date.fromString
        |> Result.withDefault (Date.fromTime 0)


dateWithoutTimeToDate : DateWithoutTime -> Date.Date
dateWithoutTimeToDate dateWithoutTime =
    add0PaddingVal (toString dateWithoutTime.year)
        ++ "-"
        ++ add0PaddingVal (toString dateWithoutTime.month)
        ++ "-"
        ++ add0PaddingVal (toString dateWithoutTime.day)
        |> fromStringWithDefault


add0PaddingVal : String -> String
add0PaddingVal dateVal =
    if String.length dateVal < 2 then
        "0" ++ dateVal
    else
        dateVal


dateToDateWithoutTime : Date.Date -> DateWithoutTime
dateToDateWithoutTime date =
    DateWithoutTime (Date.year date) (getDateMonthInt date) (Date.day date)


dateToTimeOfDay : Date.Date -> TimeOfDay
dateToTimeOfDay date =
    TimeOfDay (Date.hour date) (Date.minute date)


timeOfDayToTime : DateWithoutTime -> TimeOfDay -> Time.Time
timeOfDayToTime dateWithoutTime timeOfDay =
    ((toString dateWithoutTime.year) ++ "-" ++ (toString dateWithoutTime.month) ++ "-" ++ (toString dateWithoutTime.day))
        |> fromStringWithDefault
        |> Date.toTime
        |> (\t -> t + ((toFloat timeOfDay.hour) * Time.hour + (toFloat timeOfDay.minute) * Time.minute))


dateWithoutTimeToValueString : DateWithoutTime -> String
dateWithoutTimeToValueString dateWithoutTime =
    (toString dateWithoutTime.year)
        ++ "-"
        ++ (toString dateWithoutTime.month)
        ++ "-"
        ++ (toString dateWithoutTime.day)


valueStringToDateWithoutTime : String -> DateWithoutTime
valueStringToDateWithoutTime dateString =
    let
        parts =
            String.split "-" dateString

        day =
            parts
                |> List.Extra.getAt 2
                |> Maybe.withDefault ""
                |> String.toInt
                |> Result.withDefault -1

        month =
            parts
                |> List.Extra.getAt 1
                |> Maybe.withDefault ""
                |> String.toInt
                |> Result.withDefault -1

        year =
            parts
                |> List.Extra.getAt 0
                |> Maybe.withDefault ""
                |> String.toInt
                |> Result.map
                    (\i ->
                        if i < 2000 then
                            i + 2000
                        else
                            i
                    )
                |> Result.withDefault -1
    in
        { year = year, month = month, day = day }


pikadayValueToDate : String -> DateWithoutTime
pikadayValueToDate val =
    let
        parts =
            String.split " " val

        day =
            parts
                |> List.Extra.getAt 0
                |> Maybe.withDefault ""
                |> String.toInt
                |> Result.withDefault -1

        month =
            parts
                |> List.Extra.getAt 1
                |> Maybe.withDefault ""
                |> monthToInt

        year =
            parts
                |> List.Extra.getAt 2
                |> Maybe.withDefault ""
                |> String.toInt
                |> Result.map
                    (\i ->
                        if i < 2000 then
                            i + 2000
                        else
                            i
                    )
                |> Result.withDefault -1
    in
        { year = year, month = month, day = day }


displayDateWithoutTime : DateWithoutTime -> String
displayDateWithoutTime d =
    (toString d.day) ++ " " ++ (intToMonthString d.month) ++ " " ++ (toString d.year)


displayTimeOfDay : TimeOfDay -> String
displayTimeOfDay timeOfDay =
    timeOfDay
        |> (\d ->
                [ d |> .hour |> toString |> add0Padding
                , d |> .minute |> toString |> add0Padding
                ]
           )
        |> String.join ":"


parseTimeOfDay : String -> Maybe TimeOfDay
parseTimeOfDay s =
    let
        parts =
            String.split ":" s

        hour =
            parts
                |> List.head
                |> Maybe.andThen (String.toInt >> Result.toMaybe)

        minute =
            parts
                |> List.Extra.last
                |> Maybe.andThen (String.toInt >> Result.toMaybe)
    in
        case ( hour, minute ) of
            ( Just hour, Just minute ) ->
                Just
                    { hour = hour
                    , minute = minute
                    }

            _ ->
                Nothing


{-| Converts a timestamp into a humam readable hours and minutes format
-}
displayTime : Time.Time -> String
displayTime =
    Date.fromTime >> dateToTimeOfDay >> displayTimeOfDay


monthToInt : String -> Int
monthToInt dateString =
    case dateString of
        "Jan" ->
            1

        "Feb" ->
            2

        "Mar" ->
            3

        "Apr" ->
            4

        "May" ->
            5

        "Jun" ->
            6

        "Jul" ->
            7

        "Aug" ->
            8

        "Sep" ->
            9

        "Oct" ->
            10

        "Nov" ->
            11

        "Dec" ->
            12

        _ ->
            -1


intToMonth : Int -> Date.Month
intToMonth int =
    case int of
        1 ->
            Date.Jan

        2 ->
            Date.Feb

        3 ->
            Date.Mar

        4 ->
            Date.Apr

        5 ->
            Date.May

        6 ->
            Date.Jun

        7 ->
            Date.Jul

        8 ->
            Date.Aug

        9 ->
            Date.Sep

        10 ->
            Date.Oct

        11 ->
            Date.Nov

        _ ->
            Date.Dec


intToMonthString : Int -> String
intToMonthString =
    intToMonth >> toString


getDateMonthInt : Date.Date -> Int
getDateMonthInt date =
    case (Date.month date) of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12


timeIsBetween : TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
timeIsBetween min max t =
    let
        greaterThanOrEqual t1 t2 =
            case compare t1.hour t2.hour of
                GT ->
                    True

                LT ->
                    False

                EQ ->
                    t1.minute >= t2.minute
    in
        (greaterThanOrEqual t min) && (greaterThanOrEqual max t)


dateWithoutTimeToDay : DateWithoutTime -> String
dateWithoutTimeToDay d =
    Date.Extra.calendarDate d.year (intToMonth d.month) d.day
        |> Date.Extra.fromSpec Date.Extra.local Date.Extra.noTime
        |> Date.dayOfWeek
        |> toString
