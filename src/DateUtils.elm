module DateUtils
    exposing
        ( add0Padding
        , dateWithoutTimeToDate
        , dateToDateWithoutTime
        , displayDateWithoutTime
        , displayTime
        , displayTimeOfDay
        , dateWithoutTimeToValueString
        , getDateMonthInt
        , fromStringWithDefault
        , timeOfDayToTime
        , valueStringToDateWithoutTime
        )

import Date
import MainModel exposing (..)
import Time


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


add0PaddingVal dateVal =
    let
        int =
            dateVal
                |> String.toInt
                |> Result.withDefault 0
    in
        if int < 10 then
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
    dateString
        |> fromStringWithDefault
        |> dateToDateWithoutTime


displayDateWithoutTime : DateWithoutTime -> String
displayDateWithoutTime dateWithoutTime =
    dateWithoutTime
        |> dateWithoutTimeToDate
        |> (\d -> (toString (Date.day d)) ++ " " ++ (toString (Date.month d)) ++ " " ++ (toString (Date.year d)))


displayTimeOfDay : TimeOfDay -> String
displayTimeOfDay timeOfDay =
    timeOfDay
        |> (\d ->
                [ d |> .hour |> toString |> add0Padding
                , d |> .minute |> toString |> add0Padding
                ]
           )
        |> String.join ":"


{-| Converts a timestamp into a humam readable hours and minutes format
-}
displayTime : Time.Time -> String
displayTime =
    Date.fromTime >> dateToTimeOfDay >> displayTimeOfDay


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
