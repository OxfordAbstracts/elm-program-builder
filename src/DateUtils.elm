module DateUtils
    exposing
        ( dateWithoutTimeToDate
        , displayTimeOfDay
        , parse
        , timeOfDayToTime
        )

import Date
import MainModel exposing (..)
import Time


parse : String -> Date.Date
parse string =
    string
        |> Date.fromString
        |> Result.withDefault (Date.fromTime 0)


dateWithoutTimeToDate : DateWithoutTime -> Date.Date
dateWithoutTimeToDate dateWithoutTime =
    ((toString dateWithoutTime.year) ++ "-" ++ (toString dateWithoutTime.month) ++ "-" ++ (toString dateWithoutTime.day))
        |> parse


timeOfDayToTime : DateWithoutTime -> TimeOfDay -> Time.Time
timeOfDayToTime dateWithoutTime timeOfDay =
    ((toString dateWithoutTime.year) ++ "-" ++ (toString dateWithoutTime.month) ++ "-" ++ (toString dateWithoutTime.day))
        |> parse
        |> Date.toTime
        |> (\t -> t + ((toFloat timeOfDay.hour) * Time.hour + (toFloat timeOfDay.minute) * Time.minute))


displayTimeOfDay : TimeOfDay -> String
displayTimeOfDay timeOfDay =
    let
        addHour0Padding hour =
            if String.length hour == 1 then
                "0" ++ hour
            else
                hour

        addMin0Padding min =
            if String.length min == 1 then
                min ++ "0"
            else
                min
    in
        timeOfDay
            |> (\d ->
                    [ d |> .hour |> toString |> addHour0Padding
                    , d |> .minute |> toString |> addMin0Padding
                    ]
               )
            |> String.join ":"
