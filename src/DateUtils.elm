module DateUtils
    exposing
        ( parse
        , displayTimeOfDay
        , timeOfDayToTime
        )

import Date
import DateModels exposing (..)
import Time


parse : String -> Date.Date
parse string =
    string
        |> Date.fromString
        |> Result.withDefault (Date.fromTime 0)


timeOfDayToTime : Date.Date -> TimeOfDay -> Time.Time
timeOfDayToTime date timeOfDay =
    ((Date.toTime date) + toFloat ((60 * timeOfDay.hour) + timeOfDay.minute)) * 60 * 1000


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
