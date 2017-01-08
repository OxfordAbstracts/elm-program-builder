module Utils
    exposing
        ( dropDuplicates
        , displayTime
        , last
        )

import Date
import Set
import Time
import Tuple exposing (second)


{-| Drop _all_ duplicate elements from the list
-}
dropDuplicates : List comparable -> List comparable
dropDuplicates list =
    let
        step next ( set, acc ) =
            if Set.member next set then
                ( set, acc )
            else
                ( Set.insert next set, next :: acc )
    in
        List.foldl step ( Set.empty, [] ) list |> second |> List.reverse


{-| Converts a timestamp into a humam readable hours and minutes format
-}
displayTime : Time.Time -> String
displayTime timeDelimiter =
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
        timeDelimiter
            |> Date.fromTime
            |> (\d ->
                    [ d |> Date.hour |> toString |> addHour0Padding
                    , d |> Date.minute |> toString |> addMin0Padding
                    ]
               )
            |> String.join ":"


{-| Extract the last element of the list
-}
last : List a -> Maybe a
last list =
    list
        |> List.drop ((List.length list) - 1)
        |> List.head
