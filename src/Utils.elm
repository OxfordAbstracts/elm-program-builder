module Utils
    exposing
        ( dropDuplicates
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


{-| Extract the last element of the list
-}
last : List a -> Maybe a
last list =
    list
        |> List.drop ((List.length list) - 1)
        |> List.head
