port module Ports exposing (..)


port openDatepicker : String -> Cmd msg


port changeDates : (List String -> msg) -> Sub msg


port changePickedDates : (List String -> msg) -> Sub msg
