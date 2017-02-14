port module Ports exposing (..)

-- import MainModel


port openDatepicker : String -> Cmd msg


port changeDates : (List String -> msg) -> Sub msg



-- port changePickedDates : (List String -> msg) -> Sub msg
