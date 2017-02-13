port module Ports exposing (..)

-- import MainModel


port openDatepicker : () -> Cmd msg


port changeDateValue : (List String -> msg) -> Sub msg
