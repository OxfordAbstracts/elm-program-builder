port module Ports exposing (..)

-- import MainModel


port openDatepicker : () -> Cmd msg


port changeDateValue : (String -> msg) -> Sub msg
