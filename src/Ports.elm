port module Ports exposing (..)


port openDatepicker : String -> Cmd msg


port showDeleteConfirmation : Int -> Cmd msg


port deleteSession : (Int -> msg) -> Sub msg


port changeDates : (List String -> msg) -> Sub msg


port changePickedDates : (List String -> msg) -> Sub msg


type alias FilePortData =
    { id : String
    , contents : String
    , filename : String
    , filetitle : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (FilePortData -> msg) -> Sub msg
