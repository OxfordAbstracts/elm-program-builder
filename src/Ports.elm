port module Ports exposing (..)


port openDatepicker : String -> Cmd msg


port showDeleteConfirmation : Int -> Cmd msg


port showDeleteInformationConfirmation : Int -> Cmd msg


port deleteSession : (Int -> msg) -> Sub msg


port deleteInformation : (Int -> msg) -> Sub msg


port changeDates : (List String -> msg) -> Sub msg


port changePickedDates : (List String -> msg) -> Sub msg


type alias FilePortData =
    { id : String
    , contents : String
    , filename : String
    , infoTitle : String
    , infoDescription : String
    }


type alias ChangedFilePortData =
    { id : String
    , contents : String
    , filename : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (Maybe FilePortData -> msg) -> Sub msg


port changedFileContentRead : (Maybe ChangedFilePortData -> msg) -> Sub msg


port fileChanged : String -> Cmd msg
