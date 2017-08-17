module Helpers exposing (onChange)

import Html exposing (..)
import Json.Decode
import Html.Events exposing (on)


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" <| Json.Decode.map handler <| Json.Decode.at [ "target", "value" ] Json.Decode.string
