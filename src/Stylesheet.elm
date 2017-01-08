module Stylesheet exposing (view)

import Html exposing (node)
import Html.Attributes exposing (..)


view =
    node "link" [ href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" ] []
