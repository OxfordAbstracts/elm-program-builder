module PublishedUrlView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)


view : Model -> Html Msg
view model =
    let
        showPublishUrl =
            if model.published then
                False
            else
                True
    in
        div [ hidden showPublishUrl ]
            [ span [] [ text ("Your programme builder has been published and can be accessed here: ") ]
            , a [ href ("/events/" ++ model.eventId ++ "/programme-builder/view") ] [ text (model.host ++ "/events/" ++ model.eventId ++ "/programme-builder/view") ]
            ]
