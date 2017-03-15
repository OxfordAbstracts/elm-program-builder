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
        displayPublishUrl =
            if model.published then
                "block"
            else
                "none"
    in
        div [ class "bar", style [ ( "display", displayPublishUrl ) ] ]
            [ span [ class "bar__copy bar__copy--space" ]
                [ text ("Your programme builder has been published and can be accessed here: ")
                , a [ class "bar__copy bar__copy--link", href ("/events/" ++ model.eventId ++ "/programme-builder/view") ] [ text (model.host ++ "/events/" ++ model.eventId ++ "/programme-builder/view") ]
                ]
            ]
