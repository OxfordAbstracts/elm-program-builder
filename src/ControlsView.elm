module ControlsView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MainMessages exposing (..)
import MainModel exposing (..)
import NewSessionView
import NewColumnView


view : Model -> Html Msg
view model =
    div []
        [ viewUiButtons model
        , text (toString model.columns)
        , text (toString (List.map .id model.sessions))
        , NewSessionView.view model
        , NewColumnView.view model
        ]


viewUiButtons : Model -> Html Msg
viewUiButtons model =
    let
        toggleNewSessionClass =
            if model.showNewSessionUi then
                "btn btn-default active"
            else
                "btn btn-default"

        toggleNewColumnClass =
            if model.showNewColumnUi then
                "btn btn-default active"
            else
                "btn btn-default"
    in
        div [ class "btn-toolbar", attribute "role" "toolbar", style [ ( "margin", "3rem" ) ] ]
            [ div [ class "btn-group btn-group-lg", attribute "role" "group" ]
                [ button [ class toggleNewSessionClass, type_ "button", onClick ToggleNewSessionUi ]
                    [ text "New Session" ]
                , button [ class "btn btn-default", type_ "button", onClick ToggleNewTrackUi ]
                    [ text "New Track" ]
                , button [ class toggleNewColumnClass, type_ "button", onClick ToggleNewColumnUi ]
                    [ text "New Column" ]
                ]
            ]
