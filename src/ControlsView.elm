module ControlsView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MainMessages exposing (..)
import MainModel exposing (..)
import NewSessionView exposing (NewSessionContext, view)
import NewColumnView
import NewTrackView
import ManageDatesView


view : Model -> Html Msg
view model =
    let
        context =
            case model.idOfSessionBeingEdited of
                Just id ->
                    NewSessionContext
                        "Save"
                        EditSession
                        model.editSession
                        model.editSessionDate

                Nothing ->
                    NewSessionContext
                        "Save"
                        CreateNewSession
                        model.newSession
                        model.newSessionDate
    in
        div []
            [ viewUiButtons model
            , NewSessionView.view context model
            , NewColumnView.view model
            , NewTrackView.view model
            , ManageDatesView.view model
            ]


viewUiButtons : Model -> Html Msg
viewUiButtons model =
    let
        toggleNewSessionClass =
            if model.showNewSessionUi && model.idOfSessionBeingEdited == Nothing then
                "btn btn-default active"
            else
                "btn btn-default"

        toggleNewColumnClass =
            if model.showNewColumnUi then
                "btn btn-default active"
            else
                "btn btn-default"

        toggleNewTrackClass =
            if model.showNewTrackUi then
                "btn btn-default active"
            else
                "btn btn-default"

        toggleManageDatesClass =
            if model.showManageDatesUi then
                "btn btn-default active"
            else
                "btn btn-default"

        publishButtonText =
            if model.published then
                "Unpublish"
            else
                "Publish"
    in
        div [ class "btn-toolbar", attribute "role" "toolbar", style [ ( "margin", "3rem" ) ] ]
            [ div [ class "btn-group btn-group-lg", attribute "role" "group" ]
                [ button [ class "button button--new", type_ "button", onClick ToggleNewSessionUi ]
                    [ text "+ New Session" ]
                , button [ class "button button--secondary", type_ "button", onClick ToggleNewTrackUi ]
                    [ text "Manage Tracks" ]
                , button [ class "button button--secondary", type_ "button", onClick ToggleNewColumnUi ]
                    [ text "Manage Columns" ]
                , button [ class "button button--secondary", type_ "button", onClick ToggleManageDatesUi ]
                    [ text "Manage Dates" ]
                , a [ class "button button--secondary", href ("/events/" ++ model.eventId ++ "/programme-builder/preview"), target "_blank", type_ "button" ]
                    [ text "Preview" ]
                , button [ class "button button--primary", type_ "button", onClick PublishProgrammeBuilder ]
                    [ text publishButtonText ]
                ]
            ]
