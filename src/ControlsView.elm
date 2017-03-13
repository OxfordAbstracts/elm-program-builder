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
import PublishedUrlView exposing (view)


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
        toggleNewColumnClass =
            if model.showNewColumnUi then
                "prog-bar__button-dropdown prog-bar__button-dropdown--active"
            else
                "prog-bar__button-dropdown"

        toggleNewTrackClass =
            if model.showNewTrackUi then
                "prog-bar__button-dropdown prog-bar__button-dropdown--active"
            else
                "prog-bar__button-dropdown"

        toggleManageDatesClass =
            if model.showManageDatesUi then
                "prog-bar__button-dropdown prog-bar__button-dropdown--active"
            else
                "prog-bar__button-dropdown"

        publishButtonText =
            if model.published then
                "Unpublish"
            else
                "Publish"
    in
        div [ class "prog-bar" ]
            [ div []
                [ button [ class "button button--new prog-bar__button", type_ "button", onClick ToggleNewSessionUi ]
                    [ text "+ New Session" ]
                , button [ class toggleNewTrackClass, type_ "button", onClick ToggleNewTrackUi ]
                    [ text "Manage Tracks" ]
                , button [ class toggleNewColumnClass, type_ "button", onClick ToggleNewColumnUi ]
                    [ text "Manage Columns" ]
                , button [ class toggleManageDatesClass, type_ "button", onClick ToggleManageDatesUi ]
                    [ text "Manage Dates" ]
                , a [ class "button button--secondary prog-bar__button", href ("/events/" ++ model.eventId ++ "/programme-builder/preview"), target "_blank", type_ "button" ]
                    [ text "Preview" ]
                , button [ class "button button--primary prog-bar__button", type_ "button", onClick PublishProgrammeBuilder ]
                    [ text publishButtonText ]
                , PublishedUrlView.view model
                ]
            ]
