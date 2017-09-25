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
import ManageLocationView
import ManageChairView
import PublishedUrlView exposing (view)
import Helpers exposing (onChange)


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
            , ManageLocationView.view model
            , ManageChairView.view model
            , ManageDatesView.view model
            ]


viewUiButtons : Model -> Html Msg
viewUiButtons model =
    let
        toggleNewColumnClass =
            if model.showNewColumnUi then
                "programme-controls__button-dropdown programme-controls__button-dropdown--active"
            else
                "programme-controls__button-dropdown"

        toggleNewTrackClass =
            if model.showNewTrackUi then
                "programme-controls__button-dropdown programme-controls__button-dropdown--active"
            else
                "programme-controls__button-dropdown"

        toggleManageDatesClass =
            if model.showManageDatesUi then
                "programme-controls__button-dropdown programme-controls__button-dropdown--active"
            else
                "programme-controls__button-dropdown"

        toggleManageLocationsClass =
            if model.showManageLocationsUi then
                "programme-controls__button-dropdown programme-controls__button-dropdown--active"
            else
                "programme-controls__button-dropdown"

        toggleManageChairsClass =
            if model.showManageChairsUi then
                "programme-controls__button-dropdown programme-controls__button-dropdown--active"
            else
                "programme-controls__button-dropdown"

        publishButtonText =
            if model.published then
                "Unpublish"
            else
                "Publish"
    in
        div []
            [ div [ class "programme-controls" ]
                [ button [ class toggleManageDatesClass, type_ "button", onClick ToggleManageDatesUi ]
                    [ text "Manage Dates" ]
                , button [ class toggleManageLocationsClass, type_ "button", onClick ToggleManageLocationsUi ]
                    [ text "Manage Locations" ]
                , button [ class toggleManageChairsClass, type_ "button", onClick ToggleManageChairsUi ]
                    [ text "Manage Chairs" ]
                , button [ class toggleNewColumnClass, type_ "button", onClick ToggleNewColumnUi ]
                    [ text "Manage Columns" ]
                , button [ class toggleNewTrackClass, type_ "button", onClick ToggleNewTrackUi ]
                    [ text "Manage Tracks" ]
                , button [ class "button button--new", type_ "button", onClick ToggleNewSessionUi ]
                    [ text "+ New Session" ]
                , div [ class "prog-controls__dividing-section" ]
                    [ a [ class "button button--secondary button--wider", href ("/events/" ++ model.eventId ++ "/programme-builder/preview"), target "_blank", type_ "button" ]
                        [ text "Preview" ]
                    ]
                , button [ class "button button--primary button--wider", type_ "button", onClick PublishProgrammeBuilder ]
                    [ text publishButtonText ]
                ]
            , PublishedUrlView.view model
            ]
