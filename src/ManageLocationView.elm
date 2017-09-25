module ManageLocationView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)


newLocationWarning model =
    let
        blankPickedLocationNameInput =
            model.pickedLocations
                |> List.map .name
                |> List.any (\n -> n == "")
    in
        if model.showManageLocationsUi && blankPickedLocationNameInput then
            getWarning "Location name field cannot be empty" model
        else
            ""


view : Model -> Html Msg
view model =
    let
        toStringIgnore0 int =
            if int == 0 then
                ""
            else
                toString int

        disableInput locationId =
            let
                locationsWithSessions =
                    model.datesWithSessions
                        |> List.concatMap .sessions
                        |> List.map .locationId
            in
                if List.member (Just locationId) locationsWithSessions then
                    disabled True
                else
                    disabled False

        onClickUpdate =
            if String.isEmpty (newLocationWarning model) then
                UpdateLocations
            else
                ShowValidationMessage

        validationWarningDiv =
            if model.showValidation then
                div [ class "form__hint form__hint--warning" ]
                    [ text (newLocationWarning model) ]
            else
                Html.text ""

        listLocations =
            model.pickedLocations
                |> List.sortBy .id
                |> List.indexedMap
                    (\i l ->
                        div [ class "form__question-section form__question-section--table form__question-section--table-auto" ]
                            [ div [ class "form__question-sub-section form__question-sub-section--table" ]
                                [ label [ class "form__label" ]
                                    [ text "Location name *" ]
                                , input
                                    [ class "form__input"
                                    , value l.name
                                    , onInput (UpdatePickedLocation l.id)
                                    ]
                                    []
                                ]
                            , div [ class "form__question-sub-section form__question-sub-section--table form__question-sub-section--button" ]
                                [ button
                                    [ onClick (DeleteLocation l.id)
                                    , disableInput l.id
                                    , class "button button--secondary icon icon--bin"
                                    ]
                                    []
                                ]
                            ]
                    )

        column1 =
            div []
                [ div []
                    listLocations
                , button
                    [ class "button button--tertiary button--wider"
                    , id "add-new-date-btn"
                    , type_ "button"
                    , onClick AddNewLocation
                    ]
                    [ text "Add New Location" ]
                , validationWarningDiv
                , button
                    [ class "button button--secondary button--wider"
                    , onClick CancelAction
                    ]
                    [ text "Cancel" ]
                , div [ class "bar bar--button" ]
                    [ button [ class "button button--primary button--wider", type_ "button", onClick onClickUpdate ]
                        [ text "Save" ]
                    ]
                ]

        displayWarning =
            if not (String.isEmpty (newLocationWarning model)) then
                "block"
            else
                "none"

        displayDiv =
            if (not model.showManageLocationsUi) then
                "none"
            else
                "block"
    in
        div [ class "form form--add-to-view", style [ ( "display", displayDiv ) ] ]
            [ span [ class "form__hint" ]
                [ i [ class "icon icon--warning icon--margin-right" ] [], text "You will be unable to change any locations that have sessions" ]
            , span [ class "form__hint" ]
                [ span [ class "form__hint form__hint--large" ] [ text "*" ], text " indicates field is mandatory" ]
            , column1
            ]
