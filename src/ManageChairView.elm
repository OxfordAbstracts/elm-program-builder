module ManageChairView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)


newChairWarning model =
    let
        blankPickedChairNameInput =
            model.pickedChairs
                |> List.map .name
                |> List.any (\n -> n == "")
    in
        if model.showManageChairsUi && blankPickedChairNameInput then
            getWarning "Chair name field cannot be empty" model
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

        disableInput chairId =
            let
                chairsWithSessions =
                    model.datesWithSessions
                        |> List.concatMap .sessions
                        |> List.map .chairId
            in
                if List.member (Just chairId) chairsWithSessions then
                    disabled True
                else
                    disabled False

        onClickUpdate =
            if String.isEmpty (newChairWarning model) then
                UpdateChairs
            else
                ShowValidationMessage

        validationWarningDiv =
            if model.showValidation then
                div [ class "form__hint form__hint--warning" ]
                    [ text (newChairWarning model) ]
            else
                Html.text ""

        listChairs =
            model.pickedChairs
                |> List.sortBy .id
                |> List.indexedMap
                    (\i l ->
                        div [ class "form__question-section form__question-section--table form__question-section--table-auto" ]
                            [ div [ class "form__question-sub-section form__question-sub-section--table" ]
                                [ label [ class "form__label" ]
                                    [ text "Chair name *" ]
                                , input
                                    [ class "form__input"
                                    , value l.name
                                    , onInput (UpdatePickedChair l.id)
                                    ]
                                    []
                                ]
                            , div [ class "form__question-sub-section form__question-sub-section--table form__question-sub-section--button" ]
                                [ button
                                    [ onClick (DeleteChair l.id)
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
                    listChairs
                , button
                    [ class "button button--tertiary button--wider"
                    , id "add-new-date-btn"
                    , type_ "button"
                    , onClick AddNewChair
                    ]
                    [ text "Add New Chair" ]
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
            if not (String.isEmpty (newChairWarning model)) then
                "block"
            else
                "none"

        displayDiv =
            if (not model.showManageChairsUi) then
                "none"
            else
                "block"
    in
        div [ class "form form--add-to-view", style [ ( "display", displayDiv ) ] ]
            [ span [ class "form__hint" ]
                [ i [ class "icon icon--warning icon--margin-right" ] [], text "You will be unable to change any chairs that have sessions" ]
            , span [ class "form__hint" ]
                [ span [ class "form__hint form__hint--large" ] [ text "*" ], text " indicates field is mandatory" ]
            , column1
            ]
