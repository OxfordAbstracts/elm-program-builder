module NewColumnView exposing (view, newColumnWarning)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)


newColumnWarning model =
    let
        blankPickedColumn =
            model.pickedColumns
                |> List.map .name
                |> List.any (\n -> String.isEmpty n)
    in
        if model.showNewColumnUi && blankPickedColumn then
            getWarning "Column name fields cannot be empty" model
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

        disableInput columnId =
            let
                columnsWithSessions =
                    model.datesWithSessions
                        |> List.concatMap .sessions
                        |> List.map .sessionColumn
                        |> List.filter (\c -> c /= AllColumns)
            in
                if List.member columnId columnsWithSessions then
                    disabled True
                else
                    disabled False

        onClickUpdate =
            if String.isEmpty (newColumnWarning model) then
                UpdateColumns
            else
                ShowValidationMessage

        validationWarningDiv =
            if model.showValidation then
                div [ class "form__hint form__hint--warning" ]
                    [ text (newColumnWarning model) ]
            else
                Html.text ""

        listColumns =
            model.pickedColumns
                |> List.sortBy .id
                |> List.map
                    (\c ->
                        div [ class "form__question-section form__question-section--table form__question-section--table-auto" ]
                            [ div [ class "form__question-sub-section form__question-sub-section--table" ]
                                [ label [ class "form__label" ]
                                    [ text "Column name *" ]
                                , input
                                    [ class "form__input"
                                    , value c.name
                                    , onInput (UpdatePickedColumn c.id)
                                    ]
                                    []
                                ]
                            , div [ class "form__question-sub-section form__question-sub-section--table form__question-sub-section--button" ]
                                [ button
                                    [ onClick (DeleteColumn c.id)
                                    , disableInput (ColumnId c.id)
                                    , class "button button--secondary icon icon--bin"
                                    ]
                                    []
                                ]
                            ]
                    )

        column1 =
            div []
                [ div []
                    listColumns
                , div []
                    [ button
                        [ class "button button--tertiary button--wider"
                        , id "add-new-date-btn"
                        , type_ "button"
                        , onClick AddNewColumn
                        ]
                        [ text "Add New Column" ]
                    ]
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

        displayDiv =
            if (not model.showNewColumnUi) then
                "none"
            else
                "block"
    in
        div [ class "form form--add-to-view", style [ ( "display", displayDiv ) ] ]
            [ span [ class "form__hint" ]
                [ span [ class "form__hint form__hint--large" ] [ text "*" ], text " indicates field is mandatory" ]
            , div [] [ column1 ]
            ]
