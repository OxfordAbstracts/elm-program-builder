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

        listColumns =
            model.pickedColumns
                |> List.sortBy .id
                |> List.map
                    (\c ->
                        div []
                            [ input
                                [ value c.name
                                , onInput (UpdatePickedColumn c.id)
                                ]
                                []
                            , button
                                [ onClick (DeleteColumn c.id)
                                , disableInput (ColumnId c.id)
                                ]
                                [ text "Delete" ]
                            ]
                    )

        column1 =
            div []
                [ div []
                    listColumns
                , div []
                    [ button
                        [ class "btn btn-default"
                        , id "add-new-date-btn"
                        , type_ "button"
                        , onClick AddNewColumn
                        ]
                        [ text "Add New Column" ]
                    ]
                , div [] [ text (newColumnWarning model) ]
                , div []
                    [ button [ class "btn btn-default", type_ "button", disabled (newColumnWarning model /= ""), onClick UpdateColumns ]
                        [ text "Save Changes" ]
                    ]
                ]
    in
        div [ hidden (not model.showNewColumnUi) ]
            [ div [] [ column1 ]
            ]
