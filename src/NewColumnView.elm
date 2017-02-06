module NewColumnView exposing (view, newColumnWarning)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)


newColumnWarning model =
    if model.showNewColumnUi && model.newColumn.name == "" then
        getWarning "Column name field is empty" model
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

        column1 =
            div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ label [ for "column-name-input" ]
                        [ text "Column name" ]
                    , input
                        [ class "form-control"
                        , id "column-name-input"
                        , type_ "text"
                        , value model.newColumn.name
                        , onInput UpdateNewColumnName
                        ]
                        [ text model.newColumn.name ]
                    ]
                , div [ style [ ( "margin-top", "1rem" ) ] ] [ text (newColumnWarning model) ]
                , div [ style [ ( "margin-top", "1rem" ) ] ]
                    [ button [ class "btn btn-default", type_ "button", disabled (newColumnWarning model /= ""), onClick CreateNewColumn ]
                        [ text "Create Column" ]
                    ]
                ]
    in
        div [ hidden (not model.showNewColumnUi), class "row" ]
            [ div [ class "col-md-4" ] [ column1 ]
            ]
