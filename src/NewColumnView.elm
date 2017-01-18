module NewColumnView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)


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
                    , div [ style [ ( "margin-top", "1rem" ) ] ] [ text (getWarning model) ]
                    , div [ style [ ( "margin-top", "1rem" ) ] ]
                        [ button [ class "btn btn-default", type_ "button", disabled (getWarning model /= ""), onClick CreateNewColumn ]
                            [ text "Create Column" ]
                  ]
                ]

    in
        div [ hidden (not model.showNewColumnUi), class "row" ]
            [ div [ class "col-md-4" ] [ column1 ]
            ]

getWarning model =
    let
        warningSuffix =
            getWarningSuffix model
    in
        if warningSuffix /= "" then
            "Cannot create column: " ++ warningSuffix
        else
            ""

-- to test


getWarningSuffix model =
    if model.newColumn.name == "" then
        "Column name field is empty"
    else
        ""
