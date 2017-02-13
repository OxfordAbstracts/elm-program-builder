module ManageDatesView exposing (view, manageDatesWarning)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, onFocus)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)
import DateUtils exposing (displayDateWithoutTime)


manageDatesWarning model =
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

        datesInputs =
            model.dates
                |> List.map
                    (\d ->
                        input
                            [ class "form-control pikaday-input"
                              -- , id "pikaday"
                              -- , value ((toString d.day) ++ "/" ++ (toString d.month) ++ "/" ++ (toString d.year))
                            , value (displayDateWithoutTime d)
                              -- , type_ "date"
                              -- , onBlur (DatePicked (displayDateWithoutTime d))
                            ]
                            []
                    )

        column1 =
            div [ class "form-group" ]
                [ div [ class "input-group" ]
                    datesInputs
                , div [ style [ ( "margin-top", "1rem" ) ] ] [ text (manageDatesWarning model) ]
                , div [ style [ ( "margin-top", "1rem" ) ] ]
                    [ button [ class "btn btn-default", type_ "button", disabled (manageDatesWarning model /= ""), onClick CreateNewColumn ]
                        [ text "Save Dates" ]
                    ]
                ]
    in
        div [ hidden (not model.showManageDatesUi), class "row" ]
            [ div [ class "col-md-4" ] [ column1 ]
            ]
