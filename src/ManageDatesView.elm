module ManageDatesView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import DateUtils exposing (displayDateWithoutTime)
import Html.Events exposing (onClick)


view : Model -> Html Msg
view model =
    let
        toStringIgnore0 int =
            if int == 0 then
                ""
            else
                toString int

        datesHaveSessions =
            model.datesWithSessions
                |> List.filter (\d -> List.length d.sessions > 1)
                |> List.map .date

        disableInput date =
            if List.member date datesHaveSessions then
                disabled True
            else
                disabled False

        datesInputs =
            model.pickedDates
                |> List.indexedMap
                    (\i d ->
                        div []
                            [ input
                                [ class "form-control pikaday-input"
                                , id ("pikaday-instance-" ++ (toString i))
                                , value (displayDateWithoutTime d)
                                , disableInput d
                                ]
                                []
                            , button [ onClick (DeleteDate d), style [ ( "margin-left", "0.2rem" ) ], disableInput d ] [ text "Delete" ]
                            ]
                    )

        column1 =
            div [ class "form-group" ]
                [ div [ class "input-group" ]
                    datesInputs
                , div [ style [ ( "margin-top", "1rem" ) ] ]
                    [ button
                        [ class "btn btn-default"
                        , id "add-new-date-btn"
                        , type_ "button"
                        , onClick (AddNewDate (toString (List.length model.pickedDates)))
                        ]
                        [ text "Add New Date" ]
                    ]
                , div [ style [ ( "margin-top", "1rem" ) ] ]
                    [ button [ class "btn btn-default", id "save-dates-btn", type_ "button" ]
                        [ text "Save Dates" ]
                    ]
                , span []
                    [ text "You will be unable to change any dates that have sessions" ]
                ]
    in
        div [ hidden (not model.showManageDatesUi), class "row" ]
            [ div [ class "col-md-4" ] [ column1 ]
            ]
