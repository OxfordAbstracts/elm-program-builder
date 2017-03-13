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
                |> List.filter (\d -> List.length d.sessions > 0)
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
                        div [ class "form__question-section form__question-section--table" ]
                            [ div [ class "form__question-sub-section form__question-sub-section--table" ]
                                [ label [ class "form__label" ]
                                    [ text "Date" ]
                                , input
                                    [ class "form__input  pikaday-input"
                                    , id ("pikaday-instance-" ++ (toString i))
                                    , value (displayDateWithoutTime d)
                                    , disableInput d
                                    ]
                                    []
                                ]
                            , div [ class "form__question-sub-section form__question-sub-section--table form__question-sub-section--button" ]
                                [ button
                                    [ onClick (DeleteDate d)
                                    , disableInput d
                                    , class "button button--secondary icon icon--bin"
                                    ]
                                    []
                                ]
                            ]
                    )

        column1 =
            div []
                [ span [ class "form__hint" ]
                    [ i [ class "icon icon--warning" ] [], text "You will be unable to change any dates that have sessions" ]
                , div []
                    datesInputs
                , button
                    [ class "button button--tertiary"
                    , id "add-new-date-btn"
                    , type_ "button"
                    , onClick (GetDateAndThenAddDate <| toString <| List.length model.pickedDates)
                    ]
                    [ text "Add New Date" ]
                , button
                    [ class "button button--secondary"
                    , onClick CancelAction
                    ]
                    [ text "Cancel" ]
                , div [ class "bar bar--button" ]
                    [ button [ class "button button--primary button--wider", id "save-dates-btn", type_ "button" ]
                        [ text "Save" ]
                    ]
                ]

        displayDiv =
            if (not model.showManageDatesUi) then
                "none"
            else
                "block"
    in
        div [ class "form form--add-to-view", style [ ( "display", displayDiv ) ] ] [ column1 ]
