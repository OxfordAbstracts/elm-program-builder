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
                        div []
                            [ div [ class "inline-element" ]
                                [ input
                                    [ class "form__input form__input--dropdown pikaday-input"
                                    , id ("pikaday-instance-" ++ (toString i))
                                    , value (displayDateWithoutTime d)
                                    , disableInput d
                                    ]
                                    []
                                ]
                            , div [ class "inline-element" ]
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
            div [ class "form__question-sub-section--inline" ]
                [ div [ class "input-group" ]
                    datesInputs
                , div []
                    [ button
                        [ class "button button--tertiary"
                        , id "add-new-date-btn"
                        , type_ "button"
                        , onClick (GetDateAndThenAddDate <| toString <| List.length model.pickedDates)
                        ]
                        [ text "Add New Date" ]
                    ]
                , div []
                    [ button [ class "button button--primary", id "save-dates-btn", type_ "button" ]
                        [ text "Save Dates" ]
                    ]
                , span []
                    [ text "You will be unable to change any dates that have sessions" ]
                ]
    in
        div [ class "prog-form", hidden (not model.showManageDatesUi) ] [ column1 ]
