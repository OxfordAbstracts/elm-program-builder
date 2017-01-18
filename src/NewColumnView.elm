module NewColumnView exposing (view)

import Date
import DateUtils
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
                    [ label [ for "sesssion-name-input" ]
                        [ text "Session name" ]
                    , input
                        [ class "form-control"
                        , id "sesssion-name-input"
                        , type_ "text"
                        , value model.newSession.name
                        , onInput UpdateNewSessionName
                        ]
                        [ text model.newSession.name ]
                    ]
                , div [ class "input-group" ]
                    [ label [ for "description-input" ]
                        [ text "Description" ]
                    , textarea
                        [ class "form-control"
                        , id "description-input"
                        , attribute "rows" "5"
                        , attribute "cols" "32"
                        , value model.newSession.description
                        , onInput UpdateNewSessionDescription
                        ]
                        [ text model.newSession.description ]
                    ]
                ]

        column3 =
            div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ label []
                        [ text "Start time" ]
                    , div []
                        [ input
                            [ class "form-control"
                            , type_ "number"
                            , style [ ( "width", "6rem" ) ]
                            , value (toStringIgnore0 model.newSession.startTime.hour)
                            , onInput UpdateNewSessionStartHour
                            , placeholder "00"
                            ]
                            []
                        , input
                            [ class "form-control"
                            , type_ "number"
                            , style [ ( "width", "6rem" ) ]
                            , value (toStringIgnore0 model.newSession.startTime.minute)
                            , onInput UpdateNewSessionStartMinute
                            , placeholder "00"
                            ]
                            []
                        ]
                    ]
                , div [ class "input-group" ]
                    [ label []
                        [ text "End time" ]
                    , div []
                        [ input
                            [ class "form-control"
                            , type_ "number"
                            , style [ ( "width", "6rem" ) ]
                            , value (toStringIgnore0 model.newSession.endTime.hour)
                            , onInput UpdateNewSessionEndHour
                            , placeholder "00"
                            ]
                            []
                        , input
                            [ class "form-control"
                            , type_ "number"
                            , style [ ( "width", "6rem" ) ]
                            , value (toStringIgnore0 model.newSession.endTime.minute)
                            , onInput UpdateNewSessionEndMinute
                            , placeholder "00"
                            ]
                            []
                        ]
                    ]
                , div [ style [ ( "margin-top", "1rem" ) ] ]
                    [ button [ class "btn btn-default", type_ "button", onClick CreateNewColumn ]
                        [ text "Create Column" ]
                    ]
                ]
    in
        div [ hidden (not model.showNewColumnUi), class "row" ]
            [ div [ class "col-md-4" ] [ column1 ]
            , div [ class "col-md-4" ] [ column3 ]
            ]
