module NewSessionView exposing (view)

import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)


view : Model -> Html Msg
view model =
    let
        column1 =
            div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ label [ for "sesssion-name-input" ]
                        [ text "Session name" ]
                    , input [ class "form-control", id "sesssion-name-input", type_ "text", onInput UpdateNewSessionName ]
                        [ text model.newSession.name ]
                    ]
                , div [ class "input-group" ]
                    [ label [ for "description-input" ]
                        [ text "Description" ]
                    , textarea [ class "form-control", id "description-input", attribute "rows" "5", attribute "cols" "32", onInput UpdateNewSessionDescription ]
                        [ text model.newSession.description ]
                    ]
                ]

        column2 =
            let
                dayOptions =
                    model.dates
                        |> List.map (\d -> (toString (Date.day d)) ++ " " ++ (toString (Date.month d)))
                        |> List.map (\d -> option [ value d ] [ text d ])
            in
                div [ class "form-group" ]
                    [ div [ class "input-group" ]
                        [ label [ for "column-input" ] [ text "Column" ]
                        , br [] []
                        , select [ id "column-input", onInput UpdateNewSessionColumn ]
                            (List.map (\c -> option [ value (toString c.id) ] [ text c.name ]) model.columns)
                        ]
                    , div [ class "input-group" ]
                        [ label [ for "track-input" ] [ text "Track " ]
                        , br [] []
                        , select [ id "track-input", onInput UpdateNewSessionTrack ]
                            (List.map (\t -> option [ value (toString t.id) ] [ text t.name ]) model.tracks)
                        ]
                    , div [ class "input-group" ]
                        [ label [ for "day-input" ] [ text "Day " ]
                        , br [] []
                        , select [ id "day-input" ]
                            dayOptions
                        ]
                    , div [] [ text (toString model.newSession) ]
                    ]

        column3 =
            div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ label [ for "start-time-input" ]
                        [ text "Start time" ]
                    , input [ class "form-control", id "start-time-input", type_ "time" ]
                        []
                    ]
                , div [ class "input-group" ]
                    [ label [ for "end-time-input" ]
                        [ text "End time" ]
                    , input [ class "form-control", id "end-time-input", type_ "time" ]
                        []
                    ]
                , div [ style [ ( "margin-top", "2rem" ) ] ]
                    [ button [ class "btn btn-default", type_ "button", onClick ToggleNewColumnUi ]
                        [ text "Create Session" ]
                    ]
                ]
    in
        div [ hidden (not model.showNewSessionUi), class "row" ]
            [ div [ class "col-md-4" ] [ column1 ]
            , div [ class "col-md-4" ] [ column2 ]
            , div [ class "col-md-4" ] [ column3 ]
            ]
