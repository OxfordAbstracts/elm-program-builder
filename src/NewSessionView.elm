module NewSessionView exposing (view)

import Date
import DateUtils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import Tuple


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
                        |> List.map (\d -> option [ value (DateUtils.displayDateWithoutTime d) ] [ text (DateUtils.displayDateWithoutTime d) ])

                displayDate date =
                    (toString (date.day)) ++ "/" ++ (toString (date.month)) ++ "/" ++ (toString (date.year))
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
                    , div [ class "input-group", onInput UpdateNewSessionDate ]
                        [ label [ for "day-input" ] [ text "Date " ]
                        , br [] []
                        , select [ id "day-input" ]
                            dayOptions
                        ]
                      -- , div [] [ text (toString model.newSession) ]
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
                , div [] [ text (getWarning model) ]
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


getWarning model =
    let
        warningSuffix =
            getWarningSuffix model
    in
        if warningSuffix /= "" then
            "Cannot create Session: " ++ warningSuffix
        else
            ""


getWarningSuffix model =
    if model.newSession.name == "" then
        "session name field is empty"
    else
        ""



-- if model.newSession.name = "" then
