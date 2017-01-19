module NewSessionView exposing (view)

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

        sessionBeingEditted =
            case model.idOfSessionBeingEdited of
                Nothing ->
                    False

                Just val ->
                    True

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

        column2 =
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
                    [ label [ for "chair-input" ]
                        [ text "Chair" ]
                    , input
                        [ class "form-control"
                        , id "chair-input"
                        , type_ "text"
                        , value model.newSession.chair
                        , onInput UpdateNewSessionChair
                        ]
                        [ text model.newSession.chair ]
                    ]
                , div [ class "input-group" ]
                    [ label [ for "location-input" ]
                        [ text "Location" ]
                    , input
                        [ class "form-control"
                        , id "location-input"
                        , type_ "text"
                        , value model.newSession.location
                        , onInput UpdateNewSessionLocation
                        ]
                        [ text model.newSession.location ]
                    ]
                , div [ hidden True ] [ text (toString model.newSession) ]
                ]

        column3 =
            let
                dayOptions =
                    model.dates
                        |> List.map (\d -> option [ value (DateUtils.dateWithoutTimeToValueString d) ] [ text (DateUtils.displayDateWithoutTime d) ])
            in
                div [ class "form-group" ]
                    [ div [ class "input-group", onInput UpdateNewSessionDate ]
                        [ label [ for "day-input" ] [ text "Date " ]
                        , br [] []
                        , select [ id "day-input" ]
                            dayOptions
                        ]
                    , div
                        [ class "input-group" ]
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
                    , div [ style [ ( "margin-top", "1rem" ) ] ] [ text (getWarning model) ]
                    , div [ style [ ( "margin-top", "1rem" ) ] ]
                        [ button [ class "btn btn-default", type_ "button", disabled (getWarning model /= ""), onClick CreateNewSession ]
                            [ text "Create Session" ]
                        ]
                    ]
    in
        div [ hidden ((not model.showNewSessionUi) && (not sessionBeingEditted)), class "row" ]
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
            "Cannot create session: " ++ warningSuffix
        else
            ""



-- to test


getWarningSuffix model =
    if model.newSession.name == "" then
        "session name field is empty"
    else if endNotMoreThanStart model.newSession then
        "session end time must be greater than start time"
    else if sessionsAreOverLapping model.newSession model.sessions then
        "session times overlap another session in the same column"
    else
        ""


endNotMoreThanStart newSession =
    (DateUtils.timeOfDayToTime newSession.date newSession.startTime)
        >= (DateUtils.timeOfDayToTime newSession.date newSession.endTime)


sessionsAreOverLapping newSession sessions =
    sessions
        |> List.filter (\s -> s.columnId == newSession.columnId)
        |> List.any (overLappingTime newSession)



--to test


overLappingTime newSession session =
    let
        newSessionStart =
            DateUtils.timeOfDayToTime newSession.date newSession.startTime

        newSessionEnd =
            DateUtils.timeOfDayToTime newSession.date newSession.endTime

        sessionStart =
            DateUtils.timeOfDayToTime session.date session.startTime

        sessionEnd =
            DateUtils.timeOfDayToTime session.date session.endTime
    in
        newSessionStart < sessionEnd && newSessionEnd > sessionStart



-- over
-- if model.newSession.name = "" then
