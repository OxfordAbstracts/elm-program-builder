module NewSessionView exposing (view)

import Date
import DateUtils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)


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
                        (List.map (\c -> option [ value (toString c.id), selected (model.newSession.columnId == c.id) ] [ text c.name ]) model.columns)
                    ]
                  -- if newSession.columnId == c.id the selected
                , div [ class "input-group" ]
                    [ label [ for "track-input" ] [ text "Track " ]
                    , br [] []
                    , select [ id "track-input", onInput UpdateNewSessionTrack ]
                        (List.map (\t -> option [ value (toString t.id), selected (model.newSession.trackId == t.id) ] [ text t.name ]) model.tracks)
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
                        |> List.map (\d -> option [ value (DateUtils.dateWithoutTimeToValueString d), selected (model.newSession.date == d) ] [ text (DateUtils.displayDateWithoutTime d) ])
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
            , div [] [ text (toString model.newSession) ]
            ]
