module NewSessionView exposing (view, newSessionViewWarning, NewSessionContext)

import DateUtils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)


type alias NewSessionContext =
    { buttonText : String
    , onClickAction : Msg
    , session : Session
    }


newSessionViewWarning : NewSessionContext -> Model -> String
newSessionViewWarning context model =
    if model.showNewSessionUi && context.session.name == "" then
        getWarning "Session name field is empty" model
    else if model.showNewSessionUi && endNotMoreThanStart context.session then
        getWarning "Session end time must be greater than start time" model
    else if
        model.showNewSessionUi
            && sessionsAreOverLapping
                context.session
                model.sessions
                model.idOfSessionBeingEdited
    then
        getWarning "Session times overlap another session in the same column" model
    else
        ""


view : NewSessionContext -> Model -> Html Msg
view context model =
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
                        , value context.session.name
                        , onInput UpdateNewSessionName
                        ]
                        [ text context.session.name ]
                    ]
                , div [ class "input-group" ]
                    [ label [ for "description-input" ]
                        [ text "Description" ]
                    , textarea
                        [ class "form-control"
                        , id "description-input"
                        , attribute "rows" "5"
                        , attribute "cols" "32"
                        , value context.session.description
                        , onInput UpdateNewSessionDescription
                        ]
                        [ text context.session.description ]
                    ]
                , div [ class "input-group" ]
                    [ label [ for "submissions-input" ]
                        [ text "Submissions" ]
                    , textarea
                        [ class "form-control"
                        , id "submissions-input"
                        , attribute "rows" "2"
                        , attribute "cols" "32"
                        , value model.submissionIdsInput
                        , onInput UpdateNewSessionSubmissionIds
                        ]
                        [ text model.submissionIdsInput ]
                    ]
                , span []
                    [ text "Please separate submission ids by , e.g. 1,3,14. Any invalid submission ids will not be assigned" ]
                ]

        column2 =
            div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ label [ for "column-input" ] [ text "Column" ]
                    , br [] []
                    , select [ id "column-input", onInput UpdateNewSessionColumn ]
                        (List.map (\c -> option [ value (toString c.id), selected (context.session.columnId == c.id) ] [ text c.name ]) model.columns)
                    ]
                  -- if newSession.columnId == c.id the selected
                , div [ class "input-group" ]
                    [ label [ for "track-input" ] [ text "Track " ]
                    , br [] []
                    , select [ id "track-input", onInput UpdateNewSessionTrack ]
                        (List.map (\t -> option [ value (toString t.id), selected (context.session.trackId == t.id) ] [ text t.name ]) model.tracks)
                    ]
                , div [ class "input-group" ]
                    [ label [ for "chair-input" ]
                        [ text "Chair" ]
                    , input
                        [ class "form-control"
                        , id "chair-input"
                        , type_ "text"
                        , value context.session.chair
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
                        , value context.session.location
                        , onInput UpdateNewSessionLocation
                        ]
                        [ text context.session.location ]
                    ]
                ]

        column3 =
            let
                dayOptions =
                    model.dates
                        |> List.map
                            (\d ->
                                option
                                    [ value (DateUtils.dateWithoutTimeToValueString d)
                                    , selected (context.session.date == d)
                                    ]
                                    [ text (DateUtils.displayDateWithoutTime d) ]
                            )
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
                                , value (toStringIgnore0 context.session.startTime.hour)
                                , onInput UpdateNewSessionStartHour
                                , placeholder "00"
                                ]
                                []
                            , input
                                [ class "form-control"
                                , type_ "number"
                                , style [ ( "width", "6rem" ) ]
                                , value (toStringIgnore0 context.session.startTime.minute)
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
                                , value (toStringIgnore0 context.session.endTime.hour)
                                , onInput UpdateNewSessionEndHour
                                , placeholder "00"
                                ]
                                []
                            , input
                                [ class "form-control"
                                , type_ "number"
                                , style [ ( "width", "6rem" ) ]
                                , value (toStringIgnore0 context.session.endTime.minute)
                                , onInput UpdateNewSessionEndMinute
                                , placeholder "00"
                                ]
                                []
                            ]
                        , div [ style [ ( "margin-top", "1rem" ) ] ] [ text (newSessionViewWarning context model) ]
                        , div [ style [ ( "margin-top", "1rem" ) ] ]
                            [ button
                                [ class "btn btn-default"
                                , type_ "button"
                                , disabled (newSessionViewWarning context model /= "")
                                , onClick context.onClickAction
                                ]
                                [ text context.buttonText ]
                            ]
                        ]
                    ]
    in
        div [ hidden ((not model.showNewSessionUi) && (not sessionBeingEditted)), class "row" ]
            [ div [ class "col-md-4" ] [ column1 ]
            , div [ class "col-md-4" ] [ column2 ]
            , div [ class "col-md-4" ] [ column3 ]
            ]
