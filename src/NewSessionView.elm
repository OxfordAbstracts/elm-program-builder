module NewSessionView exposing (view, newSessionViewWarning, NewSessionContext)

import DateUtils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, targetChecked, on)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)


type alias NewSessionContext =
    { buttonText : String
    , onClickAction : Msg
    , session : Session
    , date : DateWithoutTime
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
                context.date
                model.datesWithSessions
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

        allColumnsDropdownOption =
            option [ value ("ALL COLUMNS"), selected (context.session.sessionColumn == AllColumns) ] [ text "ALL COLUMNS" ]

        columnOptions =
            case context.session.sessionColumn of
                ColumnId columnIdInt ->
                    (List.map (\c -> option [ value (toString c.id), selected (columnIdInt == c.id) ] [ text c.name ]) model.columns)

                _ ->
                    (List.map (\c -> option [ value (toString c.id) ] [ text c.name ]) model.columns)

        column1 =
            div []
                [ div []
                    [ label [ for "sesssion-name-input" ]
                        [ text "Session name" ]
                    , input
                        [ class "form__input"
                        , id "sesssion-name-input"
                        , type_ "text"
                        , value context.session.name
                        , onInput UpdateNewSessionName
                        ]
                        [ text context.session.name ]
                    ]
                , div []
                    [ label [ for "description-input" ]
                        [ text "Description" ]
                    , textarea
                        [ class "form__input"
                        , id "description-input"
                        , rows 5
                        , cols 32
                        , value context.session.description
                        , onInput UpdateNewSessionDescription
                        ]
                        [ text context.session.description ]
                    ]
                , div []
                    [ label [ for "submissions-input" ]
                        [ text "Submissions" ]
                    , textarea
                        [ class "form__input"
                        , id "submissions-input"
                        , rows 2
                        , cols 32
                        , value model.submissionIdsInput
                        , onInput UpdateNewSessionSubmissionIds
                        ]
                        [ text model.submissionIdsInput ]
                    ]
                , span [ class "form__hint" ]
                    [ text "Please separate submission ids by , e.g. 1,3,14. Any invalid submission ids will not be assigned" ]
                ]

        column2 =
            div []
                [ div []
                    [ label [ for "column-input" ] [ text "Column" ]
                    , br [] []
                    , select [ id "column-input", onInput UpdateNewSessionColumn, class "form__input form__input--dropdown" ]
                        (allColumnsDropdownOption :: columnOptions)
                    ]
                , div []
                    [ label [ for "track-input" ] [ text "Track " ]
                    , br [] []
                    , select [ id "track-input", onInput UpdateNewSessionTrack, class "form__input form__input--dropdown" ]
                        (List.map (\t -> option [ value (toString t.id), selected (context.session.trackId == t.id) ] [ text t.name ]) model.tracks)
                    ]
                , div []
                    [ label [ for "chair-input" ]
                        [ text "Chair" ]
                    , input
                        [ class "form__input"
                        , id "chair-input"
                        , type_ "text"
                        , value context.session.chair
                        , onInput UpdateNewSessionChair
                        ]
                        [ text model.newSession.chair ]
                    ]
                , div []
                    [ label [ for "location-input" ]
                        [ text "Location" ]
                    , input
                        [ class "form__input"
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
                    model.datesWithSessions
                        |> List.map .date
                        |> List.map
                            (\d ->
                                option
                                    [ value (DateUtils.dateWithoutTimeToValueString d)
                                    , selected (context.date == d)
                                    ]
                                    [ text (DateUtils.displayDateWithoutTime d) ]
                            )
            in
                div []
                    [ div [ onInput UpdateNewSessionDate ]
                        [ label [ for "day-input" ] [ text "Date " ]
                        , br [] []
                        , select [ id "day-input", class "form__input" ]
                            dayOptions
                        ]
                    , div
                        []
                        [ label []
                            [ text "Start time" ]
                        , div []
                            [ input
                                [ class "form__input form__input--time-hour-prog-builder"
                                , type_ "number"
                                , value (toStringIgnore0 context.session.startTime.hour)
                                , onInput UpdateNewSessionStartHour
                                , placeholder "00"
                                ]
                                []
                            , input
                                [ class "form__input form__input--time-min-prog-builder"
                                , type_ "number"
                                , value (toStringIgnore0 context.session.startTime.minute)
                                , onInput UpdateNewSessionStartMinute
                                , placeholder "00"
                                ]
                                []
                            ]
                        ]
                    , div []
                        [ label []
                            [ text "End time" ]
                        , div []
                            [ input
                                [ class "form__input form__input--time-hour-prog-builder"
                                , type_ "number"
                                , value (toStringIgnore0 context.session.endTime.hour)
                                , onInput UpdateNewSessionEndHour
                                , placeholder "00"
                                ]
                                []
                            , input
                                [ class "form__input form__input--time-min-prog-builder"
                                , type_ "number"
                                , value (toStringIgnore0 context.session.endTime.minute)
                                , onInput UpdateNewSessionEndMinute
                                , placeholder "00"
                                ]
                                []
                            ]
                        , div [ class "prog-form--warning" ] [ text (newSessionViewWarning context model) ]
                        , div []
                            [ button
                                [ class "button button--primary"
                                , type_ "button"
                                , disabled (newSessionViewWarning context model /= "")
                                , onClick context.onClickAction
                                ]
                                [ text context.buttonText ]
                            ]
                        ]
                    ]
    in
        div [ class "prog-form", hidden ((not model.showNewSessionUi) && (not sessionBeingEditted)) ]
            [ div [ class "form__question-sub-section--inline" ]
                [ div [ class "inline-element" ] [ column1 ]
                , div [ class "inline-element" ] [ column2 ]
                , div [ class "inline-element" ] [ column3 ]
                ]
            ]
