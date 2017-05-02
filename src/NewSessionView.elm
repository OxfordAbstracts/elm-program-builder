module NewSessionView exposing (view, newSessionViewWarning, NewSessionContext)

import DateUtils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, targetChecked, on)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)
import Json.Decode


type alias NewSessionContext =
    { buttonText : String
    , onClickAction : Msg
    , session : Session
    , date : DateWithoutTime
    }


newSessionViewWarning : NewSessionContext -> Model -> String
newSessionViewWarning context model =
    if List.length model.columns == 0 then
        getWarning "You will need to create a column before you can save this session" model
    else if List.length model.datesWithSessions == 0 then
        getWarning "You will need to create a date before you can save this session" model
    else if model.showNewSessionUi && endNotMoreThanStart context.session then
        getWarning "Session end time must be greater than start time" model
    else if model.showNewSessionUi && context.session.name == "" then
        getWarning "Session name field is empty" model
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


invalidSubmissionsWarning : NewSessionContext -> Model -> String
invalidSubmissionsWarning context model =
    if not (String.isEmpty model.invalidSubmissionIdsInput) then
        "The following submissions are invalid and will not be saved to this session: " ++ model.invalidSubmissionIdsInput
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

        noTracksDropdownOption =
            option [ value "", selected (context.session.trackId == Nothing) ] [ text "No track" ]

        column1 =
            div []
                [ label [ class "form__label", for "session-name-input" ]
                    [ text "Session name *" ]
                , input
                    [ class "form__input"
                    , id "session-name-input"
                    , type_ "text"
                    , value context.session.name
                    , onInput UpdateNewSessionName
                    ]
                    [ text context.session.name ]
                , label [ class "form__label", for "description-input" ]
                    [ text "Description" ]
                , textarea
                    [ class "form__input form__input--textarea"
                    , id "description-input"
                    , rows 5
                    , cols 32
                    , value context.session.description
                    , onInput UpdateNewSessionDescription
                    ]
                    [ text context.session.description ]
                , label [ for "submissions-input" ]
                    [ text "Submissions"
                    , span [ class "form__label form__label--sub" ]
                        [ text "Please separate submission ids by , e.g. 1,3,14. Any invalid submission ids will not be assigned. " ]
                    ]
                , textarea
                    [ class "form__input form__input--textarea"
                    , id "submissions-input"
                    , rows 2
                    , cols 32
                    , value model.submissionIdsInput
                    , onInput UpdateNewSessionSubmissionIds
                    ]
                    [ text model.submissionIdsInput ]
                , span [ class "form__hint" ]
                    [ text "" ]
                , b [] [ text (invalidSubmissionsWarning context model) ]
                ]

        toTrackId trackIdString =
            if trackIdString == "" then
                Nothing
            else
                Just
                    (trackIdString
                        |> String.toInt
                        |> Result.withDefault 0
                    )

        column2 =
            div []
                [ div []
                    [ label [ class "form__label", for "track-input" ] [ text "Track *" ]
                    , select [ id "track-input", onChange (UpdateNewSessionTrack << toTrackId), class "form__input form__input--dropdown" ]
                        (noTracksDropdownOption :: List.map (\t -> option [ value (toString t.id), selected (context.session.trackId == Just t.id) ] [ text t.name ]) model.tracks)
                    ]
                , div []
                    [ label [ class "form__label", for "chair-input" ]
                        [ text "Chair"
                        , span [ class "form__label form__label--sub" ]
                            [ text "This will be the person in charge of this session" ]
                        ]
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
                    [ label [ class "form__label", for "location-input" ]
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

        onClickUpdate =
            if String.isEmpty (newSessionViewWarning context model) then
                context.onClickAction
            else
                ShowValidationMessage

        validationWarningDiv =
            if model.showValidation then
                div [ class "form__hint form__hint--warning" ]
                    [ text (newSessionViewWarning context model) ]
            else
                Html.text ""

        column3 =
            div []
                [ label [ class "form__label" ]
                    [ text "Start time *" ]
                , input
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
                , label [ class "form__label" ]
                    [ text "End time *" ]
                , input
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
                , validationWarningDiv
                , button
                    [ class "button button--secondary button--wider"
                    , onClick CancelAction
                    ]
                    [ text "Cancel" ]
                , button
                    [ class "button button--primary button--wider"
                    , type_ "button"
                    , onClick onClickUpdate
                    ]
                    [ text context.buttonText ]
                ]

        displayDiv =
            if (not model.showNewSessionUi && not sessionBeingEditted) then
                "none"
            else
                "block"

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

        showInitialWarning =
            if List.length model.columns == 0 || List.length model.datesWithSessions == 0 then
                "block"
            else
                "none"
    in
        div [ class "form form--add-to-view", style [ ( "display", displayDiv ) ] ]
            [ span [ class "form__hint" ]
                [ span [ class "form__hint form__hint--large" ] [ text "*" ], text " indicates field is mandatory" ]
            , span [ class "form__hint form__hint--warning", style [ ( "display", showInitialWarning ) ] ]
                [ i [ class "icon icon--warning icon--margin-right" ] [], text "At least one column and date must be added before a session can be saved" ]
            , div [ class "form__question-section form__question-section--table form__question-section--divided" ]
                [ div
                    [ class "form__question-sub-section form__question-sub-section--table", onInput UpdateNewSessionDate ]
                    [ label [ class "form__label", for "day-input" ] [ text "Date *" ]
                    , select [ id "day-input", class "form__input" ]
                        dayOptions
                    ]
                , div
                    [ class "form__question-sub-section form__question-sub-section--table" ]
                    [ label [ class "form__label", for "column-input" ] [ text "Column *" ]
                    , select [ id "column-input", onChange UpdateNewSessionColumn, class "form__input form__input--dropdown" ]
                        (allColumnsDropdownOption :: columnOptions)
                    ]
                ]
            , div [ class "form__question-section form__question-section--table" ]
                [ div [ class "form__question-sub-section form__question-sub-section--table" ] [ column1 ]
                , div [ class "form__question-sub-section form__question-sub-section--table" ] [ column2 ]
                , div [ class "form__question-sub-section form__question-sub-section--table" ] [ column3 ]
                ]
            ]


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" <| Json.Decode.map handler <| Json.Decode.at [ "target", "value" ] Json.Decode.string
