module NewSessionView exposing (view, newSessionViewWarning, NewSessionContext)

import DateUtils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, targetChecked, on)
import MainMessages exposing (..)
import MainModel exposing (..)
import GetWarning exposing (..)
import SessionSubmissionsView
import Json.Decode
import Helpers exposing (onChange)


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

        noLocationDropdownOption =
            option [ value "", selected (context.session.locationId == Nothing) ] [ text "No location" ]

        noChairDropdownOption =
            option [ value "", selected (context.session.chairId == Nothing) ] [ text "No chair" ]

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
                        [ text "Please put submission ids on separate lines or separate submission ids by , e.g. 1,3,14. Any invalid submission ids will not be assigned. " ]
                    ]
                , SessionSubmissionsView.view model context.session
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

        toLocationId locationIdString =
            if locationIdString == "" then
                Nothing
            else
                Just
                    (locationIdString
                        |> String.toInt
                        |> Result.withDefault 0
                    )

        toChairId chairIdString =
            if chairIdString == "" then
                Nothing
            else
                Just
                    (chairIdString
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
                    , select [ id "chair-input", onChange (UpdateNewSessionChair << toChairId), class "form__input form__input--dropdown" ]
                        (noChairDropdownOption :: List.map (\c -> option [ value (toString c.id), selected (context.session.chairId == Just c.id) ] [ text c.name ]) model.chairs)
                    ]
                , div []
                    [ label [ class "form__label", for "location-input" ]
                        [ text "Location" ]
                    , select [ id "location-input", onChange (UpdateNewSessionLocation << toLocationId), class "form__input form__input--dropdown" ]
                        (noLocationDropdownOption :: List.map (\l -> option [ value (toString l.id), selected (context.session.locationId == Just l.id) ] [ text l.name ]) model.locations)
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
                    [ class "form__question-sub-section form__question-sub-section--table", onChange UpdateNewSessionDate ]
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
