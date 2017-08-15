module SessionSubmissionsView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, targetChecked)
import MainMessages exposing (..)
import MainModel exposing (..)
import DateUtils
import List.Extra
import Dict
import String
import Helpers exposing (onChange)


view : Model -> Session -> Html Msg
view model session =
    let
        toggleScheduleIndividually =
            span []
                [ label
                    [ for "schedule-individually" ]
                    [ text "Schedule individually" ]
                , input
                    [ type_ "checkbox"
                    , id "schedule-individually"
                    , class "form__checkbox"
                    , checked model.scheduleSubmissionsIndividually
                    , onClick ToggleScheduleSubmissionsIndividually
                    ]
                    []
                ]

        submissionsWithNoTimes =
            model.submissionIdsInputs
                |> List.Extra.find
                    (\{ submissionIds, startTime, endTime } -> startTime == Nothing && endTime == Nothing)
                |> Maybe.map .submissionIds
                |> Maybe.withDefault ""
    in
        div []
            (if model.scheduleSubmissionsIndividually then
                [ toggleScheduleIndividually
                , viewSessionSubmissionTimes model.submissionIdsInputs session
                , b [] [ text (invalidSubmissionsWarning model) ]
                ]
             else
                [ toggleScheduleIndividually
                , textarea
                    [ class "form__input form__input--textarea"
                    , id "submissions-input"
                    , rows 2
                    , cols 32
                    , value submissionsWithNoTimes
                    , onInput (UpdateNewSessionSubmissionIds 1 Nothing Nothing)
                    ]
                    [ text submissionsWithNoTimes ]
                , span [ class "form__hint" ]
                    [ text "" ]
                , b [] [ text (invalidSubmissionsWarning model) ]
                ]
            )


viewSessionSubmissionTimes : List SubmissionIdInput -> Session -> Html Msg
viewSessionSubmissionTimes submissionIdsInputs session =
    let
        times =
            session.submissions
                |> List.map getComparableTime
                |> List.Extra.unique
    in
        div []
            [ table []
                ([ tr []
                    [ th [] [ text "Ids" ]
                    , th [] [ text "Start Time" ]
                    , th [] [ text "End Time" ]
                    , th [] [ text "" ]
                    ]
                 ]
                    ++ (List.map (viewSessionSubmissionTime session) submissionIdsInputs)
                )
            , button [ class "button button--tertiary", onClick CreateSubmissionInput ] [ text "Add new time" ]
            ]


viewSessionSubmissionTime : Session -> SubmissionIdInput -> Html Msg
viewSessionSubmissionTime session submissionIdInput =
    let
        ( start, end ) =
            getComparableTime { startTime = submissionIdInput.startTime, endTime = submissionIdInput.endTime, id = 0 }

        submissionIds =
            session.submissions
                |> List.filter (getComparableTime >> (==) ( start, end ))
                |> List.map .id

        submissionIdsString =
            submissionIds
                |> List.map toString
                |> String.join ", "
    in
        tr []
            [ td []
                [ input
                    [ value submissionIdInput.submissionIds
                    , class "form__input"
                    , onInput
                        (UpdateNewSessionSubmissionIds submissionIdInput.id
                            submissionIdInput.startTime
                            submissionIdInput.endTime
                        )
                    ]
                    []
                ]
            , td []
                [ dateSelect session.startTime
                    session.endTime
                    (SetSessionSubmissionStartTimes submissionIdInput.id)
                    start
                ]
            , td []
                [ dateSelect session.startTime
                    session.endTime
                    (SetSessionSubmissionEndTimes submissionIdInput.id)
                    end
                ]
            , td []
                [ button [ class "button button--secondary icon icon--bin icon--bin-compact-table", onClick (DeleteSubmissionInput submissionIdInput.id) ] [] ]
            ]


getComparableTime : SessionSubmission -> ( String, String )
getComparableTime s =
    let
        maybeTimeOfDayToString =
            Maybe.map DateUtils.displayTimeOfDay
                >> Maybe.withDefault "none"
    in
        ( maybeTimeOfDayToString s.startTime
        , maybeTimeOfDayToString s.endTime
        )


dateSelect : TimeOfDay -> TimeOfDay -> (String -> Msg) -> String -> Html Msg
dateSelect min max msg timeString =
    let
        minutes =
            List.range 0 12
                |> List.map ((*) 5)

        times =
            List.range 0 23
                |> List.concatMap
                    (\h ->
                        List.map (\m -> { hour = h, minute = m }) minutes
                    )
                |> List.filter
                    (DateUtils.timeIsBetween min max)
                |> List.map DateUtils.displayTimeOfDay

        viewOption time =
            option [ selected (time == timeString) ] [ text time ]
    in
        select [ Helpers.onChange msg, class "form__input form__input--dropdown-compact" ]
            (List.map viewOption ([ "none" ] ++ times))


invalidSubmissionsWarning : Model -> String
invalidSubmissionsWarning model =
    let
        duplicates =
            getDuplicateSubmssionIds model.submissionIdsInputs
    in
        if not (String.isEmpty model.invalidSubmissionIdsInput) then
            "The following ids are not accepted submission ids and will not be saved to this session: " ++ model.invalidSubmissionIdsInput
        else if String.isEmpty duplicates then
            ""
        else
            "The following ids are duplicates: " ++ duplicates


getDuplicateSubmssionIds : List SubmissionIdInput -> String
getDuplicateSubmssionIds =
    List.map .submissionIds
        >> List.concatMap (String.split ",")
        >> List.filterMap (String.trim >> String.toInt >> Result.toMaybe)
        >> getDuplicates
        >> List.map toString
        >> String.join ", "


getDuplicates : List comparable -> List comparable
getDuplicates xs =
    let
        isDup x =
            xs |> List.filter ((==) x) |> List.length |> (<) 1
    in
        xs
            |> List.filter isDup
            |> List.Extra.unique
