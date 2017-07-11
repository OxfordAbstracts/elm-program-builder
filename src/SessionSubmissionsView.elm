module SessionSubmissionsView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, targetChecked, on)
import MainMessages exposing (..)
import MainModel exposing (..)
import DateUtils
import List.Extra
import Dict


view : Model -> Session -> Html Msg
view model session =
    let
        toggleScheduleIndividually =
            label []
                [ text "Schedule individually"
                , input
                    [ type_ "checkbox"
                    , checked model.showEditSubmissionTimesView
                    , onClick ToggleSubmissionTimesView
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
            (if model.showEditSubmissionTimesView then
                [ toggleScheduleIndividually, viewSessionSubmissionTimes model.submissionIdsInputs session ]
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
            [ table [ class "form" ]
                ([ tr []
                    [ th [] [ text "Submission ids" ]
                    , th [] [ text "Start Time" ]
                    , th [] [ text "End Time" ]
                    , th [] [ text "" ]
                    ]
                 ]
                    ++ (List.map (viewSessionSubmissionTime session) submissionIdsInputs)
                )
            , button [ class "button button--primary", onClick CreateSubmissionInput ] [ text "Add new time" ]
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
                    (SetSessionSubmissionStartTimes session.id submissionIds)
                    start
                ]
            , td []
                [ dateSelect session.startTime
                    session.endTime
                    (SetSessionSubmissionEndTimes session.id submissionIds)
                    end
                ]
            , td []
                [ span [ onClick (DeleteSubmissionInput submissionIdInput.id) ] [ text "X" ] ]
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
        select [ onInput msg ]
            (List.map viewOption ([ "none" ] ++ times))


invalidSubmissionsWarning : Model -> String
invalidSubmissionsWarning model =
    if not (String.isEmpty model.invalidSubmissionIdsInput) then
        "The following submissions are invalid and will not be saved to this session: " ++ model.invalidSubmissionIdsInput
    else
        ""
