module SessionSubmissionsView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, targetChecked, on)
import MainMessages exposing (..)
import MainModel exposing (..)
import DateUtils
import List.Extra


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
    in
        div []
            (if model.showEditSubmissionTimesView then
                [ toggleScheduleIndividually, viewSessionSubmissionTimes session ]
             else
                [ toggleScheduleIndividually
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
                , b [] [ text (invalidSubmissionsWarning model) ]
                ]
            )


viewSessionSubmissionTimes : Session -> Html Msg
viewSessionSubmissionTimes session =
    let
        times =
            session.submissions
                |> List.map getComparableTime
                |> List.Extra.unique
    in
        table [ class "form" ]
            ([ tr []
                [ th [] [ text "Sub id" ]
                , th [] [ text "Start Time" ]
                , th [] [ text "End Time" ]
                ]
             ]
                ++ (List.map (viewSessionSubmissionTime session) times)
            )


viewSessionSubmissionTime : Session -> ( String, String ) -> Html Msg
viewSessionSubmissionTime session ( start, end ) =
    let
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
            [ td [] [ input [ value submissionIdsString ] [] ]
            , td [] [ dateSelect session.startTime session.endTime (SetSessionSubmissionStartTimes session.id submissionIds) start ]
            , td [] [ dateSelect session.startTime session.endTime (SetSessionSubmissionEndTimes session.id submissionIds) end ]
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
