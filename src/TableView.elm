module TableView exposing (..)

import Date
import DateUtils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MainMessages exposing (..)
import MainModel exposing (..)
import Time
import Utils


view : Model -> Html Msg
view model =
    div [ class "agenda", style [ ( "margin", "3rem" ) ] ]
        [ div [ class "table-responsive" ]
            [ table [ class "table table-condensed table-bordered" ]
                [ thead []
                    [ tr []
                        (defaultHeaders
                            ++ (List.map viewColumnHeader model.columns)
                        )
                    ]
                , tbody []
                    (List.concatMap (viewDate model.columns model.tracks) model.datesWithSessions)
                ]
            ]
        ]


defaultHeaders : List (Html msg)
defaultHeaders =
    [ th []
        [ text "Date" ]
    , th []
        [ text "Time" ]
    ]


viewColumnHeader : Column -> Html msg
viewColumnHeader column =
    th [] [ text column.name ]


viewDate : List Column -> List Track -> DateWithSessions -> List (Html Msg)
viewDate columns tracks dateWithSessions =
    let
        lengthOfDay =
            Time.hour * 24

        timeDelimiters =
            dateWithSessions.sessions
                |> List.concatMap
                    (\s ->
                        [ DateUtils.timeOfDayToTime dateWithSessions.date s.startTime
                        , DateUtils.timeOfDayToTime dateWithSessions.date s.endTime
                        ]
                    )
                |> Utils.dropDuplicates
                |> List.sort

        firstTime =
            timeDelimiters
                |> List.head
                |> Maybe.withDefault 8
    in
        [ tr []
            (viewDateCell dateWithSessions timeDelimiters firstTime
                ++ (List.map (appendFirstRowCell dateWithSessions timeDelimiters tracks) columns)
            )
        ]
            ++ (viewOtherRows dateWithSessions columns tracks (List.drop 1 timeDelimiters))


viewDateCell : DateWithSessions -> List Float -> Float -> List (Html msg)
viewDateCell dateWithSessions timeDelimiters firstTime =
    let
        timeDisplay =
            displayTimeDelimiter dateWithSessions timeDelimiters firstTime

        timeClass =
            if timeDisplay == "" then
                "active"
            else
                ""

        elmDate =
            DateUtils.dateWithoutTimeToDate dateWithSessions.date
    in
        [ td [ class "active", attribute "rowspan" (toString ((List.length timeDelimiters) - 1)) ]
            [ div [ class "dayofmonth" ]
                [ text (toString (Date.day elmDate)) ]
            , div [ class "dayofweek" ]
                [ text (toString (Date.dayOfWeek elmDate)) ]
            , div [ class "shortdate text-muted" ]
                [ text ((toString (Date.month elmDate)) ++ ", " ++ (toString (Date.year elmDate))) ]
            ]
        , td [ class timeClass ]
            [ text timeDisplay ]
        ]


appendFirstRowCell : DateWithSessions -> List Float -> List Track -> Column -> Html Msg
appendFirstRowCell dateWithSessions timeDelimiters tracks column =
    let
        timeDelimiter =
            timeDelimiters
                |> List.head
                |> Maybe.withDefault 0

        sessionStarting =
            dateWithSessions.sessions
                |> List.filter
                    (\s ->
                        (DateUtils.timeOfDayToTime dateWithSessions.date s.startTime)
                            == timeDelimiter
                            && s.columnId
                            == ColumnId column.id
                    )
                |> List.head

        endTime =
            sessionStarting
                |> Maybe.map .endTime
                |> Maybe.withDefault (TimeOfDay 0 0)

        rowSpan =
            getRowSpan timeDelimiters timeDelimiter dateWithSessions.date endTime

        lastTime =
            timeDelimiters
                |> Utils.last
                |> Maybe.withDefault 0

        trackId =
            sessionStarting
                |> Maybe.map .trackId
                |> Maybe.withDefault 0

        trackName =
            tracks
                |> List.filter (\t -> t.id == trackId)
                |> List.map .name
                |> List.head
                |> Maybe.withDefault ""
    in
        if timeDelimiter == lastTime then
            text ""
        else
            case sessionStarting of
                Just sessionStarting ->
                    td [ attribute "rowspan" rowSpan ]
                        [ div []
                            [ span []
                                [ text
                                    (sessionStarting.name
                                        ++ "  "
                                        ++ "Chair:  "
                                        ++ sessionStarting.chair
                                        ++ "  "
                                        ++ "Location:  "
                                        ++ sessionStarting.location
                                        ++ "  "
                                        ++ (DateUtils.displayTimeOfDay sessionStarting.startTime)
                                        ++ " - "
                                        ++ (DateUtils.displayTimeOfDay sessionStarting.endTime)
                                    )
                                ]
                            , button [ onClick (SelectSessionToEdit sessionStarting.id), style [ ( "margin-left", "0.2rem" ) ] ] [ text "edit" ]
                            , button [ onClick (DeleteSession sessionStarting.id), style [ ( "margin-left", "0.2rem" ) ] ] [ text "delete" ]
                            , br [] []
                            , b [] [ text ("Track: " ++ trackName) ]
                            ]
                        ]

                Nothing ->
                    td [ class "agenda-date active", attribute "rowspan" rowSpan ]
                        [ div [ class "agenda-event" ] []
                        ]


viewOtherRows : DateWithSessions -> List Column -> List Track -> List Float -> List (Html Msg)
viewOtherRows dateWithSessions columns tracks timeDelimiters =
    List.map (viewOtherRow dateWithSessions columns tracks timeDelimiters) timeDelimiters


viewOtherRow : DateWithSessions -> List Column -> List Track -> List Float -> Float -> Html Msg
viewOtherRow dateWithSessions columns tracks timeDelimiters timeDelimiter =
    let
        timeDisplay =
            displayTimeDelimiter dateWithSessions timeDelimiters timeDelimiter

        timeClass =
            if timeDisplay == "" then
                "active"
            else
                ""

        lastTime =
            timeDelimiters
                |> Utils.last
                |> Maybe.withDefault -1
    in
        if timeDelimiter == lastTime then
            text ""
        else
            tr []
                ([ td [ class timeClass ]
                    [ text (displayTimeDelimiter dateWithSessions timeDelimiters timeDelimiter) ]
                 ]
                    ++ (viewCells dateWithSessions columns tracks timeDelimiters timeDelimiter)
                )


viewCells : DateWithSessions -> List Column -> List Track -> List Float -> Float -> List (Html Msg)
viewCells dateWithSessions columns tracks timeDelimiters timeDelimiter =
    columns
        |> List.indexedMap (viewCell dateWithSessions tracks timeDelimiters timeDelimiter)


viewCell : DateWithSessions -> List Track -> List Float -> Float -> Int -> Column -> Html Msg
viewCell dateWithSessions tracks timeDelimiters timeDelimiter index column =
    let
        sessionsInColumn =
            dateWithSessions
                |> .sessions
                |> List.filter (\s -> (s.columnId == ColumnId column.id) || (s.columnId == AllColumns))

        sessionStarting =
            sessionsInColumn
                |> List.filter
                    (\s ->
                        (DateUtils.timeOfDayToTime dateWithSessions.date s.startTime)
                            == timeDelimiter
                            && ((s.columnId
                                    == ColumnId column.id
                                )
                                    || (index == 0 && (s.columnId == AllColumns))
                               )
                    )
                |> List.head

        sessionDate =
            dateWithSessions.date

        endTime =
            sessionStarting
                |> Maybe.map .endTime
                |> Maybe.withDefault (TimeOfDay 0 0)

        rowSpan =
            timeDelimiters
                |> List.filter (\t -> t >= timeDelimiter && t < (DateUtils.timeOfDayToTime sessionDate endTime))
                |> List.length
                |> toString

        lastTime =
            timeDelimiters
                |> Utils.last
                |> Maybe.withDefault -1

        trackId =
            sessionStarting
                |> Maybe.map .trackId
                |> Maybe.withDefault 0

        trackName =
            tracks
                |> List.filter (\t -> t.id == trackId)
                |> List.head
                |> Maybe.map .name
                |> Maybe.withDefault ""
    in
        if timeDelimiter == lastTime then
            text ""
        else
            case sessionStarting of
                Just sessionStarting ->
                    td [ attribute "rowspan" rowSpan ]
                        [ div []
                            [ span []
                                [ text
                                    (sessionStarting.name
                                        ++ "  "
                                        ++ "Chair:  "
                                        ++ sessionStarting.chair
                                        ++ "  "
                                        ++ "Location:  "
                                        ++ sessionStarting.location
                                        ++ "  "
                                        ++ (DateUtils.displayTimeOfDay sessionStarting.startTime)
                                        ++ " - "
                                        ++ (DateUtils.displayTimeOfDay sessionStarting.endTime)
                                    )
                                ]
                            , button [ onClick (SelectSessionToEdit sessionStarting.id), style [ ( "margin-left", "0.2rem" ) ] ] [ text "edit" ]
                            , button [ onClick (DeleteSession sessionStarting.id), style [ ( "margin-left", "0.2rem" ) ] ] [ text "delete" ]
                            , br [] []
                            , b [] [ text (" Track: " ++ trackName) ]
                            ]
                        ]

                Nothing ->
                    if
                        List.any
                            (\s ->
                                (DateUtils.timeOfDayToTime dateWithSessions.date s.startTime)
                                    <= timeDelimiter
                                    && (DateUtils.timeOfDayToTime dateWithSessions.date s.endTime)
                                    > timeDelimiter
                            )
                            sessionsInColumn
                    then
                        text ""
                    else
                        td [ class "agenda-date active", attribute "rowspan" rowSpan ]
                            [ div [ class "agenda-event" ] []
                            ]



-- HELPERS


displayTimeDelimiter : DateWithSessions -> List Float -> Float -> String
displayTimeDelimiter dateWithSessions timeDelimiters timeDelimiter =
    let
        nextDelimiter =
            timeDelimiters
                |> List.filter (\t -> t > timeDelimiter)
                |> List.head
                |> Maybe.withDefault 0
    in
        if
            List.any
                (\s ->
                    (DateUtils.timeOfDayToTime dateWithSessions.date s.startTime)
                        == timeDelimiter
                        || (DateUtils.timeOfDayToTime dateWithSessions.date s.endTime)
                        == nextDelimiter
                )
                dateWithSessions.sessions
        then
            DateUtils.displayTime timeDelimiter ++ " - " ++ DateUtils.displayTime nextDelimiter
        else
            ""



--


getRowSpan timeDelimiters timeDelimiter sessionDate endTime =
    timeDelimiters
        |> List.filter
            (\t ->
                t
                    >= timeDelimiter
                    && t
                    < (DateUtils.timeOfDayToTime sessionDate endTime)
            )
        |> List.length
        |> toString
