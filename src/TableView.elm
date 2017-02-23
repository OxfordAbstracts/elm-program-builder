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
    let
        numColumns =
            List.length model.columns
    in
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
                        (List.concatMap (viewDate model.columns model.tracks numColumns) model.datesWithSessions)
                    ]
                ]
            ]


sessionAcrossAllColumns sessionsInColumn sessionStarting index =
    let
        sessionsAllColumns =
            sessionsInColumn
                |> List.filter (\s -> index == 0 && (s.columnId == AllColumns))
    in
        List.member (Maybe.withDefault (blankSession 1) sessionStarting) sessionsAllColumns


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


viewDate : List Column -> List Track -> Int -> DateWithSessions -> List (Html Msg)
viewDate columns tracks numColumns dateWithSessions =
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
                ++ (List.indexedMap (appendFirstRowCell dateWithSessions timeDelimiters tracks numColumns) columns)
            )
        ]
            ++ (viewOtherRows dateWithSessions columns tracks (List.drop 1 timeDelimiters) numColumns)


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


getSessionStarting sessionsInColumn dateWithSessions column timeDelimiter index =
    sessionsInColumn
        |> List.filter
            (\s ->
                (DateUtils.timeOfDayToTime dateWithSessions.date s.startTime)
                    == timeDelimiter
                    && ((s.columnId
                            == ColumnId column.id
                        )
                            || (index == 0 && s.columnId == AllColumns)
                       )
            )
        |> List.head


appendFirstRowCell : DateWithSessions -> List Float -> List Track -> Int -> Int -> Column -> Html Msg
appendFirstRowCell dateWithSessions timeDelimiters tracks numColumns index column =
    let
        timeDelimiter =
            timeDelimiters
                |> List.head
                |> Maybe.withDefault 0

        sessionsInColumn =
            dateWithSessions
                |> .sessions
                |> List.filter (\s -> (s.columnId == ColumnId column.id) || (s.columnId == AllColumns))

        sessionStarting =
            getSessionStarting sessionsInColumn dateWithSessions column timeDelimiter index

        sessionIsAcrossAllColumns =
            sessionAcrossAllColumns sessionsInColumn sessionStarting index

        colSpan =
            if sessionIsAcrossAllColumns then
                numColumns
            else
                1

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
                    td [ attribute "rowspan" rowSpan, attribute "colspan" (toString colSpan) ]
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
                    noSessionInDateCellView timeDelimiter dateWithSessions rowSpan sessionsInColumn


noSessionInDateCellView : Float -> DateWithSessions -> String -> List Session -> Html Msg
noSessionInDateCellView timeDelimiter dateWithSessions rowSpan sessionsInColumn =
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


viewOtherRows : DateWithSessions -> List Column -> List Track -> List Float -> Int -> List (Html Msg)
viewOtherRows dateWithSessions columns tracks timeDelimiters numColumns =
    List.map (viewOtherRow dateWithSessions columns tracks timeDelimiters numColumns) timeDelimiters


viewOtherRow : DateWithSessions -> List Column -> List Track -> List Float -> Int -> Float -> Html Msg
viewOtherRow dateWithSessions columns tracks timeDelimiters numColumns timeDelimiter =
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
                    ++ (viewCells dateWithSessions columns tracks timeDelimiters numColumns timeDelimiter)
                )


viewCells : DateWithSessions -> List Column -> List Track -> List Float -> Int -> Float -> List (Html Msg)
viewCells dateWithSessions columns tracks timeDelimiters numColumns timeDelimiter =
    columns
        |> List.indexedMap (viewCell dateWithSessions tracks timeDelimiters numColumns timeDelimiter)


getSessionAcrossAllColumns sessionsInColumn sessionStarting index =
    let
        sessionsAllColumns =
            sessionsInColumn
                |> List.filter (\s -> index == 0 && (s.columnId == AllColumns))
    in
        List.member (Maybe.withDefault (blankSession 1) sessionStarting) sessionsAllColumns


viewCell : DateWithSessions -> List Track -> List Float -> Int -> Float -> Int -> Column -> Html Msg
viewCell dateWithSessions tracks timeDelimiters numColumns timeDelimiter index column =
    let
        sessionsInColumn =
            dateWithSessions
                |> .sessions
                |> List.filter (\s -> (s.columnId == ColumnId column.id) || (s.columnId == AllColumns))

        sessionStarting =
            getSessionStarting sessionsInColumn dateWithSessions column timeDelimiter index

        sessionAcrossAllColumns =
            getSessionAcrossAllColumns sessionsInColumn sessionStarting index

        colSpan =
            if sessionAcrossAllColumns then
                numColumns
            else
                1

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
                    td [ attribute "rowspan" rowSpan, attribute "colspan" (toString colSpan) ]
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
                    noSessionInDateCellView timeDelimiter dateWithSessions rowSpan sessionsInColumn



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
