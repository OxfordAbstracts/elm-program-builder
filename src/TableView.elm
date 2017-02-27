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
                        (List.concatMap (viewDate model numColumns) model.datesWithSessions)
                    ]
                ]
            ]


sessionIsAcrossAllColumns sessionsInColumn sessionStarting index =
    case sessionStarting of
        Just session ->
            let
                sessionsAllColumns =
                    sessionsInColumn
                        |> List.filter (\s -> index == 0 && (s.sessionColumn == AllColumns))
            in
                List.member session sessionsAllColumns

        Nothing ->
            False


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


viewDate : Model -> Int -> DateWithSessions -> List (Html Msg)
viewDate model numColumns dateWithSessions =
    let
        columns =
            model.columns

        tracks =
            model.tracks

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
                ++ (List.indexedMap (appendFirstRowCell dateWithSessions timeDelimiters model numColumns) columns)
            )
        ]
            ++ (viewOtherRows dateWithSessions model (List.drop 1 timeDelimiters) numColumns)


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
    let
        sessionInFirstOrAllColumns session =
            ((session.sessionColumn
                == ColumnId column.id
             )
                || (index == 0 && session.sessionColumn == AllColumns)
            )
    in
        sessionsInColumn
            |> List.filter
                (\s ->
                    (DateUtils.timeOfDayToTime dateWithSessions.date s.startTime)
                        == timeDelimiter
                        && (sessionInFirstOrAllColumns s)
                )
            |> List.head


appendFirstRowCell : DateWithSessions -> List Float -> Model -> Int -> Int -> Column -> Html Msg
appendFirstRowCell dateWithSessions timeDelimiters model numColumns index column =
    let
        timeDelimiter =
            timeDelimiters
                |> List.head
                |> Maybe.withDefault 0

        sessionsInColumn =
            dateWithSessions
                |> .sessions
                |> List.filter (\s -> (s.sessionColumn == ColumnId column.id) || (s.sessionColumn == AllColumns))

        sessionStarting =
            getSessionStarting sessionsInColumn dateWithSessions column timeDelimiter index

        colSpanVal =
            if sessionIsAcrossAllColumns sessionsInColumn sessionStarting index then
                numColumns
            else
                1

        endTime =
            sessionStarting
                |> Maybe.map .endTime
                |> Maybe.withDefault (TimeOfDay 0 0)

        rowSpanVal =
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
            model.tracks
                |> List.filter (\t -> t.id == trackId)
                |> List.map .name
                |> List.head
                |> Maybe.withDefault ""

        publishOrPreviewUi =
            model.showPublishUi || model.showPreviewUi
    in
        if timeDelimiter == lastTime then
            text ""
        else
            case sessionStarting of
                Just sessionStarting ->
                    td [ rowspan rowSpanVal, colspan colSpanVal ]
                        [ div []
                            [ a [ href ("/events/" ++ model.eventId ++ "/sessions/" ++ (toString sessionStarting.id)) ]
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
                            , button [ hidden publishOrPreviewUi, onClick (SelectSessionToEdit sessionStarting.id), style [ ( "margin-left", "0.2rem" ) ] ] [ text "edit" ]
                            , button [ hidden publishOrPreviewUi, onClick (DeleteSession sessionStarting.id), style [ ( "margin-left", "0.2rem" ) ] ] [ text "delete" ]
                            , br [] []
                            , b [] [ text ("Track: " ++ trackName) ]
                            ]
                        ]

                Nothing ->
                    noSessionInDateCellView timeDelimiter dateWithSessions rowSpanVal sessionsInColumn


noSessionInDateCellView : Float -> DateWithSessions -> Int -> List Session -> Html Msg
noSessionInDateCellView timeDelimiter dateWithSessions rowSpanVal sessionsInColumn =
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
        td [ class "agenda-date active", rowspan rowSpanVal ]
            [ div [ class "agenda-event" ] []
            ]


viewOtherRows : DateWithSessions -> Model -> List Float -> Int -> List (Html Msg)
viewOtherRows dateWithSessions model timeDelimiters numColumns =
    List.map (viewOtherRow dateWithSessions model timeDelimiters numColumns) timeDelimiters


viewOtherRow : DateWithSessions -> Model -> List Float -> Int -> Float -> Html Msg
viewOtherRow dateWithSessions model timeDelimiters numColumns timeDelimiter =
    let
        columns =
            model.columns

        tracks =
            model.tracks

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
                    ++ (viewCells dateWithSessions model timeDelimiters numColumns timeDelimiter)
                )


viewCells : DateWithSessions -> Model -> List Float -> Int -> Float -> List (Html Msg)
viewCells dateWithSessions model timeDelimiters numColumns timeDelimiter =
    model.columns
        |> List.indexedMap (viewCell dateWithSessions model timeDelimiters numColumns timeDelimiter)


viewCell : DateWithSessions -> Model -> List Float -> Int -> Float -> Int -> Column -> Html Msg
viewCell dateWithSessions model timeDelimiters numColumns timeDelimiter index column =
    let
        sessionsInColumn =
            dateWithSessions
                |> .sessions
                |> List.filter (\s -> (s.sessionColumn == ColumnId column.id) || (s.sessionColumn == AllColumns))

        sessionStarting =
            getSessionStarting sessionsInColumn dateWithSessions column timeDelimiter index

        colSpanVal =
            if sessionIsAcrossAllColumns sessionsInColumn sessionStarting index then
                numColumns
            else
                1

        sessionDate =
            dateWithSessions.date

        endTime =
            sessionStarting
                |> Maybe.map .endTime
                |> Maybe.withDefault (TimeOfDay 0 0)

        rowSpanVal =
            timeDelimiters
                |> List.filter (\t -> t >= timeDelimiter && t < (DateUtils.timeOfDayToTime sessionDate endTime))
                |> List.length

        lastTime =
            timeDelimiters
                |> Utils.last
                |> Maybe.withDefault -1

        trackId =
            sessionStarting
                |> Maybe.map .trackId
                |> Maybe.withDefault 0

        trackName =
            model.tracks
                |> List.filter (\t -> t.id == trackId)
                |> List.head
                |> Maybe.map .name
                |> Maybe.withDefault ""

        publishOrPreviewUi =
            model.showPublishUi || model.showPreviewUi
    in
        if timeDelimiter == lastTime then
            text ""
        else
            case sessionStarting of
                Just sessionStarting ->
                    td [ rowspan rowSpanVal, colspan colSpanVal ]
                        [ div []
                            [ a [ href ("/events/" ++ model.eventId ++ "/sessions/" ++ (toString sessionStarting.id)) ]
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
                            , button [ hidden publishOrPreviewUi, onClick (SelectSessionToEdit sessionStarting.id), style [ ( "margin-left", "0.2rem" ) ] ] [ text "edit" ]
                            , button [ hidden publishOrPreviewUi, onClick (DeleteSession sessionStarting.id), style [ ( "margin-left", "0.2rem" ) ] ] [ text "delete" ]
                            , br [] []
                            , b [] [ text (" Track: " ++ trackName) ]
                            ]
                        ]

                Nothing ->
                    noSessionInDateCellView timeDelimiter dateWithSessions rowSpanVal sessionsInColumn



-- HELPERS


displayTimeDelimiter : DateWithSessions -> List Float -> Float -> String
displayTimeDelimiter dateWithSessions timeDelimiters timeDelimiter =
    let
        nextDelimiter =
            timeDelimiters
                |> List.filter (\t -> t > timeDelimiter)
                |> List.head
                |> Maybe.withDefault 0

        sessionExistsInTimeDelimiter =
            List.any
                (\s ->
                    (DateUtils.timeOfDayToTime dateWithSessions.date s.startTime)
                        == timeDelimiter
                        || (DateUtils.timeOfDayToTime dateWithSessions.date s.endTime)
                        == nextDelimiter
                )
                dateWithSessions.sessions
    in
        if sessionExistsInTimeDelimiter then
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
