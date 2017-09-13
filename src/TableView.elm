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

        columnElements =
            model.columns |> List.map (\c -> col [] [])
    in
        div [ class "prog-table__wrapper" ]
            [ table [ class "prog-table" ]
                [ col [ attribute "span" "2" ] []
                , div [] columnElements
                , thead []
                    [ tr []
                        (defaultHeaders
                            ++ (model.columns |> List.map viewColumnHeader)
                        )
                    ]
                , tbody []
                    (List.concatMap (viewDate model numColumns) model.datesWithSessions)
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


getTrackId : Maybe Session -> TrackId
getTrackId sessionStarting =
    case (Maybe.map .trackId sessionStarting) of
        Just trackId ->
            trackId
                |> Maybe.withDefault 0

        Nothing ->
            0


getTrackName tracks trackId =
    tracks
        |> List.filter (\t -> t.id == trackId)
        |> List.map .name
        |> List.head
        |> Maybe.withDefault ""


defaultHeaders : List (Html msg)
defaultHeaders =
    [ th [ class "prog-table__header prog-table__header--datetime" ]
        [ text "Date" ]
    , th [ class "prog-table__header prog-table__header--datetime" ]
        [ text "Time" ]
    ]


viewColumnHeader : Column -> Html msg
viewColumnHeader column =
    th [ class "prog-table__header" ] [ text column.name ]


viewDate : Model -> Int -> DateWithSessions -> List (Html Msg)
viewDate model numColumns dateWithSessions =
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
        [ tr [ class "prog-table__row" ]
            (viewDateCell dateWithSessions timeDelimiters firstTime
                ++ (List.indexedMap
                        (appendFirstRowCell dateWithSessions timeDelimiters model numColumns)
                        model.columns
                   )
            )
        ]
            ++ (viewOtherRows dateWithSessions model (List.drop 1 timeDelimiters) numColumns)


viewDateCell : DateWithSessions -> List Float -> Float -> List (Html msg)
viewDateCell dateWithSessions timeDelimiters firstTime =
    let
        timeElements =
            displayTimeDelimiter dateWithSessions timeDelimiters firstTime
                |> List.map
                    (\t ->
                        span [ class "prog-table__time " ] [ text t ]
                    )

        -- elmDate =
        --     DateUtils.dateWithoutTimeToDate dateWithSessions.date
        date =
            dateWithSessions.date
    in
        [ td [ class "prog-cell", attribute "rowspan" (toString ((List.length timeDelimiters) - 1)) ]
            [ span [ class "prog-table__date dayofweek" ]
                [ text (DateUtils.dateWithoutTimeToDay date) ]
            , br [] []
            , span [ class "prog-table__date dayofmonth" ]
                [ text (toString (date.day) ++ " ") ]
            , span [ class "prog-table__date shortdate text-muted" ]
                [ text ((DateUtils.intToMonthString date.month) ++ " " ++ (toString date.year)) ]
            ]
        , td [ class "prog-cell" ]
            timeElements
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
                |> List.filter
                    (\s ->
                        (s.sessionColumn == ColumnId column.id)
                            || (s.sessionColumn == AllColumns)
                    )

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
            getTrackId sessionStarting

        trackName =
            getTrackName model.tracks trackId

        hideTrackName =
            if String.isEmpty trackName then
                "none"
            else
                "inline-block"

        query =
            if model.showPreviewUi then
                "?view=preview"
            else if model.showPublishPage then
                "?view=published"
            else
                ""
    in
        if timeDelimiter == lastTime then
            text ""
        else
            case sessionStarting of
                Just sessionStarting ->
                    td [ class "prog-session", rowspan rowSpanVal, colspan colSpanVal ]
                        [ div [ class "prog-session__header" ]
                            [ (if model.showBasicPage then
                                span [ class "prog-session__name" ]
                                    [ text (sessionStarting.name)
                                    ]
                               else
                                a
                                    [ class "prog-session__name"
                                    , href
                                        ("/events/"
                                            ++ model.eventId
                                            ++ "/sessions/"
                                            ++ (toString sessionStarting.id)
                                            ++ query
                                        )
                                    ]
                                    [ text (sessionStarting.name)
                                    ]
                              )
                            , div [ class "prog-session__divider" ]
                                [ button
                                    [ hidden
                                        (model.showPreviewUi
                                            || model.showPublishPage
                                            || model.showBasicPage
                                        )
                                    , class "prog-session__action"
                                    , onClick (DeleteSession sessionStarting.id)
                                    ]
                                    [ text "delete" ]
                                , button
                                    [ hidden
                                        (model.showPreviewUi
                                            || model.showPublishPage
                                            || model.showBasicPage
                                        )
                                    , class "prog-session__action"
                                    , onClick (SelectSessionToEdit sessionStarting.id)
                                    ]
                                    [ text "edit" ]
                                ]
                            ]
                        , span [ class "prog-session__data prog-session__location" ]
                            [ text (sessionStarting.location)
                            ]
                        , span [ class "prog-session__data prog-session__chair" ]
                            [ text (sessionStarting.chair) ]
                        , span [ class "prog-session__data prog-session__track", style [ ( "display", hideTrackName ) ] ]
                            [ text trackName ]
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
        td [ rowspan rowSpanVal ]
            [ div [ class "agenda-event" ] []
            ]


viewOtherRows : DateWithSessions -> Model -> List Float -> Int -> List (Html Msg)
viewOtherRows dateWithSessions model timeDelimiters numColumns =
    List.map (viewOtherRow dateWithSessions model timeDelimiters numColumns) timeDelimiters


viewOtherRow : DateWithSessions -> Model -> List Float -> Int -> Float -> Html Msg
viewOtherRow dateWithSessions model timeDelimiters numColumns timeDelimiter =
    let
        timeElements =
            -- list of start and end time
            displayTimeDelimiter dateWithSessions timeDelimiters timeDelimiter
                |> List.map
                    (\t ->
                        span [ class "prog-table__time" ] [ text t ]
                    )

        lastTime =
            timeDelimiters
                |> Utils.last
                |> Maybe.withDefault -1
    in
        if timeDelimiter == lastTime then
            text ""
        else
            tr [ class "prog-table__row" ]
                ([ td [ class "prog-cell" ]
                    timeElements
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

        hideSession =
            if rowSpanVal == 0 then
                True
            else
                False

        lastTime =
            timeDelimiters
                |> Utils.last
                |> Maybe.withDefault -1

        trackId =
            getTrackId sessionStarting

        trackName =
            getTrackName model.tracks trackId

        hideTrackName =
            if String.isEmpty trackName then
                "none"
            else
                "inline-block"

        query =
            if model.showPreviewUi then
                "?view=preview"
            else if model.showPublishPage then
                "?view=published"
            else
                ""
    in
        if timeDelimiter == lastTime then
            text ""
        else
            case sessionStarting of
                Just sessionStarting ->
                    td [ class "prog-session", rowspan rowSpanVal, colspan colSpanVal ]
                        [ div [ class "prog-session__header" ]
                            [ a
                                [ class "prog-session__name"
                                , href
                                    ("/events/"
                                        ++ model.eventId
                                        ++ "/sessions/"
                                        ++ (toString sessionStarting.id)
                                        ++ query
                                    )
                                ]
                                [ text (sessionStarting.name) ]
                            , div [ class "prog-session__divider" ]
                                [ button
                                    [ hidden
                                        (model.showPreviewUi
                                            || model.showPublishPage
                                            || model.showBasicPage
                                        )
                                    , class "prog-session__action"
                                    , onClick (DeleteSession sessionStarting.id)
                                    ]
                                    [ text "delete" ]
                                , button
                                    [ hidden
                                        (model.showPreviewUi
                                            || model.showPublishPage
                                            || model.showBasicPage
                                        )
                                    , class "prog-session__action"
                                    , onClick (SelectSessionToEdit sessionStarting.id)
                                    ]
                                    [ text "edit" ]
                                ]
                            ]
                        , span [ class "prog-session__data prog-session__location" ]
                            [ text (sessionStarting.location)
                            ]
                        , span [ class "prog-session__data prog-session__chair" ]
                            [ text (sessionStarting.chair) ]
                        , span
                            [ class "prog-session__data prog-session__track"
                            , style [ ( "display", hideTrackName ) ]
                            ]
                            [ text trackName ]
                        ]

                Nothing ->
                    noSessionInDateCellView timeDelimiter dateWithSessions rowSpanVal sessionsInColumn



-- HELPERS


displayTimeDelimiter : DateWithSessions -> List Float -> Float -> List String
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
            -- list of start and end time
            [ DateUtils.displayTime timeDelimiter, DateUtils.displayTime nextDelimiter ]
        else
            []



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
