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
                    (List.concatMap (viewDate model.sessions model.columns model.tracks) model.dates)
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


viewDate : List Session -> List Column -> List Track -> DateWithoutTime -> List (Html Msg)
viewDate sessions columns tracks date =
    let
        sessionsInDate =
            List.filter isInDate sessions

        isInDate session =
            session.date == date

        lengthOfDay =
            Time.hour * 24

        timeDelimiters =
            sessionsInDate
                |> List.concatMap
                    (\s ->
                        [ DateUtils.timeOfDayToTime s.date s.startTime
                        , DateUtils.timeOfDayToTime s.date s.endTime
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
            (viewDateCell date sessionsInDate timeDelimiters firstTime
                ++ (List.map (appendFirstRowCell sessionsInDate timeDelimiters tracks) columns)
            )
        ]
            ++ (viewOtherRows sessionsInDate columns tracks (List.drop 1 timeDelimiters))


viewDateCell : DateWithoutTime -> List Session -> List Float -> Float -> List (Html msg)
viewDateCell date sessionsInDate timeDelimiters firstTime =
    let
        timeDisplay =
            displayTimeDelimiter sessionsInDate timeDelimiters firstTime

        timeClass =
            if timeDisplay == "" then
                "active"
            else
                ""

        elmDate =
            DateUtils.dateWithoutTimeToDate date
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


appendFirstRowCell : List Session -> List Float -> List Track -> Column -> Html Msg
appendFirstRowCell sessionsInDate timeDelimiters tracks column =
    let
        timeDelimiter =
            timeDelimiters
                |> List.head
                |> Maybe.withDefault 0

        sessionStarting =
            sessionsInDate
                |> List.filter
                    (\s ->
                        (DateUtils.timeOfDayToTime s.date s.startTime)
                            == timeDelimiter
                            && s.columnId
                            == column.id
                    )
                |> List.head

        sessionDate =
            sessionStarting
                |> Maybe.map .date
                |> Maybe.withDefault (DateWithoutTime 0 0 0)

        endTime =
            sessionStarting
                |> Maybe.map .endTime
                |> Maybe.withDefault (TimeOfDay 0 0)

        rowSpan =
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

        lastTime =
            timeDelimiters
                |> Utils.last
                |> Maybe.withDefault 0

        trackId =
            sessionStarting
                |> Maybe.map .trackId
                |> Maybe.withDefault (0)

        trackName =
            tracks
                |> List.filter (\t -> t.id == trackId)
                |> List.map .name
                |> List.head
                |> Maybe.withDefault ("")
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
                                        ++ "Track:  "
                                        ++ trackName
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
                            , button [ style [ ( "margin-left", "0.2rem" ) ] ] [ text "edit" ]
                            , button [ onClick (DeleteSession sessionStarting.id), style [ ( "margin-left", "0.2rem" ) ] ] [ text "delete" ]
                            ]
                        ]

                Nothing ->
                    td [ class "agenda-date active", attribute "rowspan" rowSpan ]
                        [ div [ class "agenda-event" ] []
                        ]


viewOtherRows : List Session -> List Column -> List Track -> List Float -> List (Html Msg)
viewOtherRows sessionsInDate columns tracks timeDelimiters =
    List.map (viewOtherRow sessionsInDate columns tracks timeDelimiters) timeDelimiters


viewOtherRow : List Session -> List Column -> List Track -> List Float -> Float -> Html Msg
viewOtherRow sessionsInDate columns tracks timeDelimiters timeDelimiter =
    let
        timeDisplay =
            displayTimeDelimiter sessionsInDate timeDelimiters timeDelimiter

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
                    [ text (displayTimeDelimiter sessionsInDate timeDelimiters timeDelimiter) ]
                 ]
                    ++ (viewCells sessionsInDate columns tracks timeDelimiters timeDelimiter)
                )


viewCells : List Session -> List Column -> List Track -> List Float -> Float -> List (Html Msg)
viewCells sessionsInDate columns tracks timeDelimiters timeDelimiter =
    columns
        |> List.map (viewCell sessionsInDate tracks timeDelimiters timeDelimiter)


viewCell : List Session -> List Track -> List Float -> Float -> Column -> Html Msg
viewCell sessionsInDate tracks timeDelimiters timeDelimiter column =
    let
        sessionsInColumn =
            sessionsInDate
                |> List.filter (\s -> s.columnId == column.id)

        sessionStarting =
            sessionsInColumn
                |> List.filter
                    (\s ->
                        (DateUtils.timeOfDayToTime s.date s.startTime)
                            == timeDelimiter
                            && s.columnId
                            == column.id
                    )
                |> List.head

        sessionDate =
            sessionStarting
                |> Maybe.map .date
                |> Maybe.withDefault (DateWithoutTime 0 0 0)

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
                |> Maybe.withDefault (0)

        trackName =
            tracks
                |> List.filter (\t -> t.id == trackId)
                |> List.map .name
                |> List.head
                |> Maybe.withDefault ("")
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
                                        ++ "Track:  "
                                        ++ trackName
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
                            , button [ style [ ( "margin-left", "0.2rem" ) ] ] [ text "edit" ]
                            , button [ onClick (DeleteSession sessionStarting.id), style [ ( "margin-left", "0.2rem" ) ] ] [ text "delete" ]
                            ]
                        ]

                Nothing ->
                    if
                        List.any
                            (\s ->
                                (DateUtils.timeOfDayToTime s.date s.startTime)
                                    <= timeDelimiter
                                    && (DateUtils.timeOfDayToTime s.date s.endTime)
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


displayTimeDelimiter : List Session -> List Float -> Float -> String
displayTimeDelimiter sessionsInDate timeDelimiters timeDelimiter =
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
                    (DateUtils.timeOfDayToTime s.date s.startTime)
                        == timeDelimiter
                        || (DateUtils.timeOfDayToTime s.date s.endTime)
                        == nextDelimiter
                )
                sessionsInDate
        then
            Utils.displayTime timeDelimiter ++ " - " ++ Utils.displayTime nextDelimiter
        else
            ""
