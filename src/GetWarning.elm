module GetWarning exposing (..)

import MainModel exposing (..)
import DateUtils


getWarning warningSuffix model =
    if warningSuffix /= "" then
        "Cannot create: " ++ warningSuffix
    else
        ""


endNotMoreThanStart newSession =
    timeIsGreaterThan newSession.startTime newSession.endTime


timeIsGreaterThan time1 time2 =
    (time2.hour < time1.hour)
        || (time2.hour == time1.hour && (time2.minute < time1.minute || time2.minute == time1.minute))


sessionsAreOverLapping : Session -> DateWithoutTime -> List DateWithSessions -> Maybe Int -> Bool
sessionsAreOverLapping newSession newDate datesWithSessions idOfSessionBeingEdited =
    case newSession.sessionColumn of
        ColumnId columnId ->
            datesWithSessions
                |> List.filter (\d -> d.date == newDate)
                |> List.concatMap .sessions
                |> List.filter (\s -> s.sessionColumn == newSession.sessionColumn)
                |> List.filter (\s -> s.id /= (Maybe.withDefault -1 idOfSessionBeingEdited))
                |> List.any (overLappingTime newSession)

        AllColumns ->
            datesWithSessions
                |> List.filter (\d -> d.date == newDate)
                |> List.concatMap .sessions
                |> List.filter (\s -> s.id /= (Maybe.withDefault -1 idOfSessionBeingEdited))
                |> List.any (overLappingTime newSession)

        NoColumns ->
            False


overLappingTime newSession session =
    let
        x =
            Debug.log "session" session

        y =
            Debug.log "newSession" session

        z =
            Debug.log "gadf"
                ((timeIsGreaterThan session.endTime newSession.startTime)
                    && (timeIsGreaterThan newSession.endTime session.endTime)
                )
    in
        (timeIsGreaterThan session.endTime newSession.startTime)
            && (timeIsGreaterThan newSession.endTime session.endTime)
