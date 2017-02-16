module GetWarning exposing (..)

import MainModel exposing (..)
import DateUtils


getWarning warningSuffix model =
    if warningSuffix /= "" then
        "Cannot create: " ++ warningSuffix
    else
        ""


endNotMoreThanStart newSession =
    (DateUtils.timeOfDayToTime newSession.date newSession.startTime)
        >= (DateUtils.timeOfDayToTime newSession.date newSession.endTime)


sessionsAreOverLapping : NewSession -> List DateWithSessions -> Maybe Int -> Bool
sessionsAreOverLapping newSession datesWithSessions idOfSessionBeingEdited =
    datesWithSessions
        |> List.map .sessions
        |> List.filter (\s -> s.columnId == newSession.session.columnId)
        |> List.filter (\s -> s.id /= (Maybe.withDefault -1 idOfSessionBeingEdited))
        |> List.any (overLappingTime newSession)


overLappingTime newSession dateWithSessions =
    let
        newSessionStart =
            DateUtils.timeOfDayToTime newSession.date newSession.session.startTime

        newSessionEnd =
            DateUtils.timeOfDayToTime newSession.date newSession.session.endTime

        sessionStart =
            DateUtils.timeOfDayToTime newSession.date newSession.session.startTime

        sessionEnd =
            DateUtils.timeOfDayToTime newSession.date newSession.session.endTime
    in
        newSessionStart < sessionEnd && newSessionEnd > sessionStart
