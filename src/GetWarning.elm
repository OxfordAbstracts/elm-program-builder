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


sessionsAreOverLapping : DateWithNewSession -> List DateWithSessions -> Maybe Int -> Bool
sessionsAreOverLapping dateWithNewSession datesWithSessions idOfSessionBeingEdited =
    datesWithSessions
        |> List.map .sessions
        |> List.filter (\s -> s.columnId == dateWithNewSession.session.columnId)
        |> List.filter (\s -> s.id /= (Maybe.withDefault -1 idOfSessionBeingEdited))
        |> List.any (overLappingTime dateWithNewSession)


overLappingTime dateWithNewSession dateWithSessions =
    let
        newSessionStart =
            DateUtils.timeOfDayToTime dateWithNewSession.date dateWithNewSession.session.startTime

        newSessionEnd =
            DateUtils.timeOfDayToTime dateWithNewSession.date dateWithNewSession.session.endTime

        sessionStart =
            DateUtils.timeOfDayToTime dateWithNewSession.date dateWithNewSession.session.startTime

        sessionEnd =
            DateUtils.timeOfDayToTime dateWithNewSession.date dateWithNewSession.session.endTime
    in
        newSessionStart < sessionEnd && newSessionEnd > sessionStart
