module GetWarning exposing (..)

import DateUtils


getWarning warningSuffix model =
    if warningSuffix /= "" then
        "Cannot create: " ++ warningSuffix
    else
        ""


endNotMoreThanStart newSession =
    (DateUtils.timeOfDayToTime newSession.date newSession.startTime)
        >= (DateUtils.timeOfDayToTime newSession.date newSession.endTime)


sessionsAreOverLapping newSession sessions =
    sessions
        |> List.filter (\s -> s.columnId == newSession.columnId)
        |> List.any (overLappingTime newSession)


overLappingTime newSession session =
    let
        newSessionStart =
            DateUtils.timeOfDayToTime newSession.date newSession.startTime

        newSessionEnd =
            DateUtils.timeOfDayToTime newSession.date newSession.endTime

        sessionStart =
            DateUtils.timeOfDayToTime session.date session.startTime

        sessionEnd =
            DateUtils.timeOfDayToTime session.date session.endTime
    in
        newSessionStart < sessionEnd && newSessionEnd > sessionStart
