module GetWarning exposing (..)

import DateUtils


getWarning model =
    let
        warningSuffix =
            getWarningSuffix model
    in
        if warningSuffix /= "" then
            "Cannot create: " ++ warningSuffix
        else
            ""


getWarningSuffix model =
    if model.showNewSessionUi && model.newSession.name == "" then
        "Session name field is empty"
    else if model.showNewSessionUi && endNotMoreThanStart model.newSession then
        "Session end time must be greater than start time"
    else if model.showNewSessionUi && sessionsAreOverLapping model.newSession model.sessions then
        "Session times overlap another session in the same column"
    else if model.showNewTrackUi && model.newTrack.name == "" then
        "Track name field is empty"
    else if model.showNewColumnUi && model.newColumn.name == "" then
        "Column name field is empty"
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
