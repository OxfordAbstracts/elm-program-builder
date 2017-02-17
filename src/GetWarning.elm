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
        || (time2.hour == time1.hour && time2.minute < time1.minute)


sessionsAreOverLapping : Session -> DateWithoutTime -> List DateWithSessions -> Maybe Int -> Bool
sessionsAreOverLapping newSession newDate datesWithSessions idOfSessionBeingEdited =
    datesWithSessions
        |> List.filter (\d -> d.date == newDate)
        |> List.concatMap .sessions
        |> List.filter (\s -> s.columnId == newSession.columnId)
        |> List.filter (\s -> s.id /= (Maybe.withDefault -1 idOfSessionBeingEdited))
        |> List.any (overLappingTime newSession)


overLappingTime newSession session =
    -- let
    --     newSessionStart =
    --         DateUtils.timeOfDayToTime dateWithNewSession.date dateWithNewSession.session.startTime
    --
    --     newSessionEnd =
    --         DateUtils.timeOfDayToTime dateWithNewSession.date dateWithNewSession.session.endTime
    --
    --     sessionStart =
    --         DateUtils.timeOfDayToTime dateWithNewSession.date dateWithNewSession.session.startTime
    --
    --     sessionEnd =
    --         DateUtils.timeOfDayToTime dateWithNewSession.date dateWithNewSession.session.endTime
    -- in
    (timeIsGreaterThan session.endTime newSession.startTime)
        && (timeIsGreaterThan newSession.endTime session.endTime)



-- newSessionStart < sessionEnd && newSessionEnd > sessionStart
