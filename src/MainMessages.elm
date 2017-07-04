module MainMessages exposing (..)

import Http
import MainModel
import Date exposing (Date)


type Msg
    = AddNewColumn
    | AddNewDate String Date
    | AddNewTrack
    | CancelAction
    | UpdateColumns
    | CreateNewSession
    | UpdateTracks
    | DeleteColumn Int
    | DeleteDate Int
    | DeleteSession Int
    | DeleteTrack MainModel.TrackId
    | EditSession
    | GetDateAndThenAddDate String
    | NewColumn
    | NewTrack
    | PublishProgrammeBuilder
    | UpdatePickedDates (List String)
    | UpdateDates (List String)
    | SaveModel (Result Http.Error MainModel.ApiUpdatePost)
    | SelectSessionToEdit Int
    | ScheduleAnotherIndividualSubmission
    | ShowValidationMessage
    | ToggleManageDatesUi
    | ToggleNewColumnUi
    | ToggleNewSessionUi
    | ToggleNewTrackUi
    | ToggleScheduleSubmissionsIndividually
    | UpdateModel (Result Http.Error MainModel.ApiUpdateGet)
    | UpdateNewColumnName String
    | UpdateNewSessionChair String
    | UpdateNewSessionColumn String
    | UpdateNewSessionDate String
    | UpdateNewSessionDescription String
    | UpdateNewSessionEndHour String
    | UpdateNewSessionEndMinute String
    | UpdateNewSessionLocation String
    | UpdateNewSessionName String
    | UpdateNewSessionStartHour String
    | UpdateNewSessionStartMinute String
    | UpdateNewSessionSubmissionIds String
    | UpdateNewSessionTrack (Maybe MainModel.TrackId)
    | UpdateNewTrackDescription String
    | UpdateNewTrackName String
    | UpdatePickedColumn Int String
    | UpdatePickedTrack Int MainModel.TrackFields String
