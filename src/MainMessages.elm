module MainMessages exposing (..)

import Http
import MainModel
import Date exposing (Date)


type Msg
    = AddNewDate String Date
    | AddNewTrack
    | CreateNewColumn
    | CreateNewSession
    | UpdateTracks
    | DeleteDate MainModel.DateWithoutTime
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
    | ToggleManageDatesUi
    | ToggleNewColumnUi
    | ToggleNewSessionUi
    | ToggleNewTrackUi
    | TogglePreviewUi
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
    | UpdateNewSessionTrack String
    | UpdateNewTrackDescription String
    | UpdateNewTrackName String
    | UpdatePickedTrack Int MainModel.TrackFields String



-- | UpdatePickedDates (List String)
