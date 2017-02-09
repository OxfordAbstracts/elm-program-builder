module MainMessages exposing (..)

import Http
import MainModel


-- import MainModel exposing (..)


type Msg
    = NewTrack
    | NewColumn
    | ToggleNewSessionUi
    | ToggleNewTrackUi
    | ToggleNewColumnUi
    | CreateNewColumn
    | UpdateNewColumnName String
    | CreateNewSession
    | CreateNewTrack
    | UpdateModel (Result Http.Error MainModel.ApiUpdate)
    | SaveModel (Result Http.Error MainModel.ApiUpdate)
    | UpdateNewSessionName String
    | UpdateNewTrackName String
    | UpdateNewSessionDescription String
    | UpdateNewSessionColumn String
    | UpdateNewSessionChair String
    | UpdateNewSessionLocation String
    | UpdateNewSessionTrack String
    | UpdateNewSessionDate String
    | UpdateNewSessionSubmissionIds String
    | UpdateNewSessionStartHour String
    | UpdateNewSessionStartMinute String
    | UpdateNewSessionEndHour String
    | UpdateNewSessionEndMinute String
    | DeleteSession Int
    | SelectSessionToEdit Int
    | EditSession
