module MainMessages exposing (..)

import Http
import MainModel
import Date exposing (Date)


-- import MainModel exposing (..)


type Msg
    = AddNewDate String
    | CreateNewColumn
    | CreateNewSession
    | CreateNewTrack
    | DeleteSession Int
    | EditSession
      -- | GetTodayAndAdd String
    | NewColumn
    | NewTrack
    | UpdatePickedDates (List String)
    | UpdateDates (List String)
    | SaveModel (Result Http.Error MainModel.ApiUpdatePost)
    | SelectSessionToEdit Int
    | ToggleManageDatesUi
    | ToggleNewColumnUi
    | ToggleNewSessionUi
    | ToggleNewTrackUi
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



-- | UpdatePickedDates (List String)
