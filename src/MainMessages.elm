module MainMessages exposing (..)

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
    | UpdateNewSessionName String
    | UpdateNewSessionDescription String
    | UpdateNewSessionColumn String
    | UpdateNewSessionTrack String
    | UpdateNewSessionDate String
    | UpdateNewSessionStartHour String
    | UpdateNewSessionStartMinute String
    | UpdateNewSessionEndHour String
    | UpdateNewSessionEndMinute String
    | DeleteSession Int
