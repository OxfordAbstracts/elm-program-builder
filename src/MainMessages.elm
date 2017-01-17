module MainMessages exposing (..)

-- import MainModel exposing (..)


type Msg
    = NewSession
    | NewTrack
    | NewColumn
    | ToggleNewSessionUi
    | ToggleNewTrackUi
    | ToggleNewColumnUi
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
    | SelectSessionToEdit Int
