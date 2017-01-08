module MainMessages exposing (..)

-- import MainModel exposing (..)


type Msg
    = NewSession
    | NewTrack
    | NewColumn
    | ToggleNewSessionUi
    | ToggleNewTrackUi
    | ToggleNewColumnUi
    | UpdateNewSessionName String
    | UpdateNewSessionDescription String
    | UpdateNewSessionColumn String
    | UpdateNewSessionTrack String
    | UpdateNewSessionDay String
