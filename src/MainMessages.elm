module MainMessages exposing (..)

import Http
import MainModel
import Date exposing (Date)
import Window
import Ports exposing (FilePortData, ChangedFilePortData, fileSelected, fileContentRead, changedFileContentRead)


type Msg
    = AddNewColumn
    | AddNewDate String Date
    | AddNewTrack
    | AddNewLocation
    | AddNewInformation
    | AddNewChair
    | CancelAction
    | ChangeInfoToSaveTitle Int String
    | ChangeInfoToSaveDescription Int String
    | ChangeSavedInfoTitle Int String
    | ChangeSavedInfoDescription Int String
    | UpdateColumns
    | CreateNewSession
    | CreateSubmissionInput
    | DeleteSubmissionInput Int
    | UpdateTracks
    | DeleteColumn Int
    | DeleteDate Int
    | ConfirmDeleteSession Int
    | ConfirmDeleteInformation Int
    | DeleteSession Int
    | DeleteTrack MainModel.TrackId
    | DeleteLocation MainModel.LocationId
    | DeleteChair MainModel.ChairId
    | DeleteInfoToSave Int
    | DeleteSavedInfo Int
    | EditSession
    | FileSelected Int
    | FileChanged Int
    | FileRead (Maybe FilePortData)
    | ChangedFileRead (Maybe ChangedFilePortData)
    | GetDateAndThenAddDate String
    | MoveColumnUp Int
    | MoveColumnDown Int
    | MoveInfoUp Int
    | MoveInfoDown Int
    | NewColumn
    | NewTrack
    | PublishProgrammeBuilder
    | UpdatePickedDates (List String)
    | UpdateDates (List String)
    | SaveInfo
    | SaveModel (Result Http.Error MainModel.ApiUpdatePost)
    | SelectSessionToEdit Int
    | SetSessionSubmissionStartTimes Int String
    | SetSessionSubmissionEndTimes Int String
    | ShowValidationMessage
    | ToggleManageDatesUi
    | ToggleManageLocationsUi
    | ToggleManageChairsUi
    | ToggleNewColumnUi
    | ToggleManageInformationUi
    | ToggleNewSessionUi
    | ToggleNewTrackUi
    | ToggleScheduleSubmissionsIndividually
    | UpdateDisplayedColumn String
    | UpdateModel (Result Http.Error MainModel.ApiUpdateGet)
    | UpdateNewColumnName String
    | UpdateNewSessionChair (Maybe MainModel.ChairId)
    | UpdateNewSessionColumn String
    | UpdateNewSessionDate String
    | UpdateNewSessionDescription String
    | UpdateNewSessionEndHour String
    | UpdateNewSessionEndMinute String
    | UpdateNewSessionLocation (Maybe MainModel.LocationId)
    | UpdateNewSessionName String
    | UpdateNewSessionStartHour String
    | UpdateNewSessionStartMinute String
    | UpdateNewSessionSubmissionIds Int (Maybe MainModel.TimeOfDay) (Maybe MainModel.TimeOfDay) String
    | UpdateNewSessionTrack (Maybe MainModel.TrackId)
    | UpdateNewTrackDescription String
    | UpdateNewTrackName String
    | UpdatePickedColumn Int String
    | UpdatePickedTrack Int MainModel.TrackFields String
    | UpdatePickedLocation Int String
    | UpdateLocations
    | UpdatePickedChair Int String
    | UpdateChairs
    | UpdateShowMobileView Window.Size
