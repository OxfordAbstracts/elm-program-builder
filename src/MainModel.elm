-- MODEL


module MainModel exposing (..)


type alias Model =
    { tracks : List Track
    , columns : List Column
    , locations : List Location
    , chairs : List Chair
    , showNewSessionUi : Bool
    , showNewTrackUi : Bool
    , showNewColumnUi : Bool
    , showManageDatesUi : Bool
    , showManageLocationsUi : Bool
    , showManageChairsUi : Bool
    , published : Bool
    , showPreviewUi : Bool
    , newSession : Session
    , newSessionDate : DateWithoutTime
    , editSession : Session
    , editSessionDate : DateWithoutTime
    , newColumn : Column
    , newTrack : Track
    , newLocation : Location
    , newChair : Chair
    , idOfSessionBeingEdited : Maybe Int
    , eventId : String
    , submissionIdsInputs : List SubmissionIdInput
    , submissions : List Submission
    , datePickerClosed : Bool
    , pickedDates : List DateWithoutTime
    , pickedTracks : List Track
    , pickedColumns : List Column
    , pickedLocations : List Location
    , pickedChairs : List Chair
    , datesWithSessions : List DateWithSessions
    , host : String
    , showPublishPage : Bool
    , showBasicPage : Bool
    , invalidSubmissionIdsInput : String
    , showValidation : Bool
    , scheduleSubmissionsIndividually : Bool
    , showMobileView : Bool
    , displayedColumn : Maybe ColumnId
    , showManageInformationUi : Bool
    , infoToSave : List InfoToSave
    , savedInfo : List SavedInfo
    , changedInfo : List InfoToSave
    , showSavingFilesSpinner : Bool
    , hasSecureProgrammeBuilder : Bool
    }


type alias SubmissionIdInput =
    { submissionIds : String
    , startTime : Maybe TimeOfDay
    , endTime : Maybe TimeOfDay
    , id : Int
    }


type alias Flags =
    { eventId : String
    , host : String
    , showPreviewUi : Bool
    , showPublishPage : Bool
    , showBasicPage : Bool
    }


type alias Submission =
    { id : Int
    , title : String
    , programmeCode : String
    }


type SessionColumn
    = ColumnId Int
    | AllColumns
    | NoColumns


initialModel : Model
initialModel =
    { tracks = []
    , columns = []
    , locations = []
    , chairs = []
    , showNewSessionUi = False
    , showNewTrackUi = False
    , showNewColumnUi = False
    , showManageDatesUi = False
    , showManageLocationsUi = False
    , showManageChairsUi = False
    , published = False
    , showPreviewUi = False
    , newSession = blankSession 1
    , newSessionDate = DateWithoutTime 2017 1 1
    , editSession = blankSession 1
    , editSessionDate = DateWithoutTime 2017 1 1
    , newColumn = blankColumn 1
    , newTrack = blankTrack 1
    , newLocation = blankLocation 1
    , newChair = blankChair 1
    , idOfSessionBeingEdited = Nothing
    , eventId = ""
    , submissionIdsInputs = [ { submissionIds = "", startTime = Nothing, endTime = Nothing, id = 1 } ]
    , submissions = [ Submission 1 "Title" "P01" ]
    , datePickerClosed = True
    , pickedDates = initialDates
    , pickedTracks = []
    , pickedColumns = []
    , pickedLocations = []
    , pickedChairs = []
    , datesWithSessions = []
    , host = ""
    , showPublishPage = False
    , showBasicPage = False
    , invalidSubmissionIdsInput = ""
    , showValidation = False
    , scheduleSubmissionsIndividually = False
    , showMobileView = False
    , displayedColumn = Nothing
    , showManageInformationUi = False
    , infoToSave = [ (InfoToSave 0 "" "" "" "") ]
    , savedInfo = []
    , changedInfo = []
    , showSavingFilesSpinner = False
    , hasSecureProgrammeBuilder = True
    }


type alias InfoToSave =
    { id : Int
    , contents : String
    , filename : String
    , infoTitle : String
    , infoDescription : String
    }


type alias SavedInfo =
    { id : Int
    , filelink : String
    , filename : String
    , infoTitle : String
    , infoDescription : String
    }


initialDates : List DateWithoutTime
initialDates =
    [ DateWithoutTime 2017 1 1, DateWithoutTime 2017 1 2 ]


type alias Session =
    { id : Int
    , name : String
    , description : String
    , startTime : TimeOfDay
    , endTime : TimeOfDay
    , sessionColumn : SessionColumn
    , trackId : Maybe TrackId
    , locationId : Maybe LocationId
    , chairId : Maybe ChairId
    , submissions : List SessionSubmission
    }


type alias SessionSubmission =
    { id : Int
    , startTime : Maybe TimeOfDay
    , endTime : Maybe TimeOfDay
    }


type alias DateWithSessions =
    { date : DateWithoutTime
    , sessions : List Session
    }


type alias Column =
    { id : ColumnId
    , name : String
    }


type alias ColumnId =
    Int


type alias DateWithoutTime =
    { year : Int
    , month : Int
    , day : Int
    }


type alias TimeOfDay =
    { hour : Int
    , minute : Int
    }


blankSession : Int -> Session
blankSession id =
    Session id
        ""
        ""
        (TimeOfDay 9 0)
        (TimeOfDay 12 0)
        AllColumns
        Nothing
        Nothing
        Nothing
        []


blankColumn : Int -> Column
blankColumn id =
    Column id
        ""


blankTrack : Int -> Track
blankTrack id =
    Track id
        ""
        ""


blankLocation : Int -> Location
blankLocation id =
    Location id
        ""


blankChair : Int -> Chair
blankChair id =
    Chair id
        ""


defaultDateWithoutTime : DateWithoutTime
defaultDateWithoutTime =
    DateWithoutTime 0 0 0


initialSessions : List Session
initialSessions =
    []


type alias Track =
    { id : TrackId
    , name : String
    , description : String
    }


type alias TrackId =
    Int


type alias Location =
    { id : LocationId
    , name : String
    }


type alias Chair =
    { id : ChairId
    , name : String
    }


type alias LocationId =
    Int


type alias ChairId =
    Int


type alias ApiUpdatePost =
    { datesWithSessions : List DateWithSessions
    , tracks : List Track
    , locations : List Location
    , chairs : List Chair
    , columns : List Column
    , published : Bool
    , infoToSave : List InfoToSave
    , savedInfo : List SavedInfo
    , changedInfo : List InfoToSave
    }


type alias ApiUpdateGet =
    { datesWithSessions : List DateWithSessions
    , tracks : List Track
    , locations : List Location
    , chairs : List Chair
    , columns : List Column
    , submissions : List Submission
    , published : Bool
    , savedInfo : List SavedInfo
    , hasSecureProgrammeBuilder : Bool
    }


type TrackFields
    = Name
    | Description
