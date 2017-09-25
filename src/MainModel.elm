-- MODEL


module MainModel exposing (..)


type alias Model =
    { tracks : List Track
    , columns : List Column
    , locations : List Location
    , showNewSessionUi : Bool
    , showNewTrackUi : Bool
    , showNewColumnUi : Bool
    , showManageDatesUi : Bool
    , showManageLocationsUi : Bool
    , published : Bool
    , showPreviewUi : Bool
    , newSession : Session
    , newSessionDate : DateWithoutTime
    , editSession : Session
    , editSessionDate : DateWithoutTime
    , newColumn : Column
    , newTrack : Track
    , newLocation : Location
    , idOfSessionBeingEdited : Maybe Int
    , eventId : String
    , submissionIdsInputs : List SubmissionIdInput
    , submissions : List Submission
    , datePickerClosed : Bool
    , pickedDates : List DateWithoutTime
    , pickedTracks : List Track
    , pickedColumns : List Column
    , pickedLocations : List Location
    , datesWithSessions : List DateWithSessions
    , host : String
    , showPublishPage : Bool
    , showBasicPage : Bool
    , invalidSubmissionIdsInput : String
    , showValidation : Bool
    , scheduleSubmissionsIndividually : Bool
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
    , showNewSessionUi = False
    , showNewTrackUi = False
    , showNewColumnUi = False
    , showManageDatesUi = False
    , showManageLocationsUi = False
    , published = False
    , showPreviewUi = False
    , newSession = blankSession 1
    , newSessionDate = DateWithoutTime 2017 1 1
    , editSession = blankSession 1
    , editSessionDate = DateWithoutTime 2017 1 1
    , newColumn = blankColumn 1
    , newTrack = blankTrack 1
    , newLocation = blankLocation 1
    , idOfSessionBeingEdited = Nothing
    , eventId = ""
    , submissionIdsInputs = [ { submissionIds = "", startTime = Nothing, endTime = Nothing, id = 1 } ]
    , submissions = [ Submission 1 ]
    , datePickerClosed = True
    , pickedDates = initialDates
    , pickedTracks = []
    , pickedColumns = []
    , pickedLocations = []
    , datesWithSessions = []
    , host = ""
    , showPublishPage = False
    , showBasicPage = False
    , invalidSubmissionIdsInput = ""
    , showValidation = False
    , scheduleSubmissionsIndividually = False
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
    , submissions : List SessionSubmission
    , chair : String
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
        (Just 1)
        []
        ""


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


type alias LocationId =
    Int


type alias ApiUpdatePost =
    { datesWithSessions : List DateWithSessions
    , tracks : List Track
    , locations : List Location
    , columns : List Column
    , published : Bool
    }


type alias ApiUpdateGet =
    { datesWithSessions : List DateWithSessions
    , tracks : List Track
    , locations : List Location
    , columns : List Column
    , submissions : List Submission
    , published : Bool
    }


type TrackFields
    = Name
    | Description
