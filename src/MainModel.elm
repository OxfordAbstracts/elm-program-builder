-- MODEL


module MainModel exposing (..)


type alias Model =
    { tracks : List Track
    , columns : List Column
    , showNewSessionUi : Bool
    , showNewTrackUi : Bool
    , showNewColumnUi : Bool
    , showManageDatesUi : Bool
    , showPublishUi : Bool
    , showPreviewUi : Bool
    , newSession : Session
    , newSessionDate : DateWithoutTime
    , editSession : Session
    , editSessionDate : DateWithoutTime
    , newColumn : Column
    , newTrack : Track
    , idOfSessionBeingEdited : Maybe Int
    , eventId : String
    , submissionIdsInput : String
    , submissions : List Submission
    , datePickerClosed : Bool
    , pickedDates : List DateWithoutTime
    , pickedTracks : List Track
    , pickedColumns : List Column
    , datesWithSessions : List DateWithSessions
    }


type alias Flags =
    { eventId : String }


type alias Submission =
    { id : Int
    }


type SessionColumn
    = ColumnId Int
    | AllColumns
    | NoColumns


initialModel : Model
initialModel =
    { tracks = [ Track 1 "track 1" "track 1 description", Track 2 "track 2" "track 2 description" ]
    , columns = [ Column 1 "Pediatric Sessions", Column 2 "Other Sessions" ]
    , showNewSessionUi = False
    , showNewTrackUi = False
    , showNewColumnUi = False
    , showManageDatesUi = False
    , showPublishUi = False
    , showPreviewUi = False
    , newSession = blankSession 1
    , newSessionDate = DateWithoutTime 2017 1 1
    , editSession = blankSession 1
    , editSessionDate = DateWithoutTime 2017 1 1
    , newColumn = blankColumn 1
    , newTrack = blankTrack 1
    , idOfSessionBeingEdited = Nothing
    , eventId = ""
    , submissionIdsInput = ""
    , submissions = [ Submission 1 ]
    , datePickerClosed = True
    , pickedDates = initialDates
    , pickedTracks = []
    , pickedColumns = []
    , datesWithSessions = [ { date = DateWithoutTime 2017 1 1, sessions = initialSessions } ]
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
    , trackId : TrackId
    , location : String
    , submissionIds : List Int
    , chair : String
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
        (ColumnId 1)
        1
        ""
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


defaultDateWithoutTime : DateWithoutTime
defaultDateWithoutTime =
    DateWithoutTime 0 0 0


initialSessions : List Session
initialSessions =
    [ Session
        1
        "Conceptualising diabetes self-management as an occupation"
        "This a description of the inital session"
        (TimeOfDay 9 0)
        (TimeOfDay 9 1)
        (ColumnId 1)
        1
        "The aquariam"
        []
        "Chairman Dave"
    , Session
        2
        "Computers n stuff sesh 2"
        "This a description of the second inital session"
        (TimeOfDay 10 30)
        (TimeOfDay 11 0)
        (ColumnId 1)
        1
        "The observatory"
        []
        "Chairwoman Sue"
    , Session
        3
        "Sessioning hard 3"
        "This a description of the third inital session"
        (TimeOfDay 13 30)
        (TimeOfDay 15 0)
        (ColumnId 1)
        1
        "The games room"
        []
        ""
    , Session
        4
        "Other column sesh 4"
        "This a description of the fourth inital session"
        (TimeOfDay 13 0)
        (TimeOfDay 13 30)
        (ColumnId 1)
        1
        "The mystery room"
        []
        ""
    , Session
        5
        "first column 1 day 2 sesh 5"
        "This a description of the fifth inital session"
        (TimeOfDay 11 0)
        (TimeOfDay 11 30)
        AllColumns
        1
        "The mystery room 4"
        []
        ""
    ]


type alias Track =
    { id : TrackId
    , name : String
    , description : String
    }


type alias TrackId =
    Int


type alias ApiUpdatePost =
    { datesWithSessions :
        List DateWithSessions
    , tracks :
        List Track
    , columns :
        List Column
    }


type alias ApiUpdateGet =
    { datesWithSessions :
        List DateWithSessions
    , tracks :
        List Track
    , columns :
        List Column
    , submissions : List Submission
    }


type TrackFields
    = Name
    | Description
