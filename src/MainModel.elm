-- MODEL


module MainModel exposing (..)


type alias Model =
    { tracks : List Track
    , columns : List Column
    , displayedColumns : List Column
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
    , columns =
        [ { id = 1, name = "Columnus Maximus" }
        , { id = 2, name = "Columnus Minimus" }
        ]
    , displayedColumns =
        [ { id = 1, name = "Columnus Maximus" }
        , { id = 2, name = "Columnus Minimus" }
        ]
    , locations =
        [ { id = 3, name = "Focus Hub" }
        , { id = 2, name = "Harry's House" }
        , { id = 1, name = "Naaz's House" }
        ]
    , chairs =
        [ { id = 2, name = "Harry's Mum" }
        , { id = 1, name = "Inês Teles-Correia" }
        ]
    , showNewSessionUi = False
    , showNewTrackUi = False
    , showNewColumnUi = False
    , showManageDatesUi = False
    , showManageLocationsUi = False
    , showManageChairsUi = False
    , published = True
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
    , submissions = [ Submission 1 ]
    , datePickerClosed = True
    , pickedDates = initialDates
    , pickedTracks = []
    , pickedColumns = []
    , pickedLocations = []
    , pickedChairs = []
    , datesWithSessions =
        [ { date =
                { day = 17
                , month = 10
                , year = 2017
                }
          , sessions =
                [ { chairId = Just 2
                  , description = ""
                  , endTime = { hour = 16, minute = 0 }
                  , id = 2
                  , locationId = Just 2
                  , name = "Resting off the bevs"
                  , sessionColumn = ColumnId 2
                  , startTime = { hour = 9, minute = 15 }
                  , submissions = []
                  , trackId = Nothing
                  }
                , { chairId = Just 1
                  , description = ""
                  , endTime = { hour = 10, minute = 0 }
                  , id = 1
                  , locationId = Just 3
                  , name = "Bevs at The Drunken Pirate"
                  , sessionColumn = ColumnId 1
                  , startTime = { hour = 9, minute = 30 }
                  , submissions = []
                  , trackId = Nothing
                  }
                , { chairId = Just 1
                  , description = ""
                  , endTime = { hour = 12, minute = 0 }
                  , id = 1
                  , locationId = Just 3
                  , name = "Tristan's house"
                  , sessionColumn = ColumnId 1
                  , startTime = { hour = 9, minute = 0 }
                  , submissions = []
                  , trackId = Nothing
                  }
                ]
          }
        , { date =
                { day = 18
                , month = 10
                , year = 2017
                }
          , sessions =
                [ { chairId = Just 2
                  , description = ""
                  , endTime = { hour = 15, minute = 0 }
                  , id = 2
                  , locationId = Just 2
                  , name = "Conor's bday"
                  , sessionColumn = ColumnId 2
                  , startTime = { hour = 11, minute = 30 }
                  , submissions = []
                  , trackId = Nothing
                  }
                , { chairId = Just 1
                  , description = ""
                  , endTime = { hour = 12, minute = 0 }
                  , id = 1
                  , locationId = Just 3
                  , name = "Rory's wax"
                  , sessionColumn = ColumnId 1
                  , startTime = { hour = 9, minute = 0 }
                  , submissions = []
                  , trackId = Nothing
                  }
                ]
          }
        ]
    , host = ""
    , showPublishPage = False
    , showBasicPage = False
    , invalidSubmissionIdsInput = ""
    , showValidation = False
    , scheduleSubmissionsIndividually = False
    , showMobileView = False
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
        (Just 1)
        (Just 1)
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
    }


type alias ApiUpdateGet =
    { datesWithSessions : List DateWithSessions
    , tracks : List Track
    , locations : List Location
    , chairs : List Chair
    , columns : List Column
    , submissions : List Submission
    , published : Bool
    }


type TrackFields
    = Name
    | Description
