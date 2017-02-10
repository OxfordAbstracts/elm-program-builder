-- MODEL


module MainModel exposing (..)


type alias Model =
    { sessions : List Session
    , tracks : List Track
    , columns : List Column
    , dates : List DateWithoutTime
    , showNewSessionUi : Bool
    , showNewTrackUi : Bool
    , showNewColumnUi : Bool
    , newSession : Session
    , editSession : Session
    , newColumn : Column
    , newTrack : Track
    , idOfSessionBeingEdited : Maybe Int
    , eventId : String
    , submissionIdsInput : String
    , submissions : List Submission
    }


type alias Flags =
    { eventId : String }


type alias Submission =
    { id : Int
    }


initialModel : Model
initialModel =
    { sessions = initialSessions
    , tracks = [ Track 1 "track 1" "track 1 description", Track 2 "track 2" "track 2 description" ]
    , columns = [ Column 1 "Pediatric Sessions", Column 2 "Other Sessions" ]
    , dates = initialDates
    , showNewSessionUi = False
    , showNewTrackUi = True
    , showNewColumnUi = False
    , newSession = blankSession 1
    , editSession = blankSession 1
    , newColumn = blankColumn 1
    , newTrack = blankTrack 1
    , idOfSessionBeingEdited = Nothing
    , eventId = ""
    , submissionIdsInput = ""
    , submissions = [ Submission 1 ]
    }


initialDates : List DateWithoutTime
initialDates =
    [ DateWithoutTime 2017 1 1, DateWithoutTime 2017 1 2 ]


type alias Session =
    { id : Int
    , name : String
    , description : String
    , date : DateWithoutTime
    , startTime : TimeOfDay
    , endTime : TimeOfDay
    , columnId : ColumnId
    , trackId : TrackId
    , location : String
    , submissionIds : List Int
    , chair : String
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
        (initialDates
            |> List.head
            |> Maybe.withDefault defaultDateWithoutTime
        )
        (TimeOfDay 9 0)
        (TimeOfDay 12 0)
        1
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
        (DateWithoutTime 2017 1 1)
        (TimeOfDay 9 0)
        (TimeOfDay 9 1)
        1
        1
        "The aquariam"
        []
        "Chairman Dave"
    , Session
        2
        "Computers n stuff sesh 2"
        "This a description of the second inital session"
        (DateWithoutTime 2017 1 1)
        (TimeOfDay 10 30)
        (TimeOfDay 11 0)
        1
        1
        "The observatory"
        []
        "Chairwoman Sue"
    , Session
        3
        "Sessioning hard 3"
        "This a description of the third inital session"
        (DateWithoutTime 2017 1 1)
        (TimeOfDay 13 30)
        (TimeOfDay 15 0)
        1
        1
        "The games room"
        []
        ""
    , Session
        4
        "Other column sesh 4"
        "This a description of the fourth inital session"
        (DateWithoutTime 2017 1 1)
        (TimeOfDay 13 0)
        (TimeOfDay 15 30)
        2
        2
        "The mystery room"
        []
        ""
    , Session
        5
        "first column 1 day 2 sesh 5"
        "This a description of the fifth inital session"
        (DateWithoutTime 2017 1 2)
        (TimeOfDay 11 0)
        (TimeOfDay 14 30)
        1
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
    { sessions :
        List Session
    , tracks :
        List Track
    , columns :
        List Column
    , dates : List DateWithoutTime
    }


type alias ApiUpdateGet =
    { sessions :
        List Session
    , tracks :
        List Track
    , columns :
        List Column
    , dates : List DateWithoutTime
    , submissions : List Submission
    }
