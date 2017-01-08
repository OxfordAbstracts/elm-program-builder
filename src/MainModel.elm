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
    , newSessionBlurred : Session
    }


initialModel : Model
initialModel =
    { sessions = initialSessions
    , tracks = [ Track 1 "track 1", Track 2 "track 2" ]
    , columns = [ Column 1 "Pediatric Sessions", Column 2 "Other Sessions" ]
    , dates = initialDates
    , showNewSessionUi = True
    , showNewTrackUi = False
    , showNewColumnUi = False
    , newSession = blankSession 1
    , newSessionBlurred = blankSession 1
    }


newSession =
    Session


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
    }


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
    , Session
        4
        "Other column sesh 4"
        "This a description of the fourth inital session"
        (DateWithoutTime 2017 1 1)
        (TimeOfDay 13 0)
        (TimeOfDay 15 30)
        2
        1
        "The mystery room"
        []
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
    ]


type alias Track =
    { id : TrackId
    , name : String
    }


type alias TrackId =
    Int


type alias Column =
    { id : ColumnId
    , name : String
    }


type alias ColumnId =
    Int
