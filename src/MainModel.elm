-- MODEL


module MainModel exposing (..)

import Date
import Time


type alias Model =
    { sessions : List Session
    , tracks : List Track
    , columns : List Column
    , dates : List Date.Date
    , showNewSessionUi : Bool
    , showNewTrackUi : Bool
    , showNewColumnUi : Bool
    , newSession : Session
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
    }


type alias Session =
    { id : Int
    , name : String
    , description : String
    , startTime : Time.Time
    , endTime : Time.Time
    , columnId : ColumnId
    , trackId : TrackId
    , location : String
    , submissionIds : List Int
    }


blankSession : Int -> Session
blankSession id =
    Session id "" "" 0 0 1 1 "" []


initialSessions : List Session
initialSessions =
    [ Session
        1
        "Conceptualising diabetes self-management as an occupation"
        "This a description of the inital session"
        1483282800000
        1483286400000
        1
        1
        "The aquariam"
        []
    , Session
        2
        "Computers n stuff sesh 2"
        "This a description of the second inital session"
        1483286400000
        1483291800000
        1
        1
        "The observatory"
        []
    , Session
        3
        "Sessioning hard 3"
        "This a description of the third inital session"
        1483293600000
        1483295400000
        1
        1
        "The games room"
        []
    , Session
        4
        "Other column sesh 4"
        "This a description of the fourth inital session"
        1483286400000
        1483295100000
        2
        1
        "The mystery room"
        []
    , Session
        5
        "first column 1 day 2 sesh 5"
        "This a description of the fifth inital session"
        1483380000000
        1483381800000
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


initialDates : List Date.Date
initialDates =
    [ Date.fromTime 1483228800000, Date.fromTime 1483315200000 ]
