module DummyTypes exposing (..)

import MainModel
import MainUpdate


dummyModel : MainModel.Model
dummyModel =
    MainModel.initialModel


dummyApiUpdate : MainModel.ApiUpdate
dummyApiUpdate =
    { sessions = dummySessions
    , tracks = dummyTracks
    , columns = dummyColumn
    , dates = dummyDates
    }


updatedModel =
    MainUpdate.updateModelWithApiUpdate dummyModel dummyApiUpdate


dummyColumn : List MainModel.Column
dummyColumn =
    [ { id = 1
      , name = "Test column"
      }
    ]


dummyDates : List MainModel.DateWithoutTime
dummyDates =
    [ { year = 2017
      , month = 1
      , day = 1
      }
    ]


dummyTracks : List MainModel.Track
dummyTracks =
    [ { id = 1
      , name = "Test track"
      , description = "Test track description"
      }
    ]


dummySessions : List MainModel.Session
dummySessions =
    [ MainModel.Session
        1
        "Conceptualising diabetes self-management as an occupation"
        "This a description of the inital session"
        (MainModel.DateWithoutTime 2017 1 1)
        (MainModel.TimeOfDay 9 0)
        (MainModel.TimeOfDay 9 1)
        1
        1
        "The aquariam"
        []
        "Chairman Dave"
    , MainModel.Session
        2
        "Computers n stuff sesh 2"
        "This a description of the second inital session"
        (MainModel.DateWithoutTime 2017 1 1)
        (MainModel.TimeOfDay 10 30)
        (MainModel.TimeOfDay 11 0)
        1
        1
        "The observatory"
        []
        "Chairwoman Sue"
    ]
