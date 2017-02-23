module DummyTypes exposing (..)

import MainModel
import MainUpdate


dummyModel : MainModel.Model
dummyModel =
    MainModel.initialModel


dummyApiUpdateGet : MainModel.ApiUpdateGet
dummyApiUpdateGet =
    { datesWithSessions = dummyDatesWithSessions
    , tracks = dummyTracks
    , columns = dummyColumn
    , submissions = []
    }


updatedModel : MainModel.Model
updatedModel =
    MainUpdate.updateModelWithApiUpdateGet dummyModel dummyApiUpdateGet


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


dummyDatesWithSessions : List MainModel.DateWithSessions
dummyDatesWithSessions =
    [ { date = MainModel.DateWithoutTime 2017 1 1, sessions = dummySessions } ]


dummySessions : List MainModel.Session
dummySessions =
    [ MainModel.Session
        1
        "Conceptualising diabetes self-management as an occupation"
        "This a description of the inital session"
        (MainModel.TimeOfDay 9 0)
        (MainModel.TimeOfDay 9 1)
        (MainModel.ColumnId 1)
        1
        "The aquariam"
        []
        "Chairman Dave"
    , MainModel.Session
        2
        "Computers n stuff sesh 2"
        "This a description of the second inital session"
        (MainModel.TimeOfDay 10 30)
        (MainModel.TimeOfDay 11 0)
        (MainModel.ColumnId 1)
        1
        "The observatory"
        []
        "Chairwoman Sue"
    ]
