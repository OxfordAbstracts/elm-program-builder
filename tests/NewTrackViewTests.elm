module NewTrackViewTests exposing (..)

import Test exposing (..)
import Expect
import NewTrackView
import MainModel


-- showNewTrackUi is true for initial model


dummyModel : MainModel.Model
dummyModel =
    MainModel.initialModel


all : Test
all =
    describe "newTrackView functions"
        [ test "newTrackWarning shows warning message when track name field is empty" <|
            \() ->
                Expect.equal (NewTrackView.newTrackWarning dummyModel) ("")
        ]
