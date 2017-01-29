module GetWarningTests exposing (..)

import Test exposing (..)
import Expect
import GetWarning
import MainModel


-- showNewTrackUi is true for initial model


dummyModel : MainModel.Model
dummyModel =
    MainModel.initialModel


all : Test
all =
    describe "GetWarning functions"
        [ test "GetWarning adds provided message to 'Cannot Create'" <|
            \() ->
                Expect.equal (GetWarning.getWarning "this is a test" dummyModel) ("Cannot create: this is a test")
        ]
