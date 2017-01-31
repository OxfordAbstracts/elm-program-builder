module ApiTests exposing (..)

import Test exposing (..)
import Expect
import MainModel


-- showNewTrackUi is true for initial model


dummyModel : MainModel.Model
dummyModel =
    MainModel.initialModel


all : Test
all =
    describe "Api functions"
        [ test "decodeModel decodes model from a JSON to elm format" <|
            \() ->
                let
                    newModel =
                        { dummyModel | showNewTrackUi = False, showNewColumnUi = True }
                in
                    Expect.equal (NewColumnView.newColumnWarning newModel) ("Cannot create: Column name field is empty")
        ]
