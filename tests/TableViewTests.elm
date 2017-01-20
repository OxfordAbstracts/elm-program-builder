module TableViewTests exposing (..)

import Test exposing (..)
import Expect
import DateUtils
import Date
import TableView
import MainModel


-- showNewTrackUi is true for initial model


dummyModel =
    MainModel.initialModel



-- all : Test
-- all =
--     describe "GetWarning functions work correctly"
--         [ test "GetWarning shows correct message when there is no track name" <|
--             \() ->
--                 Expect.equal (TableView.getWarning dummyModel) ("Cannot create: Track name field is empty")
--         ]
