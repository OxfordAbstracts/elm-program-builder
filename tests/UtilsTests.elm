module UtilsTests exposing (..)

import Test exposing (..)
import Expect
import Utils
import Date


all : Test
all =
    describe "Utils Module"
        [ test "dropDuplicates removes duplicate from a list" <|
            \() ->
                Expect.equal (Utils.dropDuplicates [ 1, 1, 1, 2, 3, 4 ]) [ 1, 2, 3, 4 ]
        , test "last function returns the final value in the list" <|
            \() ->
                Expect.equal (Utils.last [ 1, 2, 3, 4 ]) (Just 4)
        ]
