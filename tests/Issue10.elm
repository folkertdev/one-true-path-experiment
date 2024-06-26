module Issue10 exposing (suite)

import Expect
import Path
import Segment
import SubPath
import Test exposing (..)


toAndFro source =
    case Path.parse source of
        Ok [ subpath ] ->
            subpath

        _ ->
            Debug.todo ""


suite =
    describe "implicit coordinate is relative 0"
        [ test "absolute" <|
            \_ ->
                Expect.equal (SubPath.toString (toAndFro "M10,10 H 30 V 20")) "M10,10 L30,10 L30,20"
        , test "relative" <|
            \_ ->
                Expect.equal (SubPath.toString (toAndFro "M10,10 h 30 v 20")) "M10,10 L40,10 L40,30"
        ]
