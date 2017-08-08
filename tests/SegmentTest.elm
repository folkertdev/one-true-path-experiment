module SegmentTest exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Segment exposing (Segment(..))
import LowLevel.Command exposing (moveTo, lineTo)
import Curve
import SubPath


angle =
    describe "angle tests"
        [ test "angle is pi / 2" <|
            \_ ->
                Segment.angle (LineSegment ( 0, 0 ) ( 100, 0 )) (LineSegment ( 0, 0 ) ( 0, 100 ))
                    |> Expect.equal (pi / 2)
        , test "angle is -pi / 2" <|
            \_ ->
                Segment.angle (LineSegment ( 0, 0 ) ( 0, 100 )) (LineSegment ( 0, 0 ) ( 100, 0 ))
                    |> Expect.equal (pi / -2)
        , test "angle is pi" <|
            \_ ->
                Segment.angle (LineSegment ( 0, 0 ) ( 100, 0 )) (LineSegment ( 100, 0 ) ( 0, 0 ))
                    |> Expect.equal (pi)
        , test "angle is 0" <|
            \_ ->
                Segment.angle (LineSegment ( 0, 0 ) ( 100, 0 )) (LineSegment ( 0, 0 ) ( 100, 0 ))
                    |> Expect.equal 0
        ]


toSegments =
    describe "conversion from and to segments"
        [ test "conversion from linear produces correctly ordered result" <|
            \_ ->
                Curve.linear [ ( 0, 0 ), ( 100, 0 ), ( 100, 100 ) ]
                    |> SubPath.toSegments
                    |> Expect.equal [ LineSegment ( 0, 0 ) ( 100, 0 ), LineSegment ( 100, 0 ) ( 100, 100 ) ]
        , test "conversion from subpath produces correctly ordered result" <|
            \_ ->
                SubPath.subpath (moveTo ( 0, 0 )) [ lineTo [ ( 100, 0 ), ( 100, 100 ) ] ]
                    |> SubPath.toSegments
                    |> Expect.equal [ LineSegment ( 0, 0 ) ( 100, 0 ), LineSegment ( 100, 0 ) ( 100, 100 ) ]
        , test "conversion from drawto to segment produces correctly ordered result" <|
            \_ ->
                lineTo [ ( 100, 0 ), ( 100, 100 ) ]
                    |> Segment.toSegment (LineSegment ( 0, 0 ) ( 0, 0 ))
                    |> Expect.equal [ LineSegment ( 0, 0 ) ( 100, 0 ), LineSegment ( 100, 0 ) ( 100, 100 ) ]
        ]
