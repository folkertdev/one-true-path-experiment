module SegmentTest exposing (..)

import Test exposing (..)
import Expect
import Segment exposing (Segment(..))
import LowLevel.Command exposing (moveTo, lineTo, largestArc, clockwise)
import Curve
import SubPath


(=>) =
    (,)


segments =
    [ "line" => LineSegment ( 0, 42 ) ( 42, 0 )
    , "quadratic" => Quadratic ( 0, 42 ) ( 0, 0 ) ( 42, 0 )
    , "cubic" => Cubic ( 0, 42 ) ( 0, 0 ) ( 0, 0 ) ( 42, 0 )
    , "arc" => Arc { start = ( 0, 42 ), end = ( 42, 0 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = largestArc, direction = clockwise }
    ]


startAndEnd =
    let
        createTests name value =
            [ test (name ++ "- first point is (0, 42)") <|
                \_ ->
                    Segment.firstPoint value
                        |> Expect.equal ( 0, 42 )
            , test (name ++ "- final point is (42, 0)") <|
                \_ ->
                    Segment.finalPoint value
                        |> Expect.equal ( 42, 0 )
            ]
    in
        describe "start and end points" <|
            List.concatMap (uncurry createTests) segments


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
