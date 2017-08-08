module Composition exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Segment exposing (Segment(..))
import Curve
import SubPath
import Vector2 as Vec2


down =
    Curve.linear [ ( 0, 0 ), ( 0, 100 ) ]


right =
    Curve.linear [ ( 0, 0 ), ( 100, 0 ) ]


up =
    Curve.linear [ ( 0, 0 ), ( 0, -100 ) ]


left =
    Curve.linear [ ( 0, 0 ), ( -100, 0 ) ]


slope =
    Curve.linear [ ( 0, 0 ), ( 100, 100 ) |> Vec2.normalize |> Vec2.scale 100 ]


u =
    down
        |> SubPath.continue right
        |> SubPath.continue up


n =
    up
        |> SubPath.continue right
        |> SubPath.continue down


tests =
    describe "composition tests"
        [ test "right smooth right produces a straight line" <|
            \_ ->
                right
                    |> SubPath.continueSmooth right
                    |> SubPath.toSegments
                    |> Expect.equal [ LineSegment ( 0, 0 ) ( 100, 0 ), LineSegment ( 100, 0 ) ( 200, 0 ) ]
        , test "right smooth down produces a straight line" <|
            \_ ->
                right
                    |> SubPath.continueSmooth down
                    |> SubPath.toSegments
                    |> Expect.equal [ LineSegment ( 0, 0 ) ( 100, 0 ), LineSegment ( 100, 0 ) ( 200, 0 ) ]
        , test "right smooth up produces a straight line" <|
            \_ ->
                right
                    |> SubPath.continueSmooth up
                    |> SubPath.toSegments
                    |> Expect.equal [ LineSegment ( 0, 0 ) ( 100, 0 ), LineSegment ( 100, 0 ) ( 200, 0 ) ]
        , test "right smooth left produces a straight line" <|
            \_ ->
                right
                    |> SubPath.continueSmooth left
                    |> SubPath.toSegments
                    |> Expect.equal [ LineSegment ( 0, 0 ) ( 100, 0 ), LineSegment ( 100, 0 ) ( 200, 0 ) ]
        , test "right smooth slope produces a straight line" <|
            \_ ->
                right
                    |> SubPath.continueSmooth slope
                    |> SubPath.toSegments
                    |> Expect.equal [ LineSegment ( 0, 0 ) ( 100, 0 ), LineSegment ( 100, 0 ) ( 200, 0 ) ]
        , test "toSegments returns segments in the correct order" <|
            \_ ->
                u
                    |> SubPath.toSegments
                    |> Expect.equal [ LineSegment ( 0, 0 ) ( 0, 100 ), LineSegment ( 0, 100 ) ( 100, 100 ), LineSegment ( 100, 100 ) ( 100, 0 ) ]
        , test "continue produces segments in the correct order" <|
            \_ ->
                (right |> SubPath.continue down)
                    |> SubPath.toSegments
                    |> Expect.equal [ LineSegment ( 0, 0 ) ( 100, 0 ), LineSegment ( 100, 0 ) ( 100, 100 ) ]
        , test "connect produces segments in the correct order" <|
            \_ ->
                Curve.linear [ ( 0, 0 ), ( 100, 0 ) ]
                    |> SubPath.connect (Curve.linear [ ( 200, 0 ), ( 300, 0 ) ])
                    |> SubPath.toSegments
                    |> Expect.equal
                        [ LineSegment ( 0, 0 ) ( 100, 0 )
                        , LineSegment ( 100, 0 ) ( 200, 0 )
                        , LineSegment ( 200, 0 ) ( 300, 0 )
                        ]
        ]
