module SubPathTest exposing (arcLengthParameterization, down, left, n, right, slope, tests, u, up)

import Curve
import Expect
import Fuzz
import LowLevel.Command exposing (lineTo, moveTo, quadraticCurveTo)
import Segment exposing (Segment(..))
import SubPath
import Test exposing (..)
import Vector2d


flip f a b =
    f b a


down =
    Curve.linear [ ( 0, 0 ), ( 0, 100 ) ]


right =
    Curve.linear [ ( 0, 0 ), ( 100, 0 ) ]


up =
    Curve.linear [ ( 0, 0 ), ( 0, -100 ) ]


left =
    Curve.linear [ ( 0, 0 ), ( -100, 0 ) ]


slope =
    Curve.linear
        [ ( 0, 0 )
        , ( 100, 100 )
            |> Vector2d.fromComponents
            |> Vector2d.normalize
            |> Vector2d.scaleBy 100
            |> Vector2d.components
        ]


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
        [ test "unwrap << subpath does not change the order of drawtos" <|
            \_ ->
                let
                    drawtos =
                        [ lineTo [ ( 1, 2 ) ], quadraticCurveTo [ ( ( 1, 2 ), ( 3, 5 ) ) ] ]
                in
                drawtos
                    |> SubPath.with (moveTo ( 0, 0 ))
                    |> SubPath.unwrap
                    |> Maybe.map .drawtos
                    |> Expect.equal (Just drawtos)
        , test "right smooth right produces a straight line" <|
            \_ ->
                right
                    |> SubPath.continueSmooth right
                    |> SubPath.toSegments
                    |> Expect.equal [ Segment.line ( 0, 0 ) ( 100, 0 ), Segment.line ( 100, 0 ) ( 200, 0 ) ]
        , test "right smooth down produces a straight line" <|
            \_ ->
                right
                    |> SubPath.continueSmooth down
                    |> tryToRound
                    |> SubPath.toSegments
                    |> Expect.equal [ Segment.line ( 0, 0 ) ( 100, 0 ), Segment.line ( 100, 0 ) ( 200, 0 ) ]
        , test "right smooth up produces a straight line" <|
            \_ ->
                right
                    |> SubPath.continueSmooth up
                    |> tryToRound
                    |> SubPath.toSegments
                    |> Expect.equal [ Segment.line ( 0, 0 ) ( 100, 0 ), Segment.line ( 100, 0 ) ( 200, 0 ) ]
        , test "right smooth left produces a straight line" <|
            \_ ->
                right
                    |> SubPath.continueSmooth left
                    |> tryToRound
                    |> SubPath.toSegments
                    |> Expect.equal [ Segment.line ( 0, 0 ) ( 100, 0 ), Segment.line ( 100, 0 ) ( 200, 0 ) ]
        , test "right smooth slope produces a straight line" <|
            \_ ->
                right
                    |> SubPath.continueSmooth slope
                    |> tryToRound
                    |> SubPath.toSegments
                    |> Expect.equal [ Segment.line ( 0, 0 ) ( 100, 0 ), Segment.line ( 100, 0 ) ( 200, 0 ) ]
        , test "toSegments returns segments in the correct order" <|
            \_ ->
                SubPath.with (moveTo ( 0, 0 )) [ lineTo [ ( 0, 100 ), ( 100, 100 ), ( 100, 0 ) ] ]
                    |> SubPath.toSegments
                    |> Expect.equal [ Segment.line ( 0, 0 ) ( 0, 100 ), Segment.line ( 0, 100 ) ( 100, 100 ), Segment.line ( 100, 100 ) ( 100, 0 ) ]
        , test "continue produces segments in the correct order" <|
            \_ ->
                (right |> SubPath.continue down)
                    |> SubPath.toSegments
                    |> Expect.equal [ Segment.line ( 0, 0 ) ( 100, 0 ), Segment.line ( 100, 0 ) ( 100, 100 ) ]
        , test "connect produces segments in the correct order" <|
            \_ ->
                Curve.linear [ ( 0, 0 ), ( 100, 0 ) ]
                    |> SubPath.connect (Curve.linear [ ( 200, 0 ), ( 300, 0 ) ])
                    |> SubPath.toSegments
                    |> Expect.equal
                        [ Segment.line ( 0, 0 ) ( 100, 0 )
                        , Segment.line ( 100, 0 ) ( 200, 0 )
                        , Segment.line ( 200, 0 ) ( 300, 0 )
                        ]
        ]


tryToRound =
    let
        f x =
            ((x * 1.0e12) |> round |> toFloat) / 1.0e12
    in
    SubPath.mapCoordinate (\( x, y ) -> ( f x, f y ))


arcLengthParameterization =
    describe "arc length parameterization tests" <|
        let
            tolerance =
                0.0001

            straightLine =
                Curve.linear [ ( 0, 0 ), ( 20, 0 ), ( 40, 0 ), ( 100, 0 ) ]
        in
        [ fuzz (Fuzz.intRange 0 100) "point along fuzz" <|
            \t ->
                let
                    curve =
                        Curve.linear [ ( 0, 0 ), ( 20, 0 ), ( 40, 0 ), ( 42, 0 ), ( 50, 0 ), ( 55, 0 ), ( 98, 0 ), ( 100, 0 ) ]
                            |> SubPath.arcLengthParameterized tolerance
                in
                curve
                    |> flip SubPath.pointAlong (toFloat t * SubPath.arcLength curve / 100)
                    |> Maybe.map (round << Tuple.first)
                    |> Expect.equal (Just t)
        , test "1. evenly spaced" <|
            \_ ->
                Curve.linear [ ( 0, 0 ), ( 100, 0 ) ]
                    |> SubPath.arcLengthParameterized tolerance
                    |> SubPath.evenlySpacedPoints 1
                    |> Expect.equal [ ( 50, 0 ) ]
        , test "2. evenly spaced" <|
            \_ ->
                Curve.linear [ ( 0, 0 ), ( 100, 0 ) ]
                    |> SubPath.arcLengthParameterized tolerance
                    |> SubPath.evenlySpacedPoints 2
                    |> Expect.equal [ ( 0, 0 ), ( 100, 0 ) ]
        , test "3. evenly spaced" <|
            \_ ->
                Curve.linear [ ( 0, 0 ), ( 20, 0 ), ( 40, 0 ), ( 100, 0 ) ]
                    |> SubPath.arcLengthParameterized tolerance
                    |> SubPath.evenlySpacedPoints 5
                    |> Expect.equal [ ( 0, 0 ), ( 25, 0 ), ( 50, 0 ), ( 75, 0 ), ( 100, 0 ) ]
        , test "quadratic bezier at t=0 is the starting point" <|
            \_ ->
                Curve.quadraticBezier ( 0, 100 ) [ ( ( 400, 400 ), ( 800, 100 ) ) ]
                    |> SubPath.arcLengthParameterized tolerance
                    |> flip SubPath.pointAlong 0
                    |> Expect.equal (Just ( 0, 100 ))
        ]
