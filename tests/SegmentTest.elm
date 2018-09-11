module SegmentTest exposing (angle, arc, cleanFloat, cleanVec2, conversionFromToDrawTo, derivative, segment, segments, startAndEnd, toSegments, vec2)

import Curve
import Curve.ParameterValue as ParameterValue
import Direction2d
import EllipticalArc2d exposing (EllipticalArc2d)
import Expect
import Fuzz
import Geometry.Ellipse as Ellipse
import LowLevel.Command exposing (arcTo, clockwise, counterClockwise, largestArc, lineTo, moveTo, smallestArc)
import Point2d exposing (Point2d)
import Segment exposing (Segment(..))
import SubPath
import Test exposing (..)
import Vector2d exposing (Vector2d)


epsilon : Expect.FloatingPointTolerance
epsilon =
    Expect.Absolute 1.0e-6


expectEqualPoints ( a, b ) =
    Expect.all
        [ \( c, d ) -> Expect.within epsilon a c
        , \( c, d ) -> Expect.within epsilon b d
        ]


vec2 =
    Fuzz.map2 Tuple.pair (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int)


uncurry f ( a, b ) =
    f a b


cleanFloat v =
    round (v * 1.0e12)
        |> toFloat
        |> (\value -> value * 1.0e-12)


cleanVec2 ( x, y ) =
    ( cleanFloat x, cleanFloat y )


directionFuzzer =
    Fuzz.frequency
        [ ( 1, Fuzz.constant clockwise )
        , ( 1, Fuzz.constant counterClockwise )
        ]


arcFlagFuzzer =
    Fuzz.frequency
        [ ( 1, Fuzz.constant largestArc )
        , ( 1, Fuzz.constant smallestArc )
        ]


xAngleFuzzer =
    let
        factor =
            2
    in
    Fuzz.map (\k -> toFloat k * (pi / 2)) (Fuzz.intRange 0 2)


segment : Fuzz.Fuzzer Segment
segment =
    Fuzz.frequency
        [ ( 1, Fuzz.map2 Segment.line vec2 vec2 )
        , ( 1, Fuzz.map3 Segment.quadratic vec2 vec2 vec2 )
        , ( 1, Fuzz.map4 Segment.cubic vec2 vec2 vec2 vec2 )
        , ( 1
          , Fuzz.map4
                (\start end radii direction arcFlag xAngle ->
                    Segment.ellipticalArc start { target = end, radii = radii, direction = direction, arcFlag = arcFlag, xAxisRotate = xAngle }
                )
                vec2
                vec2
                (Fuzz.map (\( x, y ) -> ( max x 1, max y 1 )) vec2)
                directionFuzzer
                |> Fuzz.andMap arcFlagFuzzer
                |> Fuzz.andMap xAngleFuzzer
          )
        ]



{-
   arcLengthParameterization =
       describe "arc length parameterization"
           [ test "f 2 with Segment.line (0,0) (4,0) is (2,0)" <|
               \_ ->
                   Segment.line ( 0, 0 ) ( 4, 0 )
                       |> flip Segment.ellipticalArcLengthParameterization 2
                       |> Expect.equal (Just ( 2, 0 ))
           , test "f 2 with Segment.line (0,0) (0,4) is (0,2)" <|
               \_ ->
                   Segment.line ( 0, 0 ) ( 0, 4 )
                       |> flip Segment.ellipticalArcLengthParameterization 2
                       |> Expect.equal (Just ( 0, 2 ))
           , test "f 2 with Segment.cubicSegment (0,0) (0,4) is (0,2)" <|
               \_ ->
                   Segment.cubic ( 0, 0 ) ( 0, 0 ) ( 4, 0 ) ( 4, 0 )
                       |> flip Segment.ellipticalArcLengthParameterization 2
                       |> Expect.equal (Just ( 2, 0 ))
           , test "f 2 with Ellipse (1,0) (0,1) is ( cos (pi / 4), sin (pi / 4))" <|
               \_ ->
                   let
                       arc =
                           { start = ( 1, 0 ), end = ( -1, 0 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = largestArc, direction = counterClockwise }

                       lines =
                           Segment.toChordLengths 50 (Arc arc)

                       _ =
                           List.map (uncurry Vec2.distance) lines
                               |> List.scanl (+) 0
                               |> List.drop 1
                               |> List.filter (\v -> v <= pi / 2)
                               |> List.map2 (,) lines
                               |> List.reverse
                               |> List.head
                   in
                       Arc arc
                           |> flip Segment.ellipticalArcLengthParameterization (pi / 2)
                           |> Maybe.map cleanVec2
                           |> Expect.equal (Just ( 0, 1 ))
           ]
-}


arc =
    Fuzz.map5
        (\center endAngle startAngle radii direction arcFlag xAngle ->
            let
                endpoint =
                    Ellipse.centerToEndpoint { center = center, deltaTheta = endAngle, startAngle = startAngle, radii = radii, xAxisRotate = xAngle }
            in
            Segment.ellipticalArc endpoint.start
                { target = endpoint.end
                , radii = endpoint.radii
                , direction = endpoint.direction
                , arcFlag = endpoint.arcFlag
                , xAxisRotate = endpoint.xAxisRotate
                }
        )
        (Fuzz.map (\( x, y ) -> ( max x 1, max y 1 )) vec2)
        xAngleFuzzer
        xAngleFuzzer
        (Fuzz.map (\( x, y ) -> ( max x 1, max y 1 )) vec2)
        directionFuzzer
        |> Fuzz.andMap arcFlagFuzzer
        |> Fuzz.andMap xAngleFuzzer


segments =
    [ ( "line", Segment.line ( 0, 42 ) ( 42, 0 ) )
    , ( "quadratic", Segment.quadratic ( 0, 42 ) ( 0, 0 ) ( 42, 0 ) )
    , ( "cubic", Segment.cubic ( 0, 42 ) ( 0, 0 ) ( 0, 0 ) ( 42, 0 ) )
    , ( "arc", Segment.ellipticalArc ( 0, 42 ) { target = ( 42, 0 ), radii = ( 42, 42 ), xAxisRotate = 0, arcFlag = smallestArc, direction = clockwise } )
    ]


startAndEnd =
    let
        createTests name value =
            [ test (name ++ "- first point is (0, 42)") <|
                \_ ->
                    Segment.firstPoint value
                        |> expectEqualPoints ( 0, 42 )
            , test (name ++ "- final point is (42, 0)") <|
                \_ ->
                    Segment.finalPoint value
                        |> expectEqualPoints ( 42, 0 )
            ]
    in
    describe "start and end points" <|
        List.concatMap (uncurry createTests) segments


angle =
    describe "angle tests"
        [ test "angle is pi / 2" <|
            \_ ->
                Segment.angle (Segment.line ( 0, 0 ) ( 100, 0 )) (Segment.line ( 0, 0 ) ( 0, 100 ))
                    |> Expect.within epsilon (pi / 2)
        , test "angle is -pi / 2" <|
            \_ ->
                Segment.angle (Segment.line ( 0, 0 ) ( 0, 100 )) (Segment.line ( 0, 0 ) ( 100, 0 ))
                    |> Expect.within epsilon (pi / -2)
        , test "angle is pi" <|
            \_ ->
                Segment.angle (Segment.line ( 0, 0 ) ( 100, 0 )) (Segment.line ( 100, 0 ) ( 0, 0 ))
                    |> Expect.within epsilon pi
        , test "angle is 0" <|
            \_ ->
                Segment.angle (Segment.line ( 0, 0 ) ( 100, 0 )) (Segment.line ( 0, 0 ) ( 100, 0 ))
                    |> Expect.within epsilon 0
        ]


derivative =
    describe "derivative tests"
        [ test "derivative of EllipticalArc2d" <|
            \_ ->
                let
                    myArc =
                        EllipticalArc2d.with { centerPoint = Point2d.fromCoordinates ( 5, 0 ), startAngle = -pi, sweptAngle = -(pi / 2), xDirection = Direction2d.fromAngle 0, xRadius = 5, yRadius = 5 }
                in
                EllipticalArc2d.firstDerivative myArc ParameterValue.zero
                    |> Vector2d.normalize
                    |> Vector2d.components
                    |> expectEqualPoints ( 0, 1 )
        ]


toSegments =
    describe "conversion from and to segments"
        [ test "conversion from linear produces correctly ordered result" <|
            \_ ->
                Curve.linear [ ( 0, 0 ), ( 100, 0 ), ( 100, 100 ) ]
                    |> SubPath.toSegments
                    |> Expect.equal [ Segment.line ( 0, 0 ) ( 100, 0 ), Segment.line ( 100, 0 ) ( 100, 100 ) ]
        , test "conversion from subpath produces correctly ordered result" <|
            \_ ->
                SubPath.with (moveTo ( 0, 0 )) [ lineTo [ ( 100, 0 ), ( 100, 100 ) ] ]
                    |> SubPath.toSegments
                    |> Expect.equal [ Segment.line ( 0, 0 ) ( 100, 0 ), Segment.line ( 100, 0 ) ( 100, 100 ) ]
        , test "conversion from drawto to segment produces correctly ordered result" <|
            \_ ->
                lineTo [ ( 100, 0 ), ( 100, 100 ) ]
                    |> Segment.toSegment { start = ( 0, 0 ), cursor = ( 0, 0 ), previousControlPoint = Nothing }
                    |> Expect.equal [ Segment.line ( 0, 0 ) ( 100, 0 ), Segment.line ( 100, 0 ) ( 100, 100 ) ]
        ]


conversionFromToDrawTo =
    describe "conversion from and then to a drawto should be equal to identity"
        [ test "arc" <|
            \_ ->
                let
                    state =
                        { cursor = ( 0, 0 ), start = ( 0, 0 ), previousControlPoint = Nothing }

                    config =
                        { target = ( 5, 5 )
                        , radii = ( 5, 5 )
                        , xAxisRotate = 0
                        , arcFlag = smallestArc
                        , direction = clockwise
                        }
                in
                LowLevel.Command.arcTo [ config ]
                    |> Segment.toSegment state
                    |> List.map Segment.toDrawTo
                    |> Expect.equal [ LowLevel.Command.arcTo [ config ] ]
        ]
