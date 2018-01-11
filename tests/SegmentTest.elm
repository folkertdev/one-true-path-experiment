module SegmentTest exposing (..)

import Curve
import Expect
import Fuzz
import Geometry.Ellipse as Ellipse
import LowLevel.Command exposing (arcTo, clockwise, counterClockwise, largestArc, lineTo, moveTo, smallestArc)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import Segment exposing (Segment(..))
import SubPath
import Test exposing (..)
import Vector2 as Vec2


(=>) =
    (,)


vec2 =
    Fuzz.map2 (,) (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int)


cleanFloat v =
    round (v * 1.0e12)
        |> toFloat
        |> (\v -> v * 1.0e-12)


cleanVec2 ( x, y ) =
    ( cleanFloat x, cleanFloat y )


segment : Fuzz.Fuzzer Segment
segment =
    Fuzz.frequency
        [ 1 => Fuzz.map2 Segment.line vec2 vec2
        , 1 => Fuzz.map3 Segment.quadratic vec2 vec2 vec2
        , 1 => Fuzz.map4 Segment.cubic vec2 vec2 vec2 vec2
        , 1
            => (let
                    direction =
                        Fuzz.frequency [ 1 => Fuzz.constant clockwise, 1 => Fuzz.constant counterClockwise ]

                    arcFlag =
                        Fuzz.frequency [ 1 => Fuzz.constant largestArc, 1 => Fuzz.constant smallestArc ]

                    xAngle =
                        Fuzz.floatRange 0 (2 * pi)
                in
                Fuzz.map4
                    (\start end radii direction arcFlag xAngle ->
                        Segment.arc start { target = end, radii = radii, direction = direction, arcFlag = arcFlag, xAxisRotate = xAngle }
                    )
                    vec2
                    vec2
                    (Fuzz.map (\( x, y ) -> ( max x 1, max y 1 )) vec2)
                    direction
                    |> Fuzz.andMap arcFlag
                    |> Fuzz.andMap xAngle
               )
        ]



{-
   arcLengthParameterization =
       describe "arc length parameterization"
           [ test "f 2 with Segment.line (0,0) (4,0) is (2,0)" <|
               \_ ->
                   Segment.line ( 0, 0 ) ( 4, 0 )
                       |> flip Segment.arcLengthParameterization 2
                       |> Expect.equal (Just ( 2, 0 ))
           , test "f 2 with Segment.line (0,0) (0,4) is (0,2)" <|
               \_ ->
                   Segment.line ( 0, 0 ) ( 0, 4 )
                       |> flip Segment.arcLengthParameterization 2
                       |> Expect.equal (Just ( 0, 2 ))
           , test "f 2 with Segment.cubicSegment (0,0) (0,4) is (0,2)" <|
               \_ ->
                   Segment.cubic ( 0, 0 ) ( 0, 0 ) ( 4, 0 ) ( 4, 0 )
                       |> flip Segment.arcLengthParameterization 2
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
                           |> flip Segment.arcLengthParameterization (pi / 2)
                           |> Maybe.map cleanVec2
                           |> Expect.equal (Just ( 0, 1 ))
           ]
-}


arc =
    let
        direction =
            Fuzz.frequency [ 1 => Fuzz.constant clockwise, 1 => Fuzz.constant counterClockwise ]

        arcFlag =
            Fuzz.frequency [ 1 => Fuzz.constant largestArc, 1 => Fuzz.constant smallestArc ]

        xAngle =
            let
                factor =
                    2
            in
            Fuzz.map (\k -> toFloat k * (pi / 2)) (Fuzz.intRange 0 2)
    in
    Fuzz.map5
        (\center endAngle startAngle radii direction arcFlag xAngle ->
            let
                endpoint =
                    Ellipse.centerToEndpoint { center = center, deltaTheta = endAngle, startAngle = startAngle, radii = radii, xAxisRotate = xAngle }
            in
            Segment.arc endpoint.start
                { target = endpoint.end
                , radii = endpoint.radii
                , direction = endpoint.direction
                , arcFlag = endpoint.arcFlag
                , xAxisRotate = endpoint.xAxisRotate
                }
        )
        (Fuzz.map (\( x, y ) -> ( max x 1, max y 1 )) vec2)
        xAngle
        xAngle
        (Fuzz.map (\( x, y ) -> ( max x 1, max y 1 )) vec2)
        direction
        |> Fuzz.andMap arcFlag
        |> Fuzz.andMap xAngle


segments =
    [ "line" => Segment.line ( 0, 42 ) ( 42, 0 )
    , "quadratic" => Segment.quadratic ( 0, 42 ) ( 0, 0 ) ( 42, 0 )
    , "cubic" => Segment.cubic ( 0, 42 ) ( 0, 0 ) ( 0, 0 ) ( 42, 0 )
    , "arc" => Segment.arc ( 0, 42 ) { target = ( 42, 0 ), radii = ( 42, 42 ), xAxisRotate = 0, arcFlag = smallestArc, direction = clockwise }
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
                Segment.angle (Segment.line ( 0, 0 ) ( 100, 0 )) (Segment.line ( 0, 0 ) ( 0, 100 ))
                    |> Expect.equal (pi / 2)
        , test "angle is -pi / 2" <|
            \_ ->
                Segment.angle (Segment.line ( 0, 0 ) ( 0, 100 )) (Segment.line ( 0, 0 ) ( 100, 0 ))
                    |> Expect.equal (pi / -2)
        , test "angle is pi" <|
            \_ ->
                Segment.angle (Segment.line ( 0, 0 ) ( 100, 0 )) (Segment.line ( 100, 0 ) ( 0, 0 ))
                    |> Expect.equal pi
        , test "angle is 0" <|
            \_ ->
                Segment.angle (Segment.line ( 0, 0 ) ( 100, 0 )) (Segment.line ( 0, 0 ) ( 100, 0 ))
                    |> Expect.equal 0
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
                SubPath.subpath (moveTo ( 0, 0 )) [ lineTo [ ( 100, 0 ), ( 100, 100 ) ] ]
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
