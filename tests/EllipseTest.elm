module EllipseTest exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Geometry.Ellipse as Ellipse exposing (..)
import LowLevel.Command exposing (clockwise, counterClockwise, largestArc, smallestArc)
import Random
import Vector2 as Vec2


-- attempt to round to "nice" floats for fuzz tests


cleanFloat v =
    round (v * 1.0e12)
        |> toFloat
        |> (\v -> v * 1.0e-12)


cleanVec2 ( x, y ) =
    ( cleanFloat x, cleanFloat y )


comparison expected given =
    let
        error =
            1.0e-6

        predicates =
            [ expected.radii == given.radii, expected.xAxisRotate == given.xAxisRotate, expected.arcFlag == given.arcFlag, expected.direction == given.direction ]
    in
        if not <| List.all identity predicates then
            Expect.fail
                ("Expected attributes to be the same, but \n\n"
                    ++ toString given
                    ++ "\n│\n"
                    ++ "│ "
                    ++ toString error
                    ++ "\n│\n"
                    ++ toString expected
                    ++ "\n"
                )
        else
            abs ((Vec2.distance expected.start given.start) + (Vec2.distance expected.end given.end))
                |> Expect.atMost 1.0e-5
                |> Expect.onFail
                    ("Expect the distance between vectors less than error, but \n\n"
                        ++ toString given
                        ++ "\n│\n"
                        ++ "│ Expect.distanceAtMost "
                        ++ toString error
                        ++ "\n│\n"
                        ++ toString expected
                        ++ "\n"
                    )


epsilon =
    1.0e-6


expectDistanceAtMost error expected given =
    Vec2.distance expected given
        |> Expect.atMost error
        |> Expect.onFail
            ("Expect the distance between vectors less than error, but \n\n"
                ++ toString given
                ++ "\n│\n"
                ++ "│ Expect.distanceAtMost "
                ++ toString error
                ++ "\n│\n"
                ++ toString expected
                ++ "\n"
            )


x _ =
    fuzz fuzzEndpointParameterization "centerToEndpoint << endpointToCenter = identity" <|
        \ellipse ->
            if ellipse.start == ellipse.end then
                Expect.pass
            else
                let
                    result =
                        ellipse
                            |> endpointToCenter
                            |> centerToEndpoint
                in
                    { result | start = cleanVec2 result.start, end = cleanVec2 result.end }
                        |> comparison ellipse


tests =
    describe "ellipse tests"
        [ test "splitEllipse has the expected first output" <|
            \_ ->
                let
                    arc =
                        { start = ( 1, 0 ), end = ( -1, 0 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = smallestArc, direction = counterClockwise }

                    left =
                        { start = ( 1, 0 ), end = ( 0, 1 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = smallestArc, direction = counterClockwise }
                in
                    Ellipse.splitEllipse 0.5 arc
                        |> Tuple.first
                        |> comparison left
        , test "splitEllipse has the expected second output" <|
            \_ ->
                let
                    arc =
                        { start = ( 1, 0 ), end = ( -1, 0 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = smallestArc, direction = counterClockwise }

                    right =
                        { start = ( 0, 1 ), end = ( -1, 0 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = smallestArc, direction = counterClockwise }
                in
                    Ellipse.splitEllipse 0.5 arc
                        |> Tuple.second
                        |> comparison right
          -- these tests look wrong, but are right in the SVG coordinate system, with positive y going down
        , test "correct centerpoint: largestArc & counterClockwise" <|
            \_ ->
                { start = ( 1, 0 ), end = ( 0, 1 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = largestArc, direction = counterClockwise }
                    |> endpointToCenter
                    |> .center
                    |> Expect.equal ( 1, 1 )
        , test "correct centerpoint: smallestArc & clockwise" <|
            \_ ->
                { start = ( 1, 0 ), end = ( 0, 1 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = smallestArc, direction = clockwise }
                    |> endpointToCenter
                    |> .center
                    |> Expect.equal ( 1, 1 )
        , test "correct centerpoint: largestArc & clockwise" <|
            \_ ->
                { start = ( 1, 0 ), end = ( 0, 1 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = largestArc, direction = clockwise }
                    |> endpointToCenter
                    |> .center
                    |> Expect.equal ( 0, 0 )
        , test "correct centerpoint: smallestArc & counterClockwise" <|
            \_ ->
                { start = ( 1, 0 ), end = ( 0, 1 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = smallestArc, direction = counterClockwise }
                    |> endpointToCenter
                    |> .center
                    |> Expect.equal ( 0, 0 )
        , test "at has the expected second output: smallest arc counter clockwise" <|
            \_ ->
                let
                    arc =
                        { start = ( 1, 0 ), end = ( 0, 1 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = smallestArc, direction = counterClockwise }

                    expected =
                        ( cos (pi / 4), sin (pi / 4) )
                in
                    at 0.5 (endpointToCenter arc)
                        |> expectDistanceAtMost epsilon expected
        , test "at has the expected second output: largest arc counter clockwise" <|
            \_ ->
                let
                    arc =
                        { start = ( 1, 0 ), end = ( 0, 1 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = largestArc, direction = counterClockwise }

                    expected =
                        ( 1 + cos (pi / 4), 1 + sin (pi / 4) )
                in
                    endpointToCenter arc
                        |> at 0.5
                        |> expectDistanceAtMost epsilon expected
        , test "at has the expected second output: largest arc clockwise" <|
            \_ ->
                let
                    arc =
                        { start = ( 1, 0 ), end = ( 0, 1 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = largestArc, direction = clockwise }

                    expected =
                        ( cos (pi + pi / 4), sin (pi + pi / 4) )
                in
                    endpointToCenter arc
                        |> at 0.5
                        |> expectDistanceAtMost epsilon expected
        , test "at has the expected second output: smallest arc clockwise" <|
            \_ ->
                let
                    arc =
                        { start = ( 1, 0 ), end = ( 0, 1 ), radii = ( 1, 1 ), xAxisRotate = 0, arcFlag = smallestArc, direction = clockwise }

                    expected =
                        ( 1 + cos (pi + pi / 4), 1 + sin (pi + pi / 4) )
                in
                    at 0.5 (endpointToCenter arc)
                        |> expectDistanceAtMost epsilon expected
          {-

             , test "endpoint to center - smallestArc counterClockwise" <|
                 \_ ->
                     let
                         input =
                             { start = ( 100, 100 ), end = ( 150, 150 ), radii = ( 50, 50 ), xAxisRotate = 0, arcFlag = smallestArc, direction = counterClockwise }

                         expected =
                             { center = ( 150, 100 ), xAxisRotate = 0, startAngle = pi, endAngle = 0.5 * pi, arcFlag = smallestArc, direction = counterClockwise, radii = ( 50, 50 ) }
                     in
                         input
                             |> endpointToCenter
                             |> Expect.equal expected
             , test "endpoint to center - largestArc counterClockwise" <|
                 \_ ->
                     let
                         input =
                             { start = ( 100, 100 ), end = ( 150, 150 ), radii = ( 50, 50 ), xAxisRotate = 0, arcFlag = largestArc, direction = counterClockwise }

                         expected =
                             { center = ( 100, 150 ), xAxisRotate = 0, startAngle = 1.5 * pi, endAngle = 0, arcFlag = largestArc, direction = counterClockwise, radii = ( 50, 50 ) }
                     in
                         input
                             |> endpointToCenter
                             |> Expect.equal expected
             , test "endpoint to center - largestArc clockwise" <|
                 \_ ->
                     let
                         input =
                             { start = ( 100, 100 ), end = ( 150, 150 ), radii = ( 50, 50 ), xAxisRotate = 0, arcFlag = largestArc, direction = clockwise }

                         expected =
                             { center = ( 150, 100 ), xAxisRotate = 0, startAngle = pi, endAngle = 0.5 * pi, arcFlag = largestArc, direction = clockwise, radii = ( 50, 50 ) }
                     in
                         input
                             |> endpointToCenter
                             |> Expect.equal expected
             , test "endpoint to center - smallestArc clockwise" <|
                 \_ ->
                     let
                         input =
                             { start = ( 100, 100 ), end = ( 150, 150 ), radii = ( 50, 50 ), xAxisRotate = 0, arcFlag = smallestArc, direction = clockwise }

                         expected =
                             { center = ( 100, 150 ), xAxisRotate = 0, startAngle = 1.5 * pi, endAngle = 0, arcFlag = smallestArc, direction = clockwise, radii = ( 50, 50 ) }
                     in
                         input
                             |> endpointToCenter
                             |> Expect.equal expected
             , test "center to endpoint- smallestArc counterClockwise" <|
                 \_ ->
                     let
                         expected =
                             { start = ( 100, 100 ), end = ( 150, 150 ), radii = ( 50, 50 ), arcFlag = smallestArc, direction = counterClockwise, xAxisRotate = 0 }

                         input =
                             { center = ( 150, 100 ), xAxisRotate = 0, startAngle = pi, endAngle = 0.5 * pi, arcFlag = smallestArc, direction = counterClockwise, radii = ( 50, 50 ) }
                     in
                         input
                             |> centerToEndpoint
                             |> Expect.equal expected
               {-
                  , test "arc length of a quarter 20-40 ellipse" <|
                      \_ ->
                          let
                              input =
                                  { center = ( 0, 0 ), xAxisRotate = 0, startAngle = 0, endAngle = tau / 4, arcFlag = smallestArc, direction = counterClockwise, radii = ( 20, 40 ) }
                                      |> centerToEndpoint
                                      |> Debug.log "input"
                          in
                              approximateArcLength { minDepth = 20, error = 1.0e-4 } input
                                  |> Expect.equal (193.77 / 4)
               -}
             , fuzz (Fuzz.floatRange 0 (2 * pi - 1.0e-10)) "mod2pi = id for values below 2pi" <|
                 \angle ->
                     angle
                         |> mod2pi
                         |> Expect.equal angle
             , test "derivative is sensible" <|
                 \_ ->
                     let
                         input =
                             { start = ( 100, 100 ), end = ( 200, 100 ), radii = ( 50, 50 ), arcFlag = largestArc, direction = clockwise, xAxisRotate = 0 }
                     in
                         input
                             |> validateRadii
                             |> endpointToCenter
                             |> Ellipse.derivativeAt 0.5
                             |> Expect.equal 0
             , test "derivative is infinity at 0" <|
                 \_ ->
                     let
                         input =
                             { start = ( 100, 100 ), end = ( 200, 100 ), radii = ( 50, 50 ), arcFlag = largestArc, direction = clockwise, xAxisRotate = 0 }
                     in
                         -- infinity
                         input
                             |> validateRadii
                             |> endpointToCenter
                             |> Ellipse.derivativeAt 0
                             |> Expect.equal (1 / 0)
             , test "derivative is minus infinity at 1" <|
                 \_ ->
                     let
                         input =
                             { start = ( 100, 100 ), end = ( 200, 100 ), radii = ( 50, 50 ), arcFlag = largestArc, direction = clockwise, xAxisRotate = 0 }
                     in
                         -- infinity
                         input
                             |> validateRadii
                             |> endpointToCenter
                             |> Ellipse.derivativeAt 1
                             |> Expect.equal (-1 / 0)
             , test "tangent points up at 0" <|
                 \_ ->
                     let
                         input =
                             { start = ( 100, 100 ), end = ( 200, 100 ), radii = ( 50, 50 ), arcFlag = largestArc, direction = clockwise, xAxisRotate = 0 }
                     in
                         -- infinity
                         input
                             |> validateRadii
                             |> endpointToCenter
                             |> Ellipse.tangentAt 0
                             |> cleanVec2
                             |> Expect.equal ( 0, 1 )
             , test "tangent points down at 0" <|
                 \_ ->
                     let
                         input =
                             { start = ( 100, 100 ), end = ( 200, 100 ), radii = ( 50, 50 ), arcFlag = largestArc, direction = clockwise, xAxisRotate = 0 }
                     in
                         -- infinity
                         input
                             |> validateRadii
                             |> endpointToCenter
                             |> Ellipse.tangentAt 1
                             |> cleanVec2
                             |> Expect.equal ( 0, -1 )
             , test "tangent points right at 0.5" <|
                 \_ ->
                     let
                         input =
                             { start = ( 100, 100 ), end = ( 200, 100 ), radii = ( 50, 50 ), arcFlag = largestArc, direction = clockwise, xAxisRotate = 0 }
                     in
                         -- infinity
                         input
                             |> validateRadii
                             |> endpointToCenter
                             |> Ellipse.tangentAt 0.5
                             |> cleanVec2
                             |> Expect.equal ( 1, 0 )
          -}
        ]


almostEqual a b =
    abs (a - b)
        < 1.0e-10
        |> Expect.true "expected arguments to differ by at most 1.0e-10"


fuzzPositiveInt =
    Fuzz.intRange 1 Random.maxInt


fuzzRadii =
    Fuzz.map2 (,)
        (Fuzz.map (abs << toFloat) fuzzPositiveInt)
        (Fuzz.map (abs << toFloat) fuzzPositiveInt)


fuzzEndpointParameterization =
    let
        fuzzFloat2 =
            Fuzz.map2 (,) (Fuzz.map toFloat Fuzz.int) (Fuzz.map toFloat Fuzz.int)

        fuzzArcFlag =
            Fuzz.map
                (\b ->
                    if b then
                        largestArc
                    else
                        smallestArc
                )
                Fuzz.bool

        fuzzDirection =
            Fuzz.map
                (\b ->
                    if b then
                        clockwise
                    else
                        counterClockwise
                )
                Fuzz.bool
    in
        Fuzz.constant Ellipse.EndpointParameterization
            |> andMap fuzzFloat2
            |> andMap fuzzFloat2
            |> andMap fuzzRadii
            |> andMap (Fuzz.floatRange 0 (2 * pi))
            |> andMap fuzzArcFlag
            |> andMap fuzzDirection
            |> Fuzz.map validateRadii


fuzzCenterParameterization =
    Fuzz.map endpointToCenter fuzzEndpointParameterization
