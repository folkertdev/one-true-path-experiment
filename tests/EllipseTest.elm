module EllipseTest exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Ellipse exposing (..)
import Path exposing (clockwise, counterClockwise, largestArc, smallestArc)
import Random


tests =
    describe "ellipse tests"
        [ {- fuzz fuzzEndpointParameterization "centerToEndpoint << endpointToCenter = identity" <|

             \ellipse ->
                 if ellipse.start == ellipse.end then
                     Expect.pass
                 else
                     ellipse
                         |> endpointToCenter
                         |> centerToEndpoint
                         |> Expect.equal ellipse
                         ,
          -}
          test "endpoint to center - smallestArc counterClockwise" <|
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
                        |> Expect.equal ( 1, 0 )
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
