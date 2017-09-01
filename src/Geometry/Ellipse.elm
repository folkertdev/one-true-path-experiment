module Geometry.Ellipse
    exposing
        ( CenterParameterization
        , EndpointParameterization
        , centerToEndpoint
        , endpointToCenter
        , at
        , atAngle
        , derivativeAt
        , tangentAt
        , approximateArcLength
        , mod2pi
        , tau
        , validateRadii
        , chunks
        , chord
        , splitEllipse
        , arcLengthParameterizationCircle
        , arcLengthParameterizationEllipse
        )

import LowLevel.MixedCommand exposing (ArcFlag(..), Direction(..))
import Vector2 exposing (..)
import Vector2 as Vec2 exposing (..)
import Matrix2 exposing (..)
import Geometry.Line as Line
import Geometry.Approximate as Approximate


tau : Float
tau =
    2 * pi


encodeFlags : ( ArcFlag, Direction ) -> ( Int, Int )
encodeFlags ( arcFlag, direction ) =
    case ( arcFlag, direction ) of
        ( LargestArc, Clockwise ) ->
            ( 1, 0 )

        ( SmallestArc, Clockwise ) ->
            ( 0, 0 )

        ( LargestArc, CounterClockwise ) ->
            ( 1, 1 )

        ( SmallestArc, CounterClockwise ) ->
            ( 0, 1 )


decodeFlags : ( Int, Int ) -> Maybe ( ArcFlag, Direction )
decodeFlags ( arcFlag, sweepFlag ) =
    case ( arcFlag, sweepFlag ) of
        ( 1, 0 ) ->
            Just ( LargestArc, Clockwise )

        ( 0, 0 ) ->
            Just ( SmallestArc, Clockwise )

        ( 1, 1 ) ->
            Just ( LargestArc, CounterClockwise )

        ( 0, 1 ) ->
            Just ( SmallestArc, CounterClockwise )

        _ ->
            Nothing


type alias EndpointParameterization =
    { start : Vec2 Float
    , end : Vec2 Float
    , radii : ( Float, Float )
    , xAxisRotate : Float
    , arcFlag : ArcFlag
    , direction : Direction
    }


type alias CenterParameterization =
    { center : Vec2 Float
    , radii : ( Float, Float )
    , startAngle : Float
    , deltaTheta : Float
    , xAxisRotate : Float
    }


validateRadii : EndpointParameterization -> EndpointParameterization
validateRadii ({ radii } as parameterization) =
    let
        ( x1_, y1_ ) =
            coordinatePrime parameterization

        ( rx, ry ) =
            radii

        v =
            (x1_ ^ 2 / rx ^ 2 + y1_ ^ 2 / ry ^ 2)
    in
        if v <= 1 then
            parameterization
        else
            { parameterization | radii = ( sqrt v * rx, sqrt v * ry ) }


normalize : CenterParameterization -> CenterParameterization
normalize ({ startAngle, deltaTheta, xAxisRotate } as parameterization) =
    { parameterization
        | startAngle = mod2pi (startAngle - xAxisRotate)
        , deltaTheta = mod2pi (deltaTheta - xAxisRotate)
        , xAxisRotate = 0
    }


approximateArcLength : { a | minDepth : Int, error : Float } -> EndpointParameterization -> Float
approximateArcLength { minDepth, error } ({ start, end } as parameterization) =
    if start == end then
        0
    else
        parameterization
            |> chunks 10
            |> List.map (\{ start, end } -> Vector2.distance start end)
            |> List.sum


chordLength { start, end } =
    Vector2.distance start end


outlineLength { start, end } =
    Vec2.sub end start
        |> (\( x, y ) -> abs x + abs y)


arcLengthParameterizationEllipse : EndpointParameterization -> Float -> Maybe (Vec2 Float)
arcLengthParameterizationEllipse ({ start, end } as parameterization) s =
    let
        config =
            { split = splitEllipse
            , upperBound = outlineLength
            , lowerBound = chordLength
            , percentageError = 0.01
            , baseCase = \{ start, end } -> Line.lengthParameterization start end s
            , length = approximateArcLength { minDepth = 10, error = 1.0e12 }
            }
    in
        Approximate.approximate config parameterization s


arcLengthParameterizationCircle : CenterParameterization -> Float -> Maybe (Vec2 Float)
arcLengthParameterizationCircle { startAngle, deltaTheta, radii, center } s =
    let
        ( rx, ry ) =
            radii

        ratio =
            deltaTheta / tau

        -- total circumference of the circle
        circumference =
            2 * pi * rx

        -- circumference used by the deltaAngle
        circumferenceUsed =
            ratio * circumference

        fraction =
            s / circumferenceUsed

        angleAtS =
            startAngle + fraction * deltaTheta
    in
        fromPolar ( rx, angleAtS )
            |> Vec2.add center
            |> Just


chunks : Int -> EndpointParameterization -> List EndpointParameterization
chunks itersLeft ({ start, end } as parameterization) =
    if itersLeft <= 0 then
        [ parameterization ]
    else
        let
            ( left, right ) =
                splitEllipse 0.5 parameterization

            chord =
                Vector2.distance start end

            outline =
                Vector2.distance left.start left.end + Vector2.distance right.start right.end

            average =
                (chord + outline) / 2
        in
            if (average - chord) / average > 0.001 then
                chunks (itersLeft - 1) left ++ chunks (itersLeft - 1) right
            else
                [ left, right ]


chord : EndpointParameterization -> ( ( Float, Float ), ( Float, Float ) )
chord { start, end } =
    ( start, end )


atAngle : Float -> CenterParameterization -> Vec2 Float
atAngle theta { xAxisRotate, center, radii } =
    ( cos theta, sin theta )
        |> Vector2.map2 (*) radii
        |> Matrix2.mulVector (conversionMatrix xAxisRotate)
        |> Vector2.add center


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat from to time =
    from + (to - from) * time


at : Float -> CenterParameterization -> Vec2 Float
at t ({ startAngle, deltaTheta, radii, xAxisRotate, center } as parameterization) =
    let
        angle =
            startAngle + t * deltaTheta

        ( cosr, sinr ) =
            ( cos xAxisRotate, sin xAxisRotate )

        ( rx, ry ) =
            radii

        ( cx, cy ) =
            center

        x =
            cosr * cos angle * rx - sinr * sin angle * ry + cx

        y =
            sinr * cos angle * rx + cosr * sin angle * ry + cy
    in
        ( x, y )


splitEllipse : Float -> EndpointParameterization -> ( EndpointParameterization, EndpointParameterization )
splitEllipse t parameterization =
    let
        middlePoint =
            at t (endpointToCenter parameterization)
    in
        ( { parameterization | end = middlePoint }
        , { parameterization | start = middlePoint }
        )


derivativeAt : Float -> CenterParameterization -> Float
derivativeAt t parameterization =
    let
        ( x1, y1 ) =
            Vector2.sub parameterization.center (at t parameterization)

        ( a, b ) =
            parameterization.radii
    in
        (b ^ 2 / a ^ 2) * (x1 / y1)


tangentAt : Float -> CenterParameterization -> Vec2 Float
tangentAt t parameterization =
    let
        ( x1, y1 ) =
            Vector2.sub parameterization.center (at t parameterization)

        ( a, b ) =
            parameterization.radii
    in
        Vector2.map2 (*) ( b ^ 2, a ^ 2 ) ( x1, y1 )
            |> Vector2.normalize
            |> Matrix2.mulVector (conversionMatrix (pi / 2))


mod2pi : Float -> Float
mod2pi =
    -- x - toFloat (truncate (x / (2 * pi))) * 2 * pi
    Basics.identity


mod2pi_ : Float -> Float
mod2pi_ x =
    x - toFloat (truncate (x / (2 * pi))) * 2 * pi


{-| Approximation of the circumference of the full ellipse

Using Approximation 3 from [this post](https://www.mathsisfun.com/geometry/ellipse-perimeter.html)
-}
circumference : { a | radii : ( Float, Float ) } -> Float
circumference { radii } =
    let
        ( a, b ) =
            radii

        h =
            ((a - b) ^ 2) / ((a + b) ^ 2)

        circumference =
            pi * (a + b) * (1 + (3 * h) / (10 + sqrt (4 - 3 * h)))
    in
        circumference


conversionMatrix : Float -> Mat2 Float
conversionMatrix xAxisRotate =
    ( ( cos xAxisRotate, -1 * sin xAxisRotate )
    , ( sin xAxisRotate, cos xAxisRotate )
    )


inverseConversionMatrix : Float -> Mat2 Float
inverseConversionMatrix xAxisRotate =
    ( ( cos xAxisRotate, sin xAxisRotate )
    , ( -1 * sin xAxisRotate, cos xAxisRotate )
    )


centerToEndpoint : CenterParameterization -> EndpointParameterization
centerToEndpoint { center, radii, startAngle, deltaTheta, xAxisRotate } =
    let
        conversion =
            conversionMatrix xAxisRotate

        endAngle =
            startAngle + deltaTheta

        ( rx, ry ) =
            radii

        p1 =
            ( rx * cos startAngle, ry * sin startAngle )
                |> Matrix2.mulVector conversion
                |> Vector2.add center

        p2 =
            ( rx * cos endAngle, ry * sin endAngle )
                |> Matrix2.mulVector conversion
                |> Vector2.add center

        ( arcFlag, direction ) =
            decodeFlags
                ( if abs deltaTheta > pi then
                    1
                  else
                    0
                , if deltaTheta > 0 then
                    1
                  else
                    0
                )
                |> Maybe.withDefault ( SmallestArc, CounterClockwise )
    in
        { start = p1, end = p2, radii = radii, arcFlag = arcFlag, direction = direction, xAxisRotate = xAxisRotate }


coordinatePrime : EndpointParameterization -> Vec2 Float
coordinatePrime { start, end, xAxisRotate } =
    let
        rotate =
            inverseConversionMatrix xAxisRotate

        ( x1_, y1_ ) =
            Vector2.sub start end
                |> Vector2.divideBy 2
                |> Matrix2.mulVector rotate

        ( x1, y1 ) =
            start

        ( x2, y2 ) =
            end
    in
        ( x1_, y1_ )


endpointToCenter : EndpointParameterization -> CenterParameterization
endpointToCenter ({ start, end, radii, xAxisRotate, arcFlag, direction } as parameterization) =
    let
        ( rx, ry ) =
            radii

        ( x1_, y1_ ) =
            coordinatePrime parameterization

        sign =
            {-
               if uncurry (==) (encodeFlags ( arcFlag, direction )) then

                   -1
               else
                   1
            -}
            case ( arcFlag, direction ) of
                ( LargestArc, Clockwise ) ->
                    1

                ( SmallestArc, Clockwise ) ->
                    -1

                ( LargestArc, CounterClockwise ) ->
                    -1

                ( SmallestArc, CounterClockwise ) ->
                    1

        numerator =
            (rx ^ 2 * ry ^ 2)
                - (rx ^ 2 * y1_ ^ 2)
                - (ry ^ 2 * x1_ ^ 2)

        denominator =
            (rx ^ 2 * y1_ ^ 2)
                + (ry ^ 2 * x1_ ^ 2)

        root =
            if denominator == 0 || numerator < 0 then
                0
            else
                sign * sqrt (numerator / denominator)

        center_ =
            ( ((rx * y1_) / ry) * root
            , (-1 * ((ry * x1_) / rx)) * root
            )

        center =
            center_
                |> Matrix2.mulVector (conversionMatrix xAxisRotate)
                |> Vector2.add
                    (Vector2.add start end
                        |> Vector2.divideBy 2
                    )

        p1 =
            ( x1_, y1_ )

        startAngle =
            let
                temp =
                    Vector2.sub p1 center_
                        |> flip (Vector2.map2 (/)) radii
                        |> angle ( 1, 0 )

                ( _, fs ) =
                    encodeFlags ( arcFlag, direction )
            in
                (if fs == 0 && deltaTheta > 0 then
                    temp - tau
                 else if fs == 1 && deltaTheta < 0 then
                    temp + tau
                 else
                    temp
                )
                    |> mod2pi_

        deltaTheta =
            let
                first =
                    Vector2.map2 (/) (Vector2.sub p1 center_) radii

                second =
                    Vector2.map2 (/) (Vector2.sub (Vector2.negate p1) center_) radii
            in
                case ( arcFlag, direction ) of
                    ( LargestArc, Clockwise ) ->
                        angle first second - 2 * pi

                    ( SmallestArc, Clockwise ) ->
                        angle first second

                    ( LargestArc, CounterClockwise ) ->
                        angle first second + 2 * pi

                    ( SmallestArc, CounterClockwise ) ->
                        angle first second

        result =
            { center = center
            , xAxisRotate = xAxisRotate
            , startAngle = startAngle
            , deltaTheta = deltaTheta
            , radii = radii
            }
    in
        result


{-| A signed angle between two vectors (the standard implementation is unsigned)
-}
angle : Vec2 Float -> Vec2 Float -> Float
angle u v =
    let
        ( ux, uy ) =
            u

        ( vx, vy ) =
            v

        sign =
            if ux * vy - uy * vx < 0 then
                -1
            else
                1

        argument =
            Vector2.dot u v
                / (Vector2.length u * Vector2.length v)
                |> clamp -1 1

        _ =
            if argument < -1 || argument > 1 then
                Debug.log "argument is wrong" argument
            else
                0

        q =
            acos argument
    in
        sign * abs q
