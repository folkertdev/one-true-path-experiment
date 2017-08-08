module Ellipse
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
        )

import LowLevel.Command exposing (ArcFlag, Direction, clockwise, counterClockwise, smallestArc, largestArc)
import Vector2 exposing (..)
import Matrix2 exposing (..)


tau : Float
tau =
    2 * pi


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
    , endAngle : Float
    , xAxisRotate : Float
    , arcFlag : ArcFlag
    , direction : Direction
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
normalize ({ startAngle, endAngle, xAxisRotate } as parameterization) =
    { parameterization
        | startAngle = mod2pi (startAngle - xAxisRotate)
        , endAngle = mod2pi (endAngle - xAxisRotate)
        , xAxisRotate = 0
    }


atAngle : Float -> CenterParameterization -> Vec2 Float
atAngle theta { xAxisRotate, center, radii } =
    ( cos theta, sin theta )
        |> Vector2.map2 (*) radii
        |> Matrix2.mulVector (conversionMatrix xAxisRotate)
        |> Vector2.add center


approximateArcLength : { a | minDepth : Int, error : Float } -> EndpointParameterization -> Float
approximateArcLength { minDepth, error } parameterization =
    let
        midPoint =
            at 0.5 (endpointToCenter parameterization)

        { start, end } =
            parameterization

        length =
            Vector2.distance start end

        length2 =
            Vector2.distance start midPoint + Vector2.distance midPoint end
    in
        if minDepth > 0 || length2 - length > error then
            approximateArcLength { minDepth = minDepth - 1, error = error } { parameterization | end = midPoint }
                + approximateArcLength { minDepth = minDepth - 1, error = error } { parameterization | start = midPoint }
        else
            length2


at : Float -> CenterParameterization -> Vec2 Float
at t ({ startAngle, endAngle } as parameterization) =
    atAngle (startAngle + t * (mod2pi <| endAngle - startAngle)) parameterization


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
mod2pi x =
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


arcSegmentLength : EndpointParameterization -> Float
arcSegmentLength =
    arcSegmentLengthHelper << normalize << endpointToCenter


arcSegmentLengthHelper : CenterParameterization -> Float
arcSegmentLengthHelper parameterization =
    let
        --  { start , end } = centerToEndpoint parameterization
        helper start end angleStart angleEnd =
            Vector2.distance start end / (2 * sin ((angleStart - angleEnd) / 2)) * (angleStart - angleEnd)

        startAngle =
            min parameterization.startAngle parameterization.endAngle
                |> mod2pi

        endAngle =
            max parameterization.startAngle parameterization.endAngle
                |> mod2pi

        recurse v =
            arcSegmentLengthHelper { parameterization | endAngle = tau / 2, startAngle = startAngle }
                + arcSegmentLengthHelper { parameterization | startAngle = tau / 2, endAngle = endAngle }
    in
        {-
           if startAngle < (tau / 2) && endAngle > (tau / 2) then
               recurse (tau * 0.25)
           else if startAngle < (tau / 4) && endAngle > (tau / 4) then
               recurse (tau * 0.5)
           else if startAngle < (tau * 0.75) && endAngle > (tau * 0.75) then
               recurse (tau * 0.75)
           else
        -}
        let
            { start, end } =
                centerToEndpoint parameterization
        in
            helper start end startAngle endAngle


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
centerToEndpoint { center, radii, startAngle, endAngle, xAxisRotate } =
    let
        conversion =
            conversionMatrix xAxisRotate

        deltaTheta =
            endAngle - startAngle

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

        arcFlag =
            if abs deltaTheta > pi then
                largestArc
            else
                smallestArc

        sweepFlag =
            if deltaTheta > 0 then
                clockwise
            else
                counterClockwise
    in
        { start = p1, end = p2, radii = radii, arcFlag = arcFlag, direction = sweepFlag, xAxisRotate = xAxisRotate }


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
            if arcFlag == largestArc && direction == clockwise || arcFlag == smallestArc && direction == counterClockwise then
                -1
            else
                1

        numerator =
            (rx ^ 2 * ry ^ 2)
                - (rx ^ 2 * y1_ ^ 2)
                - (ry ^ 2 * x1_ ^ 2)

        denominator =
            (rx ^ 2 * y1_ ^ 2)
                + (ry ^ 2 * x1_ ^ 2)

        root =
            if denominator == 0 then
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

        positive angle =
            if angle < 0 then
                positive (angle + 2 * pi)
            else
                angle

        startAngle =
            let
                temp =
                    Vector2.sub p1 center_
                        |> flip (Vector2.map2 (/)) radii
                        |> angle ( 1, 0 )
            in
                (if direction == counterClockwise && deltaTheta > 0 then
                    temp - tau
                 else if direction == clockwise && deltaTheta < 0 then
                    temp + tau
                 else
                    temp
                )
                    |> positive

        deltaTheta =
            let
                first =
                    Vector2.map2 (/) (Vector2.sub p1 center_) radii

                second =
                    Vector2.map2 (/) (Vector2.sub (Vector2.negate p1) center_) radii
            in
                angle first second
    in
        { center = center
        , xAxisRotate = xAxisRotate
        , startAngle = startAngle |> mod2pi
        , endAngle = startAngle + deltaTheta |> mod2pi
        , arcFlag = arcFlag
        , direction = direction
        , radii = radii
        }


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

        q =
            acos (Vector2.dot u v / (Vector2.length u * Vector2.length v))
    in
        sign * abs q
