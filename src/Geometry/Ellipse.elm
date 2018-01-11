module Geometry.Ellipse
    exposing
        ( CenterParameterization
        , EndpointParameterization
        , centerToEndpoint
        , endpointToCenter
        , mod2pi
        , validateRadii
        )

import Matrix2 exposing (..)
import Path.LowLevel as LowLevel exposing (ArcFlag(..), Direction(..))
import Vector2 as Vec2 exposing (..)


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
            x1_ ^ 2 / rx ^ 2 + y1_ ^ 2 / ry ^ 2
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


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat from to time =
    from + (to - from) * time


mod2pi : Float -> Float
mod2pi =
    -- x - toFloat (truncate (x / (2 * pi))) * 2 * pi
    Basics.identity


mod2pi_ : Float -> Float
mod2pi_ x =
    x - toFloat (truncate (x / (2 * pi))) * 2 * pi


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
                |> Vec2.add center

        p2 =
            ( rx * cos endAngle, ry * sin endAngle )
                |> Matrix2.mulVector conversion
                |> Vec2.add center

        ( arcFlag, direction ) =
            LowLevel.decodeFlags
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
            Vec2.sub start end
                |> Vec2.divideBy 2
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
            if uncurry (==) (LowLevel.encodeFlags ( arcFlag, direction )) then
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
                |> Vec2.add
                    (Vec2.add start end
                        |> Vec2.divideBy 2
                    )

        p1 =
            ( x1_, y1_ )

        startAngle =
            let
                temp =
                    Vec2.sub p1 center_
                        |> flip (Vec2.map2 (/)) radii
                        |> angle ( 1, 0 )

                ( _, fs ) =
                    LowLevel.encodeFlags ( arcFlag, direction )
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
                    Vec2.map2 (/) (Vec2.sub p1 center_) radii

                second =
                    Vec2.map2 (/) (Vec2.sub (Vec2.negate p1) center_) radii
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
signedAngle : Vec2 Float -> Vec2 Float -> Float
signedAngle u v =
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
            Vec2.dot u v
                / (Vec2.length u * Vec2.length v)
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
