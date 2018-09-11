module Geometry.Ellipse exposing
    ( CenterParameterization
    , EndpointParameterization
    , centerToEndpoint
    , endpointToCenter
    , mod2pi
    , signedAngle
    , validateRadii
    )

-- import Matrix2 exposing (..)

import Path.LowLevel as LowLevel exposing (ArcFlag(..), Direction(..))
import Vector2d exposing (Vector2d)


tau : Float
tau =
    2 * pi


type alias EndpointParameterization =
    { start : ( Float, Float )
    , end : ( Float, Float )
    , radii : ( Float, Float )
    , xAxisRotate : Float
    , arcFlag : ArcFlag
    , direction : Direction
    }


type alias CenterParameterization =
    { center : ( Float, Float )
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


conversionMatrix : Float -> ( ( Float, Float ), ( Float, Float ) )
conversionMatrix xAxisRotate =
    ( ( cos xAxisRotate, -1 * sin xAxisRotate )
    , ( sin xAxisRotate, cos xAxisRotate )
    )


inverseConversionMatrix : Float -> ( ( Float, Float ), ( Float, Float ) )
inverseConversionMatrix xAxisRotate =
    ( ( cos xAxisRotate, sin xAxisRotate )
    , ( -1 * sin xAxisRotate, cos xAxisRotate )
    )


matrixMulVector : ( ( Float, Float ), ( Float, Float ) ) -> ( Float, Float ) -> Vector2d
matrixMulVector ( ab, cd ) vec =
    let
        vector =
            Vector2d.fromComponents vec

        row1 =
            Vector2d.fromComponents ab

        row2 =
            Vector2d.fromComponents cd
    in
    Vector2d.fromComponents
        ( Vector2d.dotProduct row1 vector, Vector2d.dotProduct row2 vector )


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
                |> matrixMulVector conversion
                |> Vector2d.sum (Vector2d.fromComponents center)
                |> Vector2d.components

        p2 =
            ( rx * cos endAngle, ry * sin endAngle )
                |> matrixMulVector conversion
                |> Vector2d.sum (Vector2d.fromComponents center)
                |> Vector2d.components

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


coordinatePrime : EndpointParameterization -> ( Float, Float )
coordinatePrime { start, end, xAxisRotate } =
    let
        rotate =
            inverseConversionMatrix xAxisRotate
    in
    Vector2d.difference (Vector2d.fromComponents start) (Vector2d.fromComponents end)
        |> Vector2d.scaleBy 0.5
        |> Vector2d.components
        |> matrixMulVector rotate
        |> Vector2d.components


endpointToCenter : EndpointParameterization -> CenterParameterization
endpointToCenter ({ start, end, radii, xAxisRotate, arcFlag, direction } as parameterization) =
    let
        ( rx, ry ) =
            radii

        ( x1_, y1_ ) =
            coordinatePrime parameterization

        sign =
            if (\( a, b ) -> (==) a b) (LowLevel.encodeFlags ( arcFlag, direction )) then
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
            Vector2d.fromComponents
                ( ((rx * y1_) / ry) * root
                , (-1 * ((ry * x1_) / rx)) * root
                )

        center =
            center_
                |> Vector2d.components
                |> matrixMulVector (conversionMatrix xAxisRotate)
                |> Vector2d.sum
                    (Vector2d.sum (Vector2d.fromComponents start) (Vector2d.fromComponents end)
                        |> Vector2d.scaleBy 0.5
                    )

        p1 =
            Vector2d.fromComponents
                ( x1_, y1_ )

        ( radiusX, radiusY ) =
            radii

        startAngle =
            let
                temp =
                    Vector2d.difference p1 center_
                        |> Vector2d.components
                        |> (\( x, y ) -> ( x / radiusX, y / radiusY ))
                        |> Vector2d.fromComponents
                        |> signedAngle (Vector2d.fromComponents ( 1, 0 ))

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
                    Vector2d.difference p1 center_
                        |> Vector2d.components
                        |> (\( x, y ) -> ( x / radiusX, y / radiusY ))
                        |> Vector2d.fromComponents

                second =
                    Vector2d.difference (Vector2d.scaleBy -1 p1) center_
                        |> Vector2d.components
                        |> (\( x, y ) -> ( x / radiusX, y / radiusY ))
                        |> Vector2d.fromComponents
            in
            signedAngle first second

        {-
           case ( arcFlag, direction ) of
               ( LargestArc, Clockwise ) ->
                   angle first second - 2 * pi

               ( SmallestArc, Clockwise ) ->
                   angle first second

               ( LargestArc, CounterClockwise ) ->
                   angle first second + 2 * pi

               ( SmallestArc, CounterClockwise ) ->
                   angle first second
        -}
        result =
            { center = Vector2d.components center
            , xAxisRotate = xAxisRotate
            , startAngle = startAngle
            , deltaTheta = deltaTheta
            , radii = radii
            }
    in
    result


signedAngle : Vector2d -> Vector2d -> Float
signedAngle u v =
    let
        sign =
            if Vector2d.crossProduct u v < 0 then
                -1

            else
                1
    in
    sign * abs (acos (Vector2d.dotProduct u v / (Vector2d.length u * Vector2d.length v)))
