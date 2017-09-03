module Segment
    exposing
        ( Segment(..)
        , length
        , reverse
        , lengthWithOptions
        , at
        , derivativeAtFinal
        , derivativeAtFirst
        , angle
        , firstPoint
        , finalPoint
        , toDrawTo
        , toSegment
        , arcLengthParameterization
        , toChordLengths
        )

{-| An alternative interpretation of paths that is convenient for mathematical operations.

Here, a path is viewed as a list of segments with a start and end point.

@docs Segment

# Operations
@docs at, length, lengthWithOptions, angle, arcLengthParameterization, toChordLengths
@docs derivativeAtFirst, derivativeAtFinal
@docs firstPoint, finalPoint, reverse

# Conversion
@docs toDrawTo, toSegment

-}

import Vector2 as Vec2 exposing (Vec2, Float2)
import Geometry.Ellipse as Ellipse
import Geometry.CubicBezier as CubicBezier exposing (..)
import LowLevel.Command exposing (..)


{-| The four types of segments.

For segments, the `xAxisRotate` field is in radians.
-}
type Segment
    = LineSegment (Vec2 Float) (Vec2 Float)
    | Quadratic (Vec2 Float) (Vec2 Float) (Vec2 Float)
    | Cubic (Vec2 Float) (Vec2 Float) (Vec2 Float) (Vec2 Float)
    | Arc
        { start : Vec2 Float
        , end : Vec2 Float
        , radii : ( Float, Float )
        , xAxisRotate : Float
        , arcFlag : ArcFlag
        , direction : Direction
        }


{-| Convert a segment to a drawto instruction.
-}
toDrawTo : Segment -> DrawTo
toDrawTo segment =
    case segment of
        LineSegment start end ->
            lineTo [ end ]

        Quadratic start c1 end ->
            quadraticCurveTo [ ( c1, end ) ]

        Cubic start c1 c2 end ->
            cubicCurveTo [ ( c1, c2, end ) ]

        Arc { end, radii, xAxisRotate, arcFlag, direction } ->
            EllipticalArc [ { target = end, radii = radii, xAxisRotate = degrees xAxisRotate, arcFlag = arcFlag, direction = direction } ]


{-| Extract the first point from a segment
-}
firstPoint : Segment -> Vec2 Float
firstPoint segment =
    case segment of
        LineSegment p _ ->
            p

        Quadratic p _ _ ->
            p

        Cubic p _ _ _ ->
            p

        Arc { start } ->
            start


{-| Extract the final point from a segment
-}
finalPoint : Segment -> Vec2 Float
finalPoint segment =
    case segment of
        LineSegment _ p ->
            p

        Quadratic _ _ p ->
            p

        Cubic _ _ _ p ->
            p

        Arc { end } ->
            end


{-| Reverse a line segment
-}
reverse : Segment -> Segment
reverse segment =
    case segment of
        LineSegment start end ->
            LineSegment end start

        Quadratic start c1 end ->
            Quadratic end c1 start

        Cubic start c1 c2 end ->
            Cubic end c2 c1 start

        Arc params ->
            Arc (Ellipse.reverse params)


{-| Get the (x,y) coorindate arrived at when following the curve for some amount.



-}
arcLengthParameterization : Segment -> Float -> Maybe ( Float, Float )
arcLengthParameterization segment =
    let
        approximate _ =
            let
                lines =
                    toChordLengths 50 segment

                matchingSegment s =
                    List.map (uncurry Vec2.distance) lines
                        |> List.scanl (+) 0
                        |> List.map2 (,) lines
                        |> List.filter (\( _, v ) -> v <= s)
                        |> List.reverse
                        |> List.head
            in
                -- \s -> go s chords
                \s ->
                    case matchingSegment s of
                        Nothing ->
                            Nothing

                        Just ( ( start, end ), lengthSoFar ) ->
                            let
                                size =
                                    Vec2.distance start end
                            in
                                (Vec2.scale ((s - lengthSoFar) / size) (Vec2.sub end start))
                                    |> Vec2.add start
                                    |> Just
    in
        case segment of
            LineSegment start end ->
                approximate ()

            Arc ({ radii } as parameterization) ->
                if Tuple.first radii == Tuple.second radii then
                    -- for a circle we can be exact
                    Ellipse.arcLengthParameterizationCircle (Ellipse.endpointToCenter parameterization)
                else
                    Ellipse.arcLengthParameterizationEllipse (parameterization)

            _ ->
                approximate ()


{-| toChordLengths
-}
toChordLengths : Int -> Segment -> List ( Vec2 Float, Vec2 Float )
toChordLengths maxDepth segment =
    case segment of
        LineSegment start end ->
            [ ( start, end ) ]

        Quadratic start c1 end ->
            CubicBezier.fromQuadratic start c1 end
                |> CubicBezier.chunks maxDepth
                |> List.map CubicBezier.chord

        Cubic start c1 c2 end ->
            CubicBezier.fromPoints start c1 c2 end
                |> CubicBezier.chunks maxDepth
                |> List.map CubicBezier.chord

        Arc params ->
            Ellipse.chunks maxDepth params
                |> List.map Ellipse.chord


{-| Convert a drawto into a segment

This function needs the previous segment to the starting point and (for bezier curves) the control points
-}
toSegment : Segment -> DrawTo -> List Segment
toSegment previous drawto =
    let
        start =
            finalPoint previous

        ( startX, startY ) =
            start
    in
        case drawto of
            LineTo coordinates ->
                List.map2 LineSegment (start :: coordinates) coordinates

            Horizontal xs ->
                let
                    coordinates =
                        List.map (\x -> ( x, startY )) xs
                in
                    List.map2 LineSegment (start :: coordinates) coordinates

            Vertical ys ->
                let
                    coordinates =
                        List.map (\y -> ( startX, y )) ys
                in
                    List.map2 LineSegment (start :: coordinates) coordinates

            CurveTo coordinates ->
                let
                    folder ( c1, c2, p ) ( segmentStart, accum ) =
                        ( p, Cubic segmentStart c1 c2 p :: accum )
                in
                    traverse folder start coordinates

            SmoothCurveTo coordinates ->
                let
                    controlPoint =
                        case previous of
                            Cubic _ _ ( cx, cy ) _ ->
                                -- reflect the previous control point in the end point
                                Vec2.add start ( startX - cx, startY - cy )

                            _ ->
                                start

                    folder ( c2, p ) ( ( c1, segmentStart ), accum ) =
                        ( ( c2, p ), Cubic segmentStart c1 c2 p :: accum )
                in
                    traverse folder ( controlPoint, start ) coordinates

            QuadraticBezierCurveTo coordinates ->
                let
                    folder ( c, p ) ( segmentStart, accum ) =
                        ( p, Quadratic segmentStart c p :: accum )
                in
                    traverse folder start coordinates

            SmoothQuadraticBezierCurveTo coordinates ->
                let
                    controlPoint =
                        case previous of
                            Quadratic _ ( cx, cy ) _ ->
                                ( cx, cy )

                            _ ->
                                start

                    folder point ( ( previousControlPoint, segmentStart ), accum ) =
                        let
                            controlPoint =
                                -- reflect c in segmentStart
                                Vec2.sub segmentStart previousControlPoint
                                    |> Vec2.add point
                        in
                            ( ( controlPoint, point ), Quadratic segmentStart controlPoint point :: accum )
                in
                    traverse folder ( controlPoint, start ) coordinates

            EllipticalArc arguments ->
                let
                    folder args ( segmentStart, accum ) =
                        ( args.target
                        , Arc
                            { start = segmentStart
                            , end = args.target
                            , radii = args.radii
                            , xAxisRotate = radians args.xAxisRotate
                            , arcFlag = args.arcFlag
                            , direction = args.direction
                            }
                            :: accum
                        )
                in
                    traverse folder start arguments

            ClosePath ->
                []


traverse : (a -> ( b, List c ) -> ( b, List c )) -> b -> List a -> List c
traverse folder initial elements =
    List.foldl folder ( initial, [] ) elements
        |> Tuple.second
        |> List.reverse


{-| Get the location at a point on the curve, only defined in the range [0, 1].
-}
at : Float -> Segment -> ( Float, Float )
at t segment =
    case segment of
        LineSegment start end ->
            Vec2.directionFromTo start end
                |> Vec2.scale t

        Quadratic start c1 end ->
            CubicBezier.fromQuadratic start c1 end
                |> CubicBezier.at t

        Cubic start c1 c2 end ->
            CubicBezier.fromPoints start c1 c2 end
                |> CubicBezier.at t

        Arc parameterization ->
            Ellipse.at t (Ellipse.endpointToCenter parameterization)


{-| Get the derivative at a point on the curve, only defined in the range [0, 1].
-}
derivativeAt : Float -> Segment -> Maybe ( Float, Float )
derivativeAt t segment =
    Debug.crash ""


{-| The derivative at the starting point of the segment
-}
derivativeAtFirst : Segment -> Float2
derivativeAtFirst segment =
    case segment of
        LineSegment a b ->
            a
                |> Vec2.directionFromTo b

        Quadratic a control b ->
            derivativeAtFirst (LineSegment a control)

        Cubic a control1 control2 b ->
            derivativeAtFirst (LineSegment a control1)

        Arc params ->
            Ellipse.tangentAt 0 (Ellipse.endpointToCenter params)


{-| The derivative at the ending point of the segment
-}
derivativeAtFinal : Segment -> Float2
derivativeAtFinal segment =
    case segment of
        LineSegment a b ->
            a
                |> Vec2.directionFromTo b

        Quadratic a control b ->
            derivativeAtFinal (LineSegment control b)

        Cubic a control1 control2 b ->
            derivativeAtFinal (LineSegment control2 b)

        Arc params ->
            Ellipse.tangentAt 1 (Ellipse.endpointToCenter params)


{-| The angle between the end of segment1 and the start of segment2
-}
angle : Segment -> Segment -> Float
angle seg1 seg2 =
    signedAngle (Vec2.negate <| derivativeAtFinal seg1) (Vec2.negate <| derivativeAtFirst seg2)


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

        q =
            acos (Vec2.dot u v / (Vec2.length u * Vec2.length v))
    in
        sign * abs q


{-| The approximate length of a segment
-}
length : Segment -> Float
length =
    lengthWithOptions { minDepth = 10, error = 1.0e-12 }


{-| Supply the options for the approximation

* `minDepth`: Minimum recursion depth for calculating the length of arc segments. Default is `10`.
* `error`: Minimal amount of progress that an recursive step must make. Default is `1.0e-12`.
-}
lengthWithOptions : { minDepth : Int, error : Float } -> Segment -> Float
lengthWithOptions config segment =
    case segment of
        LineSegment ( x0, y0 ) ( x1, y1 ) ->
            let
                ( dx, dy ) =
                    ( x0 - x1, y0 - y1 )
            in
                sqrt (dx * dx + dy * dy)

        Quadratic start c1 end ->
            CubicBezier.fromQuadratic start c1 end
                |> arcLength 1.0

        Cubic start c1 c2 end ->
            CubicBezier.fromPoints start c1 c2 end
                |> arcLength 1.0

        Arc args ->
            Ellipse.approximateArcLength args


{-| Give the (x,y) locations of the intersections between two segments
-}
intersections : Segment -> Segment -> List ( Float, Float )
intersections segment1 segment2 =
    Debug.crash ""
