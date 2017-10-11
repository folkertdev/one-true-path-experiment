module Segment
    exposing
        ( Segment(..)
        , angle
        , at
        , derivativeAtFinal
        , derivativeAtFirst
        , finalPoint
        , firstPoint
        , length
        , reverse
        , toDrawTo
        , toSegment
        )

{-| An alternative interpretation of paths that is convenient for mathematical operations.

Here, a path is viewed as a list of segments with a start and end point.

@docs Segment


# Operations

@docs at, length, angle
@docs derivativeAtFirst, derivativeAtFinal
@docs firstPoint, finalPoint, reverse


# Conversion

@docs toDrawTo, toSegment

-}

import LowLevel.Command exposing (..)
import OpenSolid.Arc2d as Arc2d exposing (Arc2d)
import OpenSolid.CubicSpline2d as CubicSpline2d exposing (CubicSpline2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Vector2 as Vec2 exposing (Float2, Vec2)
import Vector3 as Vec3 exposing (Float3, Vec3)


{-| The four types of segments.

For segments, the `xAxisRotate` field is in radians.

-}
type Segment
    = LineSegment LineSegment2d
    | Quadratic QuadraticSpline2d
    | Cubic CubicSpline2d
    | Arc Arc2d


{-| Convert a segment to a drawto instruction.
-}
toDrawTo : Segment -> DrawTo
toDrawTo segment =
    case segment of
        LineSegment segment ->
            lineTo [ Point2d.coordinates <| LineSegment2d.endPoint segment ]

        Quadratic spline ->
            let
                ( start, c1, end ) =
                    QuadraticSpline2d.controlPoints spline
            in
            quadraticCurveTo [ ( Point2d.coordinates c1, Point2d.coordinates end ) ]

        Cubic spline ->
            let
                ( start, c1, c2, end ) =
                    CubicSpline2d.controlPoints spline
            in
            cubicCurveTo [ ( Point2d.coordinates c1, Point2d.coordinates c2, Point2d.coordinates end ) ]

        Arc _ ->
            -- { end, radii, xAxisRotate, arcFlag, direction } ->
            --EllipticalArc [ { target = end, radii = radii, xAxisRotate = degrees xAxisRotate, arcFlag = arcFlag, direction = direction } ]
            Debug.crash "todo"


{-| Extract the first point from a segment
-}
firstPoint : Segment -> Vec2 Float
firstPoint segment =
    Point2d.coordinates <|
        case segment of
            LineSegment segment ->
                LineSegment2d.startPoint segment

            Quadratic spline ->
                QuadraticSpline2d.startPoint spline

            Cubic spline ->
                CubicSpline2d.startPoint spline

            Arc arc ->
                Arc2d.startPoint arc


{-| Extract the final point from a segment
-}
finalPoint : Segment -> Vec2 Float
finalPoint segment =
    Point2d.coordinates <|
        case segment of
            LineSegment segment ->
                LineSegment2d.endPoint segment

            Quadratic spline ->
                QuadraticSpline2d.endPoint spline

            Cubic spline ->
                CubicSpline2d.endPoint spline

            Arc arc ->
                Arc2d.endPoint arc


{-| Reverse a line segment
-}
reverse : Segment -> Segment
reverse segment =
    case segment of
        LineSegment segment ->
            LineSegment <| LineSegment2d.reverse segment

        Quadratic spline ->
            Quadratic <| QuadraticSpline2d.reverse spline

        Cubic spline ->
            Cubic <| CubicSpline2d.reverse spline

        Arc arc ->
            Arc <| Arc2d.reverse arc


{-| Convert a drawto into a segment

This function needs the previous segment to the starting point and (for bezier curves) the control points

-}
toSegment : Segment -> DrawTo -> List Segment
toSegment previous drawto =
    let
        start =
            finalPoint previous
                |> Point2d.fromCoordinates

        ( startX, startY ) =
            Point2d.coordinates start
    in
    case drawto of
        LineTo coordinates_ ->
            let
                coordinates =
                    List.map Point2d.fromCoordinates coordinates_
            in
            List.map2 (\f t -> LineSegment <| LineSegment2d.from f t) (start :: coordinates) coordinates

        Horizontal xs ->
            let
                coordinates =
                    List.map (\x -> Point2d.fromCoordinates ( x, startY )) xs
            in
            List.map2 (\f t -> LineSegment <| LineSegment2d.from f t) (start :: coordinates) coordinates

        Vertical ys ->
            let
                coordinates =
                    List.map (\y -> Point2d.fromCoordinates ( startX, y )) ys
            in
            List.map2 (\f t -> LineSegment <| LineSegment2d.from f t) (start :: coordinates) coordinates

        CurveTo coordinates_ ->
            let
                coordinates =
                    List.map (Vec3.map Point2d.fromCoordinates) coordinates_

                folder ( c1, c2, p ) ( segmentStart, accum ) =
                    ( p, Cubic (CubicSpline2d.fromControlPoints ( segmentStart, c1, c2, p )) :: accum )
            in
            traverse folder start coordinates

        QuadraticBezierCurveTo coordinates_ ->
            let
                coordinates =
                    List.map (Vec2.map Point2d.fromCoordinates) coordinates_

                folder ( c, p ) ( segmentStart, accum ) =
                    ( p, Quadratic (QuadraticSpline2d.fromControlPoints ( segmentStart, c, p )) :: accum )
            in
            traverse folder start coordinates

        EllipticalArc arguments ->
            {-
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
            -}
            Debug.crash "todo"

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
    Point2d.coordinates <|
        case segment of
            LineSegment segment ->
                LineSegment2d.interpolate segment t

            Quadratic spline ->
                QuadraticSpline2d.pointOn spline t

            Cubic spline ->
                CubicSpline2d.pointOn spline t

            Arc arc ->
                -- Arc2d.pointOn arc t
                Debug.crash "todo"


{-| Get the derivative at a point on the curve, only defined in the range [0, 1].
-}
derivativeAt : Float -> Segment -> Maybe ( Float, Float )
derivativeAt t segment =
    Debug.crash ""


{-| The derivative at the starting point of the segment
-}
derivativeAtFirst : Segment -> Float2
derivativeAtFirst segment =
    Vector2d.components <|
        case segment of
            LineSegment segment ->
                LineSegment2d.vector segment
                    |> Vector2d.normalize

            Quadratic spline ->
                QuadraticSpline2d.startDerivative spline

            Cubic spline ->
                CubicSpline2d.startDerivative spline

            Arc arc ->
                -- Arc2d.derivative arc 0
                Debug.crash ""


{-| The derivative at the ending point of the segment
-}
derivativeAtFinal : Segment -> Float2
derivativeAtFinal segment =
    Vector2d.components <|
        case segment of
            LineSegment segment ->
                LineSegment2d.vector segment
                    |> Vector2d.normalize

            Quadratic spline ->
                QuadraticSpline2d.endDerivative spline

            Cubic spline ->
                CubicSpline2d.endDerivative spline

            Arc arc ->
                Arc2d.derivative arc 1


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
    Debug.crash "soon"


{-| Give the (x,y) locations of the intersections between two segments
-}
intersections : Segment -> Segment -> List ( Float, Float )
intersections segment1 segment2 =
    Debug.crash ""
