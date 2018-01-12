module Segment
    exposing
        ( Segment(..)
        , angle
        , arc
        , at
        , cubic
        , derivativeAt
        , derivativeAtFinal
        , derivativeAtFirst
        , finalPoint
        , firstPoint
          -- , length
        , line
        , quadratic
        , reverse
        , toCursorState
        , toDrawTo
        , toSegment
        )

{-| An alternative view on paths that is convenient for mathematical operations.

When we look at a path as a list of elemental `Segment`s, it becomes easier to reason about it.
The segment data type has four segment types:

  - `line` straigt line segment
  - `quadratic` a quadratic bezier curve segment
  - `cubic` a cubic bezier curve segment
  - `arc` an elliptical arc segment

All four of these are mathematically well-defined, and there is a wide range of
operations that we can perform on them:

  - `length` the arc length of a segment
  - `angle` between two segments
  - `derivative` or curvature
  - `reverse` reverse a segment - this can be used to [let the browser fill your svg correctly][reverse]

These operations are backed by the great [OpenSolid] package, and in turn back many of the operations
in `SubPath`.

[reverse]: https://pomax.github.io/svg-path-reverse/
[OpenSolid]: http://package.elm-lang.org/packages/opensolid/geometry/latest

@docs Segment
@docs line, quadratic, cubic, arc


# Operations

@docs at, angle
@docs derivativeAt, derivativeAtFirst, derivativeAtFinal
@docs firstPoint, finalPoint, reverse


# Conversion

@docs toDrawTo, toSegment, toCursorState

-}

import Geometry.Ellipse exposing (CenterParameterization, EndpointParameterization)
import LowLevel.Command exposing (..)
import OpenSolid.CubicSpline2d as CubicSpline2d exposing (CubicSpline2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.EllipticalArc2d as EllipticalArc2d exposing (EllipticalArc2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d exposing (QuadraticSpline2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Path.LowLevel exposing (ArcFlag(..), Direction(..), EllipticalArcArgument)
import Vector2 as Vec2 exposing (Float2, Vec2)
import Vector3 as Vec3 exposing (Float3, Vec3)


{-| The four types of segments.
-}
type Segment
    = LineSegment LineSegment2d
    | Quadratic QuadraticSpline2d
    | Cubic CubicSpline2d
    | Arc EllipticalArc2d


{-| Make a line segment
-}
line : ( Float, Float ) -> ( Float, Float ) -> Segment
line from to =
    LineSegment2d.from (Point2d.fromCoordinates from) (Point2d.fromCoordinates to)
        |> LineSegment


{-| Make a quadratic bezier segment
-}
quadratic : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Segment
quadratic start c1 end =
    QuadraticSpline2d.fromControlPoints
        ( Point2d.fromCoordinates start
        , Point2d.fromCoordinates c1
        , Point2d.fromCoordinates end
        )
        |> Quadratic


{-| Make a cubic bezier segment
-}
cubic : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Segment
cubic start c1 c2 end =
    CubicSpline2d.fromControlPoints
        ( Point2d.fromCoordinates start
        , Point2d.fromCoordinates c1
        , Point2d.fromCoordinates c2
        , Point2d.fromCoordinates end
        )
        |> Cubic


{-| Make an elliptic arc segment
-}
arc : ( Float, Float ) -> Path.LowLevel.EllipticalArcArgument -> Segment
arc start { radii, xAxisRotate, arcFlag, direction, target } =
    let
        ( rx, ry ) =
            radii

        center : CenterParameterization
        center =
            { start = start, end = target, radii = radii, arcFlag = arcFlag, direction = direction, xAxisRotate = xAxisRotate }
                |> Geometry.Ellipse.endpointToCenter
    in
        Arc <|
            EllipticalArc2d.with <|
                { centerPoint = Point2d.fromCoordinates center.center
                , xDirection = Direction2d.fromAngle center.xAxisRotate
                , xRadius = Tuple.first center.radii
                , yRadius = Tuple.second center.radii
                , startAngle = center.startAngle
                , sweptAngle = center.deltaTheta
                }


{-| Convert a segment to a drawto instruction. forgets the starting point.
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

        Arc ellipse ->
            let
                endpointParameterization : EndpointParameterization
                endpointParameterization =
                    Geometry.Ellipse.centerToEndpoint <|
                        { center = EllipticalArc2d.centerPoint ellipse |> Point2d.coordinates
                        , radii = ( EllipticalArc2d.xRadius ellipse, EllipticalArc2d.yRadius ellipse )
                        , startAngle = EllipticalArc2d.startAngle ellipse
                        , deltaTheta = EllipticalArc2d.sweptAngle ellipse
                        , xAxisRotate = Direction2d.angle (EllipticalArc2d.xDirection ellipse)
                        }
            in
                EllipticalArc
                    [ { target = endpointParameterization.end
                      , radii =
                            endpointParameterization.radii
                            -- convert from radians to degrees
                      , xAxisRotate = endpointParameterization.xAxisRotate
                      , arcFlag = endpointParameterization.arcFlag
                      , direction = endpointParameterization.direction
                      }
                    ]


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
                EllipticalArc2d.startPoint arc


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
                EllipticalArc2d.endPoint arc


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
            Arc (EllipticalArc2d.reverse arc)


{-| Convert a drawto into a segment

This function needs the previous segment to the starting point and (for bezier curves) the control points

    import LowLevel.Command exposing (DrawTo(EllipticalArc), CursorState)

    start : CursorState
    start = { start = (0,0), cursor = (0,0), previousControlPoint = Nothing }

    drawto : DrawTo
    drawto =
            EllipticalArc
                [ { target = (10, 0)
                  , radii = (5,5)
                  , xAxisRotate = 90
                  , arcFlag = largestArc
                  , direction = clockwise
                  }
                ]

    expected : List Segment
    expected =
         [ arc (0,0)
            { target = (10, 0)
            , radii = (5,5)
            , xAxisRotate = 90
            , arcFlag = largestArc
            , direction = clockwise
            }
          ]

    toSegment start drawto --> expected

-}
toSegment : CursorState -> DrawTo -> List Segment
toSegment state drawto =
    let
        start =
            state.cursor
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
                let
                    folder args ( segmentStart, accum ) =
                        ( args.target
                        , arc segmentStart args
                            {-
                               Arc
                                   { start = segmentStart
                                   , end = args.target
                                   , radii =
                                       args.radii

                                   -- convert degrees to radians
                                   , xAxisRotate = degrees args.xAxisRotate
                                   , arcFlag = args.arcFlag
                                   , direction = args.direction
                                   }
                            -}
                            ::
                                accum
                        )
                in
                    traverse folder (Point2d.coordinates start) arguments

            ClosePath ->
                []


{-| Convert a `Segment` to a `CursorState`

    toCursorState (line (0,0) (10, 10))
        --> { start = (0,0) , cursor = (10, 10) , previousControlPoint = Nothing }

-}
toCursorState : Segment -> CursorState
toCursorState segment =
    let
        vec4map f ( a, b, c, d ) =
            ( f a, f b, f c, f d )
    in
        case segment of
            Cubic curve ->
                let
                    ( start, _, control, end ) =
                        CubicSpline2d.controlPoints curve
                            |> vec4map Point2d.coordinates
                in
                    { start = start
                    , previousControlPoint = Just control
                    , cursor = end
                    }

            Quadratic curve ->
                let
                    ( start, control, end ) =
                        QuadraticSpline2d.controlPoints curve
                            |> Vec3.map Point2d.coordinates
                in
                    { start = start
                    , previousControlPoint = Just control
                    , cursor = end
                    }

            _ ->
                { start = firstPoint segment, cursor = finalPoint segment, previousControlPoint = Nothing }


traverse : (a -> ( b, List c ) -> ( b, List c )) -> b -> List a -> List c
traverse folder initial elements =
    List.foldl folder ( initial, [] ) elements
        |> Tuple.second
        |> List.reverse


{-| Get the location at a point on the curve, only defined in the range [0, 1].

    at 0.5 (line (0,0) (10, 0)) --> ( 5, 0 )

    at 0.5 (quadratic (0,0) (5, 10) (10, 0)) --> ( 5, 5 )

-}
at : Float -> Segment -> ( Float, Float )
at t segment =
    case segment of
        LineSegment segment ->
            LineSegment2d.interpolate segment t
                |> Point2d.coordinates

        Quadratic spline ->
            QuadraticSpline2d.pointOn spline t
                |> Point2d.coordinates

        Cubic spline ->
            CubicSpline2d.pointOn spline t
                |> Point2d.coordinates

        Arc arc ->
            EllipticalArc2d.pointOn arc t
                |> Point2d.coordinates


{-| Get the derivative at a point on the curve, only defined in the range [0, 1].

    import LowLevel.Command exposing
        ( EllipticalArcArgument
        , smallestArc
        , largestArc
        , clockwise
        )

    derivativeAt 0.5 (line (0,0) (1,1))
        --> (0.7071067811865475,0.7071067811865475)

    argument : EllipticalArcArgument
    argument =
        { target = ( 5, 5 )
        , radii = ( 5, 5 )
        , xAxisRotate = 0
        , arcFlag = smallestArc
        , direction = clockwise
        }

    derivativeAt 0.5 (arc (0,0)  argument)
        --> (0.7071067811865475,0.7071067811865474)

-}
derivativeAt : Float -> Segment -> ( Float, Float )
derivativeAt t segment =
    case segment of
        LineSegment segment ->
            LineSegment2d.vector segment
                |> Vector2d.normalize
                |> Vector2d.components

        Quadratic spline ->
            QuadraticSpline2d.derivative spline t
                |> Vector2d.components

        Cubic spline ->
            CubicSpline2d.derivative spline t
                |> Vector2d.components

        Arc arc ->
            EllipticalArc2d.derivative arc t
                |> Vector2d.normalize
                |> Vector2d.components


{-| The derivative at the starting point of the segment
-}
derivativeAtFirst : Segment -> Float2
derivativeAtFirst segment =
    case segment of
        LineSegment segment ->
            LineSegment2d.vector segment
                |> Vector2d.normalize
                |> Vector2d.components

        Quadratic spline ->
            Vector2d.components <|
                QuadraticSpline2d.startDerivative spline

        Cubic spline ->
            Vector2d.components <|
                CubicSpline2d.startDerivative spline

        Arc arc ->
            EllipticalArc2d.derivative arc 0
                |> Vector2d.normalize
                |> Vector2d.components


{-| The derivative at the ending point of the segment
-}
derivativeAtFinal : Segment -> Float2
derivativeAtFinal segment =
    case segment of
        LineSegment segment ->
            LineSegment2d.vector segment
                |> Vector2d.normalize
                |> Vector2d.components

        Quadratic spline ->
            QuadraticSpline2d.endDerivative spline
                |> Vector2d.components

        Cubic spline ->
            CubicSpline2d.endDerivative spline
                |> Vector2d.components

        Arc arc ->
            EllipticalArc2d.derivative arc 1
                |> Vector2d.normalize
                |> Vector2d.components


{-| The signed angle (in radians) between the end of segment1 and the start of segment2

    a : Segment
    a = line ( 0, 0 ) ( 1, 0 )

    b : Segment
    b = line ( 0, 0 ) ( 0, 1 )

    angle a b --> degrees 90

    angle b a --> degrees -90

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
length segment =
    0


{-| Give the (x,y) locations of the intersections between two segments
-}
intersections : Segment -> Segment -> List ( Float, Float )
intersections segment1 segment2 =
    []
