module Segment exposing
    ( Segment(..)
    , line, quadratic, cubic, ellipticalArc
    , at, angle
    , derivativeAt, derivativeAtFirst, derivativeAtFinal
    , firstPoint, finalPoint, reverse
    , ArcLengthParameterized
    , arcLengthParameterized, arcLength, pointAlong, tangentAlong, parameterValueToArcLength, arcLengthToParameterValue
    , toDrawTo, toSegment, toCursorState
    -- , length
    )

{-| An alternative view on paths that is convenient for mathematical operations.

When we look at a path as a list of elemental `Segment`s, it becomes easier to reason about it.
The segment data type has four segment types:

  - `line` straigt line segment
  - `quadratic` a quadratic bezier curve segment
  - `cubic` a cubic bezier curve segment
  - `arc` an elliptical arc segment

All four of these are mathematically well-defined primitives. We can uniformly apply functions like:

  - `angle` between two segments
  - `derivative` or curvature
  - `reverse` reverse a segment - this can be used to [let the browser fill your svg correctly][reverse]

`Segment` can also be `ArcLengthParameterized`, which makes operations based on arc length possible.
For instance, the total arc length or the location after walking some distance over the segment.

These operations are backed by the great [OpenSolid] package, and in turn back many of the operations
in `SubPath`.

[reverse]: https://pomax.github.io/svg-path-reverse/
[OpenSolid]: http://package.elm-lang.org/packages/opensolid/geometry/latest

@docs Segment
@docs line, quadratic, cubic, ellipticalArc


# Operations

@docs at, angle
@docs derivativeAt, derivativeAtFirst, derivativeAtFinal
@docs firstPoint, finalPoint, reverse


# Arc Length Parameterization

@docs ArcLengthParameterized
@docs arcLengthParameterized, arcLength, pointAlong, tangentAlong, parameterValueToArcLength, arcLengthToParameterValue


# Conversion

@docs toDrawTo, toSegment, toCursorState

-}

import CubicSpline2d exposing (CubicSpline2d)
import Curve.ArcLengthParameterization as ArcLengthParameterization
import Curve.ParameterValue as ParameterValue
import Direction2d exposing (Direction2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Geometry.Ellipse exposing (CenterParameterization, EndpointParameterization)
import LineSegment2d exposing (LineSegment2d)
import LowLevel.Command exposing (..)
import Path.LowLevel exposing (ArcFlag(..), Direction(..), EllipticalArcArgument)
import Point2d exposing (Point2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Vector2d exposing (Vector2d)


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
    QuadraticSpline2d.with
        { startPoint = Point2d.fromCoordinates start
        , controlPoint = Point2d.fromCoordinates c1
        , endPoint = Point2d.fromCoordinates end
        }
        |> Quadratic


{-| Make a cubic bezier segment
-}
cubic : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Segment
cubic start c1 c2 end =
    CubicSpline2d.with
        { startPoint = Point2d.fromCoordinates start
        , startControlPoint = Point2d.fromCoordinates c1
        , endControlPoint = Point2d.fromCoordinates c2
        , endPoint = Point2d.fromCoordinates end
        }
        |> Cubic


{-| Make an elliptic arc segment
-}
ellipticalArc : ( Float, Float ) -> Path.LowLevel.EllipticalArcArgument -> Segment
ellipticalArc start { radii, xAxisRotate, arcFlag, direction, target } =
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
        LineSegment lineSegment ->
            lineTo [ Point2d.coordinates <| LineSegment2d.endPoint lineSegment ]

        Quadratic spline ->
            quadraticCurveTo
                [ ( Point2d.coordinates (QuadraticSpline2d.controlPoint spline)
                  , Point2d.coordinates (QuadraticSpline2d.endPoint spline)
                  )
                ]

        Cubic spline ->
            cubicCurveTo
                [ ( Point2d.coordinates (CubicSpline2d.startControlPoint spline)
                  , Point2d.coordinates (CubicSpline2d.endControlPoint spline)
                  , Point2d.coordinates (CubicSpline2d.endPoint spline)
                  )
                ]

        Arc ellipse ->
            let
                endpointParameterization : EndpointParameterization
                endpointParameterization =
                    Geometry.Ellipse.centerToEndpoint <|
                        { center = EllipticalArc2d.centerPoint ellipse |> Point2d.coordinates
                        , radii = ( EllipticalArc2d.xRadius ellipse, EllipticalArc2d.yRadius ellipse )
                        , startAngle = EllipticalArc2d.startAngle ellipse
                        , deltaTheta = EllipticalArc2d.sweptAngle ellipse
                        , xAxisRotate = Direction2d.toAngle (EllipticalArc2d.xDirection ellipse)
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
firstPoint : Segment -> ( Float, Float )
firstPoint segment =
    Point2d.coordinates <|
        case segment of
            LineSegment lineSegment ->
                LineSegment2d.startPoint lineSegment

            Quadratic spline ->
                QuadraticSpline2d.startPoint spline

            Cubic spline ->
                CubicSpline2d.startPoint spline

            Arc arc ->
                EllipticalArc2d.startPoint arc


{-| Extract the final point from a segment
-}
finalPoint : Segment -> ( Float, Float )
finalPoint segment =
    Point2d.coordinates <|
        case segment of
            LineSegment lineSegment ->
                LineSegment2d.endPoint lineSegment

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
        LineSegment lineSegment ->
            LineSegment <| LineSegment2d.reverse lineSegment

        Quadratic spline ->
            Quadratic <| QuadraticSpline2d.reverse spline

        Cubic spline ->
            Cubic <| CubicSpline2d.reverse spline

        Arc arc ->
            Arc (EllipticalArc2d.reverse arc)


{-| Convert a drawto into a segment

This function needs the previous segment to the starting point and (for bezier curves) the control points

    import LowLevel.Command exposing (DrawTo(EllipticalArc), CursorState, clockwise, largestArc)

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

        CurveTo coordinates ->
            let
                toPoint2ds ( startControlPoint, endControlPoint, endPoint ) =
                    ( Point2d.fromCoordinates startControlPoint, Point2d.fromCoordinates endControlPoint, Point2d.fromCoordinates endPoint )

                folder ( c1, c2, p ) ( segmentStart, accum ) =
                    ( p, Cubic (CubicSpline2d.with { startPoint = segmentStart, startControlPoint = c1, endControlPoint = c2, endPoint = p }) :: accum )
            in
            coordinates
                |> List.map toPoint2ds
                |> traverse folder start

        QuadraticBezierCurveTo coordinates ->
            let
                toPoint2ds ( controlPoint, endPoint ) =
                    ( Point2d.fromCoordinates controlPoint, Point2d.fromCoordinates endPoint )

                folder ( c, p ) ( segmentStart, accum ) =
                    ( p
                    , Quadratic (QuadraticSpline2d.with { startPoint = segmentStart, controlPoint = c, endPoint = p }) :: accum
                    )
            in
            coordinates
                |> List.map toPoint2ds
                |> traverse folder start

        EllipticalArc arguments ->
            let
                folder args ( segmentStart, accum ) =
                    ( args.target
                    , ellipticalArc segmentStart args
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
                        :: accum
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
    case segment of
        Cubic curve ->
            let
                start =
                    curve
                        |> CubicSpline2d.startPoint
                        |> Point2d.coordinates

                control =
                    curve
                        |> CubicSpline2d.endControlPoint
                        |> Point2d.coordinates

                end =
                    curve
                        |> CubicSpline2d.endPoint
                        |> Point2d.coordinates
            in
            { start = start
            , previousControlPoint = Just control
            , cursor = end
            }

        Quadratic curve ->
            let
                start =
                    curve
                        |> QuadraticSpline2d.startPoint
                        |> Point2d.coordinates

                control =
                    curve
                        |> QuadraticSpline2d.controlPoint
                        |> Point2d.coordinates

                end =
                    curve
                        |> QuadraticSpline2d.endPoint
                        |> Point2d.coordinates
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

    at 0.5 (line ( 0, 0 ) ( 10, 0 )) --> ( 5, 0 )

    at 0.5 (quadratic ( 0, 0 ) ( 5, 10 ) ( 10, 0 )) --> ( 5, 5 )

-}
at : Float -> Segment -> ( Float, Float )
at t segment =
    let
        parameterValue =
            ParameterValue.clamped t
    in
    case segment of
        LineSegment lineSegment ->
            LineSegment2d.interpolate lineSegment t
                |> Point2d.coordinates

        Quadratic spline ->
            QuadraticSpline2d.pointOn spline parameterValue
                |> Point2d.coordinates

        Cubic spline ->
            CubicSpline2d.pointOn spline parameterValue
                |> Point2d.coordinates

        Arc arc ->
            EllipticalArc2d.pointOn arc parameterValue
                |> Point2d.coordinates


{-| Get the derivative at a point on the curve, only defined in the range [0, 1].

    import Vector2
    import LowLevel.Command exposing
        ( EllipticalArcArgument
        , smallestArc
        , largestArc
        , clockwise
        )

    derivativeAt 0.5 (line (0,0) (1,1))
        |> Vector2.normalize
        --> Vector2.normalize (1,1)

    argument : EllipticalArcArgument
    argument =
        { target = ( 5, 5 )
        , radii = ( 5, 5 )
        , xAxisRotate = 0
        , arcFlag = smallestArc
        , direction = clockwise
        }

    derivativeAt 0.5 (arc (0,0)  argument)
        |> Vector2.normalize
        --> Vector2.normalize (1,1)

-}
derivativeAt : Float -> Segment -> ( Float, Float )
derivativeAt t segment =
    let
        parameterValue =
            ParameterValue.clamped t
    in
    Vector2d.components <|
        case segment of
            LineSegment lineSegment ->
                LineSegment2d.vector lineSegment

            Quadratic spline ->
                QuadraticSpline2d.firstDerivative spline parameterValue

            Cubic spline ->
                CubicSpline2d.firstDerivative spline parameterValue

            Arc arc ->
                EllipticalArc2d.firstDerivative arc parameterValue


{-| The derivative at the starting point of the segment
-}
derivativeAtFirst : Segment -> ( Float, Float )
derivativeAtFirst segment =
    Vector2d.components <|
        case segment of
            LineSegment lineSegment ->
                LineSegment2d.vector lineSegment

            Quadratic spline ->
                QuadraticSpline2d.startDerivative spline

            Cubic spline ->
                CubicSpline2d.startDerivative spline

            Arc arc ->
                EllipticalArc2d.firstDerivative arc ParameterValue.zero


{-| The derivative at the ending point of the segment
-}
derivativeAtFinal : Segment -> ( Float, Float )
derivativeAtFinal segment =
    Vector2d.components <|
        case segment of
            LineSegment lineSegment ->
                LineSegment2d.vector lineSegment

            Quadratic spline ->
                QuadraticSpline2d.endDerivative spline

            Cubic spline ->
                CubicSpline2d.endDerivative spline

            Arc arc ->
                EllipticalArc2d.firstDerivative arc ParameterValue.one


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
    let
        direction1 =
            seg1
                |> derivativeAtFinal
                |> Vector2d.fromComponents
                |> Vector2d.scaleBy -1

        direction2 =
            seg2
                |> derivativeAtFirst
                |> Vector2d.fromComponents
                |> Vector2d.scaleBy -1
    in
    signedAngle_ direction1 direction2


signedAngle_ : Vector2d -> Vector2d -> Float
signedAngle_ u v =
    let
        sign =
            if Vector2d.crossProduct u v < 0 then
                -1

            else
                1
    in
    sign * abs (acos (Vector2d.dotProduct u v / (Vector2d.length u * Vector2d.length v)))


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



-- Arc Length Parameterization


{-| Opaque type for the arc length parameterization of a segment
-}
type ArcLengthParameterized
    = ParameterizedLineSegment LineSegment2d.LineSegment2d
    | ParameterizedQuadratic QuadraticSpline2d.ArcLengthParameterized
    | ParameterizedCubic CubicSpline2d.ArcLengthParameterized
    | ParameterizedArc EllipticalArc2d.ArcLengthParameterized


{-| -}
arcLengthParameterized :
    Float
    -> Segment
    -> ArcLengthParameterized
arcLengthParameterized tolerance segment =
    let
        config =
            { maxError = tolerance }
    in
    case segment of
        LineSegment lineSegment ->
            ParameterizedLineSegment lineSegment

        Quadratic spline ->
            QuadraticSpline2d.arcLengthParameterized config spline
                |> ParameterizedQuadratic

        Cubic spline ->
            CubicSpline2d.arcLengthParameterized config spline
                |> ParameterizedCubic

        Arc arc ->
            EllipticalArc2d.arcLengthParameterized config arc
                |> ParameterizedArc


{-| -}
arcLength : ArcLengthParameterized -> Float
arcLength parameterized =
    case parameterized of
        ParameterizedLineSegment lineSegment ->
            LineSegment2d.length lineSegment

        ParameterizedQuadratic spline ->
            QuadraticSpline2d.arcLength spline

        ParameterizedCubic spline ->
            CubicSpline2d.arcLength spline

        ParameterizedArc arc ->
            EllipticalArc2d.arcLength arc


{-| -}
pointAlong : ArcLengthParameterized -> Float -> Maybe ( Float, Float )
pointAlong parameterized t =
    let
        parameterValue =
            ParameterValue.clamped t
    in
    Maybe.map Point2d.coordinates <|
        case parameterized of
            ParameterizedLineSegment lineSegment ->
                LineSegment2d.interpolate lineSegment (t / LineSegment2d.length lineSegment)
                    |> Just

            ParameterizedQuadratic spline ->
                QuadraticSpline2d.pointAlong spline t

            ParameterizedCubic spline ->
                CubicSpline2d.pointAlong spline t

            ParameterizedArc arc ->
                EllipticalArc2d.pointAlong arc t


{-| -}
tangentAlong : ArcLengthParameterized -> Float -> Maybe ( Float, Float )
tangentAlong parameterized t =
    let
        parameterValue =
            ParameterValue.clamped t
    in
    Maybe.map Direction2d.components <|
        case parameterized of
            ParameterizedLineSegment lineSegment ->
                LineSegment2d.direction lineSegment

            ParameterizedQuadratic spline ->
                spline
                    |> (\validSpline -> QuadraticSpline2d.tangentDirectionAlong validSpline t)

            ParameterizedCubic spline ->
                spline
                    |> (\validSpline -> CubicSpline2d.tangentDirectionAlong validSpline t)

            ParameterizedArc arc ->
                arc
                    |> (\validSpline -> EllipticalArc2d.tangentDirectionAlong validSpline t)


{-| -}
arcLengthToParameterValue :
    ArcLengthParameterized
    -> Float
    -> Maybe Float
arcLengthToParameterValue parameterized t =
    let
        parameterValue =
            ParameterValue.clamped t

        mapper object =
            ArcLengthParameterization.arcLengthToParameterValue t object
                |> Maybe.map ParameterValue.value
    in
    case parameterized of
        ParameterizedLineSegment lineSegment ->
            LineSegment2d.length lineSegment
                / t
                |> Just

        ParameterizedQuadratic spline ->
            QuadraticSpline2d.arcLengthParameterization spline
                |> mapper

        ParameterizedCubic spline ->
            spline
                |> CubicSpline2d.arcLengthParameterization
                |> mapper

        ParameterizedArc arc ->
            EllipticalArc2d.arcLengthParameterization arc
                |> mapper


{-| -}
parameterValueToArcLength :
    ArcLengthParameterized
    -> Float
    -> Float
parameterValueToArcLength parameterized t =
    let
        parameterValue =
            ParameterValue.clamped t

        mapper object =
            ArcLengthParameterization.parameterValueToArcLength parameterValue object
    in
    case parameterized of
        ParameterizedLineSegment lineSegment ->
            LineSegment2d.length lineSegment
                * t

        ParameterizedQuadratic spline ->
            QuadraticSpline2d.arcLengthParameterization spline
                |> mapper

        ParameterizedCubic spline ->
            CubicSpline2d.arcLengthParameterization spline
                |> mapper

        ParameterizedArc arc ->
            EllipticalArc2d.arcLengthParameterization arc
                |> mapper
