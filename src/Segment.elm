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

--import ParameterValue

import Angle
import ArcLengthParameterization
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d exposing (Direction2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Geometry.Ellipse exposing (CenterParameterization, EndpointParameterization)
import LineSegment2d exposing (LineSegment2d)
import LowLevel.Command exposing (..)
import Path.LowLevel exposing (ArcFlag(..), Direction(..), EllipticalArcArgument)
import Point2d exposing (Point2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Unitless)
import Vector2d exposing (Vector2d)


{-| The four types of segments.
-}
type Segment coordinates
    = LineSegment (LineSegment2d Unitless coordinates)
    | Quadratic (QuadraticSpline2d Unitless coordinates)
    | Cubic (CubicSpline2d Unitless coordinates)
    | Arc (EllipticalArc2d Unitless coordinates)


{-| Make a line segment
-}
line : ( Float, Float ) -> ( Float, Float ) -> Segment coordinates
line from to =
    LineSegment2d.from (Point2d.fromTuple Quantity.float from) (Point2d.fromTuple Quantity.float to)
        |> LineSegment


{-| Make a quadratic bezier segment
-}
quadratic : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Segment coordinates
quadratic start c1 end =
    QuadraticSpline2d.fromControlPoints
        (Point2d.fromTuple Quantity.float start)
        (Point2d.fromTuple Quantity.float c1)
        (Point2d.fromTuple Quantity.float end)
        |> Quadratic


{-| Make a cubic bezier segment
-}
cubic : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Segment coordinates
cubic start c1 c2 end =
    CubicSpline2d.fromControlPoints
        (Point2d.fromTuple Quantity.float start)
        (Point2d.fromTuple Quantity.float c1)
        (Point2d.fromTuple Quantity.float c2)
        (Point2d.fromTuple Quantity.float end)
        |> Cubic


{-| Make an elliptic arc segment
-}
ellipticalArc : ( Float, Float ) -> Path.LowLevel.EllipticalArcArgument -> Segment coordinates
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
            { centerPoint = Point2d.fromTuple Quantity.float center.center
            , xDirection = Direction2d.fromAngle (Angle.radians center.xAxisRotate)
            , xRadius = Quantity.float <| Tuple.first center.radii
            , yRadius = Quantity.float <| Tuple.second center.radii
            , startAngle = Angle.radians center.startAngle
            , sweptAngle = Angle.radians center.deltaTheta
            }


{-| Convert a segment to a drawto instruction. forgets the starting point.
-}
toDrawTo : Segment coordinates -> DrawTo
toDrawTo segment =
    case segment of
        LineSegment lineSegment ->
            lineTo [ Point2d.toTuple Quantity.toFloat <| LineSegment2d.endPoint lineSegment ]

        Quadratic spline ->
            quadraticCurveTo
                [ ( Point2d.toTuple Quantity.toFloat (QuadraticSpline2d.secondControlPoint spline)
                  , Point2d.toTuple Quantity.toFloat (QuadraticSpline2d.endPoint spline)
                  )
                ]

        Cubic spline ->
            cubicCurveTo
                [ ( Point2d.toTuple Quantity.toFloat (CubicSpline2d.secondControlPoint spline)
                  , Point2d.toTuple Quantity.toFloat (CubicSpline2d.thirdControlPoint spline)
                  , Point2d.toTuple Quantity.toFloat (CubicSpline2d.fourthControlPoint spline)
                  )
                ]

        Arc ellipse ->
            let
                endpointParameterization : EndpointParameterization
                endpointParameterization =
                    Geometry.Ellipse.centerToEndpoint <|
                        { center = EllipticalArc2d.centerPoint ellipse |> Point2d.toTuple Quantity.toFloat
                        , radii = ( EllipticalArc2d.xRadius ellipse |> Quantity.toFloat, EllipticalArc2d.yRadius ellipse |> Quantity.toFloat )
                        , startAngle = Angle.inRadians <| EllipticalArc2d.startAngle ellipse
                        , deltaTheta = Angle.inRadians <| EllipticalArc2d.sweptAngle ellipse
                        , xAxisRotate = Angle.inRadians <| Direction2d.toAngle (EllipticalArc2d.xDirection ellipse)
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
firstPoint : Segment coordinates -> ( Float, Float )
firstPoint segment =
    Point2d.toTuple Quantity.toFloat <|
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
finalPoint : Segment coordinates -> ( Float, Float )
finalPoint segment =
    Point2d.toTuple Quantity.toFloat <|
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
reverse : Segment coordinates -> Segment coordinates
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
toSegment : CursorState -> DrawTo -> List (Segment coordinates)
toSegment state drawto =
    let
        start =
            state.cursor
                |> Point2d.fromTuple Quantity.float

        ( startX, startY ) =
            Point2d.toTuple Quantity.toFloat start
    in
    case drawto of
        LineTo coordinates_ ->
            let
                coordinates =
                    List.map (Point2d.fromTuple Quantity.float) coordinates_
            in
            List.map2 (\f t -> LineSegment <| LineSegment2d.from f t) (start :: coordinates) coordinates

        CurveTo coordinates ->
            let
                toPoint2ds ( startControlPoint, endControlPoint, endPoint ) =
                    ( Point2d.fromTuple Quantity.float startControlPoint, Point2d.fromTuple Quantity.float endControlPoint, Point2d.fromTuple Quantity.float endPoint )

                folder ( c1, c2, p ) ( segmentStart, accum ) =
                    ( p
                    , Cubic (CubicSpline2d.fromControlPoints segmentStart c1 c2 p) :: accum
                    )
            in
            coordinates
                |> List.map toPoint2ds
                |> traverse folder start

        QuadraticBezierCurveTo coordinates ->
            let
                toPoint2ds ( controlPoint, endPoint ) =
                    ( Point2d.fromTuple Quantity.float controlPoint, Point2d.fromTuple Quantity.float endPoint )

                folder ( c, p ) ( segmentStart, accum ) =
                    ( p
                    , Quadratic (QuadraticSpline2d.fromControlPoints segmentStart c p) :: accum
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
            traverse folder (Point2d.toTuple Quantity.toFloat start) arguments

        ClosePath ->
            []


{-| Convert a `Segment` to a `CursorState`

    toCursorState (line (0,0) (10, 10))
        --> { start = (0,0) , cursor = (10, 10) , previousControlPoint = Nothing }

-}
toCursorState : Segment coordinates -> CursorState
toCursorState segment =
    case segment of
        Cubic curve ->
            let
                start =
                    curve
                        |> CubicSpline2d.startPoint
                        |> Point2d.toTuple Quantity.toFloat

                control =
                    curve
                        -- TODO this was `controlPoint` before, is this the correct one?
                        |> CubicSpline2d.thirdControlPoint
                        |> Point2d.toTuple Quantity.toFloat

                end =
                    curve
                        |> CubicSpline2d.endPoint
                        |> Point2d.toTuple Quantity.toFloat
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
                        |> Point2d.toTuple Quantity.toFloat

                control =
                    curve
                        |> QuadraticSpline2d.secondControlPoint
                        |> Point2d.toTuple Quantity.toFloat

                end =
                    curve
                        |> QuadraticSpline2d.endPoint
                        |> Point2d.toTuple Quantity.toFloat
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
at : Float -> Segment coordinates -> ( Float, Float )
at t segment =
    let
        parameterValue =
            clamp 0 1 t
    in
    case segment of
        LineSegment lineSegment ->
            LineSegment2d.interpolate lineSegment t
                |> Point2d.toTuple Quantity.toFloat

        Quadratic spline ->
            QuadraticSpline2d.pointOn spline parameterValue
                |> Point2d.toTuple Quantity.toFloat

        Cubic spline ->
            CubicSpline2d.pointOn spline parameterValue
                |> Point2d.toTuple Quantity.toFloat

        Arc arc ->
            EllipticalArc2d.pointOn arc parameterValue
                |> Point2d.toTuple Quantity.toFloat


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
derivativeAt : Float -> Segment coordinates -> ( Float, Float )
derivativeAt t segment =
    let
        parameterValue =
            clamp 0 1 t
    in
    Vector2d.toTuple Quantity.toFloat <|
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
derivativeAtFirst : Segment coordinates -> ( Float, Float )
derivativeAtFirst segment =
    Vector2d.toTuple Quantity.toFloat <|
        case segment of
            LineSegment lineSegment ->
                LineSegment2d.vector lineSegment

            Quadratic spline ->
                QuadraticSpline2d.startDerivative spline

            Cubic spline ->
                CubicSpline2d.startDerivative spline

            Arc arc ->
                EllipticalArc2d.firstDerivative arc 0


{-| The derivative at the ending point of the segment
-}
derivativeAtFinal : Segment coordinates -> ( Float, Float )
derivativeAtFinal segment =
    Vector2d.toTuple Quantity.toFloat <|
        case segment of
            LineSegment lineSegment ->
                LineSegment2d.vector lineSegment

            Quadratic spline ->
                QuadraticSpline2d.endDerivative spline

            Cubic spline ->
                CubicSpline2d.endDerivative spline

            Arc arc ->
                EllipticalArc2d.firstDerivative arc 1


{-| The signed angle (in radians) between the end of segment1 and the start of segment2

    a : Segment
    a = line ( 0, 0 ) ( 1, 0 )

    b : Segment
    b = line ( 0, 0 ) ( 0, 1 )

    angle a b --> degrees 90

    angle b a --> degrees -90

-}
angle : Segment coordinates -> Segment coordinates -> Float
angle seg1 seg2 =
    let
        direction1 =
            seg1
                |> derivativeAtFinal
                |> Vector2d.fromTuple Quantity.float
                |> Vector2d.scaleBy -1

        direction2 =
            seg2
                |> derivativeAtFirst
                |> Vector2d.fromTuple Quantity.float
                |> Vector2d.scaleBy -1
    in
    Geometry.Ellipse.signedAngle direction1 direction2


{-| The approximate length of a segment
-}
length : Segment coordinates -> Float
length segment =
    0


{-| Give the (x,y) locations of the intersections between two segments
-}
intersections : Segment coordinates -> Segment coordinates -> List ( Float, Float )
intersections segment1 segment2 =
    []



-- Arc Length Parameterization


{-| Opaque type for the arc length parameterization of a segment
-}
type ArcLengthParameterized coordinates
    = ParameterizedLineSegment (LineSegment2d.LineSegment2d Unitless coordinates)
    | ParameterizedQuadratic (QuadraticSpline2d.ArcLengthParameterized Unitless coordinates)
    | ParameterizedCubic (CubicSpline2d.ArcLengthParameterized Unitless coordinates)
    | ParameterizedArc (EllipticalArc2d.ArcLengthParameterized Unitless coordinates)


{-| -}
arcLengthParameterized :
    Float
    -> Segment coordinates
    -> Maybe (ArcLengthParameterized coordinates)
arcLengthParameterized tolerance segment =
    let
        config =
            { maxError = Quantity.float tolerance }
    in
    case segment of
        LineSegment lineSegment ->
            ParameterizedLineSegment lineSegment
                |> Just

        Quadratic spline ->
            spline
                |> QuadraticSpline2d.nondegenerate
                |> Result.toMaybe
                |> Maybe.map
                    (QuadraticSpline2d.arcLengthParameterized config >> ParameterizedQuadratic)

        Cubic spline ->
            spline
                |> CubicSpline2d.nondegenerate
                |> Result.toMaybe
                |> Maybe.map
                    (CubicSpline2d.arcLengthParameterized config >> ParameterizedCubic)

        Arc arc ->
            arc
                |> EllipticalArc2d.nondegenerate
                |> Result.toMaybe
                |> Maybe.map
                    (EllipticalArc2d.arcLengthParameterized config >> ParameterizedArc)


{-| -}
arcLength : ArcLengthParameterized coordinates -> Float
arcLength parameterized =
    Quantity.toFloat <|
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
pointAlong : ArcLengthParameterized coordinates -> Float -> ( Float, Float )
pointAlong parameterized t =
    let
        lengthValue =
            Quantity.float t
    in
    Point2d.toTuple Quantity.toFloat <|
        case parameterized of
            ParameterizedLineSegment lineSegment ->
                LineSegment2d.interpolate lineSegment (t / Quantity.toFloat (LineSegment2d.length lineSegment))

            ParameterizedQuadratic spline ->
                QuadraticSpline2d.pointAlong spline lengthValue

            ParameterizedCubic spline ->
                CubicSpline2d.pointAlong spline lengthValue

            ParameterizedArc arc ->
                EllipticalArc2d.pointAlong arc lengthValue


{-| -}
tangentAlong : ArcLengthParameterized coordinates -> Float -> Maybe ( Float, Float )
tangentAlong parameterized t =
    let
        lengthValue : Quantity.Quantity Float Unitless
        lengthValue =
            Quantity.float t
    in
    Maybe.map (\direction -> direction |> Direction2d.toVector |> Vector2d.toTuple Quantity.toFloat) <|
        case parameterized of
            ParameterizedLineSegment lineSegment ->
                LineSegment2d.direction lineSegment

            ParameterizedQuadratic spline ->
                spline
                    |> (\validSpline -> QuadraticSpline2d.tangentDirectionAlong validSpline lengthValue)
                    |> Just

            ParameterizedCubic spline ->
                spline
                    |> (\validSpline -> CubicSpline2d.tangentDirectionAlong validSpline lengthValue)
                    |> Just

            ParameterizedArc arc ->
                arc
                    |> (\validSpline -> EllipticalArc2d.tangentDirectionAlong validSpline lengthValue)
                    |> Just


{-| -}
arcLengthToParameterValue :
    ArcLengthParameterized coordinates
    -> Float
    -> Float
arcLengthToParameterValue parameterized t =
    let
        mapper object =
            ArcLengthParameterization.arcLengthToParameterValue (Quantity.float t) object
    in
    case parameterized of
        ParameterizedLineSegment lineSegment ->
            LineSegment2d.length lineSegment
                |> Quantity.toFloat
                |> (\v -> v / t)

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
    ArcLengthParameterized coordinates
    -> Float
    -> Float
parameterValueToArcLength parameterized t =
    let
        mapper object =
            ArcLengthParameterization.parameterValueToArcLength t object
                |> Quantity.toFloat
    in
    case parameterized of
        ParameterizedLineSegment lineSegment ->
            LineSegment2d.length lineSegment
                |> Quantity.toFloat
                |> (\v -> v * clamp 0 1 t)

        ParameterizedQuadratic spline ->
            QuadraticSpline2d.arcLengthParameterization spline
                |> mapper

        ParameterizedCubic spline ->
            CubicSpline2d.arcLengthParameterization spline
                |> mapper

        ParameterizedArc arc ->
            EllipticalArc2d.arcLengthParameterization arc
                |> mapper
