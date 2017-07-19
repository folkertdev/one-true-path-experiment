module Segment exposing (fromPath, toPath, Segment(..), length, lengthWithOptions, at)

{-| An alternative interpretation of paths that is convenient for mathematical operations.

Here, a path is viewed as a list of segments with a start and end point.

@docs Segment

@docs fromPath, toPath

# Operations
@docs at, length, lengthWithOptions
-}

import Path exposing (..)
import Vector2 exposing (Vec2, Float2)
import Ellipse
import CubicBezier exposing (..)
import List.Extra as List


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


{-| Convert a list of segments to a path

It is assumed that for every two adjacent segments in the list, the first segment's end point is the second segment's starting point
-}
toPath : List Segment -> Path
toPath segments =
    case segments of
        [] ->
            []

        segment :: rest ->
            [ subpath (moveTo (startingPoint segment)) (List.map toDrawTo segments) ]


{-| Create segments from a path
-}
fromPath : Path -> List Segment
fromPath =
    List.concatMap subpathToSegments


subpathToSegments : SubPath -> List Segment
subpathToSegments { moveto, drawtos } =
    case moveto of
        MoveTo coordinate ->
            let
                cursorState =
                    { start = coordinate, cursor = coordinate }

                -- fake segment to get the fold going, not part of the final result
                initial =
                    LineSegment coordinate coordinate

                folder drawto ( previousSegment, accum ) =
                    let
                        newSegments =
                            toSegment previousSegment drawto

                        finalNewSegment =
                            Maybe.withDefault previousSegment (List.last newSegments)
                    in
                        ( finalNewSegment, newSegments ++ accum )
            in
                traverse folder initial drawtos


startingPoint : Segment -> Vec2 Float
startingPoint segment =
    case segment of
        LineSegment _ p ->
            p

        Quadratic _ _ p ->
            p

        Cubic _ _ _ p ->
            p

        Arc { start } ->
            start


toSegment : Segment -> DrawTo -> List Segment
toSegment previous drawto =
    let
        start =
            startingPoint previous

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
                                Vector2.add start ( startX - cx, startY - cy )

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
                                Vector2.sub segmentStart previousControlPoint
                                    |> Vector2.add point
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
            Vector2.directionFromTo start end
                |> Vector2.scale t

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
                |> arcLength 1.0 config.error

        Cubic start c1 c2 end ->
            CubicBezier.fromPoints start c1 c2 end
                |> arcLength 1.0 config.error

        Arc args ->
            Ellipse.approximateArcLength config args


{-| Give the (x,y) locations of the intersections between two segments
-}
intersections : Segment -> Segment -> List ( Float, Float )
intersections segment1 segment2 =
    Debug.crash ""
