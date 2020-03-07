module Curve exposing
    ( linear, linearClosed
    , cubicBezier, smoothCubicBezier, quadraticBezier, smoothQuadraticBezier
    , step, stepBefore, stepAfter
    , catmullRom, catmullRomClosed, catmullRomOpen
    , monotoneX, monotoneY
    , natural
    , basis, basisClosed, basisOpen, bundle
    , cardinal, cardinalClosed, cardinalOpen
    , repeatFirstPoint, repeatFinalPoint
    , radial, toPolarWithCenter
    )

{-| Construct curves from a set of points.

The problem of drawing a line through a set of points is actually quite tricky. Should the curve be smooth? Should the ends be connected?
This module gives many options for drawing lines through points.

Supports all the curves defined by [D3 Shape](https://github.com/d3/d3-shape#curves).


## Linear

Draw a straight line connecting the data points. The closed variant repeats
the final point to create a closed curve.

@docs linear, linearClosed


## Bezier

Helpers for bezier curves. Quadratic bezier segments have a start and end point, and 1 control point.
Cubic beziers have 2 control points. The smooth variants can use the previous control point to draw the next segment.

The first argument is the starting point, the second argument a list of extensions.

@docs cubicBezier, smoothCubicBezier, quadraticBezier, smoothQuadraticBezier


## Step

Step goes some distance to the right, then to the y-coordinate of the next data point, and then draws to the next point.

The first argument determines where the step is.

  - `step 1 points` is `stepAfter`
  - `step 0 points` is `stepBefore`
  - `step 0.5 points` steps exactly in the middle

@docs step, stepBefore, stepAfter


## Catmull-Rom

Catmull-Rom is perfect for animation, because data points are hit exactly and the curve is smooth.

@docs catmullRom, catmullRomClosed, catmullRomOpen


## Monotone

The monotone curves can only be increasing (staying flat or becoming higher) or decreasing (staying flat or becoming lower) between any two adjacent points.
It cannot first go down and then go up.

![monotone curve illustration](https://upload.wikimedia.org/wikipedia/en/f/fe/MonotCubInt.png)

Around 0.45, the cubic interpolation dives below the y-coordinate of the next point, whereas the monotone interpolation does not.

A nice consequence is that there are no weird bumps in the curve between the data points.

@docs monotoneX, monotoneY


## Natural

@docs natural


## Basis

@docs basis, basisClosed, basisOpen, bundle


## Cardinal

@docs cardinal, cardinalClosed, cardinalOpen


## Transformations

@docs repeatFirstPoint, repeatFinalPoint
@docs radial, toPolarWithCenter

-}

import Internal.NaturalInterpolation exposing (naturalControlPoints)
import List.Extra as List
import LowLevel.Command as Command exposing (..)
import Path.LowLevel as LowLevel exposing (Mode(..))
import Quantity exposing (Unitless)
import SubPath exposing (SubPath(..), close, connect, empty)
import Vector2d exposing (Vector2d)


epsilon : Float
epsilon =
    1.0e-12


sign : number -> number
sign x =
    if x < 0 then
        -1

    else
        1


type alias Triplet a =
    ( a, a, a )


mapTriplet : (a -> b) -> Triplet a -> Triplet b
mapTriplet f ( a, b, c ) =
    ( f a, f b, f c )


area : List ( ( Float, Float ), ( Float, Float ) ) -> SubPath
area points =
    let
        ( low, high ) =
            points
                |> List.unzip
    in
    step 1.0 low
        |> connect (step 1.0 high)
        |> close


{-| Draw straigt lines between the data points

![linear](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/linear.svg)

-}
linear : List ( Float, Float ) -> SubPath
linear points =
    case points of
        [] ->
            empty

        x :: xs ->
            SubPath.with (moveTo x) [ lineTo xs ]


{-| Draw a straigt line between the data points, connecting the ends.

![linear-closed](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/linearClosed.svg)

-}
linearClosed : List ( Float, Float ) -> SubPath
linearClosed points =
    case points of
        [] ->
            empty

        x :: xs ->
            SubPath.with (moveTo x) [ lineTo xs, closePath ]


{-| Shorthand to draw a sequence of cubic bezier segments
-}
cubicBezier : ( Float, Float ) -> List ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ) -> SubPath
cubicBezier start points =
    case points of
        [] ->
            empty

        x :: xs ->
            SubPath.with (moveTo start) [ cubicCurveTo points ]


{-| Shorthand to draw a sequence of smooth cubic bezier segments
-}
smoothCubicBezier : ( Float, Float ) -> ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ) -> List ( ( Float, Float ), ( Float, Float ) ) -> SubPath
smoothCubicBezier start first points =
    let
        lowLevelDrawTos =
            [ LowLevel.CurveTo Absolute [ first ]
            , LowLevel.SmoothCurveTo Absolute points
            ]

        lowLevelSubPath : LowLevel.SubPath
        lowLevelSubPath =
            { moveto = LowLevel.MoveTo Absolute start, drawtos = lowLevelDrawTos }
    in
    SubPath.fromLowLevel lowLevelSubPath


{-| Shorthand to draw a sequence of quadratic bezier segments
-}
quadraticBezier : ( Float, Float ) -> List ( ( Float, Float ), ( Float, Float ) ) -> SubPath
quadraticBezier start points =
    case points of
        [] ->
            empty

        x :: xs ->
            SubPath.with (moveTo start) [ quadraticCurveTo points ]


{-| Shorthand to draw a sequence of smooth quadratic bezier segments
-}
smoothQuadraticBezier : ( Float, Float ) -> ( ( Float, Float ), ( Float, Float ) ) -> List ( Float, Float ) -> SubPath
smoothQuadraticBezier start first points =
    let
        lowLevelDrawTos =
            [ LowLevel.QuadraticBezierCurveTo Absolute [ first ]
            , LowLevel.SmoothQuadraticBezierCurveTo Absolute points
            ]

        lowLevelSubPath : LowLevel.SubPath
        lowLevelSubPath =
            { moveto = LowLevel.MoveTo Absolute start, drawtos = lowLevelDrawTos }
    in
    SubPath.fromLowLevel lowLevelSubPath


{-| Convert `(angle, radius)` pairs to `(x, y)` coordinates, relative to the given vector.

This function is used by radial and can be used to use radial with different interpolations, for instance.

    radialNatural : ( Float, Float ) -> List ( Float, Float ) -> SubPath
    radialNatural ( x, y ) =
        natural << toPolarWithCenter ( x, y )

-}
toPolarWithCenter : ( Float, Float ) -> List ( Float, Float ) -> List ( Float, Float )
toPolarWithCenter ( x, y ) =
    List.map (\( angle, radius ) -> ( radius * sin angle + x, -radius * cos angle + y ))


{-| Interpret a 2D vector as a `(angle, radius)` pair. The angle is in radians. The first argument is the center.

![radial](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/radial.svg)

-}
radial : ( Float, Float ) -> List ( Float, Float ) -> SubPath
radial ( x, y ) =
    linear << toPolarWithCenter ( x, y )


basisPoint : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Triplet ( Float, Float )
basisPoint p0 p1 p =
    basisPointHelper
        (Vector2d.fromTuple Quantity.float p0)
        (Vector2d.fromTuple Quantity.float p1)
        (Vector2d.fromTuple Quantity.float p)
        |> mapTriplet (Vector2d.toTuple Quantity.toFloat)


basisPointHelper :
    Vector2d Unitless coordinates
    -> Vector2d Unitless coordinates
    -> Vector2d Unitless coordinates
    -> Triplet (Vector2d Unitless coordinates)
basisPointHelper p0 p1 p =
    ( Vector2d.scaleBy 2 p0
        |> Vector2d.plus p1
        |> Vector2d.scaleBy (1 / 3)
    , Vector2d.scaleBy 2 p1
        |> Vector2d.plus p0
        |> Vector2d.scaleBy (1 / 3)
    , Vector2d.scaleBy 4 p1
        |> Vector2d.plus p0
        |> Vector2d.plus p
        |> Vector2d.scaleBy (1 / 6)
    )


{-| Basis interpolation (also known as B-spline)

![basis](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/basis.svg)

-}
basis : List ( Float, Float ) -> SubPath
basis points =
    let
        --| The ideal case where there are at least 3 points
        commonCase : List DrawTo -> List ( Float, Float ) -> List DrawTo
        commonCase acc remainingPoints =
            case remainingPoints of
                p0 :: p1 :: [] ->
                    List.reverse
                        (lineTo [ p1 ] :: cubicCurveTo [ basisPoint p0 p1 p1 ] :: acc)

                p0 :: p1 :: p :: rest ->
                    commonCase
                        (cubicCurveTo [ basisPoint p0 p1 p ] :: acc)
                        (p1 :: p :: rest)

                _ ->
                    []

        toFirst p0 p1 =
            Vector2d.scaleBy 5 p0
                |> Vector2d.plus p1
                |> Vector2d.scaleBy (1 / 6)
                |> Vector2d.toTuple Quantity.toFloat
    in
    case points of
        p0 :: p1 :: _ :: _ ->
            SubPath.with (moveTo p0) (lineTo [ toFirst (Vector2d.fromTuple Quantity.float p0) (Vector2d.fromTuple Quantity.float p1) ] :: commonCase [] points)

        [ p0, p1 ] ->
            SubPath.with (moveTo p0) [ lineTo [ p1 ] ]

        _ ->
            empty


{-| Closed Basis interpolation (also known as B-spline)

![basis closed](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/basisClosed.svg)

-}
basisClosed : List ( Float, Float ) -> SubPath
basisClosed points =
    let
        --| The ideal case where there are at least 3 points
        commonCase : List (Triplet ( Float, Float )) -> (( Float, Float ) -> ( Float, Float ) -> List (Triplet ( Float, Float ))) -> List ( Float, Float ) -> List (Triplet ( Float, Float ))
        commonCase acc close remainingPoints =
            case remainingPoints of
                p0 :: p1 :: [] ->
                    reverseAccumulatorAppendClose acc (close p0 p1)

                p0 :: p1 :: p :: rest ->
                    commonCase (basisPoint p0 p1 p :: acc) close (p1 :: p :: rest)

                _ ->
                    []
    in
    case points of
        p2 :: p3 :: p4 :: rest ->
            let
                ( p0, p1, p ) =
                    ( p2, p3, p4 )

                closing q0 q1 =
                    [ basisPoint q0 q1 p2
                    , basisPoint q1 p2 p3
                    , basisPoint p2 p3 p4
                    ]

                start =
                    Vector2d.fromTuple Quantity.float p0
                        |> Vector2d.plus (Vector2d.scaleBy 4 (Vector2d.fromTuple Quantity.float p1))
                        |> Vector2d.plus (Vector2d.fromTuple Quantity.float p)
                        |> Vector2d.scaleBy (1 / 6)
                        |> Vector2d.toTuple Quantity.toFloat
            in
            SubPath.with (moveTo start)
                [ cubicCurveTo (commonCase [] closing (p3 :: p4 :: rest)) ]

        [ p0_, p1_ ] ->
            let
                p0 =
                    Vector2d.fromTuple Quantity.float p0_

                p1 =
                    Vector2d.fromTuple Quantity.float p1_

                start =
                    Vector2d.scaleBy (1 / 3) (Vector2d.plus p0 (Vector2d.scaleBy 2 p1))
                        |> Vector2d.toTuple Quantity.toFloat

                end =
                    Vector2d.scaleBy (1 / 3) (Vector2d.plus p1 (Vector2d.scaleBy 2 p0))
                        |> Vector2d.toTuple Quantity.toFloat
            in
            SubPath.with (moveTo start) [ lineTo [ end ], closePath ]

        _ ->
            empty


{-| ![basis open](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/basisOpen.svg)
-}
basisOpen : List ( Float, Float ) -> SubPath
basisOpen points =
    let
        helper : List (Triplet ( Float, Float )) -> List ( Float, Float ) -> List (Triplet ( Float, Float ))
        helper acc remainingPoints =
            case remainingPoints of
                p0 :: p1 :: p :: rest ->
                    helper (basisPoint p0 p1 p :: acc) (p1 :: p :: rest)

                _ ->
                    List.reverse acc
    in
    case points of
        p0 :: p1 :: p :: pp :: rest ->
            let
                start =
                    Vector2d.fromTuple Quantity.float p0
                        |> Vector2d.plus (Vector2d.scaleBy 4 (Vector2d.fromTuple Quantity.float p1))
                        |> Vector2d.plus (Vector2d.fromTuple Quantity.float p)
                        |> Vector2d.scaleBy (1 / 6)
                        |> Vector2d.toTuple Quantity.toFloat
            in
            SubPath.with (moveTo start) [ cubicCurveTo (helper [] (p1 :: p :: pp :: rest)) ]

        _ ->
            empty


{-| ![bundle](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/bundle.svg)
-}
bundle : Float -> List ( Float, Float ) -> SubPath
bundle beta points =
    case List.map (Vector2d.fromTuple Quantity.float) points of
        [] ->
            empty

        [ _ ] ->
            empty

        [ _, _ ] ->
            empty

        p0 :: rest ->
            let
                j =
                    (List.length points - 1)
                        |> toFloat

                deltas =
                    (List.last rest |> Maybe.withDefault p0) |> Vector2d.minus p0

                helper i p =
                    let
                        t =
                            toFloat i / j

                        newPoint =
                            deltas
                                |> Vector2d.scaleBy t
                                |> Vector2d.plus p0
                    in
                    Vector2d.interpolateFrom p newPoint beta
                        |> Vector2d.toTuple Quantity.toFloat
            in
            List.indexedMap helper (p0 :: rest)
                |> basis


cardinalPoint : Float -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Triplet ( Float, Float )
cardinalPoint k p0 p1 p2 p =
    cardinalPointHelper k
        (Vector2d.fromTuple Quantity.float p0)
        (Vector2d.fromTuple Quantity.float p1)
        (Vector2d.fromTuple Quantity.float p2)
        (Vector2d.fromTuple Quantity.float p)
        |> mapTriplet (Vector2d.toTuple Quantity.toFloat)


cardinalPointHelper :
    Float
    -> Vector2d Unitless coordinates
    -> Vector2d Unitless coordinates
    -> Vector2d Unitless coordinates
    -> Vector2d Unitless coordinates
    -> Triplet (Vector2d Unitless coordinates)
cardinalPointHelper k p0 p1 p2 p =
    ( p2
        |> Vector2d.minus p0
        |> Vector2d.scaleBy k
        |> Vector2d.plus p1
    , p1
        |> Vector2d.minus p
        |> Vector2d.scaleBy k
        |> Vector2d.plus p2
    , p2
    )


{-| ![cardinal](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/cardinal.svg)
-}
cardinal : Float -> List ( Float, Float ) -> SubPath
cardinal tension points =
    let
        k =
            (1 - tension) / 6

        helper : List (Triplet ( Float, Float )) -> List ( Float, Float ) -> List (Triplet ( Float, Float ))
        helper acc remainingPoints =
            case remainingPoints of
                p0 :: p1 :: p2 :: p3 :: rest ->
                    helper (cardinalPoint k p0 p1 p2 p3 :: acc) (p1 :: p2 :: p3 :: rest)

                [ p0, p1, p2 ] ->
                    List.reverse (cardinalPoint k p0 p1 p2 p1 :: acc)

                _ ->
                    []
    in
    case points of
        [ p0, p1 ] ->
            SubPath.with (moveTo p0) [ lineTo [ p1 ] ]

        p0 :: p1 :: p2 :: rest ->
            SubPath.with (moveTo p0) [ cubicCurveTo (cardinalPoint k p1 p0 p1 p2 :: helper [] points) ]

        _ ->
            empty


{-| ![cardinal open](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/cardinalOpen.svg)
-}
cardinalOpen : Float -> List ( Float, Float ) -> SubPath
cardinalOpen tension points =
    let
        k =
            (1 - tension) / 6
    in
    case points of
        p0 :: p1 :: p2 :: p3 :: rest ->
            List.map4 (cardinalPoint k) (p0 :: p1 :: p2 :: p3 :: rest) (p1 :: p2 :: p3 :: rest) (p2 :: p3 :: rest) (p3 :: rest)
                |> cubicCurveTo
                |> List.singleton
                |> SubPath.with (moveTo p1)

        _ ->
            empty


{-| ![cardinal closed](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/cardinalClosed.svg)
-}
cardinalClosed : Float -> List ( Float, Float ) -> SubPath
cardinalClosed tension points =
    let
        k =
            (1 - tension) / 6

        helper acc ending remainingPoints =
            case remainingPoints of
                [ p0, p1, p2 ] ->
                    reverseAccumulatorAppendClose acc (ending p0 p1 p2)

                p0 :: p1 :: p2 :: p3 :: rest ->
                    helper (cardinalPoint k p0 p1 p2 p3 :: acc) ending (p1 :: p2 :: p3 :: rest)

                _ ->
                    []
    in
    case points of
        [] ->
            empty

        [ p ] ->
            empty

        [ p0, p1 ] ->
            SubPath.with (moveTo p1) [ lineTo [ p0 ], closePath ]

        p3 :: p4 :: p5 :: rest ->
            let
                end p0 p1 p2 =
                    [ cardinalPoint k p0 p1 p2 p3
                    , cardinalPoint k p1 p2 p3 p4
                    , cardinalPoint k p2 p3 p4 p5
                    ]
            in
            SubPath.with (moveTo p4) [ cubicCurveTo (helper [] end points) ]


catmullRomDistance : Float -> Vector2d Unitless coordinates -> Vector2d Unitless coordinates -> ( Float, Float )
catmullRomDistance alpha p1 p2 =
    let
        (Quantity.Quantity length) =
            Vector2d.length (p1 |> Vector2d.minus p2)

        l23_2a =
            (length * length) ^ alpha
    in
    ( sqrt l23_2a, l23_2a )


{-| ![catmull rom](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/catmullRom.svg)
-}
catmullRom : Float -> List ( Float, Float ) -> SubPath
catmullRom alpha points =
    if alpha == 0 then
        cardinal 0 points

    else
        case points of
            [ p1, p2 ] ->
                SubPath.with (moveTo p1) [ lineTo [ p2 ] ]

            p0 :: p1 :: p2 :: rest ->
                let
                    ending q0 q1 q2 =
                        [ catmullRomPoint alpha q0 q1 q2 q2 ]
                in
                SubPath.with (moveTo p0) [ cubicCurveTo (catmullRomHelper alpha ending (p0 :: points) []) ]

            _ ->
                empty


catmullRomHelper :
    Float
    -> (( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> List (Triplet ( Float, Float )))
    -> List ( Float, Float )
    -> List (Triplet ( Float, Float ))
    -> List (Triplet ( Float, Float ))
catmullRomHelper alpha ending points accumulator =
    case points of
        p0 :: p1 :: p2 :: [] ->
            reverseAccumulatorAppendClose accumulator (ending p0 p1 p2)

        p0 :: p1 :: p2 :: p :: rest ->
            catmullRomHelper alpha ending (p1 :: p2 :: p :: rest) (catmullRomPoint alpha p0 p1 p2 p :: accumulator)

        _ ->
            []


{-| ![catmull rom open](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/catmullRomOpen.svg)
-}
catmullRomOpen : Float -> List ( Float, Float ) -> SubPath
catmullRomOpen alpha points =
    if alpha == 0 then
        cardinalOpen 0 points

    else
        case points of
            [ p0, p1, p2 ] ->
                SubPath.with (moveTo p1) [ closePath ]

            p0 :: p1 :: p2 :: p :: rest ->
                let
                    ending _ _ _ =
                        []
                in
                SubPath.with (moveTo p1) [ cubicCurveTo (catmullRomHelper alpha ending points []) ]

            _ ->
                empty


{-| ![catmull rom closed](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/catmullRomClosed.svg)
-}
catmullRomClosed : Float -> List ( Float, Float ) -> SubPath
catmullRomClosed alpha points =
    if alpha == 0 then
        cardinalClosed 0 points

    else
        case points of
            [] ->
                empty

            [ p ] ->
                empty

            [ p1, p2 ] ->
                SubPath.with (moveTo p2) [ lineTo [ p1 ], closePath ]

            p3 :: p4 :: p5 :: rest ->
                let
                    ending p0 p1 p2 =
                        [ catmullRomPoint alpha p0 p1 p2 p3
                        , catmullRomPoint alpha p1 p2 p3 p4
                        , catmullRomPoint alpha p2 p3 p4 p5
                        ]
                in
                SubPath.with (moveTo p4) [ cubicCurveTo (catmullRomHelper alpha ending points []) ]


catmullRomPoint : Float -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Triplet ( Float, Float )
catmullRomPoint alpha p0 p1 p2 p3 =
    catmullRomPointHelper alpha
        (Vector2d.fromTuple Quantity.float p0)
        (Vector2d.fromTuple Quantity.float p1)
        (Vector2d.fromTuple Quantity.float p2)
        (Vector2d.fromTuple Quantity.float p3)
        |> mapTriplet (Vector2d.toTuple Quantity.toFloat)


catmullRomPointHelper :
    Float
    -> Vector2d Unitless coordinates
    -> Vector2d Unitless coordinates
    -> Vector2d Unitless coordinates
    -> Vector2d Unitless coordinates
    -> Triplet (Vector2d Unitless coordinates)
catmullRomPointHelper alpha p0 p1 p2 p3 =
    let
        ( l01_a, l01_2a ) =
            catmullRomDistance alpha p0 p1

        ( l12_a, l12_2a ) =
            catmullRomDistance alpha p1 p2

        ( l23_a, l23_2a ) =
            catmullRomDistance alpha p2 p3

        helper1 : Vector2d Unitless coordinates -> Vector2d Unitless coordinates
        helper1 p =
            let
                a =
                    2 * l01_2a + 3 * l01_a * l12_a + l12_2a

                n =
                    3 * l01_a * (l01_a + l12_a)
            in
            Vector2d.scaleBy a p
                |> Vector2d.minus (Vector2d.scaleBy l12_2a p0)
                |> Vector2d.plus (Vector2d.scaleBy l01_2a p2)
                |> Vector2d.scaleBy (1 / n)

        helper2 : Vector2d Unitless coordinates -> Vector2d Unitless coordinates
        helper2 p =
            let
                b =
                    2 * l23_2a + 3 * l23_a * l12_a + l12_2a

                m =
                    3 * l23_a * (l23_a + l12_a)
            in
            Vector2d.scaleBy b p
                |> Vector2d.plus (Vector2d.scaleBy l23_2a p1)
                |> Vector2d.plus (Vector2d.scaleBy -l12_2a p3)
                |> Vector2d.scaleBy (1 / m)

        control1 =
            if l01_a > epsilon then
                helper1 p1

            else
                p1

        control2 =
            if l23_a > epsilon then
                helper2 p2

            else
                p2
    in
    ( control1, control2, p2 )


slope2 : Vector2d Unitless coordinates -> Vector2d Unitless coordinates -> Float -> Float
slope2 p0 p1 t =
    let
        ( dx, dy ) =
            p0
                |> Vector2d.minus p1
                |> Vector2d.toTuple Quantity.toFloat
    in
    if dx /= 0 then
        (3 * dy / dx - t) / 2

    else
        t


slope3 :
    Vector2d Unitless coordinates
    -> Vector2d Unitless coordinates
    -> Vector2d Unitless coordinates
    -> Float
slope3 p0 p1 p2 =
    let
        ( dx1, dy1 ) =
            p1
                |> Vector2d.minus p0
                |> Vector2d.toTuple Quantity.toFloat

        ( dx2, dy2 ) =
            p2
                |> Vector2d.minus p1
                |> Vector2d.toTuple Quantity.toFloat

        ( s0h, s1h ) =
            ( toH dx1 dx2, toH dx2 dx1 )

        s0 =
            dy1 / s0h

        s1 =
            dy2 / s1h

        p =
            (s0 * dx2 + s1 * dx1) / (dx1 + dx2)
    in
    (sign s0 + sign s1) * Basics.min (Basics.min (Basics.abs s0) (Basics.abs s1)) (0.5 * Basics.abs p)


monotonePoint : ( Float, Float ) -> ( Float, Float ) -> Float -> Float -> Triplet ( Float, Float )
monotonePoint ( x0, y0 ) ( x1, y1 ) t0 t1 =
    let
        dx =
            (x1 - x0) / 3
    in
    ( ( x0 + dx, y0 + dx * t0 )
    , ( x1 - dx, y1 - dx * t1 )
    , ( x1, y1 )
    )


{-| ![monotone in x](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/monotoneX.svg)
-}
monotoneX : List ( Float, Float ) -> SubPath
monotoneX points =
    case points of
        p0 :: p1 :: p :: rest ->
            let
                t1 : Float
                t1 =
                    slope3
                        (Vector2d.fromTuple Quantity.float p0)
                        (Vector2d.fromTuple Quantity.float p1)
                        (Vector2d.fromTuple Quantity.float p)

                initialInstruction : Triplet ( Float, Float )
                initialInstruction =
                    monotonePoint p0
                        p1
                        (slope2
                            (Vector2d.fromTuple Quantity.float p0)
                            (Vector2d.fromTuple Quantity.float p1)
                            t1
                        )
                        t1

                otherInstructions =
                    monotoneXHelper [] t1 (p1 :: p :: rest)
            in
            SubPath.with (moveTo p0) [ cubicCurveTo (initialInstruction :: otherInstructions) ]

        [ p0, p1 ] ->
            SubPath.with (moveTo p0) [ lineTo [ p1 ] ]

        _ ->
            empty


monotoneXHelper : List (Triplet ( Float, Float )) -> Float -> List ( Float, Float ) -> List (Triplet ( Float, Float ))
monotoneXHelper acc t0 remaininPoints =
    case remaininPoints of
        p0 :: p1 :: p :: rest ->
            let
                t1 : Float
                t1 =
                    slope3
                        (Vector2d.fromTuple Quantity.float p0)
                        (Vector2d.fromTuple Quantity.float p1)
                        (Vector2d.fromTuple Quantity.float p)
            in
            monotoneXHelper (monotonePoint p0 p1 t0 t1 :: acc) t1 (p1 :: p :: rest)

        [ p0, p1 ] ->
            let
                t1 : Float
                t1 =
                    slope2
                        (Vector2d.fromTuple Quantity.float p0)
                        (Vector2d.fromTuple Quantity.float p1)
                        t0
            in
            List.reverse (monotonePoint p0 p1 t0 t1 :: acc)

        _ ->
            []


{-| ![monotone in y](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/monotoneY.svg)
-}
monotoneY : List ( Float, Float ) -> SubPath
monotoneY points =
    points
        |> List.map (\( x, y ) -> ( y, x ))
        |> monotoneX
        |> SubPath.mapCoordinate (\( x, y ) -> ( y, x ))


toH : Float -> Float -> Float
toH h0 h1 =
    if h0 == 0 then
        if h1 < 0 then
            0 * -1

        else
            h1

    else
        h0


{-| ![natural](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/natural.svg)
-}
natural : List ( Float, Float ) -> SubPath
natural points =
    case points of
        [] ->
            empty

        [ x ] ->
            empty

        [ p1, p2 ] ->
            SubPath.with (moveTo p1) [ lineTo [ p2 ] ]

        p :: _ :: _ ->
            let
                cubicTriplets =
                    points
                        |> List.map (Vector2d.fromTuple Quantity.float)
                        |> naturalControlPoints
                        |> List.map (mapTriplet (Vector2d.toTuple Quantity.toFloat))
            in
            SubPath.with (moveTo p) [ cubicCurveTo cubicTriplets ]


{-| ![step](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/step.svg)
-}
step : Float -> List ( Float, Float ) -> SubPath
step factor points =
    let
        helper ( x0, y0 ) ( x, y ) =
            if factor <= 0 then
                [ ( x0, y ), ( x, y ) ]

            else
                let
                    x1 =
                        x0 * (1 - factor) + x * factor
                in
                [ ( x1, y0 ), ( x1, y ) ]
    in
    case points of
        [] ->
            empty

        p :: ps ->
            p
                :: (List.concat (List.map2 helper points ps) ++ [ List.last ps |> Maybe.withDefault p ])
                |> linear


{-| ![step before](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/stepBefore.svg)
-}
stepBefore : List ( Float, Float ) -> SubPath
stepBefore =
    step 0


{-| ![step after](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/stepAfter.svg)
-}
stepAfter : List ( Float, Float ) -> SubPath
stepAfter =
    step 1


{-| Repeat the first element of a list

This is sometimes useful for curves that don't go through their first control point (catmullRom, cardinal). Repeating the first point
makes the curve actually hit the first control point.

-}
repeatFirstPoint : List a -> List a
repeatFirstPoint items =
    case items of
        [] ->
            []

        x :: xs ->
            x :: items


{-| Repeat the final element of a list

Similar to `repeatFirstPoint`, this can be used to make some curves hit their final control point.

-}
repeatFinalPoint : List a -> List a
repeatFinalPoint items =
    case List.last items of
        Nothing ->
            items

        Just x ->
            items ++ [ x ]


{-| Reverse the accumulator (it is stored in reverse to benefit from tail-recursion) and
append the closing point(s) at the back.

For derivation see [derivation]

[derivation]: https://github.com/folkertdev/one-true-path-experiment/pull/4

-}
reverseAccumulatorAppendClose : List a -> List a -> List a
reverseAccumulatorAppendClose accumulator close =
    List.foldl (::) close accumulator
