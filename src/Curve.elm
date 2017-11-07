module Curve
    exposing
        ( basis
        , basisClosed
        , basisOpen
        , bundle
        , cardinal
        , cardinalClosed
        , cardinalOpen
        , catmullRom
        , catmullRomClosed
        , catmullRomOpen
        , cubicBezier
        , linear
        , linearClosed
        , monotoneX
        , monotoneY
        , natural
        , quadraticBezier
        , radial
        , repeatFinalPoint
        , repeatFirstPoint
        , smoothCubicBezier
        , smoothQuadraticBezier
        , step
        , stepAfter
        , stepBefore
        , toPolarWithCenter
        )

{-| Construct curves from a set of points.

The problem of drawing a line through a set of points is actually quite tricky. Should the curve be smooth? Should the ends be connected?
This module gives many options for drawing lines through points.

Supports all the curves defined by [D3 Shape](https://github.com/d3/d3-shape#curves).


## Linear

@docs linear, linearClosed


## Bezier

@docs cubicBezier , smoothCubicBezier , quadraticBezier , smoothQuadraticBezier


## Step

@docs step, stepBefore, stepAfter


## Basis

@docs basis, basisClosed, basisOpen, bundle


## Cardinal

@docs cardinal, cardinalClosed, cardinalOpen


## Catmull-Rom

Catmull-Rom splines are a special case of cardinal splines. These curves are great for animation, because the data points are
hit exactly and the curve is smooth.

@docs catmullRom, catmullRomClosed, catmullRomOpen


## Monotone

The monotone curves can only be increasing (staying flat or becoming higher) or decreasing (staying flat or becoming lower) between any two adjacent points.
It cannot first go down and then go up.

<img style="max-width: 100%;" src="https://upload.wikimedia.org/wikipedia/en/f/fe/MonotCubInt.png" />

Notice that around 0.45, the cubic interpolation dives below the y-coordinate of the next point, whereas the monotone interpolation does not.

A nice consequence is that there are no weird bumps in the curve between the data points.

@docs monotoneX, monotoneY


## Natural

@docs natural


## Transformations

@docs repeatFirstPoint , repeatFinalPoint
@docs radial, toPolarWithCenter

-}

import List.Extra as List
import LowLevel.Command as Command exposing (..)
import SubPath exposing (SubPath(..), close, connect, empty, subpath)
import Vector2 as Vec2 exposing (Vec2)
import Vector3 exposing (Vec3)
import Internal.NaturalInterpolation exposing (naturalControlPoints)
import Path.LowLevel as LowLevel exposing (Mode(Absolute))


epsilon : Float
epsilon =
    1.0e-12


sign : comparable -> number
sign x =
    if x < 0 then
        -1
    else
        1


type alias Triplet a =
    ( a, a, a )


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


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


{-| Draw straight lines between the data points.

<img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/linear.svg" />

-}
linear : List (Vec2 Float) -> SubPath
linear points =
    case points of
        [] ->
            empty

        x :: xs ->
            subpath (moveTo x) [ lineTo xs ]


{-| Shorthand to draw a sequence of cubic bezier segments
-}
cubicBezier : Vec2 Float -> List ( Vec2 Float, Vec2 Float, Vec2 Float ) -> SubPath
cubicBezier start points =
    case points of
        [] ->
            empty

        x :: xs ->
            subpath (moveTo start) [ cubicCurveTo points ]


{-| Shorthand to draw a sequence of smooth cubic bezier segments
-}
smoothCubicBezier : Vec2 Float -> ( Vec2 Float, Vec2 Float, Vec2 Float ) -> List ( Vec2 Float, Vec2 Float ) -> SubPath
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
quadraticBezier : Vec2 Float -> List ( Vec2 Float, Vec2 Float ) -> SubPath
quadraticBezier start points =
    case points of
        [] ->
            empty

        x :: xs ->
            subpath (moveTo start) [ quadraticCurveTo points ]


{-| Shorthand to draw a sequence of smooth quadratic bezier segments
-}
smoothQuadraticBezier : Vec2 Float -> ( Vec2 Float, Vec2 Float ) -> List (Vec2 Float) -> SubPath
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


{-| Draw a straigt line between the data points, connecting the ends.

<img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/linearClosed.svg" />

-}
linearClosed : List (Vec2 Float) -> SubPath
linearClosed points =
    case points of
        [] ->
            empty

        x :: xs ->
            subpath (moveTo x) [ lineTo xs, closePath ]


{-| Convert `(angle, radius)` pairs to `(x, y)` coordinates, relative to the given vector.

This function is used by radial and can be used to use radial with different interpolations, for instance.

    radialNatural : Vec2 Float -> List (Vec2 Float) -> SubPath
    radialNatural ( x, y ) =
        natural << toPolarWithCenter ( x, y )

-}
toPolarWithCenter : Vec2 Float -> List (Vec2 Float) -> List (Vec2 Float)
toPolarWithCenter ( x, y ) =
    List.map (\( angle, radius ) -> ( radius * sin angle + x, -radius * cos angle + y ))


{-| Interpret a 2D vector as a `(angle, radius)` pair. The angle is in radians. The first argument is the center.

<img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/radial.svg" />

-}
radial : Vec2 Float -> List (Vec2 Float) -> SubPath
radial ( x, y ) =
    linear << toPolarWithCenter ( x, y )


basisPoint : Vec2 Float -> Vec2 Float -> Vec2 Float -> Vec3 (Vec2 Float)
basisPoint p0 p1 p =
    ( Vec2.scale 2 p0
        |> Vec2.add p1
        |> Vec2.divideBy 3
    , Vec2.scale 2 p1
        |> Vec2.add p0
        |> Vec2.divideBy 3
    , Vec2.scale 4 p1
        |> Vec2.add p0
        |> Vec2.add p
        |> Vec2.divideBy 6
    )


{-| Basis interpolation (also known as B-spline)

<img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/basis.svg" />

-}
basis : List (Vec2 Float) -> SubPath
basis points =
    let
        --| The ideal case where there are at least 3 points
        commonCase points =
            case points of
                p0 :: p1 :: [] ->
                    [ cubicCurveTo [ basisPoint p0 p1 p1 ], lineTo [ p1 ] ]

                p0 :: p1 :: p :: rest ->
                    cubicCurveTo [ basisPoint p0 p1 p ] :: commonCase (p1 :: p :: rest)

                _ ->
                    []
    in
        case points of
            p0 :: p1 :: _ :: _ ->
                let
                    toFirst =
                        Vec2.scale 5 p0
                            |> Vec2.add p1
                            |> Vec2.divideBy 6
                in
                    subpath (moveTo p0) (lineTo [ toFirst ] :: commonCase points)

            [ p0, p1 ] ->
                subpath (moveTo p0) [ lineTo [ p1 ] ]

            _ ->
                empty


{-| Closed Basis interpolation (also known as B-spline)

<img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/basisClosed.svg" />

-}
basisClosed : List (Vec2 Float) -> SubPath
basisClosed points =
    let
        --| The ideal case where there are at least 3 points
        commonCase close points =
            case points of
                p0 :: p1 :: [] ->
                    close p0 p1

                p0 :: p1 :: p :: rest ->
                    basisPoint p0 p1 p :: commonCase close (p1 :: p :: rest)

                _ ->
                    []
    in
        case points of
            p2 :: p3 :: p4 :: rest ->
                let
                    ( p0, p1, p ) =
                        ( p2, p3, p4 )

                    closing p0 p1 =
                        [ basisPoint p0 p1 p2
                        , basisPoint p1 p2 p3
                        , basisPoint p2 p3 p4
                        ]

                    start =
                        p0
                            |> Vec2.add (Vec2.scale 4 p1)
                            |> Vec2.add p
                            |> Vec2.divideBy 6
                in
                    subpath (moveTo start)
                        [ cubicCurveTo (commonCase closing (p3 :: p4 :: rest)) ]

            [ p0, p1 ] ->
                subpath (moveTo p0) [ lineTo [ p1 ] ]

            _ ->
                empty


{-| <img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/basisOpen.svg" />
-}
basisOpen : List (Vec2 Float) -> SubPath
basisOpen points =
    let
        helper points =
            case points of
                p0 :: p1 :: p :: rest ->
                    basisPoint p0 p1 p :: helper (p1 :: p :: rest)

                _ ->
                    []
    in
        case points of
            p0 :: p1 :: p :: rest ->
                let
                    start =
                        p0
                            |> Vec2.add (Vec2.scale 4 p1)
                            |> Vec2.add p
                            |> Vec2.divideBy 6
                in
                    subpath (moveTo start) [ cubicCurveTo (helper (p1 :: p :: rest)) ]

            _ ->
                empty


{-| <img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/bundle.svg" />
-}
bundle : Float -> List (Vec2 Float) -> SubPath
bundle beta points =
    case points of
        [] ->
            empty

        p0 :: rest ->
            let
                j =
                    (List.length points - 1)
                        |> toFloat

                deltas =
                    Vec2.sub (List.last rest |> Maybe.withDefault p0) p0

                helper i p =
                    let
                        t =
                            toFloat i / j
                    in
                        deltas
                            |> Vec2.scale t
                            |> Vec2.add p0
                            |> interpolateVec2 beta p
            in
                List.indexedMap helper points
                    |> basis


interpolateVec2 : Float -> Vec2 Float -> Vec2 Float -> Vec2 Float
interpolateVec2 t a b =
    Vec2.add (Vec2.scale t a) (Vec2.scale (1 - t) b)


cardinalPoint : Float -> Vec2 Float -> Vec2 Float -> Vec2 Float -> Vec2 Float -> Vec3 (Vec2 Float)
cardinalPoint k p0 p1 p2 p =
    ( Vec2.sub p2 p0
        |> Vec2.scale k
        |> Vec2.add p1
    , Vec2.sub p1 p
        |> Vec2.scale k
        |> Vec2.add p2
    , p2
    )


{-| <img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/cardinal.svg" />
-}
cardinal : Float -> List (Vec2 Float) -> SubPath
cardinal tension points =
    let
        k =
            (1 - tension) / 6

        helper points =
            case points of
                p0 :: p1 :: p2 :: p3 :: rest ->
                    cardinalPoint k p0 p1 p2 p3 :: helper (p1 :: p2 :: p3 :: rest)

                [ p0, p1, p2 ] ->
                    [ cardinalPoint k p0 p1 p2 p1 ]

                _ ->
                    []
    in
        case points of
            [ p0, p1 ] ->
                subpath (moveTo p0) [ lineTo [ p1 ] ]

            p0 :: p1 :: p2 :: rest ->
                subpath (moveTo p0) [ cubicCurveTo (cardinalPoint k p0 p1 p1 p2 :: helper points) ]

            _ ->
                empty


{-| <img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/cardinalOpen.svg" />
-}
cardinalOpen : Float -> List (Vec2 Float) -> SubPath
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
                    |> subpath (moveTo p1)

            _ ->
                empty


{-| <img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/cardinalClosed.svg" />
-}
cardinalClosed : Float -> List (Vec2 Float) -> SubPath
cardinalClosed tension points =
    let
        k =
            (1 - tension) / 6

        helper ending points =
            case points of
                [ p0, p1, p2 ] ->
                    ending p0 p1 p2

                p0 :: p1 :: p2 :: p3 :: rest ->
                    cardinalPoint k p0 p1 p2 p3 :: helper ending (p1 :: p2 :: p3 :: rest)

                _ ->
                    []
    in
        case points of
            [] ->
                empty

            [ p ] ->
                subpath (moveTo p) [ closePath ]

            [ p0, p1 ] ->
                subpath (moveTo p1) [ lineTo [ p0 ], closePath ]

            p3 :: p4 :: p5 :: rest ->
                let
                    end p0 p1 p2 =
                        [ cardinalPoint k p0 p1 p2 p3
                        , cardinalPoint k p1 p2 p3 p4
                        , cardinalPoint k p2 p3 p4 p5
                        ]
                in
                    subpath (moveTo p4) [ cubicCurveTo (helper end points) ]


catmullRomDistance : Float -> Vec2 Float -> Vec2 Float -> ( Float, Float )
catmullRomDistance alpha p1 p2 =
    let
        l23_2a =
            Vec2.lengthSquared (Vec2.sub p1 p2) ^ alpha
    in
        ( sqrt l23_2a, l23_2a )


{-| <img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/catmullRom.svg" />
-}
catmullRom : Float -> List (Vec2 Float) -> SubPath
catmullRom alpha points =
    if alpha == 0 then
        cardinal 0 points
    else
        case points of
            [ p1, p2 ] ->
                subpath (moveTo p1) [ lineTo [ p2 ] ]

            p0 :: p1 :: p2 :: p :: rest ->
                let
                    ending p0 p1 p2 =
                        [ catmullRomPoint alpha p0 p1 p2 p2 ]
                in
                    subpath (moveTo p0) [ cubicCurveTo (catmullRomHelper alpha ending (p0 :: points)) ]

            _ ->
                empty


catmullRomHelper : Float -> (Vec2 Float -> Vec2 Float -> Vec2 Float -> List (Vec3 (Vec2 Float))) -> List (Vec2 Float) -> List (Vec3 (Vec2 Float))
catmullRomHelper alpha ending points =
    case points of
        p0 :: p1 :: p2 :: [] ->
            ending p0 p1 p2

        p0 :: p1 :: p2 :: p :: rest ->
            catmullRomPoint alpha p0 p1 p2 p :: catmullRomHelper alpha ending (p1 :: p2 :: p :: rest)

        _ ->
            []


{-| <img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/catmullRomOpen.svg" />
-}
catmullRomOpen : Float -> List (Vec2 Float) -> SubPath
catmullRomOpen alpha points =
    if alpha == 0 then
        cardinalOpen 0 points
    else
        case points of
            [ p0, p1, p2 ] ->
                subpath (moveTo p2) []

            p0 :: p1 :: p2 :: p :: rest ->
                let
                    ending _ _ _ =
                        []
                in
                    subpath (moveTo p1) [ cubicCurveTo (catmullRomHelper alpha ending points) ]

            _ ->
                empty


{-| <img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/catmullRomClosed.svg" />
-}
catmullRomClosed : Float -> List (Vec2 Float) -> SubPath
catmullRomClosed alpha points =
    if alpha == 0 then
        cardinalClosed 0 points
    else
        case points of
            [] ->
                empty

            [ p ] ->
                subpath (moveTo p) []

            [ p1, p2 ] ->
                subpath (moveTo p2) [ lineTo [ p1 ], closePath ]

            p3 :: p4 :: p5 :: rest ->
                let
                    ending p0 p1 p2 =
                        [ catmullRomPoint alpha p0 p1 p2 p3
                        , catmullRomPoint alpha p1 p2 p3 p4
                        , catmullRomPoint alpha p2 p3 p4 p5
                        ]
                in
                    subpath (moveTo p4) [ cubicCurveTo (catmullRomHelper alpha ending points) ]


catmullRomPoint : Float -> Vec2 Float -> Vec2 Float -> Vec2 Float -> Vec2 Float -> Vec3 (Vec2 Float)
catmullRomPoint alpha p0 p1 p2 p3 =
    let
        ( l01_a, l01_2a ) =
            catmullRomDistance alpha p0 p1

        ( l12_a, l12_2a ) =
            catmullRomDistance alpha p1 p2

        ( l23_a, l23_2a ) =
            catmullRomDistance alpha p2 p3

        helper1 : Vec2 Float -> Vec2 Float
        helper1 p =
            let
                a =
                    2 * l01_2a + 3 * l01_a * l12_a + l12_2a

                n =
                    3 * l01_a * (l01_a + l12_a)
            in
                Vec2.sub (Vec2.scale a p) (Vec2.scale l12_2a p0)
                    |> Vec2.add (Vec2.scale l01_2a p2)
                    |> Vec2.divideBy n

        helper2 : Vec2 Float -> Vec2 Float
        helper2 p =
            let
                b =
                    2 * l23_2a + 3 * l23_a * l12_a + l12_2a

                m =
                    3 * l23_a * (l23_a + l12_a)
            in
                Vec2.scale b p
                    |> Vec2.add (Vec2.scale l23_2a p1)
                    |> Vec2.add (Vec2.scale -l12_2a p3)
                    |> Vec2.divideBy m

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


slope2 : Vec2 Float -> Vec2 Float -> Float -> Float
slope2 p0 p1 t =
    let
        ( dx, dy ) =
            Vec2.sub p0 p1
    in
        if dx /= 0 then
            (3 * dy / dx - t) / 2
        else
            t


slope3 : Vec2 Float -> Vec2 Float -> Vec2 Float -> Float
slope3 (( x0, y0 ) as p0) (( x1, y1 ) as p1) (( x2, y2 ) as p2) =
    let
        ( dx1, dy1 ) =
            Vec2.sub p1 p0

        ( dx2, dy2 ) =
            Vec2.sub p2 p1

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


monotonePoint : Vec2 Float -> Vec2 Float -> Float -> Float -> Vec3 (Vec2 Float)
monotonePoint ( x0, y0 ) ( x1, y1 ) t0 t1 =
    let
        dx =
            (x1 - x0) / 3
    in
        ( ( x0 + dx, y0 + dx * t0 )
        , ( x1 - dx, y1 - dx * t1 )
        , ( x1, y1 )
        )


{-| Draw a curve monotone in y assuming the points are monotone in x.
<img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/monotoneX.svg" />

Note, does not deal well with coincident points

-}
monotoneX : List (Vec2 Float) -> SubPath
monotoneX points =
    case points of
        p0 :: p1 :: p :: rest ->
            let
                t1 =
                    slope3 p0 p1 p

                initialInstruction =
                    monotonePoint p0 p1 (slope2 p0 p1 t1) t1

                helper t0 points =
                    case points of
                        p0 :: p1 :: p :: rest ->
                            let
                                t1 =
                                    slope3 p0 p1 p
                            in
                                monotonePoint p0 p1 t0 t1 :: helper t1 (p1 :: p :: rest)

                        [ p0, p1 ] ->
                            [ monotonePoint p0 p1 t0 (slope2 p0 p1 t0) ]

                        _ ->
                            []
            in
                subpath (moveTo p0) [ cubicCurveTo (initialInstruction :: helper t1 (p1 :: p :: rest)) ]

        [ p0, p1 ] ->
            subpath (moveTo p0) [ lineTo [ p1 ] ]

        _ ->
            empty


{-| Draw a curve monotone in y assuming the points are monotone in x.
<img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/monotoneY.svg" />
-}
monotoneY : List (Vec2 Float) -> SubPath
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


{-| <img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/natural.svg" />
-}
natural : List (Vec2 Float) -> SubPath
natural points =
    case points of
        [] ->
            empty

        [ x ] ->
            empty

        [ p1, p2 ] ->
            subpath (moveTo p1) [ lineTo [ p2 ] ]

        p :: _ :: _ ->
            subpath (moveTo p) [ cubicCurveTo (naturalControlPoints points) ]


{-| Step goes some distance to the right, then to the y-coordinate of the next data point, and then draws to the next point.

The first argument determines where the step is.

  - `step 1 points` is `stepAfter`
  - `step 0 points` is `stepBefore`
  - `step 0.5 points` steps exactly in the middle

<img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/step.svg" />

-}
step : Float -> List (Vec2 Float) -> SubPath
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


{-| <img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/stepBefore.svg" />
-}
stepBefore : List (Vec2 Float) -> SubPath
stepBefore =
    step 0


{-| <img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/stepAfter.svg" />
-}
stepAfter : List (Vec2 Float) -> SubPath
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
