module Curve
    exposing
        ( linear
        , linearClosed
        , step
        , stepBefore
        , stepAfter
        , basis
        , basisClosed
        , basisOpen
        , bundle
        , cardinal
        , cardinalClosed
        , cardinalOpen
        , catmullRom
        , catmullRomClosed
        , catmullRomOpen
        , monotoneX
        , monotoneY
        , natural
        , radial
        , toPolarWithCenter
        , naturalControlPoints
        )

{-|

## Linear

@docs linear, linearClosed

## Step

@docs step, stepBefore, stepAfter

## Basis

@docs basis, basisClosed, basisOpen, bundle

## Cardinal

@docs cardinal, cardinalClosed, cardinalOpen

## Catmull-Rom

@docs catmullRom, catmullRomClosed, catmullRomOpen

## Monotone

@docs monotoneX, monotoneY

## Natural

@docs natural

## Transformations

@docs radial, toPolarWithCenter

## WIP TEST
@docs naturalControlPoints

-}

import Path exposing (..)
import List.Extra as List
import Vector2 as Vec2 exposing (Vec2)
import Vector3 exposing (Vec3)
import List.Extra as List


epsilon : Float
epsilon =
    1.0e-12


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


{-| -}
linear : List (Vec2 Float) -> Path
linear points =
    case points of
        [] ->
            []

        x :: xs ->
            [ subpath (moveTo x) [ lineTo xs ] ]


{-| -}
linearClosed : List (Vec2 Float) -> Path
linearClosed points =
    case points of
        [] ->
            []

        x :: xs ->
            [ subpath (moveTo x) [ lineTo xs, closePath ] ]


{-| -}
toPolarWithCenter : Vec2 Float -> List (Vec2 Float) -> List (Vec2 Float)
toPolarWithCenter ( x, y ) =
    List.map (\( angle, radius ) -> ( radius * sin angle + x, -radius * cos angle + y ))


{-| -}
radial : Vec2 Float -> List (Vec2 Float) -> Path
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
-}
basis : List (Vec2 Float) -> Path
basis points =
    let
        --| The ideal case where there are at least 3 points
        commonCase points =
            case points of
                p0 :: p1 :: [] ->
                    [ cubicCurveTo [ basisPoint p0 p1 p1 ], lineTo [ p1 ] ]

                p0 :: p1 :: p :: rest ->
                    (cubicCurveTo [ basisPoint p0 p1 p ]) :: commonCase (p1 :: p :: rest)

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
                    [ subpath (moveTo p0) (lineTo [ toFirst ] :: commonCase points) ]

            [ p0, p1 ] ->
                [ subpath (moveTo p0) [ lineTo [ p1 ] ] ]

            _ ->
                []


{-| Closed Basis interpolation (also known as B-spline)
-}
basisClosed : List (Vec2 Float) -> Path
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
                    [ subpath (moveTo start) [ cubicCurveTo (commonCase closing (p3 :: p4 :: rest)) ]
                    ]

            [ p0, p1 ] ->
                [ subpath (moveTo p0) [ lineTo [ p1 ] ] ]

            _ ->
                []


{-| -}
basisOpen : List (Vec2 Float) -> Path
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
                    [ subpath (moveTo start) [ cubicCurveTo (helper (p1 :: p :: rest)) ] ]

            _ ->
                []


{-| -}
bundle : Float -> List (Vec2 Float) -> Path
bundle beta points =
    case points of
        [] ->
            []

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


{-| -}
cardinal : Float -> List (Vec2 Float) -> Path
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
                [ subpath (moveTo p0) [ lineTo [ p1 ] ] ]

            p0 :: p1 :: p2 :: rest ->
                [ subpath (moveTo p0) [ cubicCurveTo (cardinalPoint k p0 p1 p1 p2 :: helper points) ] ]

            _ ->
                []


{-| -}
cardinalOpen : Float -> List (Vec2 Float) -> Path
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
                    |> List.singleton

            _ ->
                []


{-| -}
cardinalClosed : Float -> List (Vec2 Float) -> Path
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
                []

            [ p ] ->
                [ subpath (moveTo p) [ closePath ] ]

            [ p0, p1 ] ->
                [ subpath (moveTo p1) [ lineTo [ p0 ], closePath ] ]

            p3 :: p4 :: p5 :: rest ->
                let
                    end p0 p1 p2 =
                        [ cardinalPoint k p0 p1 p2 p3
                        , cardinalPoint k p1 p2 p3 p4
                        , cardinalPoint k p2 p3 p4 p5
                        ]
                in
                    [ subpath (moveTo p4) [ cubicCurveTo (helper end points) ] ]


catmullRomDistance : Float -> Vec2 Float -> Vec2 Float -> ( Float, Float )
catmullRomDistance alpha p1 p2 =
    let
        l23_2a =
            (Vec2.lengthSquared (Vec2.sub p1 p2)) ^ alpha
    in
        ( sqrt l23_2a, l23_2a )


{-| -}
catmullRom : Float -> List (Vec2 Float) -> Path
catmullRom alpha points =
    if alpha == 0 then
        cardinal 0 points
    else
        case points of
            [ p1, p2 ] ->
                [ subpath (moveTo p1) [ lineTo [ p2 ] ] ]

            p0 :: p1 :: p2 :: p :: rest ->
                let
                    ending p0 p1 p2 =
                        [ catmullRomPoint alpha p0 p1 p2 p2 ]
                in
                    [ subpath (moveTo p0) [ cubicCurveTo (catmullRomHelper alpha ending (p0 :: points)) ] ]

            _ ->
                []


catmullRomHelper : Float -> (Vec2 Float -> Vec2 Float -> Vec2 Float -> List (Vec3 (Vec2 Float))) -> List (Vec2 Float) -> List (Vec3 (Vec2 Float))
catmullRomHelper alpha ending points =
    case points of
        p0 :: p1 :: p2 :: [] ->
            ending p0 p1 p2

        p0 :: p1 :: p2 :: p :: rest ->
            catmullRomPoint alpha p0 p1 p2 p :: catmullRomHelper alpha ending (p1 :: p2 :: p :: rest)

        _ ->
            []


{-| -}
catmullRomOpen : Float -> List (Vec2 Float) -> Path
catmullRomOpen alpha points =
    if alpha == 0 then
        cardinalOpen 0 points
    else
        case points of
            [ p0, p1, p2 ] ->
                [ subpath (moveTo p2) [] ]

            p0 :: p1 :: p2 :: p :: rest ->
                let
                    ending _ _ _ =
                        []
                in
                    [ subpath (moveTo p1) [ cubicCurveTo (catmullRomHelper alpha ending points) ] ]

            _ ->
                []


{-| -}
catmullRomClosed : Float -> List (Vec2 Float) -> Path
catmullRomClosed alpha points =
    if alpha == 0 then
        cardinalClosed 0 points
    else
        case points of
            [] ->
                []

            [ p ] ->
                [ subpath (moveTo p) [] ]

            [ p1, p2 ] ->
                [ subpath (moveTo p2) [ lineTo [ p1 ], closePath ] ]

            p3 :: p4 :: p5 :: rest ->
                let
                    ending p0 p1 p2 =
                        [ catmullRomPoint alpha p0 p1 p2 p3
                        , catmullRomPoint alpha p1 p2 p3 p4
                        , catmullRomPoint alpha p2 p3 p4 p5
                        ]
                in
                    [ subpath (moveTo p4) [ cubicCurveTo (catmullRomHelper alpha ending points) ] ]


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
        (sign s0 + sign s1) * Basics.min (Basics.min (Basics.abs s0) (Basics.abs s1)) (0.5 * Basics.abs (p))


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


{-| Note, does not deal well with coincident points
-}
monotoneX : List (Vec2 Float) -> Path
monotoneX points =
    case points of
        p0 :: p1 :: p :: rest ->
            let
                t1 =
                    slope3 p0 p1 p

                initialInstruction =
                    monotonePoint p0 p1 t1 t1

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
                [ subpath (moveTo p0) [ cubicCurveTo (initialInstruction :: helper t1 (p1 :: p :: rest)) ] ]

        [ p0, p1 ] ->
            [ subpath (moveTo p0) [ lineTo [ p1 ] ] ]

        _ ->
            []


{-| -}
monotoneY : List (Vec2 Float) -> Path
monotoneY points =
    points
        |> List.map (\( x, y ) -> ( y, x ))
        |> monotoneX
        |> Path.mapCoordinate (\( x, y ) -> ( y, x ))


toH : Float -> Float -> Float
toH h0 h1 =
    if h0 == 0 then
        if h1 < 0 then
            0 * -1
        else
            h1
    else
        h0


step1 : List Float -> Maybe ( List number, List number1, List Float )
step1 coordinates =
    case coordinates of
        x0 :: x1 :: rest ->
            let
                n =
                    List.length coordinates - 1

                r =
                    (x0 + 2 * x1) :: List.map2 (\current next -> 4 * current + 2 * next) (x1 :: rest) rest

                a =
                    0 :: List.repeat (n - 2) 1 ++ [ 2 ]

                b =
                    2 :: List.repeat (n - 2) 4 ++ [ 7 ]

                ( butFinal, final ) =
                    List.foldl (\elem ( _, previous ) -> ( previous, elem )) ( x0, x1 ) rest

                r_ =
                    List.updateAt (n - 1) (\_ -> 8 * butFinal + final) r
                        |> Maybe.withDefault r
            in
                Just ( a, b, r_ )

        _ ->
            Nothing


step2 : ( List Float, List Float, List Float ) -> Maybe ( List Float, List Float, List Float )
step2 ( a, b, r ) =
    case ( b, r ) of
        ( firstB :: _, firstR :: _ ) ->
            let
                scanner ( a, b, r ) ( prevB, prevR ) =
                    ( b - (a / prevB)
                    , r - (a / prevB) * prevR
                    )

                ( b_, r_ ) =
                    List.scanl scanner ( firstB, firstR ) (List.map3 (,,) a b r)
                        |> List.drop 1
                        |> List.unzip
            in
                Just ( a, b_, r_ )

        _ ->
            Nothing


step3 : List Float -> ( List Float, List Float, List Float ) -> Maybe ( List Float, List Float )
step3 points ( a, b, r ) =
    let
        helper finalR finalB finalX =
            let
                finalA =
                    finalR / finalB

                scanner ( b, r ) prevA =
                    (r - prevA) / b

                a_ =
                    List.scanr scanner finalA (List.map2 (,) (unsafeInit b) (unsafeInit r))

                b_ =
                    (List.map2 (\xx aa -> 2 * xx - aa) (unsafeTail points) (unsafeTail a_)) ++ [ (finalX + finalA) / 2 ]
            in
                ( a_, b_ )
    in
        Maybe.map3 helper (List.last r) (List.last b) (List.last points)


controlPoints : List Float -> Maybe ( List Float, List Float )
controlPoints points =
    step1 points
        |> Maybe.andThen step2
        |> Maybe.andThen (step3 points)


unsafeInit =
    Maybe.withDefault [] << List.init


unsafeTail =
    Maybe.withDefault [] << List.tail


{-| -}
natural : List (Vec2 Float) -> Path
natural points =
    case points of
        [] ->
            []

        [ x ] ->
            []

        [ p1, p2 ] ->
            [ subpath (moveTo p1) [ lineTo [ p2 ] ] ]

        p :: _ :: _ ->
            [ subpath (moveTo p) [ cubicCurveTo (naturalControlPoints points) ] ]


{-| calculate the control points for natural spline interpolation
-}
naturalControlPoints : List (Vec2 Float) -> List (Vec3 (Vec2 Float))
naturalControlPoints points =
    let
        ( xs, ys ) =
            List.unzip points
    in
        case Maybe.map2 (,) (controlPoints xs) (controlPoints ys) of
            Just ( ( px0, px1 ), ( py0, py1 ) ) ->
                let
                    pa =
                        List.map2 (,) px0 py0

                    pb =
                        List.map2 (,) px1 py1
                in
                    List.map3 (,,) pa pb (List.drop 1 points)

            Nothing ->
                []


{-| -}
step : Float -> List (Vec2 Float) -> Path
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
                []

            p :: ps ->
                p
                    :: (List.concat (List.map2 helper points ps) ++ [ List.last ps |> Maybe.withDefault p ])
                    |> linear


{-| -}
stepBefore : List (Vec2 Float) -> Path
stepBefore =
    step 0


{-| -}
stepAfter : List (Vec2 Float) -> Path
stepAfter =
    step 1
