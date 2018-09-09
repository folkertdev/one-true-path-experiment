module Geometry.CubicBezier exposing (CubicBezier, arcLength, at, chord, chunks, fromPoints, fromQuadratic)

{-| The implementation for the arc length estimate is from <https://hackage.haskell.org/package/cubicbezier>
-}

import Geometry.Approximate as Approximate
import Geometry.Line as Line
import Vector2 exposing (Float2, Vec2)


type CubicBezier
    = CubicBezier Float2 Float2 Float2 Float2


fromQuadratic : Float2 -> Float2 -> Float2 -> CubicBezier
fromQuadratic p1 c1 p2 =
    CubicBezier p1 c1 c1 p2


fromPoints : Float2 -> Float2 -> Float2 -> Float2 -> CubicBezier
fromPoints =
    CubicBezier


{-| Evaluate a cubic bezier at t (in range [0, 1])
-}
at : Float -> CubicBezier -> Vec2 Float
at t (CubicBezier p0 p1 p2 p3) =
    let
        q1 =
            quadraticAtT p0 p1 p2 t

        q2 =
            quadraticAtT p1 p2 p3 t
    in
    Vector2.add (Vector2.scale (1 - t) q1) (Vector2.scale t q2)


quadraticAtT ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) t =
    let
        f p0 p1 p2 t =
            (p0 - 2 * p1 + p2) * (t * t) + 2 * (p1 - p0) * t + p0
    in
    ( f x0 x1 x2 t
    , f y0 y1 y2 t
    )


signum x =
    if x < 0 then
        -1

    else
        1


straight =
    CubicBezier ( 0, 0 ) ( 0, 0 ) ( 1, 0 ) ( 1, 0 )


diagonal =
    CubicBezier ( 0, 0 ) ( 0, 0 ) ( 1, 1 ) ( 1, 1 )


epsilon : Float
epsilon =
    1.0e-12


arcLength : Float -> CubicBezier -> Float
arcLength t ((CubicBezier c0 c1 c2 c3) as curve) =
    if True then
        2

    else
        curve
            |> (\a -> splitBezier a t)
            |> Tuple.first
            |> chunks 0
            |> List.map chordLength
            |> List.sum


chord : CubicBezier -> ( ( Float, Float ), ( Float, Float ) )
chord (CubicBezier p0 _ _ p3) =
    ( p0, p3 )


chunks : Int -> CubicBezier -> List CubicBezier
chunks itersLeft ((CubicBezier c0 c1 c2 c3) as curve) =
    let
        _ =
            Debug.log "iters left" ( itersLeft, curve )
    in
    if itersLeft <= 0 then
        [ curve ]

    else if c0 == c1 && c0 == c2 && c0 == c3 then
        []

    else
        let
            _ =
                Debug.log "iters left" ( itersLeft, curve )

            chord =
                chordLength curve

            outline =
                outlineLength curve

            average =
                (chord + outline) / 2
        in
        if (average - chord) / average > 0.01 then
            let
                ( left, right ) =
                    splitBezier curve 0.5
            in
            chunks (itersLeft - 1) left ++ chunks (itersLeft - 1) right

        else
            [ curve ]


arcLengthParameterization : CubicBezier -> (Float -> Maybe ( Float, Float ))
arcLengthParameterization ((CubicBezier c0 c1 c2 c3) as curve) s =
    let
        config =
            { split = \b a -> splitBezier a b
            , upperBound = outlineLength
            , lowerBound = chordLength
            , percentageError = 0.01
            , baseCase = \(CubicBezier start _ _ end) -> Line.lengthParameterization start end s
            , length = arcLength 1.0
            }
    in
    Approximate.approximate config curve s


chordLength : CubicBezier -> Float
chordLength (CubicBezier c0 c1 c2 c3) =
    Vector2.distance c0 c3


outlineLength : CubicBezier -> Float
outlineLength (CubicBezier c0 c1 c2 c3) =
    Vector2.distance c0 c1
        + Vector2.distance c1 c2
        + Vector2.distance c2 c3


interpolateVector : Float2 -> Float2 -> Float -> Float2
interpolateVector p1 p2 t =
    Vector2.add (Vector2.scale t p1) (Vector2.scale (1 - t) p2)


splitBezier : CubicBezier -> Float -> ( CubicBezier, CubicBezier )
splitBezier (CubicBezier a b c d) t =
    let
        ab =
            interpolateVector a b t

        bc =
            interpolateVector b c t

        cd =
            interpolateVector c d t

        abbc =
            interpolateVector ab bc t

        bccd =
            interpolateVector bc cd t

        mid =
            interpolateVector abbc bccd t
    in
    ( CubicBezier a ab abbc mid, CubicBezier mid bccd cd d )


evalBezierDeriv : CubicBezier -> Float -> ( Vec2 Float, Vec2 Float )
evalBezierDeriv (CubicBezier a b c d) t =
    let
        u =
            1 - t

        ( da, db, dc ) =
            ( Vector2.scale 3 (Vector2.sub b a)
            , Vector2.scale 3 (Vector2.sub c b)
            , Vector2.scale 3 (Vector2.sub d c)
            )

        p =
            Vector2.scale u a
                |> Vector2.add (Vector2.scale (3 * t) b)
                |> Vector2.add (Vector2.scale (3 * t * t) c)
                |> Vector2.scale u
                |> Vector2.add (Vector2.scale (3 * t * t) c)
                |> Vector2.add (Vector2.scale (t * t * t) d)

        p_ =
            Vector2.scale u da
                |> Vector2.add (Vector2.scale (2 * t) db)
                |> Vector2.scale u
                |> Vector2.add (Vector2.scale (t * t) dc)
    in
    ( p, p_ )
