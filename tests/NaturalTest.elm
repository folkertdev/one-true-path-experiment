module NaturalTest exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Internal.NaturalInterpolation exposing (naturalControlPoints)
import Path exposing (Path)
import SubPath exposing (subpath)
import LowLevel.Command exposing (lineTo, moveTo, cubicCurveTo)
import Vector2 exposing (Vec2)
import Vector3 exposing (Vec3)
import List.Extra as List


type alias Triplet a =
    ( a, a, a )


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
                    List.indexedMap
                        (\i elem ->
                            if i == n - 1 then
                                8 * butFinal + final
                            else
                                elem
                        )
                        r
            in
                ( a, b, r_ )

        _ ->
            Debug.crash "invalid input"


step2 coordinates =
    let
        ( a, b, r ) =
            step1 coordinates

        firstB =
            case b of
                x :: xs ->
                    x

                _ ->
                    Debug.crash "invalid input"

        firstR =
            case r of
                x :: xs ->
                    x

                _ ->
                    Debug.crash "invalid input"

        ( b_, r_ ) =
            List.foldl
                (\( a, b, r ) ( ( prevB, prevR ), ( accum1, accum2 ) ) ->
                    let
                        m_ =
                            b - (a / prevB)

                        r_ =
                            r - (a / prevB) * prevR
                    in
                        ( ( m_, r_ ), ( m_ :: accum1, r_ :: accum2 ) )
                )
                ( ( firstB, firstR ), ( [], [] ) )
                (List.map3 (,,) a b r)
                |> Tuple.second
                |> (\( a, b ) -> ( List.reverse a, List.reverse b ))
    in
        ( a, b_, r_ )


controlPoints : List Float -> ( List Float, List Float )
controlPoints points =
    let
        ( a, b, r ) =
            step2 points

        finalA =
            case ( List.last b, List.last r ) of
                ( Just bb, Just rr ) ->
                    rr / bb

                _ ->
                    Debug.crash "invalid input"

        n =
            List.length points - 1

        a_ =
            List.foldr
                (\( a, b, r ) ( prevA, accum ) ->
                    let
                        value =
                            (r - prevA) / b
                    in
                        ( value, value :: accum )
                )
                ( finalA, [ finalA ] )
                (List.map3 (,,) a (List.take (n - 1) b) (List.take (n - 1) r))
                |> Tuple.second

        lastX =
            case List.last points of
                Nothing ->
                    Debug.crash "invalid input"

                Just v ->
                    v

        lastA =
            case List.last a_ of
                Nothing ->
                    Debug.crash "invalid input"

                Just v ->
                    v

        b_ =
            (List.map2 (\xx aa -> 2 * xx - aa) (List.drop 1 points) (List.drop 1 a_)) ++ [ (lastX + lastA) / 2 ]
    in
        ( a_, b_ )


natural : List (Vec2 Float) -> Path
natural points =
    case points of
        [] ->
            []

        [ x ] ->
            []

        [ p1, p2 ] ->
            [ subpath (moveTo p1) [ lineTo [ p2 ] ] ]

        p :: ps ->
            [ subpath (moveTo p) [ cubicCurveTo (naturalControlPoints points) ] ]


naturalControlPoints : List (Vec2 Float) -> List (Vec3 (Vec2 Float))
naturalControlPoints points =
    let
        ( xs, ys ) =
            List.unzip points

        ( px0, px1 ) =
            controlPoints xs

        ( py0, py1 ) =
            controlPoints ys

        pa =
            List.map2 (,) px0 py0

        pb =
            List.map2 (,) px1 py1
    in
        List.map3 (,,) pa pb (List.drop 1 points)


tests =
    describe "natural interpolation"
        [ fuzz (list (tuple ( float, float ))) "control points" <|
            \points ->
                case points of
                    _ :: _ :: _ ->
                        Internal.NaturalInterpolation.naturalControlPoints points
                            |> Expect.equal (naturalControlPoints points)

                    _ ->
                        Expect.pass
        , fuzz (tuple3 ( list float, list float, list float )) "scanr " <|
            \( a, b, r ) ->
                let
                    scanner ( a, b, r ) ( prevB, prevR ) =
                        ( b - (a / prevB)
                        , r - (a / prevB) * prevR
                        )

                    firstB =
                        1

                    firstR =
                        1

                    p =
                        List.scanl scanner ( firstB, firstR ) (List.map3 (,,) a b r)
                            |> List.unzip
                            |> (\( a, b ) -> ( List.drop 1 a, List.drop 1 b ))

                    q =
                        List.foldl
                            (\( a, b, r ) ( ( prevB, prevR ), ( accum1, accum2 ) ) ->
                                let
                                    m_ =
                                        b - (a / prevB)

                                    r_ =
                                        r - (a / prevB) * prevR
                                in
                                    ( ( m_, r_ ), ( m_ :: accum1, r_ :: accum2 ) )
                            )
                            ( ( firstB, firstR ), ( [], [] ) )
                            (List.map3 (,,) a b r)
                            |> Tuple.second
                            |> (\( a, b ) -> ( List.reverse a, List.reverse b ))
                in
                    Expect.equal (toString p) (toString q)
        ]
