module NaturalTest exposing (Triplet, controlPoints, natural, naturalControlPoints, step1, step2, tests)

import Expect
import Fuzz exposing (..)
import Internal.NaturalInterpolation exposing (naturalControlPoints)
import List.Extra as List
import LowLevel.Command exposing (cubicCurveTo, lineTo, moveTo)
import Path exposing (Path)
import SubPath
import Test exposing (..)
import Vector2d


type alias Triplet a =
    ( a, a, a )


mapTriplet f ( a, b, c ) =
    ( f a, f b, f c )


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
            Debug.todo "invalid input"


step2 coordinates =
    let
        ( x, y, z ) =
            firstStep

        firstStep =
            step1 coordinates

        firstB =
            case firstStep of
                ( _, v :: vs, _ ) ->
                    v

                _ ->
                    Debug.todo "invalid input"

        firstR =
            case firstStep of
                ( _, _, v :: vs ) ->
                    v

                _ ->
                    Debug.todo "invalid input"

        ( newB, newR ) =
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
                (List.map3 triplet x y z)
                |> Tuple.second
                |> (\( a, b ) -> ( List.reverse a, List.reverse b ))
    in
    let
        ( a, _, _ ) =
            firstStep
    in
    ( a, newB, newR )


triplet =
    \x y z -> ( x, y, z )


controlPoints : List Float -> ( List Float, List Float )
controlPoints points =
    let
        folder =
            \( _, currentB, currentR ) ( prevA, accum ) ->
                let
                    value =
                        (currentR - prevA) / currentB
                in
                ( value, value :: accum )

        ( a, b, r ) =
            step2 points

        finalA =
            case ( List.last b, List.last r ) of
                ( Just bb, Just rr ) ->
                    rr / bb

                _ ->
                    Debug.todo "invalid input"

        n =
            List.length points - 1

        a_ =
            List.foldr folder
                ( finalA, [ finalA ] )
                (List.map3 (\x y z -> ( x, y, z )) a (List.take (n - 1) b) (List.take (n - 1) r))
                |> Tuple.second

        lastX =
            case List.last points of
                Nothing ->
                    Debug.todo "invalid input"

                Just v ->
                    v

        lastA =
            case List.last a_ of
                Nothing ->
                    Debug.todo "invalid input"

                Just v ->
                    v

        b_ =
            List.map2 (\xx aa -> 2 * xx - aa) (List.drop 1 points) (List.drop 1 a_) ++ [ (lastX + lastA) / 2 ]
    in
    ( a_, b_ )


natural : List ( Float, Float ) -> Path
natural points =
    case points of
        [] ->
            []

        [ x ] ->
            []

        [ p1, p2 ] ->
            [ SubPath.with (moveTo p1) [ lineTo [ p2 ] ] ]

        p :: ps ->
            [ SubPath.with (moveTo p) [ cubicCurveTo (naturalControlPoints points) ] ]


naturalControlPoints : List ( Float, Float ) -> List ( ( Float, Float ), ( Float, Float ), ( Float, Float ) )
naturalControlPoints points =
    let
        ( xs, ys ) =
            List.unzip points

        ( px0, px1 ) =
            controlPoints xs

        ( py0, py1 ) =
            controlPoints ys

        pa =
            List.map2 Tuple.pair px0 py0

        pb =
            List.map2 Tuple.pair px1 py1
    in
    List.map3 (\x y z -> ( x, y, z )) pa pb (List.drop 1 points)


tests =
    describe "natural interpolation"
        [ fuzz (list (tuple ( float, float ))) "control points" <|
            \points ->
                case points of
                    _ :: _ :: _ ->
                        Internal.NaturalInterpolation.naturalControlPoints (List.map Vector2d.fromComponents points)
                            |> List.map (mapTriplet Vector2d.components)
                            |> Expect.equal (naturalControlPoints points)

                    _ ->
                        Expect.pass
        , fuzz (tuple3 ( list float, list float, list float )) "scanr " <|
            \( a, b, r ) ->
                let
                    scanner ( currentA, currentB, currentR ) ( prevB, prevR ) =
                        ( currentB - (currentA / prevB)
                        , currentR - (currentA / prevB) * prevR
                        )

                    firstB =
                        1

                    firstR =
                        1

                    input =
                        List.map3 triplet a b r

                    p =
                        List.scanl scanner ( firstB, firstR ) input
                            |> List.drop 2
                            |> List.unzip

                    q =
                        List.foldl
                            (\( currentA, currentB, currentR ) ( ( prevB, prevR ), ( accum1, accum2 ) ) ->
                                let
                                    m_ =
                                        currentB - (currentA / prevB)

                                    r_ =
                                        currentR - (currentA / prevB) * prevR
                                in
                                ( ( m_, r_ ), ( m_ :: accum1, r_ :: accum2 ) )
                            )
                            ( ( firstB, firstR ), ( [], [] ) )
                            input
                            |> Tuple.second
                            |> (\( x, y ) -> ( List.drop 1 x, List.drop 1 y ))
                in
                p
                    |> Expect.equal q
        ]
