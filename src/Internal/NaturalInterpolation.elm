module Internal.NaturalInterpolation exposing (naturalControlPoints)

{-| This interpolation mode is absolutely awful code, as is the D3 implementation this is based off of.

It's in a special module so that the internals can be tested separately.

-}

import List.Extra as List
import Vector2d exposing (Vector2d)


{-| calculate the control points for natural spline interpolation
-}
naturalControlPoints : List Vector2d -> List ( Vector2d, Vector2d, Vector2d )
naturalControlPoints points =
    let
        ( xs, ys ) =
            points
                |> List.map Vector2d.components
                |> List.unzip
    in
    case Maybe.map2 (\a b -> ( a, b )) (controlPoints xs) (controlPoints ys) of
        Just ( ( px0, px1 ), ( py0, py1 ) ) ->
            let
                pa =
                    List.map2 (\a b -> Vector2d.fromComponents ( a, b )) px0 py0

                pb =
                    List.map2 (\a b -> Vector2d.fromComponents ( a, b )) px1 py1
            in
            List.map3 (\a b c -> ( a, b, c )) pa pb (List.drop 1 points)

        Nothing ->
            []


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
                    r
                        |> List.reverse
                        |> List.drop 1
                        |> (\list -> (8 * butFinal + final) :: list)
                        |> List.reverse
            in
            Just ( a, b, r_ )

        _ ->
            Nothing


step2Scanner ( a, b, r ) ( prevB, prevR ) =
    ( b - (a / prevB)
    , r - (a / prevB) * prevR
    )


step2 : ( List Float, List Float, List Float ) -> Maybe ( List Float, List Float, List Float )
step2 ( a, b, r ) =
    case ( b, r ) of
        ( firstB :: _, firstR :: _ ) ->
            let
                ( b_, r_ ) =
                    List.scanl step2Scanner ( firstB, firstR ) (List.map3 (\x y z -> ( x, y, z )) a b r)
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

                scanner ( currentB, currentR ) prevA =
                    (currentR - prevA) / currentB

                a_ =
                    List.scanr scanner finalA (List.map2 Tuple.pair (unsafeInit b) (unsafeInit r))

                b_ =
                    List.map2 (\xx aa -> 2 * xx - aa) (unsafeTail points) (unsafeTail a_) ++ [ (finalX + finalA) / 2 ]
            in
            ( a_, b_ )
    in
    Maybe.map3 helper (List.last r) (List.last b) (List.last points)


controlPoints : List Float -> Maybe ( List Float, List Float )
controlPoints points =
    step1 points
        |> Maybe.andThen step2
        |> Maybe.andThen (step3 points)


unsafeInit : List a -> List a
unsafeInit =
    Maybe.withDefault [] << List.init


unsafeTail : List a -> List a
unsafeTail =
    Maybe.withDefault [] << List.tail
