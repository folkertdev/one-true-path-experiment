module Internal.NaturalInterpolation exposing (naturalControlPoints)

{-| This interpolation mode is absolutely awful code, as is the D3 implementation this is based off of.

It's in a special module so that the internals can be tested separately.

-}

import List.Extra as List
import Vector2 as Vec2 exposing (Vec2)
import Vector3 exposing (Vec3)


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
                    updateAt (n - 1) (\_ -> 8 * butFinal + final) r

            in
                Just ( a, b, r_ )

        _ ->
            Nothing
{-| A copy from elm-list-extra

The signature of this function changed in 7.0.0, but I want 
to support 6.0.0 to 8.0.0 for now.
-}
updateAt : Int -> (a -> a) -> List a -> List a
updateAt index fn list =
    if index < 0 then
        list
    else
        let
            head =
                List.take index list

            tail =
                List.drop index list
        in
            case tail of
                x :: xs ->
                    head ++ fn x :: xs

                _ ->
                    list

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
