module Generator exposing (..)

import Path exposing (..)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import State exposing (State)
import Svg exposing (..)
import Svg.Attributes exposing (..)


(:+:) =
    Vec2.add


(:*:) =
    Vec2.scale


(:/:) vector factor =
    Vec2.scale (1 / factor) vector


infixl 6 :+:


infixl 7 :/:


infixl 7 :*:


main =
    Svg.svg [ width "1000", height "1000", viewBox "0 0 1000 1000" ]
        [ Svg.path [ d myCurve, stroke "black", fill "none" ] []
        , Svg.path [ d myCurve2, stroke "red", fill "none" ] []
        ]


myCurve2 =
    data
        |> List.map Vec2.fromTuple
        |> interpolateLinear
        |> Path.stringifyPath


myCurve =
    data
        |> List.map Vec2.fromTuple
        |> interpolateBasisClosed
        -- |> interpolateLinear
        |>
            Path.stringifyPath


data =
    [ ( 100, 100 ), ( 200, 350 ), ( 300, 50 ), ( 400, 200 ), ( 500, 250 ), ( 600, 40 ), ( 700, 100 ) ]


type Interpolation
    = Linear


type alias Point =
    ( Float, Float )


type Segment
    = LineTo Vec2 Vec2
    | CubicBezier Vec2 Vec2 Vec2 Vec2


type alias LineGenerator a =
    { toX : a -> Float
    , toY : a -> Float
    , interpolation : Interpolation
    }


type MyMachine
    = Empty
    | One Vec2
    | Two Vec2 Vec2
    | Threes
        { first : ( Vec2, Vec2, Vec2 )
        , final : ( Vec2, Vec2, Vec2 )
        , intermediate : List ( Vec2, Vec2, Vec2 )
        }


q : Vec2 -> MyMachine -> MyMacine
q point machine =
    case machine of
        Empty ->
            One point

        One p ->
            Two p point

        Two p1 p2 ->
            Threes { first = ( p1, p2, point ), final = ( p1, p2, point ), intermediate = [] }

        Threes { first, final, intermediate } ->
            Threes ( p2, p3, point ) (( p1, p2, p3 ) :: rest)


h points =
    case points of
        [] ->
            Empty

        [ p ] ->
            One p

        [ p1, p ] ->
            Two p1 p

        x :: y :: z :: rest ->
            Threes ( x, y, z ) (List.map3 (,,) (y :: z :: rest) (z :: rest) rest)


g state =
    case state of
        Empty ->
            []

        One p ->
            subpath (moveTo (Vec2.toTuple p)) [ closepath ]
                |> List.singleton

        Two p1 p ->
            let
                start =
                    p
                        |> Vec2.scale 2
                        |> Vec2.add p1
                        |> Vec2.scale (1 / 3)
                        |> Vec2.toTuple

                target =
                    p1
                        |> Vec2.scale 2
                        |> Vec2.add p
                        |> Vec2.scale (1 / 3)
                        |> Vec2.toTuple
            in
                subpath (moveTo start) [ lineTo [ target ], closepath ]
                    |> List.singleton

        Threes ( p2, p3, p4 ) preceding ->
            let
                start =
                    [ interpolateBasisPoint p0 p1 p2
                    , interpolateBasisPoint p1 p2 p3
                    , interpolateBasisPoint p2 p3 p4
                    ]
            in
                List.foldl (\( p0, p1, p ) accum -> interpolateBasisPoint p0 p1 p :: accum) start preceding


interpolateBasisClosed : List Vec2 -> Path.Path
interpolateBasisClosed points =
    case points of
        [] ->
            []

        [ p ] ->
            subpath (moveTo (Vec2.toTuple p)) [ closepath ]
                |> List.singleton

        [ p1, p ] ->
            let
                start =
                    p
                        |> Vec2.scale 2
                        |> Vec2.add p1
                        |> Vec2.scale (1 / 3)
                        |> Vec2.toTuple

                target =
                    p1
                        |> Vec2.scale 2
                        |> Vec2.add p
                        |> Vec2.scale (1 / 3)
                        |> Vec2.toTuple
            in
                subpath (moveTo start) [ lineTo [ target ], closepath ]
                    |> List.singleton

        p2 :: p3 :: p4 :: rest ->
            let
                ( p0, p1, p ) =
                    ( p2, p3, p4 )

                start =
                    ((p0 :+: 4 :*: p1 :+: p) :/: 6)
                        |> Vec2.toTuple

                helper points =
                    case points of
                        [ p0, p1 ] ->
                            [ interpolateBasisPoint p0 p1 p2
                            , interpolateBasisPoint p1 p2 p3
                            , interpolateBasisPoint p2 p3 p4
                            ]

                        p0 :: p1 :: p :: rest ->
                            interpolateBasisPoint p0 p1 p :: helper (p1 :: p :: rest)

                        _ ->
                            []
            in
                subpath (moveTo start) (helper points)
                    |> List.singleton


interpolateBasis : List Vec2 -> Path.Path
interpolateBasis points =
    case points of
        [] ->
            []

        [ p ] ->
            -- relativeSubpath p []
            subpath (moveTo (Vec2.toTuple p)) []
                |> List.singleton

        [ p0, p1 ] ->
            subpath (moveTo (Vec2.toTuple p0)) [ lineTo [ Vec2.toTuple p1 ] ]
                |> List.singleton

        p0 :: p1 :: p :: rest ->
            let
                helper points =
                    case points of
                        [ p0, p1 ] ->
                            [ interpolateBasisPoint p0 p1 p1, lineTo [ Vec2.toTuple p1 ] ]

                        p0 :: p1 :: p :: rest ->
                            interpolateBasisPoint p0 p1 p :: helper (p1 :: p :: rest)

                        _ ->
                            []

                start =
                    p0
                        |> Vec2.scale 5
                        |> Vec2.add p1
                        |> Vec2.scale (1 / 6)
                        |> Vec2.toTuple
            in
                subpath (moveTo (Vec2.toTuple p0)) (lineTo [ start ] :: helper points)
                    |> List.singleton


type alias CursorState =
    { start : Vec2, cursor : Vec2 }


toSubpath : Segment -> Path.SubPath
toSubpath segment =
    case segment of
        LineTo start end ->
            subpath (moveTo (Vec2.toTuple start)) [ lineTo [ Vec2.toTuple end ] ]

        CubicBezier start c1 c2 end ->
            subpath (moveTo (Vec2.toTuple start)) [ Path.cubicCurveTo [ ( Vec2.toTuple c1, Vec2.toTuple c2, Vec2.toTuple end ) ] ]


relativeSubpath : Vec2 -> List (State CursorState Segment) -> Path.Path
relativeSubpath move relInstructions =
    State.combine relInstructions
        |> State.finalValue { start = move, cursor = move }
        |> List.map toSubpath


extendLine : Vec2 -> State CursorState Segment
extendLine point =
    State.advance <|
        \state ->
            ( LineTo state.cursor point
            , { state | cursor = point }
            )


extendCubicBezier : Vec2 -> Vec2 -> Vec2 -> State CursorState Segment
extendCubicBezier c1 c2 point =
    State.advance <|
        \state ->
            ( CubicBezier state.cursor c1 c2 point
            , { state | cursor = point }
            )


interpolateBasisPoint p0 p1 p =
    let
        ( c1, c2, point ) =
            ( (p0 |> Vec2.scale 2 |> Vec2.add p1 |> Vec2.scale (1 / 3) |> Vec2.toTuple)
            , (p1 |> Vec2.scale 2 |> Vec2.add p0 |> Vec2.scale (1 / 3) |> Vec2.toTuple)
            , (p1 |> Vec2.scale 4 |> Vec2.add p0 |> Vec2.add p |> Vec2.scale (1 / 6) |> Vec2.toTuple)
            )
    in
        cubicCurveTo [ ( c1, c2, point ) ]



{-
   , extendCubicBezier
       (p0 |> Vec2.scale 2 |> Vec2.add p1 |> Vec2.scale (1 / 3))
       (p1 |> Vec2.scale 2 |> Vec2.add p0 |> Vec2.scale (1 / 3))
       (p1 |> Vec2.scale 4 |> Vec2.add p0 |> Vec2.add p |> Vec2.scale (1 / 6))
-}


f ( x0, y0 ) ( x1, y1 ) ( x, y ) =
    ( ( (2 * x0 + x1) / 3
      , (2 * y0 + y1) / 3
      )
    , ( (x0 + 2 * x1) / 3
      , (y0 + 2 * y1) / 3
      )
    , ( (x0 + 4 * x1 + x) / 6
      , (y0 + 4 * y1 + y) / 6
      )
    )


interpolateLinear points =
    case points of
        [] ->
            []

        p :: rest ->
            List.map2 LineTo points rest
                |> List.map toSubpath


generate : LineGenerator a -> List a -> DrawTo
generate { toX, toY, interpolation } data =
    let
        values =
            List.map (\d -> ( toX d, toY d )) data
    in
        case interpolation of
            Linear ->
                lineTo values
