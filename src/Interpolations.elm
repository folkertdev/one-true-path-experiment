module Interpolations exposing (..)

import Svg
import Svg.Attributes exposing (..)
import Path exposing (..)


points =
    [ ( 0, 85 ), ( 120, 45 ), ( 150, 89 ), ( 167, 42 ), ( 180, 23 ), ( 190, 23 ), ( 200, 89 ), ( 230, 200 ) ]
        |> List.map (\( x, y ) -> ( x * 2, y * 2 ))



{-
   v =
       Debug.log "xs" (List.map Tuple.first points)


   vv =
       Debug.log "ys" (List.map Tuple.second points)
-}


correct =
    [ ( ( 95.50028627046834, 123.62853544028398 ), ( 191.00057254093667, 77.25707088056797 ), ( 240, 90 ) )
    , ( ( 288.9994274590633, 102.74292911943203 ), ( 291.49799610672164, 174.60025191801213 ), ( 300, 178 ) )
    , ( ( 308.50200389327836, 181.39974808198787 ), ( 323.00744303217675, 116.34192144738348 ), ( 334, 84 ) )
    , ( ( 344.99255696782325, 51.658078552616516 ), ( 352.4722317645712, 52.032062292453915 ), ( 360, 46 ) )
    , ( ( 367.5277682354288, 39.967937707546085 ), ( 375.10362990953854, 27.52982938280087 ), ( 380, 46 ) )
    , ( ( 384.89637009046146, 64.47017061719913 ), ( 387.11324859727466, 113.84862017634262 ), ( 400, 178 ) )
    ]
        |> Debug.log "correct"


monoY =
    List.map (\( x, y ) -> ( y, 500 - x )) points


tau =
    2 * pi


radialPoints =
    [ ( 0, 80 )
    , ( pi * 0.25, 80 )
    , ( pi * 0.5, 30 )
    , ( pi * 0.75, 80 )
    , ( pi, 80 )
    , ( pi * 1.25, 80 )
    , ( pi * 1.5, 80 )
    , ( pi * 1.75, 80 )
    , ( pi * 2, 80 )
    ]


main =
    Svg.svg [ width "1000", height "1000" ]
        [ Svg.path [ d (Path.toString (linear points)), fill "none", stroke "orange" ] []
          --, Svg.path [ d (Path.toString (bundle 0.5 points)), fill "none", stroke "green" ] []
          --, Svg.path [ d (Path.toString (bundle 0.25 points)), fill "none", stroke "green" ] []
        , Svg.path [ d (Path.toString (natural points)), fill "none", stroke "green" ] []
          --, Svg.path [ d (Path.toString (monotoneX points)), fill "none", stroke "green" ] []
          -- , Svg.path [ d (Path.toString (basis points)), fill "none", stroke "green" ] []
          --, Svg.path [ d (Path.toString (basis points)), fill "none", stroke "black" ] []
          --, Svg.path [ d (Path.toString (radial radialPoints)), fill "none", stroke "orange", transform "translate(200, 200)" ] []
        ]


polyline points =
    case points of
        [] ->
            []

        x :: xs ->
            [ subpath (moveTo x) [ lineTo xs ] ]


closedPolyline points =
    case points of
        [] ->
            []

        x :: xs ->
            [ subpath (moveTo x) [ lineTo xs, closePath ] ]


f =
    uncurry <| flip <| curry toPolar


radial =
    basis << List.map (\( angle, radius ) -> ( radius * sin angle, -radius * cos angle ))


linear points =
    polyline points


point ( x0, y0 ) ( x1, y1 ) ( x, y ) =
    -- Path.cubicCurveTo
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


{-| Basis interpolation (also known as B-spline)
-}
basis points =
    let
        --| The ideal case where there are at least 3 points
        commonCase points =
            case points of
                p0 :: p1 :: [] ->
                    [ cubicCurveTo [ point p0 p1 p1 ], lineTo [ p1 ] ]

                p0 :: p1 :: p :: rest ->
                    (cubicCurveTo [ point p0 p1 p ]) :: commonCase (p1 :: p :: rest)

                _ ->
                    []
    in
        case points of
            ( x0, y0 ) :: ( x1, y1 ) :: _ :: _ ->
                let
                    toFirst =
                        ( (5 * x0 + x1) / 6, (5 * y0 + y1) / 6 )
                in
                    [ subpath (moveTo ( x0, y0 )) (lineTo [ toFirst ] :: commonCase points) ]

            [ p0, p1 ] ->
                [ subpath (moveTo p0) [ lineTo [ p1 ] ] ]

            _ ->
                []


{-| Closed Basis interpolation (also known as B-spline)
-}
closedBasis points =
    let
        --| The ideal case where there are at least 3 points
        commonCase close points =
            case points of
                p0 :: p1 :: [] ->
                    close p0 p1

                p0 :: p1 :: p :: rest ->
                    point p0 p1 p :: commonCase close (p1 :: p :: rest)

                _ ->
                    []
    in
        case points of
            p2 :: p3 :: p4 :: rest ->
                let
                    ( ( x0, y0 ), ( x1, y1 ), ( x, y ) ) =
                        ( p2, p3, p4 )

                    closing p0 p1 =
                        [ point p0 p1 p2
                        , point p1 p2 p3
                        , point p2 p3 p4
                        ]
                in
                    [ subpath (moveTo ( (x0 + 4 * x1 + x) / 6, (y0 + 4 * y1 + y) / 6 )) [ cubicCurveTo (commonCase closing (p3 :: p4 :: rest)) ]
                    ]

            [ p0, p1 ] ->
                [ subpath (moveTo p0) [ lineTo [ p1 ] ] ]

            _ ->
                []


openBasis points =
    let
        helper points =
            case points of
                p0 :: p1 :: p :: rest ->
                    point p0 p1 p :: helper (p1 :: p :: rest)

                _ ->
                    []
    in
        case points of
            ( x0, y0 ) :: ( x1, y1 ) :: ( x, y ) :: rest ->
                let
                    start =
                        ( (x0 + 4 * x1 + x) / 6, (y0 + 4 * y1 + y) / 6 )
                in
                    [ subpath (moveTo start) [ cubicCurveTo (helper (( x1, y1 ) :: ( x, y ) :: rest)) ] ]

            _ ->
                []


bundle beta points =
    case points of
        [] ->
            []

        ( x0, y0 ) :: rest ->
            let
                j =
                    List.length points - 1

                ( xn, yn ) =
                    last rest |> Maybe.withDefault ( x0, y0 )

                ( dx, dy ) =
                    ( xn - x0, yn - y0 )
            in
                List.indexedMap
                    (\i ( x, y ) ->
                        let
                            t =
                                toFloat i / toFloat j
                        in
                            ( beta * x + (1 - beta) * (x0 + t * dx)
                            , beta * y + (1 - beta) * (y0 + t * dy)
                            )
                    )
                    points
                    |> basis


last points =
    case points of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: rest ->
            last rest


cardinalPoint k ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) ( x, y ) =
    ( ( x1 + k * (x2 - x0)
      , y1 + k * (y2 - y0)
      )
    , ( x2 + k * (x1 - x)
      , y2 + k * (y1 - y)
      )
    , ( x2
      , y2
      )
    )


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
                [ subpath (moveTo p0) [ cubicCurveTo (helper points) ] ]

            _ ->
                []


openCardinal : Float -> List Coordinate -> Path
openCardinal tension points =
    let
        k =
            (1 - tension) / 6
    in
        case points of
            p0 :: p1 :: p2 :: p3 :: rest ->
                List.map4 (cardinalPoint k) (p1 :: p2 :: p3 :: rest) (p2 :: p3 :: rest) (p3 :: rest) rest
                    |> cubicCurveTo
                    |> List.singleton
                    |> subpath (moveTo p1)
                    |> List.singleton

            _ ->
                []


closedCardinal : Float -> List Coordinate -> Path
closedCardinal tension points =
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


distances alpha points =
    case points of
        [] ->
            []

        x :: xs ->
            List.map2 (catmullRomDistance alpha) (x :: xs) xs


catmullRomDistance alpha ( a, b ) ( c, d ) =
    let
        ( x23, y23 ) =
            ( a - c, b - d )

        l23_2a =
            (x23 * x23 + y23 * y23) ^ alpha
    in
        ( sqrt l23_2a, l23_2a )


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
                    [ subpath (moveTo p0) [ cubicCurveTo (catmullRomHelper alpha ending points) ] ]

            _ ->
                []


catmullRomHelper alpha ending points =
    case points of
        p0 :: p1 :: p2 :: [] ->
            ending p0 p1 p2

        p0 :: p1 :: p2 :: p :: rest ->
            catmullRomPoint alpha p0 p1 p2 p :: catmullRomHelper alpha ending (p1 :: p2 :: p :: rest)

        _ ->
            []


openCatmullRom alpha points =
    if alpha == 0 then
        openCardinal 0 points
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


closedCatmullRom alpha points =
    if alpha == 0 then
        closedCardinal 0 points
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


epsilon =
    1.0e-12


catmullRomPoint alpha (( x0, y0 ) as p0) (( x1, y1 ) as p1) (( x2, y2 ) as p2) (( x3, y3 ) as p3) =
    let
        ( l01_a, l01_2a ) =
            catmullRomDistance alpha p0 p1

        ( l12_a, l12_2a ) =
            catmullRomDistance alpha p1 p2

        ( l23_a, l23_2a ) =
            catmullRomDistance alpha p2 p3

        helper1 : Coordinate -> Coordinate
        helper1 ( x1, y1 ) =
            let
                a =
                    2 * l01_2a + 3 * l01_a * l12_a + l12_2a

                n =
                    3 * l01_a * (l01_a + l12_a)
            in
                ( (x1 * a - x0 * l12_2a + x2 * l01_2a) / n
                , (y1 * a - y0 * l12_2a + y2 * l01_2a) / n
                )

        helper2 : Coordinate -> Coordinate
        helper2 ( x2, y2 ) =
            let
                b =
                    2 * l23_2a + 3 * l23_a * l12_a + l12_2a

                m =
                    3 * l23_a * (l23_a + l12_a)
            in
                ( (x2 * b + x1 * l23_2a - x3 * l12_2a) / m
                , (y2 * b + y1 * l23_2a - y3 * l12_2a) / m
                )

        control1 =
            if l01_a > epsilon then
                helper1 ( x1, y1 )
            else
                ( x1, y1 )

        control2 =
            if l23_a > epsilon then
                helper2 ( x2, y2 )
            else
                ( x2, y2 )
    in
        ( control1, control2, p2 )



--             (p0 :: p1 :: p :: rest, ((l01_a, l01_2a) :: (l23_a, l23_sa)) :: restDistances)->


sign x =
    if x < 0 then
        -1
    else
        1


slope3 ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) =
    let
        h0 =
            x1 - x0

        h1 =
            x2 - x1

        ( s0h, s1h ) =
            ( toH h0 h1, toH h1 h0 )

        s0 =
            (y1 - y0) / s0h

        s1 =
            (y2 - y1) / s1h

        p =
            (s0 * h1 + s1 * h0) / (h0 + h1)
    in
        (sign s0 + sign s1) * Basics.min (Basics.min (Basics.abs s0) (Basics.abs s1)) (0.5 * Basics.abs (p))


slope2 ( x0, y0 ) ( x1, y1 ) t =
    let
        h =
            x1 - x0
    in
        if h /= 0 then
            (3 * (y1 - y0) / h - t) / 2
        else
            t


monotonePoint ( x0, y0 ) ( x1, y1 ) t0 t1 =
    let
        dx =
            (x1 - x0) / 3
    in
        ( ( x0 + dx, y0 + dx * t0 ), ( x1 - dx, y1 - dx * t1 ), ( x1, y1 ) )


{-| Note, does not deal well with coincident points
-}
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
            case ( last b, last r ) of
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

        -- b[n - 1] = (x[n] + a[n - 1]) / 2;
        -- for (i = 0; i < n - 1; ++i) b[i] = 2 * x[i + 1] - a[i + 1];
        lastX =
            case last points of
                Nothing ->
                    Debug.crash "invalid input"

                Just v ->
                    v

        lastA =
            case last a_ of
                Nothing ->
                    Debug.crash "invalid input"

                Just v ->
                    v

        b_ =
            (List.map2 (\xx aa -> 2 * xx - aa) (List.drop 1 points) (List.drop 1 a_)) ++ [ (lastX + lastA) / 2 ]
    in
        ( a_, b_ )


natural points =
    case points of
        [] ->
            []

        [ x ] ->
            []

        [ p1, p2 ] ->
            [ subpath (moveTo p1) [ lineTo [ p2 ] ] ]

        p :: ps ->
            [ subpath (moveTo p) [ cubicCurveTo (naturalControlPoints p ps) ] ]


naturalControlPoints p ps =
    let
        points =
            p :: ps

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

        segments =
            List.map3 (,,) pa pb (List.drop 1 points)

        --|> List.take (List.length points - 2)
    in
        segments
