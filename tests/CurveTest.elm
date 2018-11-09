module CurveTest exposing (testBasis, testBundle, testCardinal, testCatmullRom, testMonotone, testNatural)

{-| Notes:

====

D3 has a weird way of handling input of length 1.

Intuitively, there is no curve to draw, so the output should be `""`.
But D3 doesn't look ahead, so when it finds the first point in the input, it generates
a moveto. Then when there is no more output, there is a problem.

The next curve will add another moveto to the context, so now there are two
consecutive moveto's. SVG treats the second moveto as a lineto in this case, which is clearly wrong.

So, to prevent `Mx,y Mz,w`, which is equivalent to `Mx,y Lz,w`, instead it writes `Mx,y Z Mz,w`.
The moveto followed by a closePath does nothing, but because of how D3 does things, it is unavoidable.

But in our case, we do look ahead and thus produce the empty string on input of length 1.

===

Code used to generate some of these tests

```javascript
// ---
points = []
result = d3.line().curve(d3.curveMonotoneY)(points);

console.log("0 points", result)

// ---

points = [[1,2]]
result = d3.line().curve(d3.curveMonotoneY)(points);

console.log("1 points", result)

// ---

points = [[1,2], [3,4]]
result = d3.line().curve(d3.curveMonotoneY)(points);

console.log("2 points", result)

// ---
points = [[1,2], [3,4], [5,6]]
result = d3.line().curve(d3.curveMonotoneY)(points);

console.log("3 points", result)
// ---
points = [[1,2], [3,4], [5,6], [7,8]]
result = d3.line().curve(d3.curveMonotoneY)(points);

console.log("4 points", result)
```

-}

import Curve exposing (..)
import Expect
import LowLevel.Command exposing (DrawTo(..), MoveTo(..))
import Path
import SubPath exposing (SubPath, decimalPlaces, mergeAdjacent)
import Test exposing (..)


{-| The number of iterations that induces a stack overflow.
-}
stackSmasher : Int
stackSmasher =
    10


convert : { drawtos : List DrawTo, moveto : MoveTo } -> SubPath
convert { moveto, drawtos } =
    SubPath.with moveto drawtos


trivialPoints : List ( Float, Float )
trivialPoints =
    [ ( 1, 2 ), ( 3, 4 ), ( 5, 6 ), ( 7, 8 ) ]


points : List ( Float, Float )
points =
    [ ( 50, 0 )
    , ( 150, 150 )
    , ( 225, 200 )
    , ( 275, 200 )
    , ( 350, 50 )
    , ( 500, 50 )
    , ( 600, 150 )
    , ( 725, 100 )
    , ( 825, 50 )
    , ( 900, 75 )
    , ( 975, 0 )
    ]
        |> List.map (\( x, y ) -> ( x, 300 - y ))


{-| same as `points` above, but can easily be copied to the js console
-}
jsPoints : List (List Float)
jsPoints =
    [ [ 50, 300 ], [ 150, 150 ], [ 225, 100 ], [ 275, 100 ], [ 350, 250 ], [ 500, 250 ], [ 600, 150 ], [ 725, 200 ], [ 825, 250 ], [ 900, 225 ], [ 975, 300 ] ]


edgeCaseTests : String -> (List ( Float, Float ) -> SubPath) -> List String -> Test
edgeCaseTests name toSubPath answers =
    let
        helper i answer =
            test (name ++ " gives expected output for " ++ String.fromInt i ++ " points") <|
                \_ ->
                    case Path.parse answer of
                        Ok [ subpath ] ->
                            toSubPath (List.take i trivialPoints)
                                |> SubPath.toStringWith [ decimalPlaces 3, mergeAdjacent ]
                                |> Expect.equal (SubPath.toStringWith [ decimalPlaces 3, mergeAdjacent ] subpath)

                        Ok [] ->
                            toSubPath (List.take i trivialPoints)
                                |> SubPath.toString
                                |> Expect.equal ""

                        Ok l ->
                            let
                                _ =
                                    Debug.log "invalid number of results" e
                            in
                            toSubPath (List.take i trivialPoints)
                                |> SubPath.toString
                                |> Expect.equal answer

                        Err e ->
                            let
                                _ =
                                    Debug.log "parse error" e
                            in
                            toSubPath (List.take i trivialPoints)
                                |> SubPath.toString
                                |> Expect.equal answer
    in
    describe (name ++ " edge cases") (List.indexedMap helper answers)


testCatmullRom : Test
testCatmullRom =
    let
        expected =
            { moveto = MoveTo ( 50, 300 )
            , drawtos =
                [ CurveTo
                    [ ( ( 50, 300 ), ( 115.48220313557538, 184.51779686442455 ), ( 150, 150 ) )
                    , ( ( 174.4077682344544, 125.59223176554563 ), ( 201.5028161565295, 107.1143748231127 ), ( 225, 100 ) )
                    , ( ( 242.50027908358396, 94.70134183997995 ), ( 259.3985449774631, 90.35777052211942 ), ( 275, 100 ) )
                    , ( ( 303.5728284700476, 117.65897914921008 ), ( 310.67715104458006, 225.6971428110722 ), ( 350, 250 ) )
                    , ( ( 387.1892544416877, 272.984223261231 ), ( 457.95062325807123, 267.41742213584274 ), ( 500, 250 ) )
                    , ( ( 540.8292528272556, 233.0879697373908 ), ( 561.9232752097317, 157.81554102840957 ), ( 600, 150 ) )
                    , ( ( 637.1510925306279, 142.37445212205927 ), ( 685.9957196353516, 182.48450132539355 ), ( 725, 200 ) )
                    , ( ( 760.5443248134044, 215.9617500525209 ), ( 793.6239497752703, 247.7701727655472 ), ( 825, 250 ) )
                    , ( ( 851.3840081587954, 251.8750537280864 ), ( 876.5829642941893, 219.47198774188894 ), ( 900, 225 ) )
                    , ( ( 927.1237694606573, 231.40305339874791 ), ( 975, 300 ), ( 975, 300 ) )
                    ]
                ]
            }
                |> convert
    in
    describe "catmull rom"
        [ test "catmull rom gives expected output" <|
            \_ ->
                Curve.catmullRom 0.5 points
                    |> Expect.equal expected
        , test "reverse catmull rom gives expected output" <|
            \_ ->
                Curve.catmullRom 0.5 (List.reverse points)
                    |> SubPath.reverse
                    |> SubPath.compress
                    |> SubPath.toStringWith [ decimalPlaces 3 ]
                    |> Expect.equal (SubPath.toStringWith [ decimalPlaces 3 ] <| SubPath.compress expected)

        -- edge cases for normal
        , edgeCaseTests "catmull rom"
            (Curve.catmullRom 0.5)
            [ ""

            -- D3 gives "M1,2 Z"
            , ""
            , "M1,2 L3,4"
            , "M1,2 C1,2 2.3333333333333335,3.3333333333333335 3,4 3.6666666666666665,4.666666666666667 5,6 5,6"
            , "M1,2 C1,2 2.3333333333333335,3.3333333333333335 3,4 3.6666666666666665,4.666666666666667 4.333333333333333,5.333333333333334 5,6 5.666666666666666,6.666666666666666 7,8 7,8"
            ]

        -- edge cases for closed
        , edgeCaseTests "catmull rom closed"
            (Curve.catmullRomClosed 0.5)
            [ ""

            -- D3 gives "M1,2 Z"
            , ""
            , "M3,4 L1,2 Z"
            , "M3,4 C3.6666666666666665,4.666666666666667 5.0,6.0 5,6 5,5.999999999999999 1,1.9999999999999998 1,2 1,2 2.3333333333333335,3.3333333333333335 3,4"
            , "M3,4 C3.6666666666666665,4.666666666666667 4.333333333333333,5.333333333333334 5,6 5.666666666666666,6.666666666666666 6.999999999999999,8.0  7,8 6.999999999999998,7.999999999999997 0.9999999999999997,1.9999999999999996 1,2 1,2 2.3333333333333335,3.3333333333333335 3,4"
            ]

        -- edge cases for open
        , edgeCaseTests "catmull rom open"
            (Curve.catmullRomOpen 0.5)
            [ ""
            , ""
            , ""
            , "M3,4 Z"
            , "M3,4 C3.6666666666666665,4.666666666666667 4.333333333333333,5.333333333333334 5,6"
            ]
        ]


testNatural : Test
testNatural =
    let
        expected =
            { moveto = MoveTo ( 50, 300 )
            , drawtos =
                [ CurveTo
                    [ ( ( 84.96320063090927, 241.1090036744297 ), ( 119.92640126181854, 182.21800734885935 ), ( 150, 150 ) )
                    , ( ( 180.07359873818146, 117.78199265114065 ), ( 205.25759558363512, 112.23697427899232 ), ( 225, 100 ) )
                    , ( ( 244.74240441636488, 87.76302572100768 ), ( 259.043216403641, 68.83409553517143 ), ( 275, 100 ) )
                    , ( ( 290.956783596359, 131.16590446482857 ), ( 308.56953880180106, 212.42664358032198 ), ( 350, 250 ) )
                    , ( ( 391.43046119819894, 287.573356419678 ), ( 456.6786283891547, 281.4593301435407 ), ( 500, 250 ) )
                    , ( ( 543.3213716108453, 218.54066985645932 ), ( 564.7159476415801, 161.73603584551532 ), ( 600, 150 ) )
                    , ( ( 635.2840523584199, 138.26396415448468 ), ( 684.4575810445249, 171.59652647439802 ), ( 725, 200 ) )
                    , ( ( 765.5424189554751, 228.40347352560198 ), ( 797.4537281803202, 251.8778582568928 ), ( 825, 250 ) )
                    , ( ( 852.5462718196798, 248.1221417431072 ), ( 875.7275062341942, 220.89204049803064 ), ( 900, 225 ) )
                    , ( ( 924.2724937658058, 229.10795950196936 ), ( 949.6362468829029, 264.5539797509847 ), ( 975, 300 ) )
                    ]
                ]
            }
                |> convert
    in
    describe "natural"
        [ test "natural gives expected output" <|
            \_ ->
                Curve.natural points
                    |> Expect.equal expected
        , test "reverse natural gives expected output" <|
            \_ ->
                Curve.natural (List.reverse points)
                    |> SubPath.reverse
                    |> SubPath.compress
                    |> SubPath.toStringWith [ decimalPlaces 3 ]
                    |> Expect.equal (SubPath.toStringWith [ decimalPlaces 3 ] <| SubPath.compress expected)
        , edgeCaseTests "natural"
            Curve.natural
            [ ""
            , ""
            , "M1,2L3,4"
            , "M1,2C1.6666666666666667,2.6666666666666665,2.3333333333333335,3.333333333333333,3,4C3.6666666666666665,4.666666666666667,4.333333333333333,5.333333333333334,5,6"
            , "M1,2C1.6666666666666667,2.666666666666667,2.3333333333333335,3.333333333333334,3,4C3.6666666666666665,4.666666666666666,4.333333333333333,5.333333333333333,5,6C5.666666666666667,6.666666666666667,6.333333333333334,7.333333333333334,7,8"
            ]
        ]


testCardinal : Test
testCardinal =
    let
        expected : SubPath
        expected =
            { moveto = MoveTo ( 50, 300 )
            , drawtos =
                [ CurveTo
                    [ ( ( 50, 300 ), ( 135.41666666666666, 166.66666666666666 ), ( 150, 150 ) )
                    , ( ( 164.58333333333334, 133.33333333333334 ), ( 214.58333333333334, 104.16666666666667 ), ( 225, 100 ) )
                    , ( ( 235.41666666666666, 95.83333333333333 ), ( 264.5833333333333, 87.5 ), ( 275, 100 ) )
                    , ( ( 285.4166666666667, 112.5 ), ( 331.25, 237.5 ), ( 350, 250 ) )
                    , ( ( 368.75, 262.5 ), ( 479.1666666666667, 258.3333333333333 ), ( 500, 250 ) )
                    , ( ( 520.8333333333334, 241.66666666666666 ), ( 581.25, 154.16666666666666 ), ( 600, 150 ) )
                    , ( ( 618.75, 145.83333333333334 ), ( 706.25, 191.66666666666666 ), ( 725, 200 ) )
                    , ( ( 743.75, 208.33333333333334 ), ( 810.4166666666666, 247.91666666666666 ), ( 825, 250 ) )
                    , ( ( 839.5833333333334, 252.08333333333334 ), ( 887.5, 220.83333333333334 ), ( 900, 225 ) )
                    , ( ( 912.5, 229.16666666666666 ), ( 975, 300 ), ( 975, 300 ) )
                    ]
                ]
            }
                |> convert
    in
    describe "cardinal"
        [ test "catmull rom gives expected output" <|
            \_ ->
                Curve.cardinal 0.5 points
                    |> Expect.equal expected
        , test "reverse cardinal gives expected output" <|
            \_ ->
                Curve.cardinal 0.5 (List.reverse points)
                    |> SubPath.reverse
                    |> SubPath.compress
                    |> SubPath.toStringWith [ decimalPlaces 3 ]
                    |> Expect.equal (SubPath.toStringWith [ decimalPlaces 3 ] <| SubPath.compress expected)
        , test "cardinal doesn't stack overflow" <|
            \_ ->
                let
                    path : SubPath
                    path =
                        Curve.cardinal 0.5 (List.repeat stackSmasher ( 0, 0 ))
                in
                Expect.pass
        , test "cardinal closed doesn't stack overflow" <|
            \_ ->
                let
                    path : SubPath
                    path =
                        Curve.cardinalClosed 0.5 (List.repeat stackSmasher ( 0, 0 ))
                in
                Expect.pass

        -- edge cases for normal
        , edgeCaseTests "cardinal"
            (Curve.cardinal 0)
            [ ""

            -- D3 gives "M1,2 Z"
            , ""
            , "M1,2 L3,4"
            , "M1,2 C1,2 2.3333333333333335,3.3333333333333335 3,4 3.6666666666666665,4.666666666666667 5,6 5,6"
            , "M1,2 C1,2 2.3333333333333335,3.3333333333333335 3,4 3.6666666666666665,4.666666666666667 4.333333333333333,5.333333333333333 5,6 5.666666666666667,6.666666666666667 7,8 7,8"
            ]

        -- edge cases for closed
        , edgeCaseTests "cardinal closed"
            (Curve.cardinalClosed 0)
            [ ""

            -- D3 gives "M1,2 Z"
            , ""
            , "M3,4 L1,2 Z"
            , "M3,4 C3.6666666666666665,4.666666666666667 5.333333333333333,6.333333333333333 5,6 4.666666666666667,5.666666666666667 1.3333333333333333,2.3333333333333335 1,2 0.6666666666666667,1.6666666666666667 2.3333333333333335,3.3333333333333335 3,4"
            , "M3,4 C3.6666666666666665,4.666666666666667 4.333333333333333,5.333333333333333 5,6 5.666666666666667,6.666666666666667 7.666666666666667,8.666666666666666 7,8 6.333333333333333,7.333333333333333 1.6666666666666665,2.6666666666666665 1,2 0.33333333333333337,1.3333333333333335 2.3333333333333335,3.3333333333333335 3,4"
            ]

        -- edge cases for open
        , edgeCaseTests "cardinal open"
            (Curve.cardinalOpen 0)
            [ ""

            -- D3 gives "M1,2Z"
            , ""
            , ""

            -- D3 gives "M3,4Z"
            , ""
            , "M3,4 C3.6666666666666665,4.666666666666667 4.333333333333333,5.333333333333333 5,6"
            ]
        ]


testMonotone : Test
testMonotone =
    let
        -- to replicate, use
        -- d3.line().curve(d3.curveMonotoneX)(jsPoints)
        d3Output =
            [ ( ( 83.33333333333334, 242.06349206349205 ), ( 116.66666666666666, 184.12698412698413 ), ( 150, 150 ) )
            , ( ( 175, 124.4047619047619 ), ( 200, 100 ), ( 225, 100 ) )
            , ( ( 241.66666666666666, 100 ), ( 258.3333333333333, 100 ), ( 275, 100 ) )
            , ( ( 300, 100 ), ( 325, 250 ), ( 350, 250 ) )
            , ( ( 400, 250 ), ( 450, 250 ), ( 500, 250 ) )
            , ( ( 533.3333333333334, 250 ), ( 566.6666666666666, 150 ), ( 600, 150 ) )
            , ( ( 641.6666666666666, 150 ), ( 683.3333333333334, 181.01851851851853 ), ( 725, 200 ) )
            , ( ( 758.3333333333334, 215.1851851851852 ), ( 791.6666666666666, 250 ), ( 825, 250 ) )
            , ( ( 850, 250 ), ( 875, 225 ), ( 900, 225 ) )
            , ( ( 925, 225 ), ( 950, 262.5 ), ( 975, 300 ) )
            ]
    in
    describe "monotone"
        [ test "test against d3 output" <|
            \_ ->
                let
                    expected =
                        { moveto = MoveTo ( 50, 300 )
                        , drawtos =
                            [ CurveTo d3Output ]
                        }
                            |> convert
                in
                Curve.monotoneX points
                    |> Expect.equal expected
        , test "produces what elm-visualization produces" <|
            \_ ->
                let
                    -- should produce
                    -- "M0.1,0.1C0.13333333333333333,0.2866666666666666,0.16666666666666669,0.47333333333333333,0.2,0.6C0.25,0.7899999999999999,0.3,0.9,0.35,0.9C0.3833333333333333,0.9,0.4166666666666667,0.9,0.45,0.9C0.5,0.9,0.55,0.3,0.6,0.3C0.7,0.3,0.8,0.8,0.9,0.8C1,0.8,1.1,0.6666666666666666,1.2,0.6C1.3,0.5333333333333333,1.4,0.4866666666666667,1.5,0.4C1.5666666666666667,0.34222222222222226,1.6333333333333333,0.2,1.7,0.2C1.7666666666666666,0.2,1.8333333333333333,0.44999999999999996,1.9,0.7"
                    elmVisualizationJSPoints =
                        [ [ 0.1, 0.1 ], [ 0.2, 0.6 ], [ 0.35, 0.9 ], [ 0.45, 0.9 ], [ 0.6, 0.3 ], [ 0.9, 0.8 ], [ 1.2, 0.6 ], [ 1.5, 0.4 ], [ 1.7, 0.2 ], [ 1.9, 0.7 ] ]

                    preparedPoints =
                        [ ( 94.5, 365 ), ( 139, 190 ), ( 205.75, 85 ), ( 250.25, 85 ), ( 317, 295 ), ( 450.5, 120 ), ( 584, 190 ), ( 717.5, 260 ), ( 806.5, 330 ), ( 895.5, 155.00000000000003 ) ]

                    preparedJSPoints =
                        [ [ 94.5, 365 ], [ 139, 190 ], [ 205.75, 85 ], [ 250.25, 85 ], [ 317, 295 ], [ 450.5, 120 ], [ 584, 190 ], [ 717.5, 260 ], [ 806.5, 330 ], [ 895.5, 155.00000000000003 ] ]

                    monotoneTestPoints : List ( Float, Float )
                    monotoneTestPoints =
                        [ ( 0.1, 0.1 )
                        , ( 0.2, 0.6 )
                        , ( 0.35, 0.9 )
                        , ( 0.45, 0.9 )
                        , ( 0.6, 0.3 )
                        , ( 0.9, 0.8 )
                        , ( 1.2, 0.6 )
                        , ( 1.5, 0.4 )
                        , ( 1.7, 0.2 )
                        , ( 1.9, 0.7 )
                        ]

                    expected =
                        { moveto = MoveTo ( 0.1, 0.1 )
                        , drawtos =
                            [ CurveTo
                                [ ( ( 0.13333333333333333, 0.2866666666666666 ), ( 0.16666666666666669, 0.47333333333333333 ), ( 0.2, 0.6 ) )
                                , ( ( 0.25, 0.7899999999999999 ), ( 0.3, 0.9 ), ( 0.35, 0.9 ) )
                                , ( ( 0.3833333333333333, 0.9 ), ( 0.4166666666666667, 0.9 ), ( 0.45, 0.9 ) )
                                , ( ( 0.5, 0.9 ), ( 0.55, 0.3 ), ( 0.6, 0.3 ) )
                                , ( ( 0.7, 0.3 ), ( 0.8, 0.8 ), ( 0.9, 0.8 ) )
                                , ( ( 1, 0.8 ), ( 1.1, 0.6666666666666666 ), ( 1.2, 0.6 ) )
                                , ( ( 1.3, 0.5333333333333333 ), ( 1.4, 0.4866666666666667 ), ( 1.5, 0.4 ) )
                                , ( ( 1.5666666666666667, 0.34222222222222226 ), ( 1.6333333333333333, 0.2 ), ( 1.7, 0.2 ) )
                                , ( ( 1.7666666666666666, 0.2 ), ( 1.8333333333333333, 0.44999999999999996 ), ( 1.9, 0.7 ) )
                                ]
                            ]
                        }
                            |> convert
                in
                Curve.monotoneX monotoneTestPoints
                    |> Expect.equal expected
        , test "reverse monotoneX gives expected output" <|
            \_ ->
                let
                    expected =
                        { moveto = MoveTo ( 50, 300 )
                        , drawtos =
                            [ CurveTo d3Output ]
                        }
                            |> convert
                in
                Curve.monotoneX (List.reverse points)
                    |> SubPath.reverse
                    |> SubPath.compress
                    |> SubPath.toStringWith [ decimalPlaces 3 ]
                    |> Expect.equal (SubPath.toStringWith [ decimalPlaces 3 ] <| SubPath.compress expected)
        , test "doesn't stack overflow" <|
            \_ ->
                let
                    path : SubPath
                    path =
                        Curve.monotoneX (List.repeat stackSmasher ( 0, 0 ))
                in
                Expect.pass
        , edgeCaseTests "monotone x"
            Curve.monotoneX
            [ ""
            , ""
            , "M1,2L3,4"
            , "M1,2C1.6666666666666665,2.6666666666666665,2.3333333333333335,3.3333333333333335,3,4C3.6666666666666665,4.666666666666667,4.333333333333333,5.333333333333333,5,6"
            , "M1,2C1.6666666666666665,2.6666666666666665,2.3333333333333335,3.3333333333333335,3,4C3.6666666666666665,4.666666666666667,4.333333333333333,5.333333333333333,5,6C5.666666666666667,6.666666666666667,6.333333333333333,7.333333333333333,7,8"
            ]
        , edgeCaseTests "monotone y"
            Curve.monotoneY
            [ ""
            , ""
            , "M1,2L3,4"
            , "M1,2C1.6666666666666665,2.6666666666666665,2.3333333333333335,3.3333333333333335,3,4C3.6666666666666665,4.666666666666667,4.333333333333333,5.333333333333333,5,6"
            , "M1,2C1.6666666666666665,2.6666666666666665,2.3333333333333335,3.3333333333333335,3,4C3.6666666666666665,4.666666666666667,4.333333333333333,5.333333333333333,5,6C5.666666666666667,6.666666666666667,6.333333333333333,7.333333333333333,7,8"
            ]
        ]


testBundle : Test
testBundle =
    let
        expected =
            { moveto = MoveTo ( 50, 300 )
            , drawtos =
                [ LineTo [ ( 66.04166666666667, 287.5 ) ]
                , CurveTo [ ( ( 82.08333333333333, 275 ), ( 114.16666666666667, 250 ), ( 144.16666666666666, 233.33333333333334 ) ) ]
                , CurveTo [ ( ( 174.16666666666666, 216.66666666666666 ), ( 202.08333333333334, 208.33333333333334 ), ( 227.91666666666666, 204.16666666666666 ) ) ]
                , CurveTo [ ( ( 253.75, 200 ), ( 277.5, 200 ), ( 303.3333333333333, 212.5 ) ) ]
                , CurveTo [ ( ( 329.1666666666667, 225 ), ( 357.0833333333333, 250 ), ( 391.25, 262.5 ) ) ]
                , CurveTo [ ( ( 425.4166666666667, 275 ), ( 465.8333333333333, 275 ), ( 502.0833333333333, 266.6666666666667 ) ) ]
                , CurveTo [ ( ( 538.3333333333334, 258.3333333333333 ), ( 570.4166666666666, 241.66666666666666 ), ( 604.5833333333334, 237.5 ) ) ]
                , CurveTo [ ( ( 638.75, 233.33333333333334 ), ( 675, 241.66666666666666 ), ( 709.1666666666666, 250 ) ) ]
                , CurveTo [ ( ( 743.3333333333334, 258.3333333333333 ), ( 775.4166666666666, 266.6666666666667 ), ( 805.4166666666666, 268.75 ) ) ]
                , CurveTo [ ( ( 835.4166666666666, 270.8333333333333 ), ( 863.3333333333334, 266.6666666666667 ), ( 891.25, 270.8333333333333 ) ) ]
                , CurveTo [ ( ( 919.1666666666666, 275 ), ( 947.0833333333334, 287.5 ), ( 961.0416666666666, 293.75 ) ) ]
                , LineTo [ ( 975, 300 ) ]
                ]
            }
                |> convert
    in
    describe "bundle"
        [ edgeCaseTests "bundle"
            (Curve.bundle 0.5)
            [ ""
            , ""
            , ""
            , "M1,2L1.3333333333333333,2.3333333333333335C1.6666666666666667,2.6666666666666665,2.3333333333333335,3.3333333333333335,3,4C3.6666666666666665,4.666666666666667,4.333333333333333,5.333333333333333,4.666666666666667,5.666666666666667L5,6"
            , "M1,2L1.3333333333333333,2.3333333333333335C1.6666666666666667,2.6666666666666665,2.3333333333333335,3.3333333333333335,3,4C3.6666666666666665,4.666666666666667,4.333333333333333,5.333333333333333,5,6C5.666666666666667,6.666666666666667,6.333333333333333,7.333333333333333,6.666666666666667,7.666666666666667L7,8"
            ]
        ]



{-
   [ test "bundle gives expected output" <|
       \_ ->
           Curve.bundle 0.5 points
               |> Expect.equal expected
   , test "reverse bundle gives expected output" <|
       \_ ->
           Curve.bundle 0.5 (List.reverse points)
               |> SubPath.reverse
               |> SubPath.compress
               |> SubPath.toStringWith [ decimalPlaces 3 ]
               |> Expect.equal (SubPath.toStringWith [ decimalPlaces 3 ] <| SubPath.compress expected)
   ]
-}


testBasis : Test
testBasis =
    let
        expected =
            { moveto = MoveTo ( 50, 300 )
            , drawtos =
                [ LineTo [ ( 66.66666666666667, 275 ) ]
                , CurveTo [ ( ( 83.33333333333333, 250 ), ( 116.66666666666667, 200 ), ( 145.83333333333334, 166.66666666666666 ) ) ]
                , CurveTo [ ( ( 175, 133.33333333333334 ), ( 200, 116.66666666666667 ), ( 220.83333333333334, 108.33333333333333 ) ) ]
                , CurveTo [ ( ( 241.66666666666666, 100 ), ( 258.3333333333333, 100 ), ( 279.1666666666667, 125 ) ) ]
                , CurveTo [ ( ( 300, 150 ), ( 325, 200 ), ( 362.5, 225 ) ) ]
                , CurveTo [ ( ( 400, 250 ), ( 450, 250 ), ( 491.6666666666667, 233.33333333333334 ) ) ]
                , CurveTo [ ( ( 533.3333333333334, 216.66666666666666 ), ( 566.6666666666666, 183.33333333333334 ), ( 604.1666666666666, 175 ) ) ]
                , CurveTo [ ( ( 641.6666666666666, 166.66666666666666 ), ( 683.3333333333334, 183.33333333333334 ), ( 720.8333333333334, 200 ) ) ]
                , CurveTo [ ( ( 758.3333333333334, 216.66666666666666 ), ( 791.6666666666666, 233.33333333333334 ), ( 820.8333333333334, 237.5 ) ) ]
                , CurveTo [ ( ( 850, 241.66666666666666 ), ( 875, 233.33333333333334 ), ( 900, 241.66666666666666 ) ) ]
                , CurveTo [ ( ( 925, 250 ), ( 950, 275 ), ( 962.5, 287.5 ) ) ]
                , LineTo [ ( 975, 300 ) ]
                ]
            }
                |> convert

        expectedOpen =
            { moveto = MoveTo ( 145.83333333333334, 166.66666666666666 )
            , drawtos =
                [ CurveTo
                    [ ( ( 175, 133.33333333333334 ), ( 200, 116.66666666666667 ), ( 220.83333333333334, 108.33333333333333 ) )
                    , ( ( 241.66666666666666, 100 )
                      , ( 258.3333333333333, 100 )
                      , ( 279.1666666666667, 125 )
                      )
                    , ( ( 300, 150 ), ( 325, 200 ), ( 362.5, 225 ) )
                    , ( ( 400, 250 ), ( 450, 250 ), ( 491.6666666666667, 233.33333333333334 ) )
                    , ( ( 533.3333333333334, 216.66666666666666 ), ( 566.6666666666666, 183.33333333333334 ), ( 604.1666666666666, 175 ) )
                    , ( ( 641.6666666666666, 166.66666666666666 ), ( 683.3333333333334, 183.33333333333334 ), ( 720.8333333333334, 200 ) )
                    , ( ( 758.3333333333334, 216.66666666666666 ), ( 791.6666666666666, 233.33333333333334 ), ( 820.8333333333334, 237.5 ) )
                    , ( ( 850, 241.66666666666666 ), ( 875, 233.33333333333334 ), ( 900, 241.66666666666666 ) )
                    ]
                ]
            }
                |> convert

        expectedClosed =
            { moveto = MoveTo ( 145.83333333333334, 166.66666666666666 )
            , drawtos =
                [ CurveTo
                    [ ( ( 175, 133.33333333333334 ), ( 200, 116.66666666666667 ), ( 220.83333333333334, 108.33333333333333 ) )
                    , ( ( 241.66666666666666, 100 ), ( 258.3333333333333, 100 ), ( 279.1666666666667, 125 ) )
                    , ( ( 300, 150 ), ( 325, 200 ), ( 362.5, 225 ) )
                    , ( ( 400, 250 ), ( 450, 250 ), ( 491.6666666666667, 233.33333333333334 ) )
                    , ( ( 533.3333333333334, 216.66666666666666 ), ( 566.6666666666666, 183.33333333333334 ), ( 604.1666666666666, 175 ) )
                    , ( ( 641.6666666666666, 166.66666666666666 ), ( 683.3333333333334, 183.33333333333334 ), ( 720.8333333333334, 200 ) )
                    , ( ( 758.3333333333334, 216.66666666666666 ), ( 791.6666666666666, 233.33333333333334 ), ( 820.8333333333334, 237.5 ) )
                    , ( ( 850, 241.66666666666666 ), ( 875, 233.33333333333334 ), ( 900, 241.66666666666666 ) )
                    , ( ( 925, 250 ), ( 950, 275 ), ( 808.3333333333334, 287.5 ) )
                    , ( ( 666.6666666666666, 300 ), ( 358.3333333333333, 300 ), ( 220.83333333333334, 275 ) )
                    , ( ( 83.33333333333333, 250 ), ( 116.66666666666667, 200 ), ( 145.83333333333334, 166.66666666666666 ) )
                    ]
                ]
            }
                |> convert

        newExpectClosed =
            { moveto = MoveTo ( 145.83333333333334, 166.66666666666666 )
            , drawtos =
                [ CurveTo
                    [ ( ( 175, 133.33333333333334 ), ( 200, 116.66666666666667 ), ( 220.83333333333334, 108.33333333333333 ) )
                    , ( ( 241.66666666666666, 100 ), ( 258.3333333333333, 100 ), ( 279.1666666666667, 125 ) )
                    , ( ( 300, 150 ), ( 325, 200 ), ( 362.5, 225 ) )
                    , ( ( 400, 250 ), ( 450, 250 ), ( 491.6666666666667, 233.33333333333334 ) )
                    , ( ( 533.3333333333334, 216.66666666666666 ), ( 566.6666666666666, 183.33333333333334 ), ( 604.1666666666666, 175 ) )
                    , ( ( 641.6666666666666, 166.66666666666666 ), ( 683.3333333333334, 183.33333333333334 ), ( 720.8333333333334, 200 ) )
                    , ( ( 758.3333333333334, 216.66666666666666 ), ( 791.6666666666666, 233.33333333333334 ), ( 820.8333333333334, 237.5 ) )
                    , ( ( 850, 241.66666666666666 ), ( 875, 233.33333333333334 ), ( 900, 241.66666666666666 ) )
                    , ( ( 925, 250 ), ( 950, 275 ), ( 808.3333333333334, 287.5 ) )
                    , ( ( 666.6666666666666, 300 ), ( 358.3333333333333, 300 ), ( 220.83333333333334, 275 ) )
                    , ( ( 83.33333333333333, 250 ), ( 116.66666666666667, 200 ), ( 145.83333333333334, 166.6666666666666 ) )
                    ]
                ]
            }
                |> convert
    in
    describe "basis"
        [ test "basis gives expected output" <|
            \_ ->
                Curve.basis points
                    |> SubPath.toStringWith [ decimalPlaces 3 ]
                    |> Expect.equal (SubPath.toStringWith [ decimalPlaces 3 ] expected)
        , test "reverse basis gives expected output" <|
            \_ ->
                Curve.basis (List.reverse points)
                    |> SubPath.reverse
                    |> SubPath.compress
                    |> SubPath.toStringWith [ decimalPlaces 3 ]
                    |> Expect.equal (SubPath.toStringWith [ decimalPlaces 3 ] <| SubPath.compress expected)
        , test "basis doesn't stack overflow" <|
            \_ ->
                let
                    path : SubPath
                    path =
                        Curve.basis (List.repeat stackSmasher ( 0, 0 ))
                in
                Expect.pass
        , test "basis open gives expected output" <|
            \_ ->
                Curve.basisOpen points
                    |> SubPath.toStringWith [ decimalPlaces 3 ]
                    |> Expect.equal (SubPath.toStringWith [ decimalPlaces 3 ] expectedOpen)
        , test "basis open doesn't stack overflow" <|
            \_ ->
                let
                    path : SubPath
                    path =
                        Curve.basisOpen (List.repeat stackSmasher ( 0, 0 ))
                in
                Expect.pass
        , test "reverse basis open gives expected output" <|
            \_ ->
                Curve.basisOpen (List.reverse points)
                    |> SubPath.reverse
                    |> SubPath.compress
                    |> SubPath.toStringWith [ decimalPlaces 3 ]
                    |> Expect.equal (SubPath.toStringWith [ decimalPlaces 3 ] <| SubPath.compress expectedOpen)
        , test "basis closed gives expected output" <|
            \_ ->
                Curve.basisClosed points
                    |> SubPath.toStringWith [ decimalPlaces 3 ]
                    |> Expect.equal (expectedClosed |> SubPath.toStringWith [ decimalPlaces 3 ])
        , test "basis closed doesn't stack overflow" <|
            \_ ->
                let
                    path : SubPath
                    path =
                        Curve.basisClosed (List.repeat stackSmasher ( 0, 0 ))
                in
                Expect.pass

        {- doesn't work, but is correct. the closed variants go clockwise or counter-clockwise, and reversing does not
           give an equal, but an equivalent result

              , test "reverse basis closed gives expected output" <|
                  \_ ->
                      Curve.basisClosed (List.reverse points)
                          |> SubPath.reverse
                          |> SubPath.compress
                          |> SubPath.toStringWith [ decimalPlaces 3 ]
                          |> Expect.equal (SubPath.toStringWith [ decimalPlaces 3 ] <| newExpectClosed)
        -}
        , edgeCaseTests "basis"
            Curve.basis
            [ ""
            , ""
            , "M1,2 L3,4"
            , "M1,2 L1.3333333333333333,2.3333333333333335C1.6666666666666667,2.6666666666666665,2.3333333333333335,3.3333333333333335,3,4C3.6666666666666665,4.666666666666667,4.333333333333333,5.333333333333333,4.666666666666667,5.666666666666667L5,6"
            , "M1,2 L1.3333333333333333,2.3333333333333335C1.6666666666666667,2.6666666666666665,2.3333333333333335,3.3333333333333335,3,4C3.6666666666666665,4.666666666666667,4.333333333333333,5.333333333333333,5,6C5.666666666666667,6.666666666666667,6.333333333333333,7.333333333333333,6.666666666666667,7.666666666666667L7,8"
            ]
        , edgeCaseTests "basis closed"
            Curve.basisClosed
            [ ""
            , ""
            , "M2.3333333333333335,3.3333333333333335 L1.6666666666666667,2.6666666666666665Z"
            , "M3,4C3.6666666666666665,4.666666666666667,4.333333333333333,5.333333333333333,4,5C3.6666666666666665,4.666666666666667,2.3333333333333335,3.3333333333333335,2,3C1.6666666666666667,2.6666666666666665,2.3333333333333335,3.3333333333333335,3,4"
            , "M3,4C3.6666666666666665,4.666666666666667,4.333333333333333,5.333333333333333,5,6C5.666666666666667,6.666666666666667,6.333333333333333,7.333333333333333,5.666666666666667,6.666666666666667C5,6,3,4,2.3333333333333335,3.3333333333333335C1.6666666666666667,2.6666666666666665,2.3333333333333335,3.3333333333333335,3,4"
            ]
        , edgeCaseTests "basis open"
            Curve.basisOpen
            [ ""
            , ""
            , ""
            , ""
            , "M3,4C3.6666666666666665,4.666666666666667,4.333333333333333,5.333333333333333,5,6"
            ]
        ]
