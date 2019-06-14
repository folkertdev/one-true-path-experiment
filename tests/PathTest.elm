module PathTest exposing (docsExample, fuzzCoordinate, fuzzMode, fuzzMoveTo, toAbsoluteConversion, various)

import Expect
import Fuzz exposing (..)
import LowLevel.Command as Command exposing (DrawTo(..), MoveTo(..))
import Path
import Path.LowLevel as LowLevel exposing (ArcFlag(..), Direction(..), Mode(..))
import SubPath
import Test exposing (..)


flip f a b =
    f b a


docsExample : Test
docsExample =
    let
        example =
            """M 213.1,6.7 C 110.6,4.9,67.5-9.5,36.9,6.7
          c -32.4-14.4-73.7,0-88.1,30.6
          C 110.6,4.9,67.5-9.5,36.9,6.7
          C 2.8,22.9-13.4,62.4,13.5,110.9
          C 33.3,145.1,67.5,170.3,125,217
          c 59.3-46.7,93.5-71.9,111.5-106.1
          C 263.4,64.2,247.2,22.9,213.1,6.7
          z
          """

        expectedPath =
            SubPath.with (MoveTo ( 213.1, 6.7 ))
                [ CurveTo
                    [ ( ( 110.6, 4.9 ), ( 67.5, -9.5 ), ( 36.9, 6.7 ) )
                    , ( ( 4.5, -7.7 ), ( -36.800000000000004, 6.7 ), ( -51.199999999999996, 37.300000000000004 ) )
                    , ( ( 110.6, 4.9 ), ( 67.5, -9.5 ), ( 36.9, 6.7 ) )
                    , ( ( 2.8, 22.9 ), ( -13.4, 62.4 ), ( 13.5, 110.9 ) )
                    , ( ( 33.3, 145.1 ), ( 67.5, 170.3 ), ( 125, 217 ) )
                    , ( ( 184.3, 170.3 ), ( 218.5, 145.1 ), ( 236.5, 110.9 ) )
                    , ( ( 263.4, 64.2 ), ( 247.2, 22.9 ), ( 213.1, 6.7 ) )
                    ]
                , ClosePath
                ]

        expected =
            "M213.1,6.7 C110.6,4.9 67.5,-9.5 36.9,6.7 4.5,-7.7 -36.800000000000004,6.7 -51.199999999999996,37.300000000000004 110.6,4.9 67.5,-9.5 36.9,6.7 2.8,22.9 -13.4,62.4 13.5,110.9 33.3,145.1 67.5,170.3 125,217 184.3,170.3 218.5,145.1 236.5,110.9 263.4,64.2 247.2,22.9 213.1,6.7 Z"
    in
    test "path parsing example from the readme" <|
        \_ ->
            Path.parse example
                |> Result.toMaybe
                |> Maybe.andThen List.head
                |> Maybe.withDefault SubPath.empty
                |> SubPath.compress
                |> SubPath.toString
                |> Expect.equal expected


toAbsoluteConversion : Test
toAbsoluteConversion =
    let
        startConfig =
            { start = ( 100, 100 ), cursor = ( 100, 100 ), previousControlPoint = Nothing }

        ellipticalArcExample =
            { radii = ( 25, 25 )
            , xAxisRotate = -30
            , arcFlag = SmallestArc
            , direction = Clockwise
            , target = ( 10, 10 )
            }

        fromLowLevelDrawTo state drawto =
            Command.fromLowLevelDrawTo drawto state
    in
    describe "convert commands to absolute"
        [ test "lineto relative" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.LineTo Relative [ ( 10, 10 ) ])
                    |> Expect.equal
                        (Just ( LineTo [ ( 110, 110 ) ], { startConfig | cursor = ( 110, 110 ) } ))
        , test "lineto absolute" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.LineTo Absolute [ ( 10, 10 ) ])
                    |> Expect.equal (Just ( LineTo [ ( 10, 10 ) ], { startConfig | cursor = ( 10, 10 ) } ))
        , test "horizontal relative" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.Horizontal Relative [ 10 ])
                    |> Expect.equal (Just ( LineTo [ ( 110, 100 ) ], { startConfig | cursor = ( 110, 100 ) } ))
        , test "horizontal absolute" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.Horizontal Absolute [ 10 ])
                    |> Expect.equal (Just ( LineTo [ ( 10, 0 ) ], { startConfig | cursor = ( 10, 100 ) } ))
        , test "vertical relative" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.Vertical Relative [ 10 ])
                    |> Expect.equal (Just ( LineTo [ ( 100, 110 ) ], { startConfig | cursor = ( 100, 110 ) } ))
        , test "vertical absolute" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.Vertical Absolute [ 10 ])
                    |> Expect.equal (Just ( LineTo [ ( 0, 10 ) ], { startConfig | cursor = ( 100, 10 ) } ))
        , test "curveTo relative" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.CurveTo Relative [ ( ( 0, 5 ), ( 10, 5 ), ( 10, 10 ) ) ])
                    |> Expect.equal
                        (Just
                            ( CurveTo [ ( ( 100, 105 ), ( 110, 105 ), ( 110, 110 ) ) ]
                            , { startConfig | cursor = ( 110, 110 ), previousControlPoint = Just ( 110, 105 ) }
                            )
                        )
        , test "curveTo absolute" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.CurveTo Absolute [ ( ( 0, 5 ), ( 10, 5 ), ( 10, 10 ) ) ])
                    |> Expect.equal
                        (Just
                            ( CurveTo [ ( ( 0, 5 ), ( 10, 5 ), ( 10, 10 ) ) ]
                            , { startConfig | cursor = ( 10, 10 ), previousControlPoint = Just ( 10, 5 ) }
                            )
                        )
        , test "smoothCurveTo relative" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.SmoothCurveTo Relative [ ( ( 10, 5 ), ( 10, 10 ) ) ])
                    |> Expect.equal
                        (Just
                            ( CurveTo [ ( ( 100, 100 ), ( 110, 105 ), ( 110, 110 ) ) ]
                            , { startConfig | cursor = ( 110, 110 ), previousControlPoint = Just ( 110, 105 ) }
                            )
                        )
        , test "smoothCurveTo absolute" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.SmoothCurveTo Absolute [ ( ( 10, 5 ), ( 10, 10 ) ) ])
                    |> Expect.equal
                        (Just
                            ( CurveTo [ ( ( 100, 100 ), ( 10, 5 ), ( 10, 10 ) ) ]
                            , { startConfig | cursor = ( 10, 10 ), previousControlPoint = Just ( 10, 5 ) }
                            )
                        )
        , test "quadraticBezierCurveTo relative" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.QuadraticBezierCurveTo Relative [ ( ( 10, 5 ), ( 10, 10 ) ) ])
                    |> Expect.equal
                        (Just
                            ( QuadraticBezierCurveTo [ ( ( 110, 105 ), ( 110, 110 ) ) ]
                            , { startConfig | cursor = ( 110, 110 ), previousControlPoint = Just ( 110, 105 ) }
                            )
                        )
        , test "quadraticBezierCurveTo absolute" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.QuadraticBezierCurveTo Absolute [ ( ( 10, 5 ), ( 10, 10 ) ) ])
                    |> Expect.equal
                        (Just
                            ( QuadraticBezierCurveTo [ ( ( 10, 5 ), ( 10, 10 ) ) ]
                            , { startConfig | cursor = ( 10, 10 ), previousControlPoint = Just ( 10, 5 ) }
                            )
                        )
        , test "ellipticalArc relative" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.EllipticalArc Relative [ ellipticalArcExample ])
                    |> Expect.equal (Just ( EllipticalArc [ { ellipticalArcExample | target = ( 110, 110 ) } ], { startConfig | cursor = ( 110, 110 ) } ))
        , test "ellipticalArc absolute" <|
            \_ ->
                fromLowLevelDrawTo startConfig (LowLevel.EllipticalArc Absolute [ ellipticalArcExample ])
                    |> Expect.equal (Just ( EllipticalArc [ ellipticalArcExample ], { startConfig | cursor = ( 10, 10 ) } ))
        , test "closepath" <|
            \_ ->
                fromLowLevelDrawTo startConfig LowLevel.ClosePath
                    |> Expect.equal (Just ( ClosePath, { startConfig | cursor = startConfig.start } ))
        ]


various : Test
various =
    let
        startConfig =
            { start = ( 100, 100 ), cursor = ( 100, 100 ), previousControlPoint = Nothing }
    in
    describe "various"
        [ test "foldl/traverse works with multiple subpaths" <|
            \_ ->
                "M10,10 L15,15 m20,20"
                    |> Path.parse
                    |> Expect.equal
                        (Ok
                            [ SubPath.with (Command.moveTo ( 10, 10 )) [ Command.lineTo [ ( 15, 15 ) ] ]
                            , SubPath.with (Command.moveTo ( 35, 35 )) []
                            ]
                        )
        , test "finalPoint documentation example" <|
            \_ ->
                let
                    finalPoint =
                        List.concatMap (SubPath.mapWithCursorState (flip Command.updateCursorState))
                in
                [ SubPath.with (Command.moveTo ( 10, 10 )) [ Command.lineTo [ ( 15, 15 ) ] ]
                , SubPath.with (Command.moveTo ( 35, 35 )) []
                ]
                    |> finalPoint
                    |> List.reverse
                    |> List.head
                    |> Maybe.map .cursor
                    |> Expect.equal (Just ( 15, 15 ))
        , fuzz fuzzMoveTo "a moveto always changes the CursorState starting position" <|
            \(LowLevel.MoveTo mode coordinate) ->
                let
                    add ( a, b ) ( c, d ) =
                        ( a + c, b + d )

                    ( newCursor, newStart ) =
                        if mode == Relative then
                            ( add startConfig.cursor coordinate
                            , add startConfig.start coordinate
                            )

                        else
                            ( coordinate
                            , coordinate
                            )
                in
                coordinate
                    |> LowLevel.MoveTo mode
                    |> flip Command.fromLowLevelMoveTo startConfig
                    |> Expect.equal
                        ( { startConfig | cursor = newCursor, start = newStart, previousControlPoint = Nothing }
                        , MoveTo newCursor
                        )
        , test "parsing and conversion of smooth quadratic is correct" <|
            \_ ->
                "M10 80 Q 52.5 10, 95 80 T 180 80"
                    |> Path.parse
                    |> Result.withDefault []
                    |> Path.toString
                    |> Expect.equal "M10,80 Q52.5,10 95,80 Q137.5,150 180,80"
        , test "parsing and conversion of smooth cubic is correct" <|
            \_ ->
                "M10 80 C 40 10, 65 10, 95 80 S 150 150, 180 80"
                    |> Path.parse
                    |> Result.withDefault []
                    |> Path.toString
                    |> Expect.equal "M10,80 C40,10 65,10 95,80 C125,150 150,150 180,80"
        , test "Tailor" <|
            {- Case by @unsoundscapes, it is tricky because it uses multpile relative arguments, something that none of the other tests covered. -}
            \_ ->
                "M322.7 86.5c-7.3-3.5-188.4 240.8-168.4 255 13.4 9.5 153-256.7 158-254.6 12 5.2-300 80-309.6 65s401-81 410.6-75c-66 0-130 123.5-103.6 125C345.3 204 416 83 422.3 86.4c6 3.3-93 118.7-60 118.7 21 0 133-122.7 125.4-127.7-13-8.5-94.4 119.3-71.4 119.3S630 2.4 598.7 2.4 396 286.4 423 286.4s118.3-172.2 155.3-151c-38.3 0-112.3 126.8-67 126.8 30 0 96.4-104.4 73.4-118.4-7.5-4.5-13 7.6-7.4 11.4 16.7 11 56.7-22.4 59.4-20 5 4.3-75.4 121-59.7 121s55-134 86.7-120c16 7 19.6 18.2 51.2 17.8 51-.7 204-19.4 287-77.2"
                    |> Path.parse
                    |> Result.withDefault []
                    |> Path.toString
                    |> Expect.equal "M322.7,86.5 C315.4,83 134.29999999999998,327.3 154.29999999999998,341.5 167.7,351 307.29999999999995,84.80000000000001 312.29999999999995,86.9 324.29999999999995,92.10000000000001 12.299999999999955,166.9 2.699999999999932,151.9 C-6.900000000000091,136.9 403.69999999999993,70.9 413.29999999999995,76.9 C347.29999999999995,76.9 283.29999999999995,200.4 309.69999999999993,201.9 C345.3,204 416,83 422.3,86.4 C428.3,89.7 329.3,205.10000000000002 362.3,205.10000000000002 383.3,205.10000000000002 495.3,82.40000000000002 487.70000000000005,77.40000000000002 474.70000000000005,68.90000000000002 393.30000000000007,196.70000000000002 416.30000000000007,196.70000000000002 C439.30000000000007,196.70000000000002 630,2.4 598.7,2.4 567.4000000000001,2.4 396,286.4 423,286.4 C450,286.4 541.3,114.19999999999999 578.3,135.39999999999998 C540,135.39999999999998 465.99999999999994,262.2 511.29999999999995,262.2 541.3,262.2 607.6999999999999,157.79999999999998 584.6999999999999,143.79999999999998 577.1999999999999,139.29999999999998 571.6999999999999,151.39999999999998 577.3,155.2 594,166.2 634,132.79999999999998 636.6999999999999,135.2 641.6999999999999,139.5 561.3,256.2 576.9999999999999,256.2 C592.6999999999998,256.2 631.9999999999999,122.19999999999999 663.6999999999999,136.2 C679.6999999999999,143.2 683.3,154.39999999999998 714.9,154 765.9,153.3 918.9,134.6 1001.9,76.8"
        ]


fuzzMoveTo : Fuzzer LowLevel.MoveTo
fuzzMoveTo =
    Fuzz.map2 LowLevel.MoveTo fuzzMode fuzzCoordinate


fuzzMode : Fuzzer Mode
fuzzMode =
    Fuzz.map
        (\value ->
            if value then
                Absolute

            else
                Relative
        )
        bool


fuzzCoordinate : Fuzzer ( Float, Float )
fuzzCoordinate =
    Fuzz.map2 Tuple.pair
        (Fuzz.map toFloat int)
        (Fuzz.map toFloat int)
