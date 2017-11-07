module PathTest exposing (..)

import Expect
import Fuzz exposing (..)
import LowLevel.Command as Command exposing (DrawTo(..), MoveTo(..))
import Path
import Path.LowLevel as LowLevel exposing (ArcFlag(..), Direction(..), Mode(..))
import SubPath
import Test exposing (..)
import Vector2 as Vec2


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


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
            SubPath.subpath (MoveTo ( 213.1, 6.7 ))
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
                                [ SubPath.subpath (Command.moveTo ( 10, 10 )) [ Command.lineTo [ ( 15, 15 ) ] ]
                                , SubPath.subpath (Command.moveTo ( 35, 35 )) []
                                ]
                            )
            , test "finalPoint documentation example" <|
                \_ ->
                    let
                        finalPoint =
                            List.concatMap (SubPath.mapWithCursorState (flip Command.updateCursorState))
                    in
                        [ SubPath.subpath (Command.moveTo ( 10, 10 )) [ Command.lineTo [ ( 15, 15 ) ] ]
                        , SubPath.subpath (Command.moveTo ( 35, 35 )) []
                        ]
                            |> finalPoint
                            |> List.reverse
                            |> List.head
                            |> Maybe.map .cursor
                            |> Expect.equal (Just ( 15, 15 ))
            , fuzz fuzzMoveTo "a moveto always changes the CursorState starting position" <|
                \(LowLevel.MoveTo mode coordinate) ->
                    let
                        ( newCursor, newStart ) =
                            if mode == Relative then
                                ( Vec2.add startConfig.cursor coordinate
                                , Vec2.add startConfig.start coordinate
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
    Fuzz.map2 (,)
        (Fuzz.map toFloat int)
        (Fuzz.map toFloat int)
