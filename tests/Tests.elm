module Tests exposing (..)

import Curve
import Expect
import Fuzz exposing (..)
import LowLevel.Command as Command exposing (DrawTo(..), MoveTo(..))
import Path
import Path.LowLevel as LowLevel exposing (ArcFlag(..), Direction(..), Mode(..))
import SubPath
import Test exposing (..)
import Vector2 as Vec2


(=>) =
    (,)


toStringTests =
    let
        cases =
            [ SubPath.subpath (Command.moveTo ( 0, 0 )) [ Command.lineTo [ ( 20, 40 ), ( 73, 73 ) ] ] => "M0,0 L20,40 73,73"
            , Curve.linear [ ( 0, 0 ), ( 20, 40 ), ( 74, 74 ) ] => "M0,0 L20,40 74,74"
            ]

        createTest subpath expected =
            test ("pretty printing expecting " ++ expected) <|
                \_ ->
                    SubPath.toString subpath |> Expect.equal expected
    in
    describe "pretty printing tests" <|
        List.map (uncurry createTest) cases


pathCommand =
    [ SubPath.subpath (Command.moveTo ( 0, 0 )) [ Command.lineTo [ ( 20, 40 ) ], Command.lineTo [ ( 73, 73 ) ] ]
    , SubPath.subpath (Command.moveTo ( 20, 0 )) [ Command.lineTo [ ( 30, 50 ) ], Command.lineTo [ ( 42, 42 ) ] ]
    ]


pathTests =
    describe "functions directly related to paths"
        [ fuzz2 fuzzCoordinate (Fuzz.list fuzzCoordinate) "mapWithCursorState" <|
            \start rest ->
                [ SubPath.subpath (Command.moveTo start) (List.map (Command.lineTo << List.singleton) rest) ]
                    |> Path.mapWithCursorState (\{ cursor } _ -> cursor)
                    |> Expect.equal
                        (if rest /= [] then
                            start :: List.take (List.length rest - 1) rest
                         else
                            []
                        )
        ]



-- Path.parse "M10 80 Q 52.5 10, 95 80 T 180 80" |> Result.map Path.toString


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
        , test "foldl/traverse works with multiple subpaths" <|
            \_ ->
                "M10,10 L15,15 m20,20"
                    |> Path.parse
                    |> Expect.equal
                        (Ok
                            [ SubPath.subpath (Command.MoveTo ( 10, 10 )) [ Command.LineTo [ ( 15, 15 ) ] ]
                            , SubPath.subpath (Command.MoveTo ( 35, 35 )) []
                            ]
                        )
        , test "finalPoint documentation example" <|
            \_ ->
                let
                    finalPoint =
                        Path.mapWithCursorState (flip Command.updateCursorState)
                in
                [ SubPath.subpath (Command.MoveTo ( 10, 10 )) [ Command.LineTo [ ( 15, 15 ) ] ]
                , SubPath.subpath (Command.MoveTo ( 35, 35 )) []
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
