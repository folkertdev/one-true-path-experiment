module Example exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Parser
import MixedPath exposing (..)
import Path exposing (addCoordinates)
import ParserPrimitives exposing (..)


suite : Test
suite =
    let
        serious =
            """M600,350 l10,10 l20,20 Z
            """
    in
        describe "svg path syntax parser in elm"
            [ test "moveto drawto command group" <|
                \_ ->
                    Parser.run moveToDrawToCommandGroup serious
                        |> Expect.equal (Ok { moveto = MoveTo Absolute ( 600, 350 ), drawtos = [ LineTo Relative [ ( 10, 10 ) ], LineTo Relative [ ( 20, 20 ) ], ClosePath ] })
            , test "moveto drawto command groups" <|
                \_ ->
                    Parser.run moveToDrawToCommandGroups serious
                        |> Expect.equal (Ok [ { moveto = MoveTo Absolute ( 600, 350 ), drawtos = [ LineTo Relative [ ( 10, 10 ) ], LineTo Relative [ ( 20, 20 ) ], ClosePath ] } ])
            , test "svgPath" <|
                \_ ->
                    Parser.run svgMixedPath serious
                        |> Expect.equal (Ok [ { moveto = MoveTo Absolute ( 600, 350 ), drawtos = [ LineTo Relative [ ( 10, 10 ) ], LineTo Relative [ ( 20, 20 ) ], ClosePath ] } ])
            , test "relative moveto 0,0" <|
                \_ ->
                    Parser.run moveto "m 0,0"
                        |> Expect.equal (Ok ( MoveTo Relative ( 0, 0 ), Nothing ))
            , test "lineto argument sequence" <|
                \_ ->
                    Parser.run linetoArgumentSequence "10,10 20,20"
                        |> Expect.equal
                            (Ok [ ( 10, 10 ), ( 20, 20 ) ])
            , test "lineto command with multiple arguments " <|
                \_ ->
                    Parser.run lineto "l 10,10 20,20"
                        |> Expect.equal
                            (Ok (LineTo Relative [ ( 10, 10 ), ( 20, 20 ) ]))
            ]


toAbsoluteConversion : Test
toAbsoluteConversion =
    let
        startConfig =
            { start = ( 100, 100 ), cursor = ( 100, 100 ) }

        ellipticalArcExample =
            { radii = ( 25, 25 )
            , xAxisRotate = -30
            , arcFlag = SmallestArc
            , direction = Clockwise
            , target = ( 10, 10 )
            }
    in
        describe "convert commands to absolute"
            [ test "lineto relative" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (LineTo Relative [ ( 10, 10 ) ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( LineTo Absolute [ ( 110, 110 ) ], { startConfig | cursor = ( 110, 110 ) } )
            , test "lineto absolute" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (LineTo Absolute [ ( 10, 10 ) ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( LineTo Absolute [ ( 10, 10 ) ], { startConfig | cursor = ( 10, 10 ) } )
            , test "horizontal relative" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (Horizontal Relative [ 10 ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( Horizontal Absolute [ 110 ], { startConfig | cursor = ( 110, 100 ) } )
            , test "horizontal absolute" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (Horizontal Absolute [ 10 ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( Horizontal Absolute [ 10 ], { startConfig | cursor = ( 10, 100 ) } )
            , test "vertical relative" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (Vertical Relative [ 10 ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( Vertical Absolute [ 110 ], { startConfig | cursor = ( 100, 110 ) } )
            , test "vertical absolute" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (Vertical Absolute [ 10 ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( Vertical Absolute [ 10 ], { startConfig | cursor = ( 100, 10 ) } )
            , test "curveTo relative" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (CurveTo Relative [ ( ( 0, 5 ), ( 10, 5 ), ( 10, 10 ) ) ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( CurveTo Absolute [ ( ( 100, 105 ), ( 110, 105 ), ( 110, 110 ) ) ], { startConfig | cursor = ( 110, 110 ) } )
            , test "curveTo absolute" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (CurveTo Absolute [ ( ( 0, 5 ), ( 10, 5 ), ( 10, 10 ) ) ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( CurveTo Absolute [ ( ( 0, 5 ), ( 10, 5 ), ( 10, 10 ) ) ], { startConfig | cursor = ( 10, 10 ) } )
            , test "smoothCurveTo relative" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (SmoothCurveTo Relative [ ( ( 10, 5 ), ( 10, 10 ) ) ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( SmoothCurveTo Absolute [ ( ( 110, 105 ), ( 110, 110 ) ) ], { startConfig | cursor = ( 110, 110 ) } )
            , test "smoothCurveTo absolute" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (SmoothCurveTo Absolute [ ( ( 10, 5 ), ( 10, 10 ) ) ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( SmoothCurveTo Absolute [ ( ( 10, 5 ), ( 10, 10 ) ) ], { startConfig | cursor = ( 10, 10 ) } )
            , test "quadraticBezierCurveTo relative" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (QuadraticBezierCurveTo Relative [ ( ( 10, 5 ), ( 10, 10 ) ) ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( QuadraticBezierCurveTo Absolute [ ( ( 110, 105 ), ( 110, 110 ) ) ], { startConfig | cursor = ( 110, 110 ) } )
            , test "quadraticBezierCurveTo absolute" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (QuadraticBezierCurveTo Absolute [ ( ( 10, 5 ), ( 10, 10 ) ) ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( QuadraticBezierCurveTo Absolute [ ( ( 10, 5 ), ( 10, 10 ) ) ], { startConfig | cursor = ( 10, 10 ) } )
            , test "ellipticalArc relative" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (EllipticalArc Relative [ ellipticalArcExample ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( EllipticalArc Absolute [ { ellipticalArcExample | target = ( 110, 110 ) } ], { startConfig | cursor = ( 110, 110 ) } )
            , test "ellipticalArc absolute" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig (EllipticalArc Absolute [ ellipticalArcExample ])
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( EllipticalArc Absolute [ ellipticalArcExample ], { startConfig | cursor = ( 10, 10 ) } )
            , test "closepath" <|
                \_ ->
                    Path.toAbsoluteDrawTo startConfig ClosePath
                        |> Tuple.mapFirst Path.toMixedDrawTo
                        |> Expect.equal ( ClosePath, { startConfig | cursor = startConfig.start } )
            , fuzz fuzzMoveTo "a moveto always changes the CursorState starting position" <|
                \(MoveTo mode coordinate) ->
                    let
                        ( newCursor, newStart ) =
                            if mode == Relative then
                                ( addCoordinates startConfig.cursor coordinate
                                , addCoordinates startConfig.start coordinate
                                )
                            else
                                ( coordinate
                                , coordinate
                                )
                    in
                        coordinate
                            |> MoveTo mode
                            |> Path.toAbsoluteMoveTo startConfig
                            |> Tuple.mapFirst Path.toMixedMoveTo
                            |> Expect.equal ( MoveTo Absolute newCursor, { startConfig | cursor = newCursor, start = newStart } )
            ]


fuzzMoveTo : Fuzzer MoveTo
fuzzMoveTo =
    Fuzz.map2 MoveTo fuzzMode fuzzCoordinate


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


fuzzCoordinate : Fuzzer MixedPath.Coordinate
fuzzCoordinate =
    Fuzz.map2 (,)
        (Fuzz.map toFloat int)
        (Fuzz.map toFloat int)


{-| Test a parser against its expected result
-}
parseTest : Parser.Parser a -> String -> a -> Test
parseTest parser string output =
    test ("parsing `" ++ string ++ "`") <|
        \_ ->
            Parser.run parser string
                |> Expect.equal (Ok output)


primitives : Test
primitives =
    describe "primitive value parsers"
        [ parseTest nonNegativeNumber "25" 25
        ]


commands : Test
commands =
    let
        ellipticalArcExample =
            { radii = ( 25, 25 )
            , xAxisRotate = -30
            , arcFlag = SmallestArc
            , direction = Clockwise
            , target = ( 50, -25 )
            }
    in
        describe "parsing individual commands"
            [ parseTest moveto "M0,0" ( MoveTo Absolute ( 0, 0 ), Nothing )
            , parseTest moveto "m0,0" ( MoveTo Relative ( 0, 0 ), Nothing )
            , parseTest moveto "M0,0 20,20" ( MoveTo Absolute ( 0, 0 ), Just (LineTo Absolute [ ( 20, 20 ) ]) )
            , parseTest moveto "m0,0 20,20" ( MoveTo Relative ( 0, 0 ), Just (LineTo Relative [ ( 20, 20 ) ]) )
            , parseTest lineto "L0,0" (LineTo Absolute [ ( 0, 0 ) ])
            , parseTest lineto "l0,0" (LineTo Relative [ ( 0, 0 ) ])
            , parseTest horizontalLineto "H0 1 2 3" (Horizontal Absolute [ 0, 1, 2, 3 ])
            , parseTest horizontalLineto "h0 1 2 3" (Horizontal Relative [ 0, 1, 2, 3 ])
            , parseTest verticalLineto "V0 1 2 3" (Vertical Absolute [ 0, 1, 2, 3 ])
            , parseTest verticalLineto "v0 1 2 3" (Vertical Relative [ 0, 1, 2, 3 ])
            , parseTest closepath "Z" ClosePath
            , parseTest closepath "z" ClosePath
            , parseTest curveto "C100,100 250,100 250,200" (CurveTo Absolute [ ( ( 100, 100 ), ( 250, 100 ), ( 250, 200 ) ) ])
            , parseTest curveto "c100,100 250,100 250,200" (CurveTo Relative [ ( ( 100, 100 ), ( 250, 100 ), ( 250, 200 ) ) ])
            , parseTest smoothCurveto "S400,300 400,200" (SmoothCurveTo Absolute [ ( ( 400, 300 ), ( 400, 200 ) ) ])
            , parseTest smoothCurveto "s400,300 400,200" (SmoothCurveTo Relative [ ( ( 400, 300 ), ( 400, 200 ) ) ])
            , parseTest quadraticBezierCurveto "Q400,50 600,300" (QuadraticBezierCurveTo Absolute [ ( ( 400, 50 ), ( 600, 300 ) ) ])
            , parseTest quadraticBezierCurveto "q400,50 600,300" (QuadraticBezierCurveTo Relative [ ( ( 400, 50 ), ( 600, 300 ) ) ])
            , parseTest smoothQuadraticBezierCurveto "T1000,300" (SmoothQuadraticBezierCurveTo Absolute [ ( 1000, 300 ) ])
            , parseTest smoothQuadraticBezierCurveto "t1000,300" (SmoothQuadraticBezierCurveTo Relative [ ( 1000, 300 ) ])
            , parseTest ellipticalArc "A25,25 -30 0,1 50,-25" (EllipticalArc Absolute [ ellipticalArcExample ])
            , parseTest ellipticalArc "a25,25 -30 0,1 50,-25" (EllipticalArc Relative [ ellipticalArcExample ])
            ]
