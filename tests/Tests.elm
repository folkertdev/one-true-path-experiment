module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Parser
import MixedPath exposing (..)
import Path
import Vector2 as Vec2
import ParserPrimitives exposing (..)


--


pathPath =
    [ Path.subpath (Path.moveTo ( 0, 0 )) [ Path.lineTo [ ( 20, 40 ) ], Path.lineTo [ ( 73, 73 ) ] ]
    , Path.subpath (Path.moveTo ( 20, 0 )) [ Path.lineTo [ ( 30, 50 ) ], Path.lineTo [ ( 42, 42 ) ] ]
    ]


pathTests =
    describe "functions directly related to paths"
        [ fuzz2 fuzzCoordinate (Fuzz.list fuzzCoordinate) "mapWithCursorState" <|
            \start rest ->
                [ Path.subpath (Path.moveTo start) (List.map (Path.lineTo << List.singleton) rest) ]
                    |> Path.mapWithCursorState (\{ cursor } _ -> cursor)
                    |> Expect.equal
                        (if rest /= [] then
                            start :: (List.take (List.length rest - 1) rest)
                         else
                            []
                        )
        ]



{-
   controlPointsTest3 =
       let
           ( correctA, correctB, correctR ) =
               ( [ 1.3333333333333335, 2.333333333333333, 3.3333333333333335, 4.333333333333334, 5.333333333333333, 6.333333333333334, 7.333333333333333, 8.333333333333332 ]
               , [ 1.666666666666667, 2.6666666666666665, 3.666666666666666, 4.666666666666667, 5.666666666666666, 6.666666666666667, 7.666666666666668, 8.666666666666666 ]
               , [ 5, 11.5, 16.714285714285715, 21.5, 26.237113402061855, 30.96961325966851, 35.70170244263508, 53.86751289170964 ]
               )

           ( a, b ) =
               Interpolations.controlPoints (List.map toFloat <| List.range 1 9)
       in
           describe "step 3"
               [ test "a is correct" <| \_ -> Expect.equal correctA a
               , test "b is correct" <| \_ -> Expect.equal correctB b
               ]


   controlPointsTest2 =
       let
           ( a, b, r ) =
               Interpolations.step2 (List.map toFloat <| List.range 1 9)
       in
           describe "step 2"
               [ test "a is correct" <| \_ -> Expect.equal [ 0, 1, 1, 1, 1, 1, 1, 2 ] a
               , test "b is correct" <| \_ -> Expect.equal [ 2, 3.5, 3.7142857142857144, 3.730769230769231, 3.731958762886598, 3.7320441988950277, 3.7320503330866024, 6.464101547005157 ] b
               , test "r is correct" <| \_ -> Expect.equal [ 5, 11.5, 16.714285714285715, 21.5, 26.237113402061855, 30.96961325966851, 35.70170244263508, 53.86751289170964 ] r
               ]

-}


suite : Test
suite =
    describe "svg path syntax parser in elm" <|
        let
            serious =
                "M600,350 l10,10 l20,20 Z"
        in
            [ test "moveto drawto command group" <|
                \_ ->
                    Parser.run moveToDrawToCommandGroup serious
                        |> Expect.equal (Ok { moveto = MoveTo Absolute ( 600, 350 ), drawtos = [ LineTo Relative [ ( 10, 10 ) ], LineTo Relative [ ( 20, 20 ) ], ClosePath ] })
            , test "moveto drawto command groups" <|
                \_ ->
                    Parser.run moveToDrawToCommandGroups serious
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


whitespaceParsing : Test
whitespaceParsing =
    describe "svg path parsing uses whitespce permisively" <|
        List.map
            (\( label, serious ) ->
                test label <|
                    \_ ->
                        Parser.run svgMixedPath serious
                            |> Expect.equal (Ok [ { moveto = MoveTo Absolute ( 600, 350 ), drawtos = [ LineTo Relative [ ( 10, 10 ) ], LineTo Relative [ ( 20, 20 ) ], ClosePath ] } ])
            )
            [ ( "no spacing", "M600,350l10,10l20,20Z" )
            , ( "some spaces", "M600,350 l10,10 l20,20 Z" )
            , ( "well spaced", "M 600, 350 l 10, 10 l 20, 20 Z" )
            , ( "crazy spaced", "M   600  ,  350 l  10  , 10 l 20  , 20  Z" )
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
            , test "foldl/traverse works with multiple subpaths" <|
                \_ ->
                    "M10,10 L15,15 m20,20"
                        |> Path.parse
                        |> Result.map (List.map (\{ moveto, drawtos } -> { moveto = Path.toMixedMoveTo moveto, drawtos = List.map Path.toMixedDrawTo drawtos }))
                        |> Expect.equal
                            (Ok
                                ([ { moveto = MoveTo Absolute ( 10, 10 ), drawtos = [ LineTo Absolute [ ( 15, 15 ) ] ] }
                                 , { moveto = MoveTo Absolute ( 35, 35 ), drawtos = [] }
                                 ]
                                )
                            )
            , test "finalPoint documentation example" <|
                \_ ->
                    let
                        finalPoint =
                            Path.mapWithCursorState (flip Path.updateCursorState)
                    in
                        [ { moveto = Path.MoveTo ( 10, 10 ), drawtos = [ Path.LineTo [ ( 15, 15 ) ] ] }
                        , { moveto = Path.MoveTo ( 35, 35 ), drawtos = [] }
                        ]
                            |> finalPoint
                            |> List.reverse
                            |> List.head
                            |> Maybe.map .cursor
                            |> Expect.equal (Just ( 15, 15 ))
            , fuzz fuzzMoveTo "a moveto always changes the CursorState starting position" <|
                \(MoveTo mode coordinate) ->
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
        , parseTest digitSequence "42" 42
        , parseTest digitSequence "42X" 42
        , parseTest digitSequence "42 " 42
        , parseTest floatingPointConstant "42.0" 42
        , parseTest floatingPointConstant "42.0X" 42
        , parseTest floatingPointConstant "42.0 " 42
        , parseTest floatingPointConstant "42e0" 42
        , parseTest floatingPointConstant "42e1" 420
        , parseTest floatingPointConstant "42e1X" 420
        , parseTest (delimited { item = floatingPointConstant, delimiter = commaWsp }) "" []
        , parseTest (delimited { item = floatingPointConstant, delimiter = commaWsp }) "45" [ 45 ]
        , parseTest (delimited { item = floatingPointConstant, delimiter = commaWsp }) "45 45" [ 45, 45 ]
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
            , parseTest moveto "M0.3,0" ( MoveTo Absolute ( 0.3, 0 ), Nothing )
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
