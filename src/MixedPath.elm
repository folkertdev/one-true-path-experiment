module MixedPath exposing (..)

{-| Low-level module for working with constructing svg paths

This module provides a wrapper around the svg path interface. A path can be parsed from a string or build up using the
svg path primitives, and then converted to a string that can be used in the [`d` attribute][d-attribute] to render the path.

Note that this is not the most convenient way of drawing. This package is mainly meant as a primitive to build other packages on top of.
If you want to visualize data, have a look at [elm-plot] and [elm-visualization]. If you want to draw geometry, check out [opensolid].


For more information on svg paths, see the [MDN documentation].

[MDN documentation]: https://developer.mozilla.org/en/docs/Web/SVG/Tutorial/MixedPaths.
[elm-plot]: http://package.elm-lang.org/packages/terezka/elm-plot/latest
[elm-visualization]: http://package.elm-lang.org/packages/gampleman/elm-visualization/latest
[opensolid]: http://package.elm-lang.org/packages/opensolid/geometry/latest
[d-attribute]: https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d

[`MoveTo`]: #MoveTo
[`DrawTo`]: #DrawTo

## Data Layout

A path is a list of subpaths that are drawn in order. A subpath consists of a [`MoveTo`] instruction followed by a list of [`DrawTo`] instructions.

If the first [`MoveTo`] instruction is a relative one, it is interpreted as an absolute instruction. This makes sure there is always an absolute cursor position.
The `stringify` function in this module will always make the first [`MoveTo`] absolute.

The constructors are exposed, so if you need an escape hatch it is available. As always though, never reach for them if there are other options available.

## Data Structures
@docs Coordinate, MixedPath, MixedSubPath, subpath, stringify, parse, mapCoordinate

## Moving the cursor

@docs MoveTo, moveTo, moveBy

## Drawing on the canvas

@docs DrawTo

## Straight lines
@docs lineTo, lineBy, horizontalTo, horizontalBy, verticalTo, verticalBy

## Close MixedPath
@docs closeMixedPath

## Quadratic Beziers
@docs quadraticCurveTo, quadraticCurveBy, quadraticCurveExtendTo, quadraticCurveExtendBy

## Cubic Beziers
Every Cubic bezier can also be drawn by two quadratic beziers. The cubic bezier is thus a more compact way of writing a curve.

@docs cubicCurveTo, cubicCurveBy, cubicCurveExtendTo, cubicCurveExtendBy

## Arcs
@docs arcTo, arcBy, EllipticalArcArgument, Direction, clockwise, counterClockwise, ArcFlag, largestArc, smallestArc


## Internal Data (used by the parser)

These constructors can be used when you want to modify the path in some custom way.
@docs AbstractMoveTo, AbstractDrawTo, Mode

-}

import Char
import Parser exposing (Parser, (|.), (|=), oneOrMore, zeroOrMore, inContext, oneOf, symbol, succeed)
import ParserPrimitives exposing (delimited, isWhitespace, (|-), wsp, withDefault, coordinatePair, nonNegativeNumber, number, commaWsp, optional, flag)


{-| A path is a list of [`MixedSubPath`](#MixedSubPath)s.
-}
type alias MixedPath =
    List MixedSubPath


{-| A subpath consists of a [`MoveTo`](#MoveTo) instruction followed by a list of [`DrawTo`](#DrawTo) instructions


-}
type alias MixedSubPath =
    { moveto : MoveTo, drawtos : List DrawTo }


{-| A 2-tuple of floats representing a position in space
-}
type alias Coordinate =
    ( Float, Float )


{-| Construct a subpath

    subpath (moveTo (10,0)) [ lineTo [ (42, 73) ] ]
-}
subpath : MoveTo -> List DrawTo -> MixedSubPath
subpath =
    MixedSubPath


{-| The mode of an instruction
-}
type Mode
    = Relative
    | Absolute


{-| MoveTo instructions move the cursor, but don't draw anything.
-}
type alias MoveTo =
    AbstractMoveTo Mode


{-| Constructor for MoveTo instructions
-}
type AbstractMoveTo mode
    = MoveTo mode Coordinate


{-| DrawTo instructions draw from the current cursor position to their target.
-}
type alias DrawTo =
    AbstractDrawTo Mode


{-| Constructors for DrawTo instructions
-}
type AbstractDrawTo mode
    = LineTo mode (List Coordinate)
    | Horizontal mode (List Float)
    | Vertical mode (List Float)
    | CurveTo mode (List ( Coordinate, Coordinate, Coordinate ))
    | SmoothCurveTo mode (List ( Coordinate, Coordinate ))
    | QuadraticBezierCurveTo mode (List ( Coordinate, Coordinate ))
    | SmoothQuadraticBezierCurveTo mode (List Coordinate)
    | EllipticalArc mode (List EllipticalArcArgument)
    | ClosePath


{-| Manipulate the coordinates in your SVG. This can be useful for scaling the svg.

    -- make the image twice as big in the x direction
    [ subpath (moveTo (10,0)) [ lineTo [ (42, 42) ] ] ]
        |> mapCoordinate (\(x,y) -> (2 * x, y))
             --> [ subpath (moveTo (20,0)) [ lineTo [ (84, 42) ] ] ]
-}
mapCoordinate : (Coordinate -> Coordinate) -> MixedPath -> MixedPath
mapCoordinate f path =
    let
        helper : MixedSubPath -> MixedSubPath
        helper { moveto, drawtos } =
            case moveto of
                MoveTo mode coordinate ->
                    { moveto = MoveTo mode (f coordinate)
                    , drawtos = List.map helperDrawTo drawtos
                    }

        helperDrawTo drawto =
            case drawto of
                LineTo mode coordinates ->
                    LineTo mode (List.map f coordinates)

                Horizontal mode coordinates ->
                    coordinates
                        |> List.map ((\x -> ( x, 0 )) >> f >> Tuple.first)
                        |> Horizontal mode

                Vertical mode coordinates ->
                    coordinates
                        |> List.map ((\y -> ( 0, y )) >> f >> Tuple.second)
                        |> Vertical mode

                CurveTo mode coordinates ->
                    CurveTo mode (List.map (mapTriplet f) coordinates)

                SmoothCurveTo mode coordinates ->
                    SmoothCurveTo mode (List.map (mapTuple f) coordinates)

                QuadraticBezierCurveTo mode coordinates ->
                    QuadraticBezierCurveTo mode (List.map (mapTuple f) coordinates)

                SmoothQuadraticBezierCurveTo mode coordinates ->
                    SmoothQuadraticBezierCurveTo mode (List.map f coordinates)

                EllipticalArc mode arguments ->
                    EllipticalArc mode (List.map (\argument -> { argument | target = f argument.target }) arguments)

                ClosePath ->
                    ClosePath
    in
        List.map helper path


mapTuple : (a -> b) -> ( a, a ) -> ( b, b )
mapTuple f ( a, b ) =
    ( f a, f b )


mapTriplet : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapTriplet f ( a, b, c ) =
    ( f a, f b, f c )



-- Creating paths


{-| Move the cursor to an absolute position on the canvas
-}
moveTo : Coordinate -> MoveTo
moveTo =
    MoveTo Absolute


{-| Move the cursor by some amount
-}
moveBy : Coordinate -> MoveTo
moveBy =
    MoveTo Relative


{-| Draw a series of line segments to absolute positions
-}
lineTo : List Coordinate -> DrawTo
lineTo =
    LineTo Absolute


{-| Draw a series of line segments relative to the current cursor position
-}
lineBy : List Coordinate -> DrawTo
lineBy =
    LineTo Relative


{-| Specific version of `lineTo` that only moves horizontally.
-}
horizontalTo : List Float -> DrawTo
horizontalTo =
    Horizontal Absolute


{-| Specific version of `lineBy` that only moves horizontally

    horizontalBy [ x ] == lineBy [ (x, 0) ]
-}
horizontalBy : List Float -> DrawTo
horizontalBy =
    Horizontal Relative


{-| Specific version of `lineTo` that only moves vertically
-}
verticalTo : List Float -> DrawTo
verticalTo =
    Vertical Absolute


{-| Specific version of `lineBy` that only moves vertically

    verticalBy [ y ] == lineBy [ (0, y) ]
-}
verticalBy : List Float -> DrawTo
verticalBy =
    Vertical Relative


{-| Draw a straight line from the cursor position to the starting position of the path .
-}
closeMixedPath : DrawTo
closeMixedPath =
    ClosePath


{-| -}
quadraticCurveTo : List ( Coordinate, Coordinate ) -> DrawTo
quadraticCurveTo =
    QuadraticBezierCurveTo Absolute


{-| -}
quadraticCurveExtendTo : List Coordinate -> DrawTo
quadraticCurveExtendTo =
    SmoothQuadraticBezierCurveTo Absolute


{-| -}
quadraticCurveBy : List ( Coordinate, Coordinate ) -> DrawTo
quadraticCurveBy =
    QuadraticBezierCurveTo Relative


{-| -}
quadraticCurveExtendBy : List Coordinate -> DrawTo
quadraticCurveExtendBy =
    SmoothQuadraticBezierCurveTo Relative


{-| -}
cubicCurveTo : List ( Coordinate, Coordinate, Coordinate ) -> DrawTo
cubicCurveTo =
    CurveTo Absolute


{-| -}
cubicCurveExtendTo : List ( Coordinate, Coordinate ) -> DrawTo
cubicCurveExtendTo =
    SmoothCurveTo Absolute


{-| -}
cubicCurveBy : List ( Coordinate, Coordinate, Coordinate ) -> DrawTo
cubicCurveBy =
    CurveTo Relative


{-| -}
cubicCurveExtendBy : List ( Coordinate, Coordinate ) -> DrawTo
cubicCurveExtendBy =
    SmoothCurveTo Relative


{-| -}
arcTo : List EllipticalArcArgument -> DrawTo
arcTo =
    EllipticalArc Absolute


{-| -}
arcBy : List EllipticalArcArgument -> DrawTo
arcBy =
    EllipticalArc Relative


{-| The arguments for an Arc
-}
type alias EllipticalArcArgument =
    { radii : ( Float, Float )
    , xAxisRotate : Float
    , arcFlag : ArcFlag
    , direction : Direction
    , target : Coordinate
    }


{-| Determine which arc to draw
-}
type ArcFlag
    = SmallestArc
    | LargestArc


{-| Determine which arc to draw
-}
type Direction
    = Clockwise
    | CounterClockwise


{-| Corresponds to a sweep flag of 1
-}
clockwise : Direction
clockwise =
    Clockwise


{-| Corresponds to a sweep flag of 0
-}
counterClockwise : Direction
counterClockwise =
    CounterClockwise


{-| Corresponds to an arc flag of 1
-}
largestArc : ArcFlag
largestArc =
    LargestArc


{-| Corresponds to an arc flag of 0
-}
smallestArc : ArcFlag
smallestArc =
    SmallestArc



-- STRINGIFY


{-| Turn a `MixedPath` into a `String`. The result is ready to be used with the `d` attribute.

    stringify [ subpath (moveTo (0,0)) [ lineBy ( 42, 73 ) ] ]
        --> "M0,0 l42,73
-}
stringify : MixedPath -> String
stringify subpaths =
    String.join " " (List.map stringifyMixedSubPath subpaths)


stringifyMixedSubPath : MixedSubPath -> String
stringifyMixedSubPath { moveto, drawtos } =
    stringifyMoveTo moveto ++ " " ++ String.join " " (List.map stringifyDrawTo drawtos)


stringifyMoveTo : MoveTo -> String
stringifyMoveTo (MoveTo mode coordinate) =
    case mode of
        Absolute ->
            "M" ++ stringifyCoordinate coordinate

        Relative ->
            "m" ++ stringifyCoordinate coordinate


stringifyDrawTo : DrawTo -> String
stringifyDrawTo command =
    case command of
        LineTo mode coordinates ->
            stringifyCharacter mode 'L' ++ String.join " " (List.map stringifyCoordinate coordinates)

        Horizontal mode coordinates ->
            stringifyCharacter mode 'H' ++ String.join " " (List.map toString coordinates)

        Vertical mode coordinates ->
            stringifyCharacter mode 'V' ++ String.join " " (List.map toString coordinates)

        CurveTo mode coordinates ->
            stringifyCharacter mode 'C' ++ String.join " " (List.map stringifyCoordinate3 coordinates)

        SmoothCurveTo mode coordinates ->
            stringifyCharacter mode 'S' ++ String.join " " (List.map stringifyCoordinate2 coordinates)

        QuadraticBezierCurveTo mode coordinates ->
            stringifyCharacter mode 'Q' ++ String.join " " (List.map stringifyCoordinate2 coordinates)

        SmoothQuadraticBezierCurveTo mode coordinates ->
            stringifyCharacter mode 'T' ++ String.join " " (List.map stringifyCoordinate coordinates)

        EllipticalArc mode arguments ->
            stringifyCharacter mode 'A' ++ String.join " " (List.map stringifyEllipticalArcArgument arguments)

        ClosePath ->
            "Z"


stringifyEllipticalArcArgument : EllipticalArcArgument -> String
stringifyEllipticalArcArgument { radii, xAxisRotate, arcFlag, direction, target } =
    String.join " "
        [ stringifyCoordinate radii
        , toString xAxisRotate
        , if arcFlag == LargestArc then
            "1"
          else
            "0"
        , if direction == Clockwise then
            "1"
          else
            "0"
        , stringifyCoordinate target
        ]


stringifyCharacter : Mode -> Char -> String
stringifyCharacter mode character =
    case mode of
        Absolute ->
            String.fromChar (Char.toUpper character)

        Relative ->
            String.fromChar (Char.toLower character)


stringifyCoordinate : Coordinate -> String
stringifyCoordinate ( x, y ) =
    toString x ++ "," ++ toString y


stringifyCoordinate2 : ( Coordinate, Coordinate ) -> String
stringifyCoordinate2 ( c1, c2 ) =
    stringifyCoordinate c1 ++ " " ++ stringifyCoordinate c2


stringifyCoordinate3 : ( Coordinate, Coordinate, Coordinate ) -> String
stringifyCoordinate3 ( c1, c2, c3 ) =
    stringifyCoordinate c1 ++ " " ++ stringifyCoordinate c2 ++ " " ++ stringifyCoordinate c3



-- PARSER


{-| Parse a path string into a `MixedPath`


    parse "M0,0 l42,73"
        --> Ok [{ moveto = MoveTo Absolute (0,0), drawtos = [ LineTo Relative  [(42, 73)]]}]

Only accepts valid complete subpaths (a sequences of a move followed by zero or more draws). The types and constructors in the output are
detailed [here](#internal-data-used-by-the-parser-).

The parser uses [`elm-tools/parser`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/).
The error type is [`Parser.Error`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/Parser#Error).
-}
parse : String -> Result Parser.Error MixedPath
parse =
    Parser.run svgMixedPath


svgMixedPath : Parser (List MixedSubPath)
svgMixedPath =
    Parser.succeed identity
        |. Parser.ignore zeroOrMore isWhitespace
        |= withDefault [] moveToDrawToCommandGroups
        |. Parser.ignore zeroOrMore isWhitespace
        |. Parser.end


moveToDrawToCommandGroups : Parser (List MixedSubPath)
moveToDrawToCommandGroups =
    delimited { item = moveToDrawToCommandGroup, delimiter = Parser.ignore zeroOrMore isWhitespace }


moveToDrawToCommandGroup : Parser MixedSubPath
moveToDrawToCommandGroup =
    inContext "moveto drawto command group" <|
        Parser.succeed
            (\( move, linetos ) drawtos ->
                case linetos of
                    Nothing ->
                        MixedSubPath move drawtos

                    Just lt ->
                        MixedSubPath move (lt :: drawtos)
            )
            |= moveto
            |. Parser.ignore zeroOrMore isWhitespace
            |= withDefault [] drawtoCommands


drawtoCommands : Parser (List DrawTo)
drawtoCommands =
    inContext "drawto commands" <|
        delimited { item = drawtoCommand, delimiter = Parser.ignore zeroOrMore isWhitespace }


drawtoCommand : Parser DrawTo
drawtoCommand =
    oneOf
        [ closepath
        , lineto
        , horizontalLineto
        , verticalLineto
        , curveto
        , smoothCurveto
        , quadraticBezierCurveto
        , smoothQuadraticBezierCurveto
        , ellipticalArc
        ]



-- command : { constructor : Mode -> args -> command, character : Char, arguments : Parser args } -> Parser command


moveto : Parser ( MoveTo, Maybe DrawTo )
moveto =
    {- moveto has some corner cases

       * if a moveto is followed by extra coordinate pairs, they are interpreted as lineto commands (relative when the moveto is relative, absolute otherwise).
       * the first moveto in a path is always interpreted as absolute (but following linetos are still relative)
    -}
    inContext "moveto" <|
        command
            { constructor =
                \mode coordinates ->
                    case coordinates of
                        [] ->
                            Debug.crash "movetoArgumentSequence succeeded but parsed no coordinates"

                        [ c ] ->
                            ( MoveTo mode c, Nothing )

                        c :: cs ->
                            -- cs has at least size 1
                            ( MoveTo mode c, Just (LineTo mode cs) )
            , character = 'm'
            , arguments = movetoArgumentSequence
            }


movetoArgumentSequence : Parser (List Coordinate)
movetoArgumentSequence =
    delimited { item = coordinatePair, delimiter = withDefault () wsp }


closepath : Parser DrawTo
closepath =
    -- per the w3c spec "Since the Z and z commands take no parameters, they have an identical effect."
    inContext "closepath" <|
        oneOf
            [ symbol "z"
                |- succeed ClosePath
            , symbol "Z"
                |- succeed ClosePath
            ]


lineto : Parser DrawTo
lineto =
    inContext "lineto" <|
        command
            { constructor = LineTo
            , character = 'l'
            , arguments = linetoArgumentSequence
            }


linetoArgumentSequence : Parser (List Coordinate)
linetoArgumentSequence =
    delimited { item = coordinatePair, delimiter = withDefault () wsp }


horizontalLineto : Parser DrawTo
horizontalLineto =
    inContext "horizontal lineto" <|
        command
            { constructor = Horizontal
            , character = 'h'
            , arguments = horizontalLinetoArgumentSequence
            }


horizontalLinetoArgumentSequence : Parser (List Float)
horizontalLinetoArgumentSequence =
    delimited { item = number, delimiter = withDefault () wsp }


verticalLineto : Parser DrawTo
verticalLineto =
    inContext "vertical lineto" <|
        command
            { constructor = Vertical
            , character = 'v'
            , arguments = verticalLinetoArgumentSequence
            }


verticalLinetoArgumentSequence : Parser (List Float)
verticalLinetoArgumentSequence =
    delimited { item = number, delimiter = withDefault () wsp }


curveto : Parser DrawTo
curveto =
    inContext "curveto" <|
        command
            { constructor = CurveTo
            , character = 'c'
            , arguments = curvetoArgumentSequence
            }


curvetoArgumentSequence : Parser (List ( Coordinate, Coordinate, Coordinate ))
curvetoArgumentSequence =
    delimited { item = curvetoArgument, delimiter = withDefault () wsp }


curvetoArgument : Parser ( Coordinate, Coordinate, Coordinate )
curvetoArgument =
    succeed (,,)
        |= coordinatePair
        |. withDefault () wsp
        |= coordinatePair
        |. withDefault () wsp
        |= coordinatePair


smoothCurveto : Parser DrawTo
smoothCurveto =
    inContext "smooth curveto" <|
        command
            { constructor = SmoothCurveTo
            , character = 's'
            , arguments = smoothCurvetoArgumentSequence
            }


smoothCurvetoArgumentSequence : Parser (List ( Coordinate, Coordinate ))
smoothCurvetoArgumentSequence =
    delimited { item = smoothCurvetoArgument, delimiter = withDefault () wsp }


smoothCurvetoArgument : Parser ( Coordinate, Coordinate )
smoothCurvetoArgument =
    succeed (,)
        |= coordinatePair
        |. withDefault () wsp
        |= coordinatePair


quadraticBezierCurveto : Parser DrawTo
quadraticBezierCurveto =
    inContext "quadratic bezier curveto" <|
        command
            { constructor = QuadraticBezierCurveTo
            , character = 'q'
            , arguments = quadraticBezierCurvetoArgumentSequence
            }


quadraticBezierCurvetoArgumentSequence : Parser (List ( Coordinate, Coordinate ))
quadraticBezierCurvetoArgumentSequence =
    delimited { item = quadraticBezierCurvetoArgument, delimiter = withDefault () wsp }


quadraticBezierCurvetoArgument : Parser ( Coordinate, Coordinate )
quadraticBezierCurvetoArgument =
    succeed (,)
        |= coordinatePair
        |. withDefault () wsp
        |= coordinatePair


smoothQuadraticBezierCurveto : Parser DrawTo
smoothQuadraticBezierCurveto =
    inContext "smooth quadratic bezier curveto" <|
        command
            { constructor = SmoothQuadraticBezierCurveTo
            , character = 't'
            , arguments = smoothQuadraticBezierCurvetoArgumentSequence
            }


smoothQuadraticBezierCurvetoArgumentSequence : Parser (List Coordinate)
smoothQuadraticBezierCurvetoArgumentSequence =
    delimited { item = coordinatePair, delimiter = withDefault () wsp }


ellipticalArc : Parser DrawTo
ellipticalArc =
    inContext "elliptical arc" <|
        command
            { constructor = EllipticalArc
            , character = 'a'
            , arguments = ellipticalArcArgumentSequence
            }


ellipticalArcArgumentSequence : Parser (List EllipticalArcArgument)
ellipticalArcArgumentSequence =
    delimited { item = ellipticalArcArgument, delimiter = withDefault () wsp }


ellipticalArcArgument : Parser EllipticalArcArgument
ellipticalArcArgument =
    let
        helper rx ry xAxisRotate arcFlag direction target =
            { radii = ( rx, ry )
            , xAxisRotate = xAxisRotate
            , arcFlag =
                if arcFlag then
                    LargestArc
                else
                    SmallestArc
            , direction =
                if direction then
                    Clockwise
                else
                    CounterClockwise
            , target = target
            }
    in
        succeed helper
            |= nonNegativeNumber
            |. optional commaWsp
            |= nonNegativeNumber
            |. withDefault () commaWsp
            |= number
            |. commaWsp
            |= flag
            |. withDefault () commaWsp
            |= flag
            |. withDefault () commaWsp
            |= coordinatePair


{-| Construct both the absolute and relative parser for a command.
-}
command : { constructor : Mode -> args -> command, character : Char, arguments : Parser args } -> Parser command
command { constructor, character, arguments } =
    oneOf
        [ succeed (constructor Absolute)
            |. symbol (String.fromChar <| Char.toUpper character)
            |. Parser.ignore zeroOrMore isWhitespace
            |= arguments
        , succeed (constructor Relative)
            |. symbol (String.fromChar <| Char.toLower character)
            |. Parser.ignore zeroOrMore isWhitespace
            |= arguments
        ]
