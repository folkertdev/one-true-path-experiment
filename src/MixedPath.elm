module MixedPath
    exposing
        ( AbstractMoveTo(..)
        , AbstractDrawTo(..)
        , MoveTo
        , DrawTo
        , AbsolutePath
        , AbsoluteSubPath
        , Direction(..)
        , ArcFlag(..)
        , Mode(..)
        , MixedPath
        , MixedSubPath
        , parse
        , toString
        , moveToDrawToCommandGroup
        , moveToDrawToCommandGroups
        , linetoArgumentSequence
        , svgMixedPath
        , moveto
        , lineto
        , closepath
        , horizontalLineto
        , verticalLineto
        , quadraticBezierCurveto
        , smoothQuadraticBezierCurveto
        , curveto
        , smoothCurveto
        , ellipticalArc
        , toAbsoluteMoveTo
        , toAbsoluteDrawTo
        )

{-| Low-level module for parsing SVG path strings

This module is actually capable of parsing and working with both relative and absolute commands.

-}

import Char
import Parser exposing (Parser, (|.), (|=), oneOrMore, zeroOrMore, inContext, oneOf, symbol, succeed)
import ParserPrimitives exposing (delimited, isWhitespace, (|-), wsp, withDefault, coordinatePair, nonNegativeNumber, number, commaWsp, optional, flag)
import Vector2 as Vec2 exposing (Vec2)
import Vector3 as Vec3 exposing (Vec3)


type alias CursorState =
    { start : Vec2 Float, cursor : Vec2 Float }


{-| Exposed for testing
-}
toAbsoluteMoveTo : CursorState -> AbstractMoveTo Mode -> ( AbstractMoveTo (), CursorState )
toAbsoluteMoveTo { start, cursor } (MoveTo mode coordinate) =
    case mode of
        Absolute ->
            ( MoveTo () coordinate, { start = coordinate, cursor = coordinate } )

        Relative ->
            let
                newCoordinate =
                    uncurry Vec2.add ( cursor, coordinate )
            in
                ( MoveTo () newCoordinate, { start = newCoordinate, cursor = newCoordinate } )


{-| Exposed for testing
-}
toAbsoluteDrawTo : CursorState -> AbstractDrawTo Mode -> ( AbstractDrawTo (), CursorState )
toAbsoluteDrawTo ({ start, cursor } as state) drawto =
    case drawto of
        LineTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (coordinateToAbsolute cursor) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( LineTo () [], state )

                    Just finalCoordinate ->
                        ( LineTo () absoluteCoordinates
                        , { state | cursor = finalCoordinate }
                        )

        Horizontal mode xs ->
            let
                absoluteCoordinates =
                    List.map (\x -> ( x, 0 )) xs
                        |> coordinatesToAbsolute mode (coordinateToAbsolute cursor)
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( Horizontal () [], state )

                    Just ( finalX, _ ) ->
                        ( Horizontal () (List.map Tuple.first absoluteCoordinates)
                        , { state | cursor = ( finalX, Tuple.second cursor ) }
                        )

        Vertical mode ys ->
            let
                absoluteCoordinates =
                    List.map (\y -> ( 0, y )) ys
                        |> coordinatesToAbsolute mode (coordinateToAbsolute cursor)
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( Vertical () [], state )

                    Just ( _, finalY ) ->
                        ( Vertical () (List.map Tuple.second absoluteCoordinates)
                        , { state | cursor = ( Tuple.first cursor, finalY ) }
                        )

        CurveTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (Vec3.map (coordinateToAbsolute cursor)) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( CurveTo () [], state )

                    Just ( _, _, target ) ->
                        ( CurveTo () absoluteCoordinates, { state | cursor = target } )

        SmoothCurveTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (Vec2.map (coordinateToAbsolute cursor)) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( SmoothCurveTo () [], state )

                    Just ( _, target ) ->
                        ( SmoothCurveTo () absoluteCoordinates, { state | cursor = target } )

        QuadraticBezierCurveTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (Vec2.map (coordinateToAbsolute cursor)) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( QuadraticBezierCurveTo () [], state )

                    Just ( _, target ) ->
                        ( QuadraticBezierCurveTo () absoluteCoordinates, { state | cursor = target } )

        SmoothQuadraticBezierCurveTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (coordinateToAbsolute cursor) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( SmoothQuadraticBezierCurveTo () [], state )

                    Just finalCoordinate ->
                        ( SmoothQuadraticBezierCurveTo () absoluteCoordinates
                        , { state | cursor = finalCoordinate }
                        )

        EllipticalArc mode arguments ->
            let
                argumentToAbsolute cursor argument =
                    { argument | target = Vec2.add cursor argument.target }

                absoluteArguments =
                    coordinatesToAbsolute mode (argumentToAbsolute cursor) arguments
            in
                case last absoluteArguments of
                    Nothing ->
                        ( EllipticalArc () [], state )

                    Just { target } ->
                        ( EllipticalArc () absoluteArguments, { state | cursor = target } )

        ClosePath ->
            ( ClosePath, { state | cursor = start } )


coordinateToAbsolute : Vec2 Float -> Vec2 Float -> Vec2 Float
coordinateToAbsolute =
    Vec2.add


coordinatesToAbsolute : Mode -> (coords -> coords) -> List coords -> List coords
coordinatesToAbsolute mode toAbsolute coordinates =
    case mode of
        Absolute ->
            coordinates

        Relative ->
            List.map toAbsolute coordinates


last : List a -> Maybe a
last =
    List.foldr
        (\element accum ->
            if accum == Nothing then
                Just element
            else
                accum
        )
        Nothing


toAbsoluteSubPath : MixedSubPath -> CursorState -> ( CursorState, AbsoluteSubPath )
toAbsoluteSubPath { moveto, drawtos } ({ start, cursor } as state) =
    let
        ( newStart, newState ) =
            toAbsoluteMoveTo state moveto

        swap ( a, b ) =
            ( b, a )

        folder mixedDrawTo ( cursorState, accum ) =
            toAbsoluteDrawTo cursorState mixedDrawTo
                |> swap
                |> Tuple.mapSecond (\absoluteDrawTo -> absoluteDrawTo :: accum)

        ( newerState, newDrawtos ) =
            List.foldl folder ( newState, [] ) drawtos
                |> Tuple.mapSecond List.reverse
    in
        ( newerState
        , { moveto = newStart, drawtos = newDrawtos }
        )


{-| Helpers for converting relative instructions to absolute ones

This is possible on a path level, because the first move instruction will always be interpreted as absolute.
Therefore, there is an anchor for subsequent relative commands.
-}
toAbsolutePath : MixedPath -> AbsolutePath
toAbsolutePath subpaths =
    case subpaths of
        [] ->
            []

        ({ moveto } as sp) :: sps ->
            case moveto of
                MoveTo _ coordinate ->
                    let
                        initialState =
                            { start = coordinate, cursor = coordinate }

                        folder mixedSubPath ( cursorState, accum ) =
                            toAbsoluteSubPath mixedSubPath cursorState
                                |> Tuple.mapSecond (\item -> item :: accum)
                    in
                        List.foldl folder ( initialState, [] ) subpaths
                            |> Tuple.second
                            |> List.reverse


{-| A path is a list of [`MixedSubPath`](#MixedSubPath)s.
-}
type alias MixedPath =
    List MixedSubPath


{-| A subpath consists of a [`MoveTo`](#MoveTo) instruction followed by a list of [`DrawTo`](#DrawTo) instructions


-}
type alias MixedSubPath =
    { moveto : MoveTo, drawtos : List DrawTo }


type alias AbsolutePath =
    List AbsoluteSubPath


type alias AbsoluteSubPath =
    { moveto : AbstractMoveTo (), drawtos : List (AbstractDrawTo ()) }


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


type AbstractMoveTo mode
    = MoveTo mode (Vec2 Float)


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
                    CurveTo mode (List.map (Vec3.map f) coordinates)

                SmoothCurveTo mode coordinates ->
                    SmoothCurveTo mode (List.map (Vec2.map f) coordinates)

                QuadraticBezierCurveTo mode coordinates ->
                    QuadraticBezierCurveTo mode (List.map (Vec2.map f) coordinates)

                SmoothQuadraticBezierCurveTo mode coordinates ->
                    SmoothQuadraticBezierCurveTo mode (List.map f coordinates)

                EllipticalArc mode arguments ->
                    EllipticalArc mode (List.map (\argument -> { argument | target = f argument.target }) arguments)

                ClosePath ->
                    ClosePath
    in
        List.map helper path


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
toString : MixedPath -> String
toString subpaths =
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
            stringifyCharacter mode 'H' ++ String.join " " (List.map Basics.toString coordinates)

        Vertical mode coordinates ->
            stringifyCharacter mode 'V' ++ String.join " " (List.map Basics.toString coordinates)

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
        , Basics.toString xAxisRotate
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
    Basics.toString x ++ "," ++ Basics.toString y


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
parse : String -> Result Parser.Error AbsolutePath
parse =
    Parser.run svgMixedPath
        >> Result.map toAbsolutePath


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
