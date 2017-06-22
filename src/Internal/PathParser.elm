module PathParser exposing (..)

{-| Module for parsing SVG path syntax, using [elm-tools/parser](http://package.elm-lang.org/packages/elm-tools/parser/latest)
The data structure and parser is modeled according to [this W3C grammar](https://www.w3.org/TR/SVG/paths.html#PathDataBNF)

### data

The basic structure:

* a `path` is a list of `moveto-drawto-command-group`s (and the empty path `""` is valid).
* a `subpath` (internally `moveto-drawto-command-group`) is a moveto command followed by a list of drawto commands (also called a subpath)

The building blocks are

* moveto - contains `M` and `m` commands.
* drawto - the other commands (lineto, elliptical arc, bezier curve)

The parsers themselves often have three parts

* `*` parses the full command  `L20,20 40,40`
* `*ArgumentSequence` parses a list of arguments (`20, 20 40,40` in `L20,20 40,40`)
* `*Argument` parses a single argument (`20, 20` in `L20,20 40,40`)

### parsing strictness

This parser is more strict than the linked parser above, because the SVG has to be well-typed. Specifically,
the W3C grammar allows "coordinates" that consist of only one number. Thus it accepts `M100-200` (and equivalently `M100 M-200`), whereas this parser will not..
-}

import Parser exposing (..)
import Char


{-| A path is a list of [subpath](#SubPath)s.
-}
type alias Path =
    List SubPath


{-| A subpath consists of a [moveto](#MoveTo) instruction followed by a list of [drawto](#DrawTo) instructions
-}
type alias SubPath =
    { moveto : MoveTo, drawtos : List DrawTo }


type alias Coordinate =
    ( Float, Float )


{-| The mode of an instruction

When absolute, a given coordinate is interpreted as an absolute position on the svg canvas.
When relative, a given coordinate is interpreted as a translation from the current cursor position
-}
type Mode
    = Relative
    | Absolute


{-| MoveTo instructions

MoveTo instructions move the cursor, but don't draw anything.
-}
type alias MoveTo =
    AbstractMoveTo Mode


type AbstractMoveTo mode
    = MoveTo mode Coordinate


{-| DrawTo instructions

DrawTo instructions perform the drawing from the current cursor position to their target.
-}
type alias DrawTo =
    AbstractDrawTo Mode


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


type Sign
    = Plus
    | Minus


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


svgPath : Parser (List SubPath)
svgPath =
    succeed identity
        |. Parser.ignore zeroOrMore isWhitespace
        |= withDefault [] moveToDrawToCommandGroups
        |. Parser.ignore zeroOrMore isWhitespace
        |. Parser.end


moveToDrawToCommandGroups : Parser (List SubPath)
moveToDrawToCommandGroups =
    delimited { item = moveToDrawToCommandGroup, delimiter = Parser.ignore zeroOrMore isWhitespace }


moveToDrawToCommandGroup : Parser SubPath
moveToDrawToCommandGroup =
    inContext "moveto drawto command group" <|
        succeed
            (\( move, linetos ) drawtos ->
                case linetos of
                    Nothing ->
                        SubPath move drawtos

                    Just lt ->
                        SubPath move (lt :: drawtos)
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



-- Primitives


{-| Parse a sequence of values separated by a delimiter

This parser is used to for example parse the comma or whitespace-delimited arguments for a horizontal move

    Parser.run (delimited { delimiter = optional commaWsp, item = number }) "1 2 3 4" == [1,2,3,4]
-}
delimited : { delimiter : Parser (), item : Parser a } -> Parser (List a)
delimited { delimiter, item } =
    oneOf
        [ item
            |> Parser.andThen (\first -> delimitedEndForbidden item delimiter [ first ])
        , Parser.succeed []
        ]


delimitedEndForbidden : Parser a -> Parser () -> List a -> Parser (List a)
delimitedEndForbidden parseItem delimiter revItems =
    let
        chompRest item =
            delimitedEndForbidden parseItem delimiter (item :: revItems)
    in
        oneOf
            [ delayedCommit delimiter <|
                andThen chompRest parseItem
            , succeed (List.reverse revItems)
            ]


sign : Parser Sign
sign =
    oneOf
        [ symbol "-"
            |- succeed Minus
        , symbol "+"
            |- succeed Plus
        ]


digitSequence : Parser Int
digitSequence =
    Parser.int


type Exponent
    = Exponent Int


exponent : Parser Exponent
exponent =
    Parser.map Exponent <|
        succeed applySign
            |. oneOf [ symbol "e", symbol "E" ]
            |= withDefault Plus sign
            |= digitSequence


fractionalConstant : Parser Float
fractionalConstant =
    let
        helper left right =
            case String.toFloat (toString left ++ "." ++ toString right) of
                Err e ->
                    fail e

                Ok v ->
                    succeed v
    in
        join <|
            oneOf
                [ succeed helper
                    |= withDefault 0 digitSequence
                    |. symbol "."
                    |= digitSequence
                , succeed (\left -> helper left 0)
                    |= digitSequence
                    |. symbol "."
                ]


join : Parser (Parser a) -> Parser a
join =
    Parser.andThen identity


applyExponent : Float -> Exponent -> Parser Float
applyExponent float (Exponent exp) =
    case String.toFloat (toString float ++ "e" ++ toString exp) of
        Err e ->
            fail e

        Ok v ->
            succeed v


floatingPointConstant : Parser Float
floatingPointConstant =
    join <|
        oneOf
            [ succeed applyExponent
                |= fractionalConstant
                |= withDefault (Exponent 0) exponent
            , succeed applyExponent
                |= Parser.map toFloat digitSequence
                |= exponent
            ]


integerConstant : Parser Int
integerConstant =
    Parser.int


comma : Parser ()
comma =
    symbol ","


wsp : Parser ()
wsp =
    inContext "whitespace" <|
        -- (#x20 | #x9 | #xD | #xA)
        oneOf [ symbol " ", symbol "\t", symbol "\x0D", symbol "\n" ]


isWhitespace : Char -> Bool
isWhitespace char =
    char == ' ' || char == '\t' || char == '\x0D' || char == '\n'


commaWsp : Parser ()
commaWsp =
    inContext "comma or whitespace" <|
        oneOf
            [ succeed ()
                |. Parser.ignore oneOrMore isWhitespace
                |. withDefault () comma
                |. Parser.ignore zeroOrMore isWhitespace
            , succeed ()
                |. comma
                |. Parser.ignore zeroOrMore isWhitespace
            ]


flag : Parser Bool
flag =
    inContext "flag" <|
        oneOf
            [ symbol "1"
                |> Parser.map (\_ -> True)
            , symbol "0"
                |> Parser.map (\_ -> False)
            ]


applySign : Sign -> number -> number
applySign sign num =
    case sign of
        Plus ->
            num

        Minus ->
            -num


number : Parser Float
number =
    inContext "number" <|
        oneOf
            [ succeed applySign
                |= withDefault Plus sign
                |= integerConstant
                |> Parser.map toFloat
            , succeed applySign
                |= withDefault Plus sign
                |= floatingPointConstant
            ]


nonNegativeNumber : Parser Float
nonNegativeNumber =
    inContext "non-negative number" <|
        oneOf
            [ Parser.map toFloat integerConstant
            , floatingPointConstant
            ]


coordinatePair : Parser Coordinate
coordinatePair =
    inContext "coordinate pair" <|
        succeed (,)
            |= number
            |. commaWsp
            |= number



-- Parser Helpers


{-| Try a parser. If it fails, give back the default value
-}
withDefault : a -> Parser a -> Parser a
withDefault default parser =
    oneOf [ parser, succeed default ]


{-| Parse zero or one values of a given parser.
This function is often written as a `?` in grammars, so `int?` is `optional int`
-}
optional : Parser a -> Parser ()
optional parser =
    oneOf
        [ parser
            |- succeed ()
        , succeed ()
        ]


{-| Ignore everything that came before, start fresh
-}
(|-) : Parser ignore -> Parser keep -> Parser keep
(|-) ignoreParser keepParser =
    map2 (\_ keep -> keep) ignoreParser keepParser
