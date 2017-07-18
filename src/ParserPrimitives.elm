module ParserPrimitives exposing (..)

{-| Helpers for parsing the primitives of SVG path syntax, based on [this W3C grammar](https://www.w3.org/TR/SVG/paths.html#PathDataBNF).
-}

import Parser exposing (..)
import Char


type alias Coordinate =
    ( Float, Float )


type Sign
    = Plus
    | Minus



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
    Parser.keep oneOrMore Char.isDigit
        |> Parser.andThen
            (\v ->
                case String.toInt v of
                    Err e ->
                        Parser.fail e

                    Ok v ->
                        Parser.succeed v
            )


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
        inContext "floatingPointConstant" <|
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
    digitSequence


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
