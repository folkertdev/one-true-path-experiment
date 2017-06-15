module Path exposing (..)

{-| Module for working with svg paths
-}

import Char


type alias Coordinate =
    ( Float, Float )


{-| A path is a list of subpaths

**Note:** The first move instruction of a path will always be interpreted as absolute.
-}
type alias Path =
    List SubPath


{-| A subpath consists of a moveto instruction followed by a list of drawto instructions
-}
type alias SubPath =
    { moveto : MoveTo, drawtos : List DrawTo }


{-| MoveTo instructions
-}
type MoveTo
    = MoveTo Mode Coordinate


{-| DrawTo instructions
-}
type DrawTo
    = LineTo Mode (List Coordinate)
    | Horizontal Mode (List Float)
    | Vertical Mode (List Float)
    | CurveTo Mode (List ( Coordinate, Coordinate, Coordinate ))
    | SmoothCurveTo Mode (List ( Coordinate, Coordinate ))
    | QuadraticBezierCurveTo Mode (List ( Coordinate, Coordinate ))
    | SmoothQuadraticBezierCurveTo Mode (List Coordinate)
    | EllipticalArc Mode (List EllipticalArcArgument)
    | ClosePath


{-| The mode of an instruction

When absolute, a given coordinate is interpreted as an absolute position on the svg canvas.
When relative, a given coordinate is interpreted as a translation from the current cursor position
-}
type Mode
    = Relative
    | Absolute


type alias EllipticalArcArgument =
    { radii : ( Float, Float )
    , xAxisRotate : Float
    , arcFlag : ArcFlag
    , direction : Direction
    , target : Coordinate
    }


type ArcFlag
    = SmallestArc
    | LargestArc


type Direction
    = Clockwise
    | CounterClockwise



-- Creating paths


myPath =
    [ subpath (moveTo ( 0, 0 ))
        [ lineTo [ ( 100, 0 ), ( 100, 100 ), ( 0, 100 ) ]
        , closepath
        ]
    , subpath (moveBy ( 42, 42 ))
        [ lineBy [ ( 30, 20 ) ]
        ]
    ]


moveTo =
    MoveTo Absolute


moveBy =
    MoveTo Relative


lineTo =
    LineTo Absolute


lineBy =
    LineTo Relative


subpath =
    SubPath


closepath =
    ClosePath


quadraticCurveTo =
    QuadraticBezierCurveTo Absolute


arcTo =
    EllipticalArc Absolute



-- STRINGIFY


stringifyPath : Path -> String
stringifyPath subpaths =
    String.join " " (List.map stringifySubPath subpaths)


stringifySubPath : SubPath -> String
stringifySubPath { moveto, drawtos } =
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
