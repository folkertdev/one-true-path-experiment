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
    | EllipticArc Mode (List EllipticalArcArgument)
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
    EllipticArc Absolute



-- Composing Paths


{-| Overlay two paths (this only makes sense when both paths start with an absolute move instruction)
-}
overlay : Path -> Path -> Path
overlay =
    (++)


{-| Visually concatenate 2 paths: the instructions in the second path will be ofset by the last position in the first path
-}
concatenate : Path -> Path -> Path
concatenate pathA pathB =
    Debug.crash "not implemented"



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

        _ ->
            ""


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
