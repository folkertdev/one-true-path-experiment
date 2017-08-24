module LowLevel.SvgPathSyntax exposing (..)

import Char
import LowLevel.MixedCommand exposing (..)


stringifyMoveTo : MoveTo -> String
stringifyMoveTo (MoveTo mode coordinate) =
    case mode of
        Absolute ->
            "M" ++ stringifyCoordinate coordinate

        Relative ->
            "m" ++ stringifyCoordinate coordinate


stringifyDrawTo : DrawTo -> String
stringifyDrawTo command =
    if isEmpty command then
        ""
    else
        case command of
            LineTo mode coordinates ->
                stringifyCharacter mode 'L' ++ String.join " " (List.map stringifyCoordinate coordinates)

            Horizontal mode coordinates ->
                if List.isEmpty coordinates then
                    ""
                else
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


{-| Checks that there is an actual command in the drawto

A path string like "L" without arguments is invalid, but not ruled out by the types, so we have
to do this check when converting to string in order to ensure valid path syntax
-}
isEmpty : DrawTo -> Bool
isEmpty command =
    case command of
        LineTo mode coordinates ->
            List.isEmpty coordinates

        Horizontal mode coordinates ->
            List.isEmpty coordinates

        Vertical mode coordinates ->
            List.isEmpty coordinates

        CurveTo mode coordinates ->
            List.isEmpty coordinates

        SmoothCurveTo mode coordinates ->
            List.isEmpty coordinates

        QuadraticBezierCurveTo mode coordinates ->
            List.isEmpty coordinates

        SmoothQuadraticBezierCurveTo mode coordinates ->
            List.isEmpty coordinates

        EllipticalArc mode arguments ->
            List.isEmpty arguments

        ClosePath ->
            False


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
