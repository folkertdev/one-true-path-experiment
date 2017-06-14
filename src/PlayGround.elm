module PlayGround exposing (..)

import Html exposing (..)
import PathParser exposing (svgPath)
import Command exposing (..)
import Path exposing (..)
import ToAbsolute exposing (..)
import Parser


{-| From opensolid/svg

        pathComponents =
            [ "M"
            , toString x1
            , toString y1
            , "Q"
            , toString x2
            , toString y2
            , toString x3
            , toString y3
            ]

        pathAttribute =
            Attributes.d (String.join " " pathComponents)

-}
openSolidExample x1 y1 x2 y2 x3 y3 =
    subpath (Path.moveTo ( x1, y1 )) [ quadraticCurveTo [ ( x2, y2 ), ( x3, y3 ) ] ]
        |> stringifySubPath


arcToExample arc dx dy =
    stringifyDrawTo <|
        arcTo
            [ { radii = ( arc.radius.position, arc.radius.position )
              , xAxisRotate = 0
              , arcFlag = LargestArc
              , direction = CounterClockwise
              , target = ( arc.x.position - dx, arc.y.position - dy )
              }
            , { radii = ( arc.radius.position, arc.radius.position )
              , xAxisRotate = 0
              , arcFlag = LargestArc
              , direction = Clockwise
              , target = ( arc.x.position + dx, arc.y.position + dy )
              }
            ]


main =
    div []
        [ Parser.run svgPath "M0,0 L20,20 l20,20"
            |> Result.map stringifyPath
            |> display
        , [ { moveto = MoveTo Absolute ( 0, 0 ), drawtos = [ LineTo Absolute [ ( 42, 42 ) ], Horizontal Absolute [ 43, 45 ] ] } ]
            |> toCommands
            |> fromCommands
            |> display
        , [ { moveto = MoveTo Absolute ( 10, 10 ), drawtos = [ LineTo Relative [ ( 42, 42 ) ], Horizontal Relative [ 43, 45 ] ] } ]
            |> toAbsolutePath
            |> display
        ]


display =
    p [] << List.singleton << text << toString
