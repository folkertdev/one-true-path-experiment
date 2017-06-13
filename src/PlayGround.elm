module PlayGround exposing (..)

import Html exposing (..)
import PathParser exposing (svgPath)
import Command exposing (..)
import Path exposing (..)
import ToAbsolute exposing (..)
import Parser


main =
    div []
        [ Parser.run svgPath "M0,0 L20,20 l20,20"
            |> Result.map stringifyPath
            |> display
        , [ { moveto = MoveTo Absolute ( 0, 0 ), drawtos = [ LineTo Absolute [ ( 42, 42 ) ], Horizontal Absolute [ 43, 45 ] ] } ]
            |> toCommands
            |> fromCommands
            |> display
        , [ { moveto = MoveTo Absolute ( 10, 10 ), drawtos = [ LineTo Relative [ ( 42, 42 ) ], Horizontal Absolute [ 43, 45 ] ] } ]
            |> toAbsolutePath
            |> display
        ]


display =
    p [] << List.singleton << text << toString
