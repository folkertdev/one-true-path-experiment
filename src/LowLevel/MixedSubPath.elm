module LowLevel.MixedSubPath exposing (..)

{-|
-}

import LowLevel.MixedCommand exposing (..)
import LowLevel.SvgPathSyntax as SvgSyntax
import LowLevel.SvgPathParse
import Parser


{-| A subpath consists of a [`MoveTo`](#MoveTo) instruction followed by a list of [`DrawTo`](#DrawTo) instructions


-}
type alias MixedSubPath =
    { moveto : MoveTo, drawtos : List DrawTo }


type alias AbsoluteSubPath =
    { moveto : AbstractMoveTo (), drawtos : List (AbstractDrawTo ()) }


toString : MixedSubPath -> String
toString { moveto, drawtos } =
    SvgSyntax.stringifyMoveTo moveto ++ " " ++ String.join " " (List.map SvgSyntax.stringifyDrawTo drawtos)


{-| Parse a path string into a `MixedPath`


    parse "M0,0 l42,73"
        --> Ok [{ moveto = MoveTo Absolute (0,0), drawtos = [ LineTo Relative  [(42, 73)]]}]

Only accepts valid complete subpaths (a sequences of a move followed by zero or more draws). The types and constructors in the output are
detailed [here](#internal-data-used-by-the-parser-).

The parser uses [`elm-tools/parser`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/).
The error type is [`Parser.Error`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/Parser#Error).
-}
parse : String -> Result Parser.Error (List AbsoluteSubPath)
parse string =
    let
        convert subpaths =
            case List.map (uncurry MixedSubPath) subpaths of
                [] ->
                    []

                ({ moveto } :: ss) as subpaths ->
                    let
                        (MoveTo _ coordinate) =
                            moveto

                        initialState =
                            { start = coordinate, cursor = coordinate }

                        folder mixedSubPath ( cursorState, accum ) =
                            toAbsolute mixedSubPath cursorState
                                |> Tuple.mapSecond (\item -> item :: accum)
                    in
                        List.foldl folder ( initialState, [] ) subpaths
                            |> Tuple.second
                            |> List.reverse
    in
        string
            |> Parser.run LowLevel.SvgPathParse.svgMixedPath
            |> Result.map (convert)


parseSubPath : String -> Result Parser.Error AbsoluteSubPath
parseSubPath string =
    case Parser.run LowLevel.SvgPathParse.svgMixedSubPath string of
        Ok ( MoveTo mode coordinate, drawtos ) ->
            toAbsolute (MixedSubPath (MoveTo mode coordinate) drawtos) { start = coordinate, cursor = coordinate }
                |> Tuple.second
                |> Ok

        Err e ->
            Err e


toAbsolute : MixedSubPath -> CursorState -> ( CursorState, AbsoluteSubPath )
toAbsolute { moveto, drawtos } ({ start, cursor } as state) =
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
