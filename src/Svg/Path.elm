module Svg.Path exposing
    ( parse
    , element, toString
    , fromLowLevel, toLowLevel
    , SvgPath
    )

{-| Module for layering SubPaths into Paths.

Most of the interesting stuff happens in the `SubPath` and `Curve` modules.
`Path` is simply for combining multiple subpaths into one string or element.


## Data Structures

@docs Path


## Constructing Paths

@docs parse


## Creating SVG

@docs element, toString


## Conversion

@docs fromLowLevel, toLowLevel

-}

import LowLevel.Command as Command
import Parser.Advanced
import Path.LowLevel as LowLevel
import Path.LowLevel.Parser as PathParser
import Svg
import Svg.Attributes
import Svg.SubPath exposing (SubPath)


{-| A path is a list of [`SubPath`](#subpath)s.
-}
type alias SvgPath =
    List SubPath


{-| Construct an svg path element from a `Path` with the given attributes
-}
element : SvgPath -> List (Svg.Attribute msg) -> Svg.Svg msg
element path attributes =
    Svg.path (Svg.Attributes.d (toString path) :: attributes) []


{-| Turn a `Path` into a `String`. The result is ready to be used with the `d` attribute.

    import Svg.Curve as Curve
    import SubPath exposing (SubPath)

    myPath : SubPath
    myPath =
        Curve.linear [ (0,0), (42, 73) ]

    Path.toString [ myPath ]
        --> "M0,0 L42,73"

    -- forms a cycle (almost isomorphism) with parse
    arc : String
    arc = "M80,230 A45,45 90 0 1 125,275"

    arc
        |> parse
        |> Result.map Path.toString
        --> Ok arc

-}
toString : SvgPath -> String
toString =
    String.join " " << List.map Svg.SubPath.toString


{-| Parse a path string into a `Path`

    import Svg.Curve as Curve
    import SubPath exposing (SubPath)

    expected : SubPath
    expected =
        Curve.linear [ (0,0), (42, 73) ]

    parse "M0,0 l42,73"
        --> Ok [expected]

Only accepts valid complete subpaths (a sequences of a move followed by zero or more draws). Relative instructions are converted to absolute ones. Short-hand curve extensions are converted to explicit curve instructions.

The parser uses [`elm-tools/parser`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/).
The error type is [`Parser.Error`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/Parser#Error).

-}
parse : String -> Result (List (Parser.Advanced.DeadEnd String String)) SvgPath
parse =
    Result.map fromLowLevel << PathParser.parse


{-| Converting a svg-path-lowlevel subpath into a one-true-path subpath. Used in parsing
-}
fromLowLevel : List LowLevel.SubPath -> SvgPath
fromLowLevel lowlevels =
    case lowlevels of
        [] ->
            []

        first :: _ ->
            -- first moveto is always interpreted absolute
            case first.moveto of
                LowLevel.MoveTo _ target ->
                    let
                        initialCursorState =
                            { start = target, cursor = target, previousControlPoint = Nothing }

                        folder { moveto, drawtos } ( state, accum ) =
                            let
                                ( stateAfterMoveTo, newMoveTo ) =
                                    Command.fromLowLevelMoveTo moveto state

                                ( stateAfterDrawtos, newDrawTos ) =
                                    Command.fromLowLevelDrawTos drawtos stateAfterMoveTo
                            in
                            ( stateAfterDrawtos, Svg.SubPath.with newMoveTo newDrawTos :: accum )
                    in
                    List.foldl folder ( initialCursorState, [] ) lowlevels
                        |> Tuple.second
                        |> List.reverse


{-| Convert a path to a svg-path-lowlevel list of subpaths
-}
toLowLevel : SvgPath -> List LowLevel.SubPath
toLowLevel =
    List.filterMap Svg.SubPath.toLowLevel
