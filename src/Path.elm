module Path
    exposing
        ( Path
        , element
        , parse
        , toString
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

-}

import Parser
import Path.LowLevel.Parser as PathParser
import SubPath exposing (SubPath, subpath)
import Svg
import Svg.Attributes


{-| A path is a list of [`SubPath`](#subpath)s.
-}
type alias Path =
    List SubPath


{-| Construct an svg path element from a `Path` with the given attributes
-}
element : Path -> List (Svg.Attribute msg) -> Svg.Svg msg
element path attributes =
    Svg.path (Svg.Attributes.d (toString path) :: attributes) []


{-| Turn a `Path` into a `String`. The result is ready to be used with the `d` attribute.


    myPath : SubPath
    myPath =
        subpath (moveTo (0,0)) [ lineTo [ (42, 73) ] ]


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
toString : Path -> String
toString =
    String.join " " << List.map SubPath.toString


{-| Parse a path string into a `Path`

    import SubPath exposing (SubPath, subpath)
    import LowLevel.Command exposing (moveTo, lineTo)

    expected : SubPath
    expected =
        subpath (moveTo (0,0)) [ lineTo [ (42, 73) ] ]

    parse "M0,0 l42,73"
        --> Ok [expected]


Only accepts valid complete subpaths (a sequences of a move followed by zero or more draws). Relative instructions are converted to absolute ones. Short-hand curve extensions are converted to explicit curve instructions.

The parser uses [`elm-tools/parser`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/).
The error type is [`Parser.Error`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/Parser#Error).

-}
parse : String -> Result Parser.Error Path
parse =
    Result.map SubPath.fromLowLevel << PathParser.parse
