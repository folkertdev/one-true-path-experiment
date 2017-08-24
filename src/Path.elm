module Path
    exposing
        ( Path
        , toString
        , element
        , parse
        , mapCoordinate
        , mapWithCursorState
        )

{-| Low-level module for constructing svg paths.

This module provides a wrapper around the svg path syntax. A path can be parsed from a string or build up using the
svg path primitives, and then converted to a string that can be used in the [`d` attribute][d-attribute] to render the path.

Note that this is not the most convenient way of drawing. This package is mainly meant as a primitive to build other packages on top of.
If you want to visualize data, have a look at [elm-plot] and [elm-visualization]. If you want to draw geometry, check out [opensolid].


For more information on svg paths, see the [MDN documentation].

[MDN documentation]: https://developer.mozilla.org/en/docs/Web/SVG/Tutorial/MixedPaths.
[elm-plot]: http://package.elm-lang.org/packages/terezka/elm-plot/latest
[elm-visualization]: http://package.elm-lang.org/packages/gampleman/elm-visualization/latest
[opensolid]: http://package.elm-lang.org/packages/opensolid/geometry/latest
[d-attribute]: https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d

[`MoveTo`]: #MoveTo
[`DrawTo`]: #DrawTo
[with the spec]: https://www.w3.org/TR/SVG/paths.html#PathDataMovetoCommands

## Data Layout

A path is a list of subpaths that are drawn in order. A subpath consists of a [`MoveTo`] instruction followed by a list of [`DrawTo`] instructions.

This package only supports absolute coordinates and instructions, but it is possible to parse strings with relative intructions.
When a path is parsed, the first [`MoveTo`] instruction is always interpreted as absolute (this is in accordance [with the spec]),
thus making sure  that there is always an absolute cursor position.


The constructors are exposed, so if you need an escape hatch it is available. As always though, never reach for it when there are other options.

The vector types are from [Zinggi/elm-webgl-math](http://package.elm-lang.org/packages/Zinggi/elm-webgl-math/latest). They are just type aliases for tuples.

## Example

```elm
-- (Float, Float) is equivalent to Vec2 Float
myPoints : List (Float, Float)

-- connect all the points with a straight line
linear : List (Vec2 Float) -> Path
linear points =
    case points of
        [] ->
            []

        p::ps ->
            [ subpath (moveTo p) [ lineTo ps ] ]


main =
    Svg.svg [ width "400", height "400" ]
        [ Path.element (linear myPoints) [ fill "none" ]
        ]
```

## Data Structures
@docs Path

## Constructing Paths
@docs parse

## Creating SVG
@docs element, toString

## Modifying Paths
@docs mapCoordinate, mapWithCursorState



-}

import Svg
import Svg.Attributes
import Parser
import Vector2 as Vec2 exposing (Vec2)
import LowLevel.Command as LowLevel exposing (DrawTo, CursorState)
import LowLevel.MixedSubPath as MixedSubPath
import LowLevel.Convert as Convert


--import MixedPath exposing (AbstractMoveTo, AbstractDrawTo, MixedPath)

import SubPath exposing (SubPath, subpath)


{-| Construct an svg path element from a `Path` with the given attributes
-}
element : Path -> List (Svg.Attribute msg) -> Svg.Svg msg
element path attributes =
    Svg.path (Svg.Attributes.d (toString path) :: attributes) []


{-| A path is a list of [`subpath`](#subpath)s.
-}
type alias Path =
    List SubPath


{-| Turn a `MixedPath` into a `String`. The result is ready to be used with the `d` attribute.

    Path.toString [ subpath (moveTo (0,0)) [ lineBy ( 42, 73 ) ] ]
        --> "M0,0 l42,73"
-}
toString : Path -> String
toString =
    String.join " " << List.map SubPath.toString


{-| Parse a path string into a `Path`


    parse "M0,0 l42,73"
        --> Ok [{ moveto = MoveTo Absolute (0,0), drawtos = [ LineTo Relative  [(42, 73)]]}]

Only accepts valid complete subpaths (a sequences of a move followed by zero or more draws). Relative instructions are converted to absolute ones.
The types and constructors in the output are described [here](#internal-data-used-by-the-parser-).

The parser uses [`elm-tools/parser`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/).
The error type is [`Parser.Error`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/Parser#Error).
-}
parse : String -> Result Parser.Error (List SubPath)
parse =
    let
        convert { moveto, drawtos } =
            subpath (Convert.fromMixedMoveTo moveto) (List.map Convert.fromMixedDrawTo drawtos)
    in
        MixedSubPath.parse
            >> Result.map (List.map convert)


{-| Manipulate the coordinates in your SVG. This can be useful for scaling the svg.

    -- make the image twice as big in the x direction
    [ subpath (moveTo (10,0)) [ lineTo [ (42, 42) ] ] ]
        |> mapCoordinate (\(x,y) -> (2 * x, y))
             --> [ subpath (moveTo (20,0)) [ lineTo [ (84, 42) ] ] ]
-}
mapCoordinate : (Vec2 Float -> Vec2 Float) -> Path -> Path
mapCoordinate f path =
    List.map (SubPath.mapCoordinate f) path


{-| Map over the `DrawTo`s in a path with access to their starting point.

Many mathematical operations (length, derivative, curvature) are only possible when a segment is fully specified. A `DrawTo` on its
own misses its starting point - the current cursor position. This function makes the cursor position and the start of the current subpath available
when mapping.
-}
mapWithCursorState : (CursorState -> DrawTo -> b) -> Path -> List b
mapWithCursorState f =
    List.concatMap (SubPath.mapWithCursorState f)
