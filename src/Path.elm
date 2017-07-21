module Path
    exposing
        ( Path
        , SubPath
        , MoveTo(..)
        , DrawTo(..)
        , Direction
        , ArcFlag
        , EllipticalArcArgument
        , CursorState
        , element
        , toString
        , parse
        , subpath
        , mapCoordinate
        , mapWithCursorState
        , updateCursorState
          --
        , moveTo
        , lineTo
        , horizontalTo
        , verticalTo
        , quadraticCurveTo
        , quadraticCurveExtendTo
        , cubicCurveTo
        , cubicCurveExtendTo
        , arcTo
        , closePath
        , clockwise
        , counterClockwise
        , largestArc
        , smallestArc
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
@docs Path, SubPath

## Constructing Paths
@docs subpath, parse

## Creating SVG
@docs element, toString

## Modifying Paths
@docs mapCoordinate, CursorState, mapWithCursorState, updateCursorState

## Moving the cursor

@docs MoveTo, moveTo

## Drawing on the canvas

@docs DrawTo

## Straight lines
@docs lineTo, horizontalTo, verticalTo

## Close Path
@docs closePath

## Quadratic Beziers
@docs quadraticCurveTo, quadraticCurveExtendTo

## Cubic Beziers
@docs cubicCurveTo, cubicCurveExtendTo

## Arcs
@docs arcTo, EllipticalArcArgument, Direction, clockwise, counterClockwise, ArcFlag, largestArc, smallestArc



-}

import Svg
import Svg.Attributes
import Parser
import MixedPath exposing (..)
import Vector2 as Vec2 exposing (Vec2)
import Vector3 as Vec3 exposing (Vec3)
import List.Extra as List


{-| Construct an svg path element from a `Path` with the given attributes
-}
element : Path -> List (Svg.Attribute msg) -> Svg.Svg msg
element path attributes =
    Svg.path (Svg.Attributes.d (toString path) :: attributes) []


{-| A path is a list of [`SubPath`](#SubPath)s.
-}
type alias Path =
    List SubPath


{-| A subpath consists of a [`MoveTo`](#MoveTo) instruction followed by a list of [`DrawTo`](#DrawTo) instructions


-}
type alias SubPath =
    { moveto : MoveTo, drawtos : List DrawTo }


{-| Construct a subpath

    subpath (moveTo (10,0)) [ lineTo [ (42, 73) ] ]
-}
subpath : MoveTo -> List DrawTo -> SubPath
subpath =
    SubPath


{-| Constructors for MoveTo instructions
-}
type MoveTo
    = MoveTo (Vec2 Float)


{-| Move to a position on the canvas without drawing.
-}
moveTo : Vec2 Float -> MoveTo
moveTo =
    MoveTo


{-| Constructors for DrawTo instructions
-}
type DrawTo
    = LineTo (List (Vec2 Float))
    | Horizontal (List Float)
    | Vertical (List Float)
    | CurveTo (List ( Vec2 Float, Vec2 Float, Vec2 Float ))
    | SmoothCurveTo (List ( Vec2 Float, Vec2 Float ))
    | QuadraticBezierCurveTo (List ( Vec2 Float, Vec2 Float ))
    | SmoothQuadraticBezierCurveTo (List (Vec2 Float))
    | EllipticalArc (List EllipticalArcArgument)
    | ClosePath


{-| The arguments for an Arc
-}
type alias EllipticalArcArgument =
    { radii : ( Float, Float )
    , xAxisRotate : Float
    , arcFlag : ArcFlag
    , direction : Direction
    , target : Vec2 Float
    }


{-| Determine which arc to draw
-}
type alias Direction =
    MixedPath.Direction


{-| Determine which arc to draw
-}
type alias ArcFlag =
    MixedPath.ArcFlag


{-| Draw a series of line segments to absolute positions. The `L` instruction.
-}
lineTo : List (Vec2 Float) -> DrawTo
lineTo =
    LineTo


{-| Specific version of `lineTo` that only moves horizontally. The `H` instruction.
-}
horizontalTo : List Float -> DrawTo
horizontalTo =
    Horizontal


{-| Specific version of `lineTo` that only moves vertically. The `V` instruction
-}
verticalTo : List Float -> DrawTo
verticalTo =
    Vertical


{-| Draw a straight line from the cursor position to the starting position of the path. The `Z` instruction.
-}
closePath : DrawTo
closePath =
    ClosePath


{-| A quadratic bezier. The `Q` instruction.
-}
quadraticCurveTo : List ( Vec2 Float, Vec2 Float ) -> DrawTo
quadraticCurveTo =
    QuadraticBezierCurveTo


{-| A smooth extension to a quadratic bezier segment. The `T` instruction.
-}
quadraticCurveExtendTo : List (Vec2 Float) -> DrawTo
quadraticCurveExtendTo =
    SmoothQuadraticBezierCurveTo


{-| A cubic bezier. The `C` instruction.
-}
cubicCurveTo : List ( Vec2 Float, Vec2 Float, Vec2 Float ) -> DrawTo
cubicCurveTo =
    CurveTo


{-| A smooth extension to a cubic bezier segment. The `S` instruction.
-}
cubicCurveExtendTo : List ( Vec2 Float, Vec2 Float ) -> DrawTo
cubicCurveExtendTo =
    SmoothCurveTo


{-| An elliptical arc. The `A` instruction.
-}
arcTo : List EllipticalArcArgument -> DrawTo
arcTo =
    EllipticalArc


{-| Corresponds to a sweep flag of 1
-}
clockwise : Direction
clockwise =
    Clockwise


{-| Corresponds to a sweep flag of 0
-}
counterClockwise : Direction
counterClockwise =
    CounterClockwise


{-| Corresponds to an arc flag of 1
-}
largestArc : ArcFlag
largestArc =
    LargestArc


{-| Corresponds to an arc flag of 0
-}
smallestArc : ArcFlag
smallestArc =
    SmallestArc


{-| Contains the start of the current subpath and the current cursor position.
-}
type alias CursorState =
    { start : Vec2 Float, cursor : Vec2 Float }


{-| Turn a `MixedPath` into a `String`. The result is ready to be used with the `d` attribute.

    Path.toString [ subpath (moveTo (0,0)) [ lineBy ( 42, 73 ) ] ]
        --> "M0,0 l42,73"
-}
toString : Path -> String
toString =
    toMixedPath >> MixedPath.toString


{-| Parse a path string into a `Path`


    parse "M0,0 l42,73"
        --> Ok [{ moveto = MoveTo Absolute (0,0), drawtos = [ LineTo Relative  [(42, 73)]]}]

Only accepts valid complete subpaths (a sequences of a move followed by zero or more draws). Relative instructions are converted to absolute ones.
The types and constructors in the output are described [here](#internal-data-used-by-the-parser-).

The parser uses [`elm-tools/parser`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/).
The error type is [`Parser.Error`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/Parser#Error).
-}
parse : String -> Result Parser.Error Path
parse =
    Result.map absolutePathToPath << MixedPath.parse


absolutePathToPath : MixedPath.AbsolutePath -> Path
absolutePathToPath path =
    let
        helperMoveTo : AbstractMoveTo () -> MoveTo
        helperMoveTo (MixedPath.MoveTo _ coordinate) =
            MoveTo coordinate

        helperDrawTo : MixedPath.AbstractDrawTo () -> DrawTo
        helperDrawTo drawto =
            case drawto of
                MixedPath.LineTo _ arg ->
                    LineTo arg

                MixedPath.Horizontal _ arg ->
                    Horizontal arg

                MixedPath.Vertical _ arg ->
                    Vertical arg

                MixedPath.CurveTo _ arg ->
                    CurveTo arg

                MixedPath.SmoothCurveTo _ arg ->
                    SmoothCurveTo arg

                MixedPath.QuadraticBezierCurveTo _ arg ->
                    QuadraticBezierCurveTo arg

                MixedPath.SmoothQuadraticBezierCurveTo _ arg ->
                    SmoothQuadraticBezierCurveTo arg

                MixedPath.EllipticalArc _ arg ->
                    EllipticalArc arg

                MixedPath.ClosePath ->
                    ClosePath

        helperSubPath : MixedPath.AbsoluteSubPath -> SubPath
        helperSubPath { moveto, drawtos } =
            { moveto = helperMoveTo moveto, drawtos = List.map helperDrawTo drawtos }
    in
        List.map helperSubPath path


{-| Manipulate the coordinates in your SVG. This can be useful for scaling the svg.

    -- make the image twice as big in the x direction
    [ subpath (moveTo (10,0)) [ lineTo [ (42, 42) ] ] ]
        |> mapCoordinate (\(x,y) -> (2 * x, y))
             --> [ subpath (moveTo (20,0)) [ lineTo [ (84, 42) ] ] ]
-}
mapCoordinate : (Vec2 Float -> Vec2 Float) -> Path -> Path
mapCoordinate f path =
    let
        helper : SubPath -> SubPath
        helper { moveto, drawtos } =
            case moveto of
                MoveTo coordinate ->
                    { moveto = MoveTo (f coordinate)
                    , drawtos = List.map helperDrawTo drawtos
                    }

        helperDrawTo drawto =
            case drawto of
                LineTo coordinates ->
                    LineTo (List.map f coordinates)

                Horizontal coordinates ->
                    coordinates
                        |> List.map ((\x -> ( x, 0 )) >> f >> Tuple.first)
                        |> Horizontal

                Vertical coordinates ->
                    coordinates
                        |> List.map ((\y -> ( 0, y )) >> f >> Tuple.second)
                        |> Vertical

                CurveTo coordinates ->
                    CurveTo (List.map (Vec3.map f) coordinates)

                SmoothCurveTo coordinates ->
                    SmoothCurveTo (List.map (Vec2.map f) coordinates)

                QuadraticBezierCurveTo coordinates ->
                    QuadraticBezierCurveTo (List.map (Vec2.map f) coordinates)

                SmoothQuadraticBezierCurveTo coordinates ->
                    SmoothQuadraticBezierCurveTo (List.map f coordinates)

                EllipticalArc arguments ->
                    EllipticalArc (List.map (\argument -> { argument | target = f argument.target }) arguments)

                ClosePath ->
                    ClosePath
    in
        List.map helper path


{-| Given a cursor state, simulate the effect that a `DrawTo` has on that cursor state
-}
updateCursorState : DrawTo -> CursorState -> CursorState
updateCursorState drawto state =
    let
        ( cursorX, cursorY ) =
            state.cursor

        maybeUpdateCursor coordinate =
            { state | cursor = Maybe.withDefault state.cursor coordinate }
    in
        case drawto of
            LineTo coordinates ->
                maybeUpdateCursor (List.last coordinates)

            Horizontal coordinates ->
                List.last coordinates
                    |> Maybe.map (\x -> ( x, cursorY ))
                    |> maybeUpdateCursor

            Vertical coordinates ->
                List.last coordinates
                    |> Maybe.map (\y -> ( cursorX, y ))
                    |> maybeUpdateCursor

            CurveTo coordinates ->
                List.last coordinates
                    |> Maybe.map ((\( _, _, c ) -> c))
                    |> maybeUpdateCursor

            SmoothCurveTo coordinates ->
                List.last coordinates
                    |> Maybe.map Tuple.second
                    |> maybeUpdateCursor

            QuadraticBezierCurveTo coordinates ->
                List.last coordinates
                    |> Maybe.map Tuple.second
                    |> maybeUpdateCursor

            SmoothQuadraticBezierCurveTo coordinates ->
                List.last coordinates
                    |> maybeUpdateCursor

            EllipticalArc arguments ->
                List.last arguments
                    |> Maybe.map .target
                    |> maybeUpdateCursor

            ClosePath ->
                state


{-| Map over the `DrawTo`s in a path with access to their starting point.

Many mathematical operations (length, derivative, curvature) are only possible when a segment is fully specified. A `DrawTo` on its
own misses its starting point - the current cursor position. This function makes the cursor position and the start of the current subpath available
when mapping.
-}
mapWithCursorState : (CursorState -> DrawTo -> b) -> Path -> List b
mapWithCursorState f =
    List.concatMap (mapWithCursorStateSubPath f)


mapWithCursorStateSubPath : (CursorState -> DrawTo -> b) -> SubPath -> List b
mapWithCursorStateSubPath mapDrawTo { moveto, drawtos } =
    let
        start =
            case moveto of
                MoveTo coordinate ->
                    coordinate

        folder : DrawTo -> ( CursorState, List b ) -> ( CursorState, List b )
        folder drawto ( cursorState, accum ) =
            ( updateCursorState drawto cursorState
            , mapDrawTo cursorState drawto :: accum
            )
    in
        List.foldl folder ( { start = start, cursor = start }, [] ) drawtos
            |> Tuple.second
            |> List.reverse


{-| Exposed for testing purposes
-}
toMixedDrawTo : DrawTo -> MixedPath.DrawTo
toMixedDrawTo drawto =
    case drawto of
        LineTo coordinates ->
            MixedPath.LineTo MixedPath.Absolute coordinates

        Horizontal coordinates ->
            MixedPath.Horizontal MixedPath.Absolute coordinates

        Vertical coordinates ->
            MixedPath.Vertical MixedPath.Absolute coordinates

        CurveTo coordinates ->
            MixedPath.CurveTo MixedPath.Absolute coordinates

        SmoothCurveTo coordinates ->
            MixedPath.SmoothCurveTo MixedPath.Absolute coordinates

        QuadraticBezierCurveTo coordinates ->
            MixedPath.QuadraticBezierCurveTo MixedPath.Absolute coordinates

        SmoothQuadraticBezierCurveTo coordinates ->
            MixedPath.SmoothQuadraticBezierCurveTo MixedPath.Absolute coordinates

        EllipticalArc arguments ->
            MixedPath.EllipticalArc MixedPath.Absolute arguments

        ClosePath ->
            MixedPath.ClosePath


{-| Exposed for testing purposes
-}
toMixedMoveTo : MoveTo -> MixedPath.MoveTo
toMixedMoveTo (MoveTo coordinates) =
    MixedPath.MoveTo Absolute coordinates


toMixedPath : Path -> MixedPath
toMixedPath path =
    List.map (\{ moveto, drawtos } -> { moveto = toMixedMoveTo moveto, drawtos = List.map toMixedDrawTo drawtos }) path
