module Path
    exposing
        ( Path
        , SubPath
        , Coordinate
        , MoveTo(..)
        , DrawTo(..)
        , Direction
        , ArcFlag
        , EllipticalArcArgument
        , toString
        , parse
        , subpath
        , mapCoordinate
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
        , toAbsoluteMoveTo
        , toAbsoluteDrawTo
        , toMixedMoveTo
        , toMixedDrawTo
        , addCoordinates
        )

{-| Low-level module for working with constructing svg paths

This module provides a wrapper around the svg path interface. A path can be parsed from a string or build up using the
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

The constructors are exposed, so if you need an escape hatch it is available. As always though, never reach for it when there are other options available.

## Data Structures
@docs Coordinate, Path, SubPath, subpath, toString, parse, mapCoordinate

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

## Internal
@docs toAbsoluteMoveTo, toAbsoluteDrawTo, toMixedDrawTo, toMixedMoveTo, addCoordinates

-}

import State exposing (State)
import Parser
import MixedPath exposing (..)


{-| A path is a list of [`SubPath`](#SubPath)s.
-}
type alias Path =
    List SubPath


{-| A subpath consists of a [`MoveTo`](#MoveTo) instruction followed by a list of [`DrawTo`](#DrawTo) instructions


-}
type alias SubPath =
    { moveto : MoveTo, drawtos : List DrawTo }


{-| A 2-tuple of floats representing a position in space
-}
type alias Coordinate =
    ( Float, Float )


{-| Construct a subpath

    subpath (moveTo (10,0)) [ lineTo [ (42, 73) ] ]
-}
subpath : MoveTo -> List DrawTo -> SubPath
subpath =
    SubPath


{-| Constructors for MoveTo instructions
-}
type MoveTo
    = MoveTo Coordinate


{-| Move to a position on the canvas without drawing.
-}
moveTo : Coordinate -> MoveTo
moveTo =
    MoveTo


{-| Constructors for DrawTo instructions
-}
type DrawTo
    = LineTo (List Coordinate)
    | Horizontal (List Float)
    | Vertical (List Float)
    | CurveTo (List ( Coordinate, Coordinate, Coordinate ))
    | SmoothCurveTo (List ( Coordinate, Coordinate ))
    | QuadraticBezierCurveTo (List ( Coordinate, Coordinate ))
    | SmoothQuadraticBezierCurveTo (List Coordinate)
    | EllipticalArc (List EllipticalArcArgument)
    | ClosePath


{-| The arguments for an Arc
-}
type alias EllipticalArcArgument =
    { radii : ( Float, Float )
    , xAxisRotate : Float
    , arcFlag : ArcFlag
    , direction : Direction
    , target : Coordinate
    }


{-| Determine which arc to draw
-}
type alias Direction =
    MixedPath.Direction


{-| Determine which arc to draw
-}
type alias ArcFlag =
    MixedPath.ArcFlag


{-| Draw a series of line segments to absolute positions
-}
lineTo : List Coordinate -> DrawTo
lineTo =
    LineTo


{-| Specific version of `lineTo` that only moves horizontally.
-}
horizontalTo : List Float -> DrawTo
horizontalTo =
    Horizontal


{-| Specific version of `lineTo` that only moves vertically
-}
verticalTo : List Float -> DrawTo
verticalTo =
    Vertical


{-| Draw a straight line from the cursor position to the starting position of the path .
-}
closePath : DrawTo
closePath =
    ClosePath


{-| -}
quadraticCurveTo : List ( Coordinate, Coordinate ) -> DrawTo
quadraticCurveTo =
    QuadraticBezierCurveTo


{-| -}
quadraticCurveExtendTo : List Coordinate -> DrawTo
quadraticCurveExtendTo =
    SmoothQuadraticBezierCurveTo


{-| -}
cubicCurveTo : List ( Coordinate, Coordinate, Coordinate ) -> DrawTo
cubicCurveTo =
    CurveTo


{-| -}
cubicCurveExtendTo : List ( Coordinate, Coordinate ) -> DrawTo
cubicCurveExtendTo =
    SmoothCurveTo


{-| -}
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


{-| Store the start of the current subpath and the current cursor position
-}
type alias CursorState =
    { start : Coordinate, cursor : Coordinate }


{-| Exposed for testing
-}
addCoordinates : Coordinate -> Coordinate -> Coordinate
addCoordinates ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


{-| Turn a `MixedPath` into a `String`. The result is ready to be used with the `d` attribute.

    Path.toString [ subpath (moveTo (0,0)) [ lineBy ( 42, 73 ) ] ]
        --> "M0,0 l42,73"
-}
toString : Path -> String
toString =
    toMixedPath >> MixedPath.stringify


{-| Parse a path string into a `MixedPath`


    parse "M0,0 l42,73"
        --> Ok [{ moveto = MoveTo Absolute (0,0), drawtos = [ LineTo Relative  [(42, 73)]]}]

Only accepts valid complete subpaths (a sequences of a move followed by zero or more draws). The types and constructors in the output are
detailed [here](#internal-data-used-by-the-parser-).

The parser uses [`elm-tools/parser`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/).
The error type is [`Parser.Error`](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/Parser#Error).
-}
parse : String -> Result Parser.Error Path
parse =
    Result.map fromMixedPath << MixedPath.parse


{-| Manipulate the coordinates in your SVG. This can be useful for scaling the svg.

    -- make the image twice as big in the x direction
    [ subpath (moveTo (10,0)) [ lineTo [ (42, 42) ] ] ]
        |> mapCoordinate (\(x,y) -> (2 * x, y))
             --> [ subpath (moveTo (20,0)) [ lineTo [ (84, 42) ] ] ]
-}
mapCoordinate : (Coordinate -> Coordinate) -> Path -> Path
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
                    CurveTo (List.map (mapTriplet f) coordinates)

                SmoothCurveTo coordinates ->
                    SmoothCurveTo (List.map (mapTuple f) coordinates)

                QuadraticBezierCurveTo coordinates ->
                    QuadraticBezierCurveTo (List.map (mapTuple f) coordinates)

                SmoothQuadraticBezierCurveTo coordinates ->
                    SmoothQuadraticBezierCurveTo (List.map f coordinates)

                EllipticalArc arguments ->
                    EllipticalArc (List.map (\argument -> { argument | target = f argument.target }) arguments)

                ClosePath ->
                    ClosePath
    in
        List.map helper path


{-| Helpers for converting relative instructions to absolute ones

This is possible on a path level, because the first move instruction will always be interpreted as absolute.
Therefore, there is an anchor for subsequent relative commands.

This module defines a CursorState

    type alias CursorState =
        { start : Coordinate, cursor : Coordinate }

And threads it through a path. There are functions that modify a relative moveto/drawto based on
a `CursorState`, and also returns an updated CursorState. Then we use [elm-state](https://github.com/folkertdev/elm-state) to
chain updating the `CursorState` first with the `MoveTo` and then with a list of `DrawTo`s.

Similarly, we can easily chain making a path - consisting of many `SubPath`s - absolute with elm-state.
-}
fromMixedPath : MixedPath -> Path
fromMixedPath subpaths =
    case subpaths of
        [] ->
            []

        ({ moveto } as sp) :: sps ->
            case moveto of
                MixedPath.MoveTo _ coordinate ->
                    let
                        initialState =
                            { start = coordinate, cursor = coordinate }
                    in
                        State.traverse toSubPath_ subpaths
                            |> State.finalValue initialState


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


toSubPath_ : MixedPath.MixedSubPath -> State CursorState SubPath
toSubPath_ subpath =
    State.advance (\state -> toSubPath state subpath)


toSubPath : CursorState -> MixedPath.MixedSubPath -> ( SubPath, CursorState )
toSubPath ({ start, cursor } as state) { moveto, drawtos } =
    drawtos
        |> List.reverse
        |> State.traverse toAbsoluteDrawTo_
        |> State.map List.reverse
        |> State.map2 SubPath (toAbsoluteMoveTo_ moveto)
        |> State.run state


toAbsoluteMoveTo_ : MixedPath.MoveTo -> State CursorState MoveTo
toAbsoluteMoveTo_ moveto =
    State.advance (\state -> toAbsoluteMoveTo state moveto)


toAbsoluteDrawTo_ : MixedPath.DrawTo -> State CursorState DrawTo
toAbsoluteDrawTo_ drawto =
    State.advance (\state -> toAbsoluteDrawTo state drawto)


{-| Exposed for testing
-}
toAbsoluteMoveTo : CursorState -> MixedPath.MoveTo -> ( MoveTo, CursorState )
toAbsoluteMoveTo { start, cursor } (MixedPath.MoveTo mode coordinate) =
    case mode of
        MixedPath.Absolute ->
            ( MoveTo coordinate, { start = coordinate, cursor = coordinate } )

        Relative ->
            let
                newCoordinate =
                    addCoordinates cursor coordinate
            in
                ( MoveTo newCoordinate, { start = newCoordinate, cursor = newCoordinate } )


{-| Exposed for testing
-}
toAbsoluteDrawTo : CursorState -> MixedPath.DrawTo -> ( DrawTo, CursorState )
toAbsoluteDrawTo ({ start, cursor } as state) drawto =
    case drawto of
        MixedPath.LineTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (coordinateToAbsolute cursor) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( LineTo [], state )

                    Just finalCoordinate ->
                        ( LineTo absoluteCoordinates
                        , { state | cursor = finalCoordinate }
                        )

        MixedPath.Horizontal mode xs ->
            let
                absoluteCoordinates =
                    List.map (\x -> ( x, 0 )) xs
                        |> coordinatesToAbsolute mode (coordinateToAbsolute cursor)
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( Horizontal [], state )

                    Just ( finalX, _ ) ->
                        ( Horizontal (List.map Tuple.first absoluteCoordinates)
                        , { state | cursor = ( finalX, Tuple.second cursor ) }
                        )

        MixedPath.Vertical mode ys ->
            let
                absoluteCoordinates =
                    List.map (\y -> ( 0, y )) ys
                        |> coordinatesToAbsolute mode (coordinateToAbsolute cursor)
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( Vertical [], state )

                    Just ( _, finalY ) ->
                        ( Vertical (List.map Tuple.second absoluteCoordinates)
                        , { state | cursor = ( Tuple.first cursor, finalY ) }
                        )

        MixedPath.CurveTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (coordinateToAbsolute3 cursor) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( CurveTo [], state )

                    Just ( _, _, target ) ->
                        ( CurveTo absoluteCoordinates, { state | cursor = target } )

        MixedPath.SmoothCurveTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (coordinateToAbsolute2 cursor) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( SmoothCurveTo [], state )

                    Just ( _, target ) ->
                        ( SmoothCurveTo absoluteCoordinates, { state | cursor = target } )

        MixedPath.QuadraticBezierCurveTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (coordinateToAbsolute2 cursor) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( QuadraticBezierCurveTo [], state )

                    Just ( _, target ) ->
                        ( QuadraticBezierCurveTo absoluteCoordinates, { state | cursor = target } )

        MixedPath.SmoothQuadraticBezierCurveTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (coordinateToAbsolute cursor) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( SmoothQuadraticBezierCurveTo [], state )

                    Just finalCoordinate ->
                        ( SmoothQuadraticBezierCurveTo absoluteCoordinates
                        , { state | cursor = finalCoordinate }
                        )

        MixedPath.EllipticalArc mode arguments ->
            let
                argumentToAbsolute cursor argument =
                    { argument | target = addCoordinates cursor argument.target }

                absoluteArguments =
                    coordinatesToAbsolute mode (argumentToAbsolute cursor) arguments
            in
                case last absoluteArguments of
                    Nothing ->
                        ( EllipticalArc [], state )

                    Just { target } ->
                        ( EllipticalArc absoluteArguments, { state | cursor = target } )

        MixedPath.ClosePath ->
            ( ClosePath, { state | cursor = start } )


coordinateToAbsolute =
    addCoordinates


coordinateToAbsolute2 cursor ( p1, p2 ) =
    ( addCoordinates cursor p1, addCoordinates cursor p2 )


coordinateToAbsolute3 cursor ( p1, p2, p3 ) =
    ( addCoordinates cursor p1, addCoordinates cursor p2, addCoordinates cursor p3 )


coordinatesToAbsolute : Mode -> (coords -> coords) -> List coords -> List coords
coordinatesToAbsolute mode toAbsolute coordinates =
    case mode of
        MixedPath.Absolute ->
            coordinates

        Relative ->
            List.map toAbsolute coordinates


last : List a -> Maybe a
last =
    List.foldr
        (\element accum ->
            if accum == Nothing then
                Just element
            else
                accum
        )
        Nothing
