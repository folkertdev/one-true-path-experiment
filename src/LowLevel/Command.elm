module LowLevel.Command
    exposing
        ( MoveTo(..)
        , DrawTo(..)
        , Direction
        , ArcFlag
        , EllipticalArcArgument
        , CursorState
        , updateCursorState
        , clockwise
        , counterClockwise
        , largestArc
        , smallestArc
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
        )

{-| Low-level access to absolute svg drawing commands.

As the name implies, this is a low-level module that you probably shouldn't deal with.

## Threading State
@docs CursorState, updateCursorState

## Moving the cursor

@docs MoveTo, moveTo

## Drawing on the canvas

## Type

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
@docs arcTo, EllipticalArcArgument, clockwise, counterClockwise, largestArc, smallestArc
@docs ArcFlag, Direction

-}

import Vector2 as Vec2 exposing (Vec2)
import Geometry.Ellipse as Ellipse exposing (ArcFlag(..), Direction(..))
import List.Extra as List


{-| Constructors for MoveTo instructions
-}
type MoveTo
    = MoveTo (Vec2 Float)


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
    Ellipse.Direction


{-| Determine which arc to draw
-}
type alias ArcFlag =
    Ellipse.ArcFlag


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


{-| Move to a position on the canvas without drawing.
-}
moveTo : Vec2 Float -> MoveTo
moveTo =
    MoveTo


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


{-| Contains the start of the current subpath and the current cursor position.
-}
type alias CursorState =
    { start : Vec2 Float, cursor : Vec2 Float }


{-| Simulate the effect of a drawto command on the cursor position
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
