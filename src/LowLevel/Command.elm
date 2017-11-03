module LowLevel.Command
    exposing
        ( ArcFlag
        , CursorState
        , Direction
        , DrawTo(..)
        , EllipticalArcArgument
        , MoveTo(..)
        , arcTo
        , clockwise
        , closePath
        , counterClockwise
        , cubicCurveTo
        , fromLowLevelDrawTo
        , fromLowLevelDrawTos
        , fromLowLevelMoveTo
        , horizontalTo
        , largestArc
        , lineTo
        , moveTo
        , quadraticCurveTo
        , smallestArc
          --
        , toLowLevelDrawTo
        , toLowLevelMoveTo
        , updateCursorState
        , verticalTo
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


## Beziers

@docs quadraticCurveTo, cubicCurveTo


## Arcs

@docs arcTo, EllipticalArcArgument, clockwise, counterClockwise, largestArc, smallestArc
@docs ArcFlag, Direction


## Conversion

@docs fromLowLevelMoveTo, fromLowLevelDrawTos, fromLowLevelDrawTo
@docs toLowLevelDrawTo, toLowLevelMoveTo

-}

import List.Extra as List
import Path.LowLevel as LowLevel exposing (ArcFlag(..), Direction(..), Mode(..))
import Vector2 as Vec2 exposing (Vec2)
import Vector3 as Vec3 exposing (Vec3)


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
    | QuadraticBezierCurveTo (List ( Vec2 Float, Vec2 Float ))
    | EllipticalArc (List EllipticalArcArgument)
    | ClosePath


{-| The arguments for an Arc
-}
type alias EllipticalArcArgument =
    { radii : ( Float, Float )
    , xAxisRotate : Float
    , arcFlag : LowLevel.ArcFlag
    , direction : LowLevel.Direction
    , target : Vec2 Float
    }


{-| Determine which arc to draw
-}
type alias Direction =
    LowLevel.Direction


{-| Determine which arc to draw
-}
type alias ArcFlag =
    LowLevel.ArcFlag


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
thDefault
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


{-| A cubic bezier. The `C` instruction.
-}
cubicCurveTo : List ( Vec2 Float, Vec2 Float, Vec2 Float ) -> DrawTo
cubicCurveTo =
    CurveTo


{-| An elliptical arc. The `A` instruction.
-}
arcTo : List EllipticalArcArgument -> DrawTo
arcTo =
    EllipticalArc


{-| Contains the start of the current subpath and the current cursor position.
-}
type alias CursorState =
    { start : Vec2 Float, cursor : Vec2 Float, previousControlPoint : Maybe ( Float, Float ) }


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


{-| -}
fromLowLevelMoveTo : LowLevel.MoveTo -> CursorState -> ( CursorState, MoveTo )
fromLowLevelMoveTo (LowLevel.MoveTo mode target) ({ cursor } as state) =
    case mode of
        Absolute ->
            ( { state | start = target, cursor = target, previousControlPoint = Nothing }
            , MoveTo target
            )

        Relative ->
            let
                absoluteTarget =
                    Vec2.add target cursor
            in
                ( { state | start = absoluteTarget, cursor = absoluteTarget, previousControlPoint = Nothing }
                , MoveTo absoluteTarget
                )


{-| Convert a one-true-path moveto to a svg-path-lowlevel moveto. Used in conversion to string
-}
toLowLevelMoveTo : MoveTo -> LowLevel.MoveTo
toLowLevelMoveTo (MoveTo target) =
    LowLevel.MoveTo Absolute target


{-| -}
fromLowLevelDrawTos : List LowLevel.DrawTo -> CursorState -> ( CursorState, List DrawTo )
fromLowLevelDrawTos drawtos state =
    let
        folder element ( state, elements ) =
            case fromLowLevelDrawTo element state of
                Nothing ->
                    ( state, elements )

                Just ( newDrawTo, newState ) ->
                    ( newState, newDrawTo :: elements )
    in
        drawtos
            |> List.foldl folder ( state, [] )
            |> Tuple.mapSecond List.reverse


{-| -}
fromLowLevelDrawTo : LowLevel.DrawTo -> CursorState -> Maybe ( DrawTo, CursorState )
fromLowLevelDrawTo drawto ({ start, cursor } as state) =
    case drawto of
        LowLevel.LineTo mode oldPoints ->
            let
                updateState ( final, points ) =
                    ( LineTo points
                    , { state | cursor = final, previousControlPoint = Nothing }
                    )
            in
                coordinatesToAbsolute mode (Vec2.add cursor) oldPoints
                    |> Maybe.map updateState

        LowLevel.Horizontal mode xs ->
            let
                updateState ( final, points ) =
                    ( LineTo points
                    , { state | cursor = ( Tuple.first final, Tuple.second cursor ), previousControlPoint = Nothing }
                    )
            in
                xs
                    |> List.map (\x -> ( x, 0 ))
                    |> coordinatesToAbsolute mode (Vec2.add cursor)
                    |> Maybe.map updateState

        LowLevel.Vertical mode ys ->
            let
                updateState ( final, points ) =
                    ( LineTo points
                    , { state | cursor = ( Tuple.first cursor, Tuple.second final ), previousControlPoint = Nothing }
                    )
            in
                ys
                    |> List.map (\y -> ( 0, y ))
                    |> coordinatesToAbsolute mode (Vec2.add cursor)
                    |> Maybe.map updateState

        LowLevel.CurveTo mode coordinates ->
            let
                updateState ( ( _, c2, final ), points ) =
                    ( CurveTo points
                    , { state | cursor = final, previousControlPoint = Just c2 }
                    )
            in
                coordinates
                    |> coordinatesToAbsolute mode (Vec3.map (Vec2.add cursor))
                    |> Maybe.map updateState

        LowLevel.SmoothCurveTo mode coordinates ->
            -- (If there is no previous command or if the previous command was not an C, c, S or s,
            -- assume the first control point is coincident with the current point.)
            coordinates
                |> coordinatesToAbsolute mode (Vec2.map (Vec2.add cursor))
                |> Maybe.map (makeControlPointExplicitVec2 state << Tuple.second)
                |> Maybe.map
                    (\( finalState, finalPoints ) ->
                        ( CurveTo finalPoints
                        , finalState
                        )
                    )

        LowLevel.QuadraticBezierCurveTo mode coordinates ->
            let
                updateState ( ( c1, final ), points ) =
                    ( QuadraticBezierCurveTo points
                    , { state | cursor = final, previousControlPoint = Just c1 }
                    )
            in
                coordinates
                    |> coordinatesToAbsolute mode (Vec2.map (Vec2.add cursor))
                    |> Maybe.map updateState

        LowLevel.SmoothQuadraticBezierCurveTo mode coordinates ->
            coordinates
                |> coordinatesToAbsolute mode (Vec2.add cursor)
                |> Maybe.map (makeControlPointExplicitVec1 state << Tuple.second)
                |> Maybe.map
                    (\( finalState, finalPoints ) ->
                        ( QuadraticBezierCurveTo finalPoints
                        , finalState
                        )
                    )

        LowLevel.EllipticalArc mode arguments ->
            let
                argumentToAbsolute argument =
                    { argument | target = Vec2.add cursor argument.target }

                updateState ( { target }, points ) =
                    ( EllipticalArc points
                    , { state | cursor = target, previousControlPoint = Nothing }
                    )
            in
                arguments
                    |> coordinatesToAbsolute mode argumentToAbsolute
                    |> Maybe.map updateState

        LowLevel.ClosePath ->
            Just ( ClosePath, { state | cursor = start } )


{-| Convert a one-true-path drawto to a svg-path-lowlevel drawto. Used in conversion to string
-}
toLowLevelDrawTo : DrawTo -> LowLevel.DrawTo
toLowLevelDrawTo drawto =
    case drawto of
        LineTo coordinates ->
            LowLevel.LineTo Absolute coordinates

        Horizontal coordinates ->
            LowLevel.Horizontal Absolute coordinates

        Vertical coordinates ->
            LowLevel.Vertical Absolute coordinates

        CurveTo coordinates ->
            LowLevel.CurveTo Absolute coordinates

        QuadraticBezierCurveTo coordinates ->
            LowLevel.QuadraticBezierCurveTo Absolute coordinates

        EllipticalArc arguments ->
            LowLevel.EllipticalArc Absolute arguments

        ClosePath ->
            LowLevel.ClosePath


makeControlPointExplicitVec1 : CursorState -> List ( Float, Float ) -> ( CursorState, List (Vec2 ( Float, Float )) )
makeControlPointExplicitVec1 initial withoutContolPoint =
    let
        folder target ( state, accum ) =
            let
                previousControlPoint =
                    Maybe.withDefault state.cursor state.previousControlPoint

                newControlPoint =
                    Vec2.sub previousControlPoint state.cursor
                        |> Vec2.negate
                        |> Vec2.add state.cursor
            in
                ( { state | cursor = target, previousControlPoint = Just newControlPoint }
                , ( newControlPoint, target ) :: accum
                )
    in
        List.foldl folder ( initial, [] ) withoutContolPoint
            |> Tuple.mapSecond List.reverse


makeControlPointExplicitVec2 : CursorState -> List (Vec2 ( Float, Float )) -> ( CursorState, List (Vec3 ( Float, Float )) )
makeControlPointExplicitVec2 initial withoutContolPoint =
    let
        folder ( c2, target ) ( state, accum ) =
            let
                previousControlPoint =
                    Maybe.withDefault state.cursor state.previousControlPoint

                newControlPoint =
                    Vec2.sub previousControlPoint state.cursor
                        |> Vec2.negate
                        |> Vec2.add state.cursor
            in
                ( { state | cursor = target, previousControlPoint = Just c2 }
                , ( newControlPoint, c2, target ) :: accum
                )
    in
        List.foldl folder ( initial, [] ) withoutContolPoint
            |> Tuple.mapSecond List.reverse


coordinatesToAbsolute : Mode -> (coords -> coords) -> List coords -> Maybe ( coords, List coords )
coordinatesToAbsolute mode toAbsolute coordinates =
    case mode of
        Absolute ->
            coordinates
                |> last
                |> Maybe.map (\final -> ( final, coordinates ))

        Relative ->
            let
                folder f element ( elements, final ) =
                    case final of
                        Nothing ->
                            ( f element :: elements, Just element )

                        Just _ ->
                            ( f element :: elements, final )
            in
                case List.foldr (folder toAbsolute) ( [], Nothing ) coordinates of
                    ( _, Nothing ) ->
                        Nothing

                    ( newCoordinates, Just final ) ->
                        Just ( toAbsolute final, newCoordinates )


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
                    |> Maybe.map (\( _, _, c ) -> c)
                    |> maybeUpdateCursor

            QuadraticBezierCurveTo coordinates ->
                List.last coordinates
                    |> Maybe.map Tuple.second
                    |> maybeUpdateCursor

            EllipticalArc arguments ->
                List.last arguments
                    |> Maybe.map .target
                    |> maybeUpdateCursor

            ClosePath ->
                state
