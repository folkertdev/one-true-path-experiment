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
        , mapCoordinateDrawTo
        , merge
        )

{-| Low-level access to drawing instructions.

This is a low-level module that you probably shouldn't deal with.
These instructions are meant to build up primitives (like in the `Curve` module); building of
curves should happen at the `SubPath` level.


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

@docs merge
@docs fromLowLevelMoveTo, fromLowLevelDrawTos, fromLowLevelDrawTo
@docs toLowLevelDrawTo, toLowLevelMoveTo
@docs mapCoordinateDrawTo

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

You may miss some constructs in comparison to SVG. Only absolute coordinates are
supported, and the smooth curve variants are removed. These choices were
made to keep the number of constructors small.

Relative coordinates can always
be transformed to abslute ones, and smooth (also known as short-hand) curve extensions
can be achieved with `Curve.smoothQuadraticBezier` and `Curve.smoothCubicBezier`.
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

    argument : EllipticalArcArgument
    argument =
        { start = ( 0, 42 )
        , end = ( 42, 0 )
        , radii = ( 1, 1 )
        , xAxisRotate = 90
        , arcFlag = largestArc
        , direction = clockwise
        }

The xAxisRotate parameter is in degrees (note that in the `Segment` module, it is in radians).
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


{-| Corresponds to a sweep flag of 0
-}
clockwise : Direction
clockwise =
    Clockwise


{-| Corresponds to a sweep flag of 1
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


{-|
* `start` start of the subpath (most recent `MoveTo`)
* `cursor` the current cursor position
* `previousControlPoint` if the previous drawto instruction was a curveTo (cubic or quadratic), then
    this value stores Just its last control point position, else Nothing
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
            let
                updateState ( finalState, finalPoints ) =
                    ( CurveTo finalPoints
                    , finalState
                    )
            in
                coordinates
                    |> coordinatesToAbsolute mode (Vec2.map (Vec2.add cursor))
                    |> Maybe.map (updateState << makeControlPointExplicitVec2 state << Tuple.second)

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
            let
                updateState ( finalState, finalPoints ) =
                    ( QuadraticBezierCurveTo finalPoints
                    , finalState
                    )
            in
                coordinates
                    |> coordinatesToAbsolute mode (Vec2.add cursor)
                    |> Maybe.map (updateState << makeControlPointExplicitVec1 state << Tuple.second)

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


    state : CursorState
    state =
        { start = (0,0)
        , cursor = (10, 10)
        , previousControlPoint = Nothing
        }

    updateCursorState (lineTo [(20, 10)]) state
        --> { start = (0,0), cursor = (20,10), previousControlPoint = Nothing }

    updateCursorState (quadraticCurveTo [(( 15, 20), (20, 10))]) state
        --> { start = (0,0), cursor = (20,10), previousControlPoint = Just (15, 20) }

-}
updateCursorState : DrawTo -> CursorState -> CursorState
updateCursorState drawto state =
    let
        ( cursorX, cursorY ) =
            state.cursor

        maybeUpdateCursor coordinate =
            { state | cursor = Maybe.withDefault state.cursor coordinate }

        noControlPoint state =
            { state | previousControlPoint = Nothing }
    in
        case drawto of
            LineTo coordinates ->
                maybeUpdateCursor (List.last coordinates)
                    |> noControlPoint

            Horizontal coordinates ->
                List.last coordinates
                    |> Maybe.map (\x -> ( x, cursorY ))
                    |> maybeUpdateCursor
                    |> noControlPoint

            Vertical coordinates ->
                List.last coordinates
                    |> Maybe.map (\y -> ( cursorX, y ))
                    |> maybeUpdateCursor
                    |> noControlPoint

            CurveTo coordinates ->
                case List.last coordinates of
                    Nothing ->
                        state

                    Just ( control1, control2, target ) ->
                        { state | cursor = target, previousControlPoint = Just control2 }

            QuadraticBezierCurveTo coordinates ->
                case List.last coordinates of
                    Nothing ->
                        state

                    Just ( control, target ) ->
                        { state | cursor = target, previousControlPoint = Just control }

            EllipticalArc arguments ->
                List.last arguments
                    |> Maybe.map .target
                    |> maybeUpdateCursor
                    |> noControlPoint

            ClosePath ->
                state
                    |> noControlPoint


{-| Transform the coordinates in a drawto
-}
mapCoordinateDrawTo : (Vec2 Float -> Vec2 Float) -> DrawTo -> DrawTo
mapCoordinateDrawTo f drawto =
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

        QuadraticBezierCurveTo coordinates ->
            QuadraticBezierCurveTo (List.map (Vec2.map f) coordinates)

        EllipticalArc arguments ->
            EllipticalArc (List.map (\argument -> { argument | target = f argument.target }) arguments)

        ClosePath ->
            ClosePath


{-| Merge adjacent commands if possible

    merge (lineTo [ (0,0) ]) (lineTo [ (10, 10) ]) --> Ok (lineTo [ (0,0) , (10, 10) ])

    merge (lineTo [ (0,0) ]) closePath --> Err (lineTo [ (0,0) ], closePath)
-}
merge : DrawTo -> DrawTo -> Result ( DrawTo, DrawTo ) DrawTo
merge instruction1 instruction2 =
    case ( instruction1, instruction2 ) of
        ( LineTo p1, LineTo p2 ) ->
            Ok <| LineTo (p1 ++ p2)

        ( Horizontal p1, Horizontal p2 ) ->
            Ok <| Horizontal (p1 ++ p2)

        ( Vertical p1, Vertical p2 ) ->
            Ok <| Vertical (p1 ++ p2)

        ( CurveTo p1, CurveTo p2 ) ->
            Ok <| CurveTo (p1 ++ p2)

        ( QuadraticBezierCurveTo p1, QuadraticBezierCurveTo p2 ) ->
            Ok <| QuadraticBezierCurveTo (p1 ++ p2)

        ( EllipticalArc p1, EllipticalArc p2 ) ->
            Ok <| EllipticalArc (p1 ++ p2)

        ( ClosePath, ClosePath ) ->
            Ok <| ClosePath

        _ ->
            Err ( instruction1, instruction2 )
