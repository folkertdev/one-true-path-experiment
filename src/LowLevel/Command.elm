module LowLevel.Command exposing
    ( MoveTo(..), moveTo
    , DrawTo(..)
    , lineTo
    , closePath
    , quadraticCurveTo, cubicCurveTo
    , arcTo, EllipticalArcArgument, clockwise, counterClockwise, largestArc, smallestArc
    , ArcFlag, Direction
    , CursorState, updateCursorState
    , merge
    , fromLowLevelMoveTo, fromLowLevelDrawTos, fromLowLevelDrawTo
    , toLowLevelDrawTo, toLowLevelMoveTo
    , mapCoordinateDrawTo, scaleMoveTo, scaleDrawTo
    --
    )

{-| Low-level access to drawing instructions.

**This is a low-level module that you probably shouldn't deal with.** It is much nicer to use
the functions in the `Curve` module or the `SubPath` module.

These functions are only meant to build up primitives.


## Moving the cursor

@docs MoveTo, moveTo


## Drawing on the canvas


## Type

@docs DrawTo


## Straight lines

@docs lineTo


## Close Path

@docs closePath


## Beziers

@docs quadraticCurveTo, cubicCurveTo


## Arcs

@docs arcTo, EllipticalArcArgument, clockwise, counterClockwise, largestArc, smallestArc
@docs ArcFlag, Direction


## Threading State

@docs CursorState, updateCursorState


## Conversion

@docs merge
@docs fromLowLevelMoveTo, fromLowLevelDrawTos, fromLowLevelDrawTo
@docs toLowLevelDrawTo, toLowLevelMoveTo
@docs mapCoordinateDrawTo, scaleMoveTo, scaleDrawTo

-}

import List.Extra as List
import Path.LowLevel as LowLevel exposing (ArcFlag(..), Direction(..), Mode(..))
import Vector2d


{-| Constructors for MoveTo instructions
-}
type MoveTo
    = MoveTo ( Float, Float )


{-| Constructors for DrawTo instructions

You may miss some constructs in comparison to SVG. Only absolute coordinates are
supported, and the smooth curve variants are removed. These choices were
made to keep the number of constructors small.

Relative coordinates can always be transformed to abslute ones.

horizontal and vertical movements can be written as `LineTo` commands,
smooth (also known as short-hand) curve extensions can be
achieved with `Curve.smoothQuadraticBezier` and `Curve.smoothCubicBezier`.

The `SubPath.parser` will do these transformations automatically.

-}
type DrawTo
    = LineTo (List ( Float, Float ))
    | CurveTo (List ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ))
    | QuadraticBezierCurveTo (List ( ( Float, Float ), ( Float, Float ) ))
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
    , target : ( Float, Float )
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

**Note:** this is clockwise in the "normal" coordinate system with positive y pointing up and positive x pointing right

-}
clockwise : Direction
clockwise =
    Clockwise


{-| Corresponds to a sweep flag of 1

**Note:** this is counter-clockwise in the "normal" coordinate system with positive y pointing up and positive x pointing right

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


{-| Move to a position on the canvas without drawing. The `M` instruction.
-}
moveTo : ( Float, Float ) -> MoveTo
moveTo =
    MoveTo


{-| Draw a series of line segments to absolute positions. The `L` instruction.
-}
lineTo : List ( Float, Float ) -> DrawTo
lineTo =
    LineTo


{-| Draw a straight line from the cursor position to the starting position of the path. The `Z` instruction.
-}
closePath : DrawTo
closePath =
    ClosePath


{-| A quadratic bezier. The `Q` instruction.
-}
quadraticCurveTo : List ( ( Float, Float ), ( Float, Float ) ) -> DrawTo
quadraticCurveTo =
    QuadraticBezierCurveTo


{-| A cubic bezier. The `C` instruction.
-}
cubicCurveTo : List ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ) -> DrawTo
cubicCurveTo =
    CurveTo


{-| An elliptical arc. The `A` instruction.
-}
arcTo : List EllipticalArcArgument -> DrawTo
arcTo =
    EllipticalArc


{-|

  - `start` start of the subpath (most recent `MoveTo`)
  - `cursor` the current cursor position
  - `previousControlPoint` if the previous drawto instruction was a curveTo (cubic or quadratic), then
    this value stores Just its last control point position, else Nothing

-}
type alias CursorState =
    { start : ( Float, Float ), cursor : ( Float, Float ), previousControlPoint : Maybe ( Float, Float ) }


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
                    Vector2d.sum (Vector2d.fromComponents target) (Vector2d.fromComponents cursor)
                        |> Vector2d.components
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
        folder element ( accumulatedState, elements ) =
            case fromLowLevelDrawTo element accumulatedState of
                Nothing ->
                    ( accumulatedState, elements )

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
            toAbsoluteFloat mode cursor oldPoints
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
                |> toAbsoluteFloat mode cursor
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
                |> toAbsoluteFloat mode cursor
                |> Maybe.map updateState

        LowLevel.CurveTo mode coordinates ->
            let
                updateState ( ( _, c2, final ), points ) =
                    ( CurveTo points
                    , { state | cursor = final, previousControlPoint = Just c2 }
                    )
            in
            toAbsoluteFloat3 mode cursor coordinates
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
                |> toAbsoluteFloat2 mode cursor
                |> Maybe.map (updateState << makeControlPointExplicitVec2 state << Tuple.second)

        LowLevel.QuadraticBezierCurveTo mode coordinates ->
            let
                updateState ( ( c1, final ), points ) =
                    ( QuadraticBezierCurveTo points
                    , { state | cursor = final, previousControlPoint = Just c1 }
                    )
            in
            coordinates
                |> toAbsoluteFloat2 mode cursor
                |> Maybe.map updateState

        LowLevel.SmoothQuadraticBezierCurveTo mode coordinates ->
            let
                updateState ( finalState, finalPoints ) =
                    ( QuadraticBezierCurveTo finalPoints
                    , finalState
                    )
            in
            coordinates
                |> toAbsoluteFloat mode cursor
                |> Maybe.map (updateState << makeControlPointExplicitVec1 state << Tuple.second)

        LowLevel.EllipticalArc mode arguments ->
            let
                argumentToAbsolute argument =
                    { argument
                        | target =
                            Vector2d.sum (Vector2d.fromComponents argument.target) (Vector2d.fromComponents cursor)
                                |> Vector2d.components
                    }

                updateState ( { target }, points ) =
                    ( EllipticalArc points
                    , { state | cursor = target, previousControlPoint = Nothing }
                    )
            in
            arguments
                |> toAbsoluteArgument mode cursor
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

        CurveTo coordinates ->
            LowLevel.CurveTo Absolute coordinates

        QuadraticBezierCurveTo coordinates ->
            LowLevel.QuadraticBezierCurveTo Absolute coordinates

        EllipticalArc arguments ->
            LowLevel.EllipticalArc Absolute arguments

        ClosePath ->
            LowLevel.ClosePath


makeControlPointExplicitVec1 : CursorState -> List ( Float, Float ) -> ( CursorState, List ( ( Float, Float ), ( Float, Float ) ) )
makeControlPointExplicitVec1 initial withoutContolPoint =
    let
        folder target ( state, accum ) =
            let
                previousControlPoint =
                    Maybe.withDefault state.cursor state.previousControlPoint

                newControlPoint =
                    {-
                       Vec2.sub previousControlPoint state.cursor
                           |> Vec2.negate
                           |> Vec2.add state.cursor
                    -}
                    Vector2d.difference (Vector2d.fromComponents state.cursor) (Vector2d.fromComponents previousControlPoint)
                        |> Vector2d.sum (Vector2d.fromComponents state.cursor)
                        |> Vector2d.components
            in
            ( { state | cursor = target, previousControlPoint = Just newControlPoint }
            , ( newControlPoint, target ) :: accum
            )
    in
    List.foldl folder ( initial, [] ) withoutContolPoint
        |> Tuple.mapSecond List.reverse


makeControlPointExplicitVec2 : CursorState -> List ( ( Float, Float ), ( Float, Float ) ) -> ( CursorState, List ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ) )
makeControlPointExplicitVec2 initial withoutContolPoint =
    let
        folder ( c2, target ) ( state, accum ) =
            let
                previousControlPoint =
                    Maybe.withDefault state.cursor state.previousControlPoint

                newControlPoint =
                    Vector2d.difference (Vector2d.fromComponents state.cursor) (Vector2d.fromComponents previousControlPoint)
                        |> Vector2d.sum (Vector2d.fromComponents state.cursor)
                        |> Vector2d.components
            in
            ( { state | cursor = target, previousControlPoint = Just c2 }
            , ( newControlPoint, c2, target ) :: accum
            )
    in
    List.foldl folder ( initial, [] ) withoutContolPoint
        |> Tuple.mapSecond List.reverse



{-
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
-}


toAbsoluteArgument mode cursor coordinates =
    case mode of
        Absolute ->
            coordinates
                |> last
                |> Maybe.map (\final -> ( final, coordinates ))

        Relative ->
            loopArgument cursor coordinates []


addArgument offset argument =
    { argument
        | target =
            Vector2d.sum (Vector2d.fromComponents argument.target) (Vector2d.fromComponents offset)
                |> Vector2d.components
    }


loopArgument : ( Float, Float ) -> List EllipticalArcArgument -> List EllipticalArcArgument -> Maybe ( EllipticalArcArgument, List EllipticalArcArgument )
loopArgument offset remaining accum =
    case remaining of
        [] ->
            Nothing

        [ x ] ->
            let
                new =
                    addArgument offset x
            in
            Just ( new, List.reverse (new :: accum) )

        x :: xs ->
            let
                (p as new) =
                    addArgument offset x
            in
            loopArgument p.target xs (new :: accum)


toAbsoluteFloat : Mode -> ( Float, Float ) -> List ( Float, Float ) -> Maybe ( ( Float, Float ), List ( Float, Float ) )
toAbsoluteFloat mode cursor coordinates =
    case mode of
        Absolute ->
            coordinates
                |> last
                |> Maybe.map (\final -> ( final, coordinates ))

        Relative ->
            loopFloat cursor coordinates []


addFloat : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
addFloat offset a =
    addVectors offset a


loopFloat : ( Float, Float ) -> List ( Float, Float ) -> List ( Float, Float ) -> Maybe ( ( Float, Float ), List ( Float, Float ) )
loopFloat offset remaining accum =
    case remaining of
        [] ->
            Nothing

        [ x ] ->
            let
                new =
                    addFloat offset x
            in
            Just ( new, List.reverse (new :: accum) )

        x :: xs ->
            let
                (p as new) =
                    addFloat offset x
            in
            loopFloat p xs (new :: accum)


type alias Float2 =
    ( ( Float, Float ), ( Float, Float ) )


toAbsoluteFloat2 : Mode -> ( Float, Float ) -> List Float2 -> Maybe ( Float2, List Float2 )
toAbsoluteFloat2 mode cursor coordinates =
    case mode of
        Absolute ->
            coordinates
                |> last
                |> Maybe.map (\final -> ( final, coordinates ))

        Relative ->
            loopFloat2 cursor coordinates []


addFloat2 : ( Float, Float ) -> Float2 -> Float2
addFloat2 offset ( a, b ) =
    ( addVectors offset a
    , addVectors offset b
    )


loopFloat2 : ( Float, Float ) -> List Float2 -> List Float2 -> Maybe ( Float2, List Float2 )
loopFloat2 offset remaining accum =
    case remaining of
        [] ->
            Nothing

        [ x ] ->
            let
                new =
                    addFloat2 offset x
            in
            Just ( new, List.reverse (new :: accum) )

        x :: xs ->
            let
                (( _, p ) as new) =
                    addFloat2 offset x
            in
            loopFloat2 p xs (new :: accum)


type alias Float3 =
    ( ( Float, Float ), ( Float, Float ), ( Float, Float ) )


toAbsoluteFloat3 : Mode -> ( Float, Float ) -> List Float3 -> Maybe ( Float3, List Float3 )
toAbsoluteFloat3 mode cursor coordinates =
    case mode of
        Absolute ->
            coordinates
                |> last
                |> Maybe.map (\final -> ( final, coordinates ))

        Relative ->
            loopFloat3 cursor coordinates []


addFloat3 : ( Float, Float ) -> Float3 -> Float3
addFloat3 offset ( a, b, c ) =
    ( addVectors offset a
    , addVectors offset b
    , addVectors offset c
    )


loopFloat3 : ( Float, Float ) -> List Float3 -> List Float3 -> Maybe ( Float3, List Float3 )
loopFloat3 offset remaining accum =
    case remaining of
        [] ->
            Nothing

        [ x ] ->
            let
                new =
                    addFloat3 offset x
            in
            Just ( new, List.reverse (new :: accum) )

        x :: xs ->
            let
                (( _, _, p ) as new) =
                    addFloat3 offset x
            in
            loopFloat3 p xs (new :: accum)


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

        noControlPoint currentState =
            { currentState | previousControlPoint = Nothing }
    in
    case drawto of
        LineTo coordinates ->
            maybeUpdateCursor (List.last coordinates)
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
mapCoordinateDrawTo : (( Float, Float ) -> ( Float, Float )) -> DrawTo -> DrawTo
mapCoordinateDrawTo f drawto =
    case drawto of
        LineTo coordinates ->
            LineTo (List.map f coordinates)

        CurveTo coordinates ->
            CurveTo (List.map (mapTuple3 f) coordinates)

        QuadraticBezierCurveTo coordinates ->
            QuadraticBezierCurveTo (List.map (mapTuple2 f) coordinates)

        EllipticalArc arguments ->
            EllipticalArc (List.map (\argument -> { argument | target = f argument.target }) arguments)

        ClosePath ->
            ClosePath


{-| Merge adjacent commands if possible

    merge (lineTo [ ( 0, 0 ) ]) (lineTo [ ( 10, 10 ) ]) --> Ok (lineTo [ (0,0) , (10, 10) ])

    merge (lineTo [ ( 0, 0 ) ]) closePath --> Err (lineTo [ (0,0) ], closePath)

-}
merge : DrawTo -> DrawTo -> Result ( DrawTo, DrawTo ) DrawTo
merge instruction1 instruction2 =
    case ( instruction1, instruction2 ) of
        ( LineTo p1, LineTo p2 ) ->
            Ok <| LineTo (p1 ++ p2)

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


{-| scale a moveto
-}
scaleMoveTo : ( Float, Float ) -> MoveTo -> MoveTo
scaleMoveTo scaleFactors (MoveTo point) =
    MoveTo (pointwise2 (*) scaleFactors point)


{-| scale a drawto
-}
scaleDrawTo : { origin : ( Float, Float ), scaleX : Float, scaleY : Float } -> DrawTo -> DrawTo
scaleDrawTo { origin, scaleX, scaleY } drawto =
    let
        scaling point =
            Vector2d.difference (Vector2d.fromComponents origin) (Vector2d.fromComponents point)
                |> Vector2d.components
                |> (\( x, y ) -> ( scaleX * x, scaleY * y ))
                |> Vector2d.fromComponents
                |> Vector2d.sum (Vector2d.fromComponents origin)
                |> Vector2d.components
    in
    case drawto of
        LineTo points ->
            LineTo (List.map scaling points)

        QuadraticBezierCurveTo points ->
            QuadraticBezierCurveTo (List.map (mapTuple2 scaling) points)

        CurveTo points ->
            CurveTo (List.map (mapTuple3 scaling) points)

        EllipticalArc configs ->
            let
                mapper config =
                    { config
                        | radii =
                            config.radii |> (\( x, y ) -> ( scaleX * x, scaleY * y ))
                        , target = scaling config.target
                    }
            in
            EllipticalArc (List.map mapper configs)

        ClosePath ->
            ClosePath


addVectors : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
addVectors x y =
    Vector2d.sum (Vector2d.fromComponents x) (Vector2d.fromComponents y)
        |> Vector2d.components


mapTuple2 : (a -> b) -> ( a, a ) -> ( b, b )
mapTuple2 f ( a, b ) =
    ( f a, f b )


pointwise2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
pointwise2 f ( a1, a2 ) ( b1, b2 ) =
    ( f a1 b1, f a2 b2 )


pointwise3 : (a -> b -> c) -> ( a, a, a ) -> ( b, b, b ) -> ( c, c, c )
pointwise3 f ( a1, a2, a3 ) ( b1, b2, b3 ) =
    ( f a1 b1, f a2 b2, f a3 b3 )


mapTuple3 : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapTuple3 f ( a, b, c ) =
    ( f a, f b, f c )
