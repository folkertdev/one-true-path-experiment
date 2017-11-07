module SubPath
    exposing
        ( SubPath
        , close
        , connect
        , continue
        , continueSmooth
        , element
        , empty
        , fromLowLevel
        , fromSegments
        , mapCoordinate
        , mapWithCursorState
        , rotate
        , scale
        , subpath
        , toLowLevel
        , toSegments
        , toString
        , translate
        , unwrap
        , reverse
        , compress
        )

{-|


## Types

@docs SubPath


## Construction

@docs subpath, empty


## Conversion

@docs element, toString, toSegments, fromSegments, unwrap


## Composition

<img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/subpath-composition.svg" />

    import Curve

    curve : SubPath
    curve =
        Curve.quadraticBezier ( 0, 0 )
            [ ( ( 0.5, -0.5 ), ( 1.0, 0 ) ) ]


    down : SubPath
    down =
        Curve.linear [ ( 0, 0 ), ( 0, 1 ) ]

    curve
        |> connect down
        |> SubPath.toString
        --> "M0,0 Q0.5,-0.5 1,0 L0,0 L0,1"

    curve
        |> continue down
        |> SubPath.toString
        --> "M0,0 Q0.5,-0.5 1,0 L1,1"

    curve
        |> continueSmooth down
        |> SubPath.toString
        --> "M0,0 Q0.5,-0.5 1,0 L1.707106781187,0.707106781187"

    close curve
        |> SubPath.toString
        --> "M0,0 Q0.5,-0.5 1,0 Z"


@docs continue , connect, continueSmooth, close


## Mapping

@docs reverse, compress
@docs translate, rotate, scale
@docs mapCoordinate, mapWithCursorState


## Conversion

@docs fromLowLevel, toLowLevel

-}

import Deque exposing (Deque)
import List.Extra as List
import LowLevel.Command as Command exposing (CursorState, DrawTo(..), MoveTo(..))
import Matrix4 as Mat4
import Path.LowLevel as LowLevel
import Segment exposing (Segment)
import Svg
import Svg.Attributes
import Vector2 as Vec2 exposing (Vec2)
import Vector3 as Vec3 exposing (Vec3)


{-| Type representing a subpath

A subpath is one moveto command followed by an arbitrary number of drawto commands.

-}
type SubPath
    = SubPath Instructions
    | Empty


type alias Instructions =
    { moveto : MoveTo, drawtos : Deque DrawTo }


{-| Construct a subpath

    subpath (moveTo (0,0)) [ lineTo [ (10,10), (10, 20) ] ]
-}
subpath : MoveTo -> List DrawTo -> SubPath
subpath moveto drawtos =
    SubPath { moveto = moveto, drawtos = Deque.fromList drawtos }


{-| An empty subpath
-}
empty : SubPath
empty =
    Empty


{-| deconstruct a subpath into its components
-}
unwrap : SubPath -> Maybe { moveto : MoveTo, drawtos : List DrawTo }
unwrap subpath =
    case subpath of
        Empty ->
            Nothing

        SubPath internal ->
            Just { internal | drawtos = Deque.toList internal.drawtos }


{-| Convert a subpath into SVG path notation

    import LowLevel.Command exposing (moveTo, lineTo)

    line : SubPath
    line = subpath (moveTo (0,0)) [ lineTo [ (10,10), (10, 20) ] ]

    SubPath.toString line --> "M0,0 L10,10 10,20"
-}
toString : SubPath -> String
toString subpath =
    toLowLevel subpath
        |> Maybe.map (LowLevel.toString << List.singleton)
        |> Maybe.withDefault ""


{-| Construct an svg path element from a `Path` with the given attributes

    Svg.svg []
        [ SubPath.element mySubPath [ stroke "black" ] ]
-}
element : SubPath -> List (Svg.Attribute msg) -> Svg.Svg msg
element path attributes =
    Svg.path (Svg.Attributes.d (toString path) :: attributes) []


{-| Map over each drawto with the CursorState available.

The CursorState contains the subpath start position and the current cursor position at the
current DrawTo

-}
mapWithCursorState : (CursorState -> DrawTo -> b) -> SubPath -> List b
mapWithCursorState mapDrawTo subpath =
    case subpath of
        Empty ->
            []

        SubPath { moveto, drawtos } ->
            let
                (MoveTo start) =
                    moveto

                folder : DrawTo -> ( CursorState, List b ) -> ( CursorState, List b )
                folder drawto ( cursorState, accum ) =
                    let
                        new =
                            { start = cursorState.start, cursor = cursorState.cursor, previousControlPoint = Nothing }
                                |> Command.updateCursorState drawto
                    in
                        ( new
                        , mapDrawTo cursorState drawto :: accum
                        )
            in
                Deque.foldl folder ( { start = start, cursor = start, previousControlPoint = Nothing }, [] ) drawtos
                    |> Tuple.second
                    |> List.reverse


{-| Start the second subpath where the first one ends

-}
continue : SubPath -> SubPath -> SubPath
continue =
    let
        helper right left =
            let
                distance =
                    Vec2.sub (finalPoint left) (firstPoint right)
            in
                unsafeConcatenate left (mapCoordinateInstructions (Vec2.add distance) right)
                    |> SubPath
    in
        map2 helper


{-| Start the second subpath where the first one ends, and rotate it to continue smoothly
-}
continueSmooth : SubPath -> SubPath -> SubPath
continueSmooth right left =
    case List.last (toSegments left) of
        Nothing ->
            right

        Just final ->
            case List.head (toSegments right) of
                Nothing ->
                    left

                Just first ->
                    let
                        angle =
                            -- angle is negated because the svg coord system has +y facing down
                            Segment.angle final first
                                |> negate
                    in
                        left
                            |> continue (rotate angle right)


{-| Join two subpaths, connecting them with a straight line
-}
connect : SubPath -> SubPath -> SubPath
connect =
    let
        helper right left =
            unsafeConcatenate (pushBack (Command.lineTo [ firstPoint right ]) left) right
                |> SubPath
    in
        map2 helper


{-| Append a ClosePath at the end of the subpath (if none is present)
-}
close : SubPath -> SubPath
close subpath =
    case subpath of
        Empty ->
            Empty

        SubPath { moveto, drawtos } ->
            case Deque.popBack drawtos of
                ( Just ClosePath, preceding ) ->
                    -- subpath is already closed, return original
                    subpath

                _ ->
                    SubPath { moveto = moveto, drawtos = Deque.pushBack Command.closePath drawtos }


{-| Map over all the 2D coordinates in a subpath
-}
mapCoordinate : (Vec2 Float -> Vec2 Float) -> SubPath -> SubPath
mapCoordinate f subpath =
    case subpath of
        SubPath { moveto, drawtos } ->
            case moveto of
                MoveTo coordinate ->
                    SubPath
                        { moveto = MoveTo (f coordinate)
                        , drawtos = Deque.map (Command.mapCoordinateDrawTo f) drawtos
                        }

        Empty ->
            Empty


mapCoordinateInstructions : (Vec2 Float -> Vec2 Float) -> Instructions -> Instructions
mapCoordinateInstructions f { moveto, drawtos } =
    case moveto of
        MoveTo coordinate ->
            { moveto = MoveTo (f coordinate)
            , drawtos = Deque.map (Command.mapCoordinateDrawTo f) drawtos
            }


finalCursorState : { moveto : MoveTo, drawtos : Deque DrawTo } -> CursorState
finalCursorState { moveto, drawtos } =
    let
        (MoveTo start) =
            moveto

        initial =
            { start = start, cursor = start, previousControlPoint = Nothing }
    in
        Deque.foldl Command.updateCursorState initial drawtos


{-| Convert a list of segments to a path

In the conversion, the starting point of a segment is discarded:
It is assumed that for every two adjacent segments in the list, the first segment's end point is the second segment's starting point

    import Segment exposing (line)
    import LowLevel.Command exposing (moveTo, lineTo)


    [ line (0,0) (10,10) , line (10, 10) (20, 10) ]
        |> fromSegments
        --> subpath (moveTo (0,0)) [ lineTo [ (10, 10)], lineTo [ (20, 10) ] ]

-}
fromSegments : List Segment -> SubPath
fromSegments segments =
    case segments of
        [] ->
            Empty

        segment :: rest ->
            subpath (Command.moveTo (Segment.firstPoint segment)) (List.map Segment.toDrawTo segments)


{-| Convert a subpath to its `Segment`s

    subpath (moveTo (0,0)) [ lineTo [ (10, 10), (20, 10) ] ]
        |> toSegments
        --> [ line (0,0) (10,10) , line (10, 10) (20, 10) ]
-}
toSegments : SubPath -> List Segment
toSegments subpath =
    case subpath of
        Empty ->
            []

        SubPath { moveto, drawtos } ->
            case moveto of
                MoveTo coordinate ->
                    let
                        cursorState =
                            { start = coordinate, cursor = coordinate, previousControlPoint = Nothing }

                        folder drawto ( previousState, accum ) =
                            let
                                newSegments =
                                    Segment.toSegment previousState drawto

                                finalNewSegment =
                                    newSegments
                                        |> List.last
                                        |> Maybe.map Segment.toCursorState
                                        |> Maybe.withDefault previousState
                            in
                                ( finalNewSegment, accum ++ newSegments )
                    in
                        List.foldl folder ( cursorState, [] ) (Deque.toList drawtos)
                            |> Tuple.second


{-| Reverse a subpath

The direction of a subpath [can be important][reverse] if you want to use SVG fills.

    mySubPath : SubPath
    mySubPath =
        subpath (moveTo (0,0))
            [ lineTo [ (10, 10)], lineTo [ (20, 10) ] ]

    mySubPath
        |> reverse
        |> reverse
        --> mySubPath



[reverse]: https://pomax.github.io/svg-path-reverse/
-}
reverse : SubPath -> SubPath
reverse =
    let
        reverseMap f =
            List.foldl (\elem accum -> f elem :: accum) []
    in
        fromSegments << reverseMap Segment.reverse << toSegments


{-| Try to merge adjacent instructions

This conversion is costly (timewise), but can shorten a subpath
considerably, meaning other functions are faster.

Additionally, the toString output can become shorter.
-}
compress : SubPath -> SubPath
compress subpath =
    case subpath of
        Empty ->
            Empty

        SubPath data ->
            SubPath { data | drawtos = compressHelper data.drawtos }


{-| merge adjacent instructions

-}
compressHelper : Deque DrawTo -> Deque DrawTo
compressHelper drawtos =
    let
        folder instruction ( previous, accum ) =
            case Command.merge previous instruction of
                Ok merged ->
                    ( merged, accum )

                Err _ ->
                    ( instruction, previous :: accum )
    in
        case Deque.toList drawtos of
            [] ->
                Deque.empty

            first :: rest ->
                List.foldl folder ( first, [] ) rest
                    |> uncurry (::)
                    |> List.reverse
                    |> Deque.fromList


{-| Translate the subpath by a vector
-}
translate : Vec2 Float -> SubPath -> SubPath
translate vec subpath =
    mapCoordinate (Vec2.add vec) subpath


{-| Rotate a subpath around its starting point by an angle (in radians).
-}
rotate : Float -> SubPath -> SubPath
rotate angle subpath =
    case subpath of
        Empty ->
            Empty

        SubPath { moveto, drawtos } ->
            let
                (MoveTo firstPoint) =
                    moveto

                -- attempt to round to "nice" floats for fuzz tests
                cleanFloat v =
                    round (v * 1.0e12)
                        |> toFloat
                        |> (\v -> v * 1.0e-12)

                cleanVec2 ( x, y ) =
                    ( cleanFloat x, cleanFloat y )

                rotate angle point =
                    -- something simpler will lead to NaNs
                    point
                        |> flip Vec3.fromV2 0
                        |> Mat4.transform (Mat4.makeRotate angle ( 0, 0, 1 ))
                        |> (\( x, y, z ) -> ( x, y ))

                transform point =
                    point
                        |> flip Vec2.sub firstPoint
                        |> rotate angle
                        |> cleanVec2
                        |> Vec2.add firstPoint
            in
                mapCoordinate transform subpath


{-| Scale the subpath in the x and y direction

For more complex scaling operations, define a transformation matrix and use `mapCoordinate`.

-}
scale : Vec2 Float -> SubPath -> SubPath
scale vec subpath =
    case subpath of
        Empty ->
            Empty

        SubPath { moveto, drawtos } ->
            let
                (MoveTo firstPoint) =
                    moveto

                transform point =
                    point
                        |> flip Vec2.sub firstPoint
                        |> flip Vec3.fromV2 0
                        |> Mat4.transform (Mat4.makeScale (Vec3.fromV2 vec 0))
                        |> (\( x, y, z ) -> ( x, y ))
                        |> Vec2.add firstPoint
            in
                mapCoordinate transform subpath


{-| Converting a svg-path-lowlevel subpath into a one-true-path subpath. Used in parsing

> Beware that the moveto is always interpreted as **Absolute**.
-}
fromLowLevel : LowLevel.SubPath -> SubPath
fromLowLevel { moveto, drawtos } =
    -- first moveto is always interpreted absolute
    case moveto of
        LowLevel.MoveTo _ target ->
            let
                initialCursorState =
                    { start = target, cursor = target, previousControlPoint = Nothing }
            in
                subpath (MoveTo target) (Tuple.second <| Command.fromLowLevelDrawTos drawtos initialCursorState)


{-| Converting a one-true-path subpath into a svg-path-lowlevel subpath. Used in toString
-}
toLowLevel : SubPath -> Maybe LowLevel.SubPath
toLowLevel subpath =
    case subpath of
        Empty ->
            Nothing

        SubPath { moveto, drawtos } ->
            Just
                { moveto = Command.toLowLevelMoveTo moveto
                , drawtos = List.map Command.toLowLevelDrawTo (Deque.toList drawtos)
                }



-- HELPERS


map : ({ moveto : MoveTo, drawtos : Deque DrawTo } -> SubPath) -> SubPath -> SubPath
map f subpath =
    case subpath of
        Empty ->
            Empty

        SubPath data ->
            f data


map2 :
    ({ moveto : MoveTo, drawtos : Deque DrawTo } -> { moveto : MoveTo, drawtos : Deque DrawTo } -> SubPath)
    -> SubPath
    -> SubPath
    -> SubPath
map2 f sub1 sub2 =
    case ( sub1, sub2 ) of
        ( Empty, Empty ) ->
            Empty

        ( Empty, subpath ) ->
            subpath

        ( subpath, Empty ) ->
            subpath

        ( SubPath a, SubPath b ) ->
            f a b


firstPoint : Instructions -> Vec2 Float
firstPoint { moveto } =
    case moveto of
        MoveTo p ->
            p


finalPoint : Instructions -> Vec2 Float
finalPoint =
    finalCursorState >> .cursor


pushBack : DrawTo -> Instructions -> Instructions
pushBack drawto data =
    { data | drawtos = Deque.pushBack drawto data.drawtos }


unsafeConcatenate : Instructions -> Instructions -> Instructions
unsafeConcatenate a b =
    { a | drawtos = Deque.append a.drawtos b.drawtos }
