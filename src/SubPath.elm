module SubPath
    exposing
        ( SubPath
        , subpath
        , empty
        , toString
        , continue
        , connect
        , continueSmooth
        , close
        , mapCoordinate
        , mapWithCursorState
        , translate
        , rotate
        , scale
        , toSegments
        , unwrap
        )

{-|


## Types
@docs SubPath, empty, subpath, toString

## Composition
@docs continue , connect, continueSmooth, close

## Mapping
@docs mapCoordinate, mapWithCursorState
@docs translate, rotate, scale

## Conversion
@docs toSegments, unwrap



-}

import Deque exposing (Deque)
import Vector2 as Vec2 exposing (Vec2)
import Vector3 as Vec3 exposing (Vec3)
import Matrix4 as Mat4
import List.Extra as List
import Segment exposing (Segment)
import LowLevel.Command as LowLevel exposing (DrawTo(..), MoveTo(..), CursorState, lineTo, closePath, moveTo)
import MixedPath


{-| Type representing a subpath

A subpath is one moveto command followed by an arbitrary number of drawto commands.
-}
type SubPath
    = SubPath { moveto : MoveTo, drawtos : Deque DrawTo }
    | Empty


{-| Construct a subpath
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
                    ( LowLevel.updateCursorState drawto cursorState
                    , mapDrawTo cursorState drawto :: accum
                    )
            in
                List.foldl folder ( { start = start, cursor = start }, [] ) (Deque.toList drawtos)
                    |> Tuple.second
                    |> List.reverse


{-| Start the second subpath where the first one ends
-}
continue : SubPath -> SubPath -> SubPath
continue =
    map2
        (\b a ->
            let
                (MoveTo firstPoint) =
                    b.moveto

                finalPoint =
                    finalCursorState a
                        |> .cursor

                distance =
                    Vec2.sub finalPoint firstPoint

                newRight =
                    b.drawtos
                        |> Deque.toList
                        |> List.map (mapCoordinateDrawTo (Vec2.add distance))
            in
                SubPath
                    { moveto = a.moveto
                    , drawtos = List.foldl Deque.pushBack a.drawtos newRight
                    }
        )


{-| Start the second subpath where the first one ends, and rotate it to continue smoothly
-}
continueSmooth : SubPath -> SubPath -> SubPath
continueSmooth b a =
    case
        ( toSegments a
            |> List.last
        , toSegments b
            |> List.head
        )
    of
        ( Just final, Just first ) ->
            let
                angle =
                    -- angle is negated because the svg coord system has +y facing down
                    Segment.angle final first
                        |> negate
            in
                a
                    |> continue (rotate angle b)

        ( _, Nothing ) ->
            a

        ( Nothing, _ ) ->
            b


{-| Join two subpaths, connecting them with a straight line
-}
connect : SubPath -> SubPath -> SubPath
connect =
    map2
        (\b a ->
            let
                (MoveTo secondStart) =
                    b.moveto
            in
                SubPath
                    { moveto = a.moveto
                    , drawtos =
                        a.drawtos
                            |> Deque.pushBack (LowLevel.lineTo [ secondStart ])
                            |> flip Deque.append b.drawtos
                    }
        )


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
                    SubPath { moveto = moveto, drawtos = Deque.pushBack LowLevel.closePath drawtos }


{-| Map over all the 2D coordinates in a subpath
-}
mapCoordinate : (Vec2 Float -> Vec2 Float) -> SubPath -> SubPath
mapCoordinate f =
    map
        (\{ moveto, drawtos } ->
            case moveto of
                MoveTo coordinate ->
                    SubPath
                        { moveto = MoveTo (f coordinate)
                        , drawtos = Deque.map (mapCoordinateDrawTo f) drawtos
                        }
        )


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


finalCursorState : { moveto : MoveTo, drawtos : Deque DrawTo } -> CursorState
finalCursorState { moveto, drawtos } =
    let
        (MoveTo start) =
            moveto

        initial =
            { start = start, cursor = start }
    in
        case Deque.popBack drawtos of
            ( Just drawto, _ ) ->
                LowLevel.updateCursorState drawto initial

            _ ->
                initial


{-| Convert a list of segments to a path

It is assumed that for every two adjacent segments in the list, the first segment's end point is the second segment's starting point
-}
fromSegments : List Segment -> SubPath
fromSegments segments =
    case segments of
        [] ->
            Empty

        segment :: rest ->
            subpath (moveTo (Segment.startingPoint segment)) (List.map Segment.toDrawTo segments)


{-| -}
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
                            { start = coordinate, cursor = coordinate }

                        -- fake segment to get the fold going, not part of the final result
                        initial =
                            Segment.LineSegment coordinate coordinate

                        folder drawto ( previousSegment, accum ) =
                            let
                                newSegments =
                                    Segment.toSegment previousSegment drawto

                                finalNewSegment =
                                    Maybe.withDefault previousSegment (List.last newSegments)
                            in
                                ( finalNewSegment, accum ++ newSegments )

                        traverse : (a -> ( b, List c ) -> ( b, List c )) -> b -> List a -> List c
                        traverse folder initial elements =
                            List.foldl folder ( initial, [] ) elements
                                |> Tuple.second
                    in
                        traverse folder initial (Deque.toList drawtos)


{-| Translate the subpath by a vector
-}
translate : Vec2 Float -> SubPath -> SubPath
translate vec subpath =
    mapCoordinate (Vec2.add vec) subpath


{-| Rotate a subpath around its starting point by an angle (in degrees).
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
                SubPath
                    { moveto = moveto
                    , drawtos = Deque.map (mapCoordinateDrawTo transform) drawtos
                    }


{-| Rotate a subpath around its starting point by an angle (in degrees).
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
                SubPath
                    { moveto = moveto
                    , drawtos = Deque.map (mapCoordinateDrawTo transform) drawtos
                    }


{-| Convert a subpath into SVG path notation

-}
toString : SubPath -> String
toString subpath =
    case subpath of
        Empty ->
            ""

        SubPath { moveto, drawtos } ->
            { moveto = toMixedMoveTo moveto, drawtos = List.map toMixedDrawTo (Deque.toList drawtos) }
                |> List.singleton
                |> MixedPath.toString


{-| Exposed for testing purposes
-}
toMixedMoveTo : MoveTo -> MixedPath.MoveTo
toMixedMoveTo (MoveTo coordinates) =
    MixedPath.MoveTo MixedPath.Absolute coordinates


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
