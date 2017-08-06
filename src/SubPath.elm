module SubPath exposing (..)

import Path exposing (DrawTo(..), MoveTo(..), lineTo, closePath, moveTo)
import Deque exposing (Deque)
import Vector2 as Vec2 exposing (Vec2)
import Vector3 as Vec3 exposing (Vec3)
import Matrix4
import List.Extra as List
import Segment


type SubPath
    = SubPath { moveto : MoveTo, drawtos : Deque DrawTo }
    | Empty


subpath : MoveTo -> List DrawTo -> SubPath
subpath moveto drawtos =
    SubPath { moveto = moveto, drawtos = Deque.fromList drawtos }


area : List ( ( Float, Float ), ( Float, Float ) ) -> SubPath
area points =
    let
        ( low, high ) =
            points
                |> List.unzip
    in
        step 1.0 low
            |> connect (step 1.0 high)
            |> close


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


{-| Start the second subpath where the first one ends
-}
continue : SubPath -> SubPath -> SubPath
continue =
    map2
        (\a b ->
            let
                (MoveTo firstPoint) =
                    b.moveto

                finalPoint =
                    finalCursorState a
                        |> .cursor

                distance =
                    Vec2.sub firstPoint finalPoint
            in
                SubPath
                    { moveto = a.moveto
                    , drawtos = a.drawtos |> Deque.append (Deque.map (mapCoordinateDrawTo (Vec2.add distance)) b.drawtos)
                    }
        )


{-| Start the second subpath where the first one ends, and rotate it to continue smoothly
-}
continueSmooth : SubPath -> SubPath -> SubPath
continueSmooth =
    map2
        (\a b ->
            let
                (MoveTo firstPoint) =
                    b.moveto

                finalPoint =
                    finalCursorState a
                        |> .cursor

                distance =
                    Vec2.sub firstPoint finalPoint
            in
                case
                    ( List.reverse <| Segment.subpathToSegments { moveto = a.moveto, drawtos = Deque.toList a.drawtos }
                    , Segment.subpathToSegments { moveto = b.moveto, drawtos = Deque.toList b.drawtos }
                    )
                of
                    ( final :: _, first :: _ ) ->
                        let
                            angle =
                                Vec2.angle (Segment.derivativeAtFinal final) (Segment.derivativeAtFirst first)

                            matrix =
                                Matrix4.makeRotate angle ( 0, 0, 1 )
                                    |> Matrix4.mul (Matrix4.makeTranslate (Vec3.fromV2 distance 0))

                            transform : Vec2 Float -> Vec2 Float
                            transform vector =
                                Vec3.fromV2 vector 0
                                    |> Matrix4.transform matrix
                                    |> (\( x, y, _ ) -> ( x, y ))
                        in
                            SubPath
                                { moveto = a.moveto
                                , drawtos = a.drawtos |> Deque.append (Deque.map (mapCoordinateDrawTo transform) b.drawtos)
                                }

                    ( _, [] ) ->
                        SubPath a

                    ( [], _ ) ->
                        SubPath b
        )


{-| Join two subpaths, connecting them with a straight line
-}
connect : SubPath -> SubPath -> SubPath
connect =
    map2
        (\a b ->
            let
                (MoveTo secondStart) =
                    b.moveto
            in
                SubPath
                    { moveto = a.moveto
                    , drawtos =
                        Deque.append a.drawtos (Deque.pushFront (lineTo [ secondStart ]) b.drawtos)
                    }
        )


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
                    SubPath { moveto = moveto, drawtos = Deque.pushBack closePath drawtos }


{-| Draw straight lines between the data points.


<img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/linear.svg" />

-}
linear : List (Vec2 Float) -> SubPath
linear points =
    case points of
        [] ->
            Empty

        x :: xs ->
            subpath (moveTo x) [ lineTo xs ]


{-| Step goes some distance to the right, then to the y-coordinate of the next data point, and then draws to the next point.

The first argument determines where the step is.

* `step 1 points` is  `stepAfter`
* `step 0 points` is `stepBefore`
* `step 0.5 points` steps exactly in the middle

<img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/step.svg" />
-}
step : Float -> List (Vec2 Float) -> SubPath
step factor points =
    let
        helper ( x0, y0 ) ( x, y ) =
            if factor <= 0 then
                [ ( x0, y ), ( x, y ) ]
            else
                let
                    x1 =
                        x0 * (1 - factor) + x * factor
                in
                    [ ( x1, y0 ), ( x1, y ) ]
    in
        case points of
            [] ->
                Empty

            p :: ps ->
                p
                    :: (List.concat (List.map2 helper points ps) ++ [ List.last ps |> Maybe.withDefault p ])
                    |> linear


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
                updateCursorState drawto initial

            _ ->
                initial


{-| Contains the start of the current subpath and the current cursor position.
-}
type alias CursorState =
    { start : Vec2 Float, cursor : Vec2 Float }


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
