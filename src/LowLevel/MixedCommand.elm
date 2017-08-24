module LowLevel.MixedCommand exposing (..)

{-| Commands that can be relative and/or absolute
-}

import Vector2 as Vec2 exposing (Vec2)
import Vector3 as Vec3


type alias Coordinate =
    Vec2 Float


{-| The mode of an instruction
-}
type Mode
    = Relative
    | Absolute


{-| MoveTo instructions move the cursor, but don't draw anything.
-}
type alias MoveTo =
    AbstractMoveTo Mode


type AbstractMoveTo mode
    = MoveTo mode (Vec2 Float)


type alias DrawTo =
    AbstractDrawTo Mode


{-| Constructors for DrawTo instructions
-}
type AbstractDrawTo mode
    = LineTo mode (List Coordinate)
    | Horizontal mode (List Float)
    | Vertical mode (List Float)
    | CurveTo mode (List ( Coordinate, Coordinate, Coordinate ))
    | SmoothCurveTo mode (List ( Coordinate, Coordinate ))
    | QuadraticBezierCurveTo mode (List ( Coordinate, Coordinate ))
    | SmoothQuadraticBezierCurveTo mode (List Coordinate)
    | EllipticalArc mode (List EllipticalArcArgument)
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
type ArcFlag
    = SmallestArc
    | LargestArc


{-| Determine which arc to draw
-}
type Direction
    = Clockwise
    | CounterClockwise


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


mapCoordinateMoveTo : (Coordinate -> Coordinate) -> AbstractMoveTo a -> AbstractMoveTo a
mapCoordinateMoveTo f moveto =
    case moveto of
        MoveTo mode coordinate ->
            MoveTo mode (f coordinate)


mapCoordinateDrawTo : (Coordinate -> Coordinate) -> AbstractDrawTo a -> AbstractDrawTo a
mapCoordinateDrawTo f drawto =
    case drawto of
        LineTo mode coordinates ->
            LineTo mode (List.map f coordinates)

        Horizontal mode coordinates ->
            coordinates
                |> List.map ((\x -> ( x, 0 )) >> f >> Tuple.first)
                |> Horizontal mode

        Vertical mode coordinates ->
            coordinates
                |> List.map ((\y -> ( 0, y )) >> f >> Tuple.second)
                |> Vertical mode

        CurveTo mode coordinates ->
            CurveTo mode (List.map (Vec3.map f) coordinates)

        SmoothCurveTo mode coordinates ->
            SmoothCurveTo mode (List.map (Vec2.map f) coordinates)

        QuadraticBezierCurveTo mode coordinates ->
            QuadraticBezierCurveTo mode (List.map (Vec2.map f) coordinates)

        SmoothQuadraticBezierCurveTo mode coordinates ->
            SmoothQuadraticBezierCurveTo mode (List.map f coordinates)

        EllipticalArc mode arguments ->
            EllipticalArc mode (List.map (\argument -> { argument | target = f argument.target }) arguments)

        ClosePath ->
            ClosePath


type alias CursorState =
    { start : Vec2 Float, cursor : Vec2 Float }


{-| Exposed for testing
-}
toAbsoluteMoveTo : CursorState -> AbstractMoveTo Mode -> ( AbstractMoveTo (), CursorState )
toAbsoluteMoveTo { start, cursor } (MoveTo mode coordinate) =
    case mode of
        Absolute ->
            ( MoveTo () coordinate, { start = coordinate, cursor = coordinate } )

        Relative ->
            let
                newCoordinate =
                    uncurry Vec2.add ( cursor, coordinate )
            in
                ( MoveTo () newCoordinate, { start = newCoordinate, cursor = newCoordinate } )


{-| Exposed for testing
-}
toAbsoluteDrawTo : CursorState -> AbstractDrawTo Mode -> ( AbstractDrawTo (), CursorState )
toAbsoluteDrawTo ({ start, cursor } as state) drawto =
    case drawto of
        LineTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (coordinateToAbsolute cursor) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( LineTo () [], state )

                    Just finalCoordinate ->
                        ( LineTo () absoluteCoordinates
                        , { state | cursor = finalCoordinate }
                        )

        Horizontal mode xs ->
            let
                absoluteCoordinates =
                    List.map (\x -> ( x, 0 )) xs
                        |> coordinatesToAbsolute mode (coordinateToAbsolute cursor)
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( Horizontal () [], state )

                    Just ( finalX, _ ) ->
                        ( Horizontal () (List.map Tuple.first absoluteCoordinates)
                        , { state | cursor = ( finalX, Tuple.second cursor ) }
                        )

        Vertical mode ys ->
            let
                absoluteCoordinates =
                    List.map (\y -> ( 0, y )) ys
                        |> coordinatesToAbsolute mode (coordinateToAbsolute cursor)
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( Vertical () [], state )

                    Just ( _, finalY ) ->
                        ( Vertical () (List.map Tuple.second absoluteCoordinates)
                        , { state | cursor = ( Tuple.first cursor, finalY ) }
                        )

        CurveTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (Vec3.map (coordinateToAbsolute cursor)) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( CurveTo () [], state )

                    Just ( _, _, target ) ->
                        ( CurveTo () absoluteCoordinates, { state | cursor = target } )

        SmoothCurveTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (Vec2.map (coordinateToAbsolute cursor)) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( SmoothCurveTo () [], state )

                    Just ( _, target ) ->
                        ( SmoothCurveTo () absoluteCoordinates, { state | cursor = target } )

        QuadraticBezierCurveTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (Vec2.map (coordinateToAbsolute cursor)) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( QuadraticBezierCurveTo () [], state )

                    Just ( _, target ) ->
                        ( QuadraticBezierCurveTo () absoluteCoordinates, { state | cursor = target } )

        SmoothQuadraticBezierCurveTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (coordinateToAbsolute cursor) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( SmoothQuadraticBezierCurveTo () [], state )

                    Just finalCoordinate ->
                        ( SmoothQuadraticBezierCurveTo () absoluteCoordinates
                        , { state | cursor = finalCoordinate }
                        )

        EllipticalArc mode arguments ->
            let
                argumentToAbsolute cursor argument =
                    { argument | target = Vec2.add cursor argument.target }

                absoluteArguments =
                    coordinatesToAbsolute mode (argumentToAbsolute cursor) arguments
            in
                case last absoluteArguments of
                    Nothing ->
                        ( EllipticalArc () [], state )

                    Just { target } ->
                        ( EllipticalArc () absoluteArguments, { state | cursor = target } )

        ClosePath ->
            ( ClosePath, { state | cursor = start } )


coordinateToAbsolute : Vec2 Float -> Vec2 Float -> Vec2 Float
coordinateToAbsolute =
    Vec2.add


coordinatesToAbsolute : Mode -> (coords -> coords) -> List coords -> List coords
coordinatesToAbsolute mode toAbsolute coordinates =
    case mode of
        Absolute ->
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
