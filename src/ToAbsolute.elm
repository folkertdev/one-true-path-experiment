module ToAbsolute exposing (..)

{-| WIP convert relative instructions to absolute ones

-}

import State exposing (State)
import Path exposing (..)


type alias CursorState =
    { start : Coordinate, cursor : Coordinate }


addCoordinates : Coordinate -> Coordinate -> Coordinate
addCoordinates ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


toAbsolutePath : Path -> Path
toAbsolutePath subpaths =
    case subpaths of
        [] ->
            []

        ({ moveto } as sp) :: sps ->
            case moveto of
                MoveTo _ coordinate ->
                    let
                        initialState =
                            { start = coordinate, cursor = coordinate }
                    in
                        State.traverse toAbsoluteSubPath_ subpaths
                            |> State.finalValue initialState


toAbsoluteSubPath_ : SubPath -> State CursorState SubPath
toAbsoluteSubPath_ subpath =
    State.advance (\state -> toAbsoluteSubPath state subpath)


toAbsoluteSubPath : CursorState -> SubPath -> ( SubPath, CursorState )
toAbsoluteSubPath ({ start, cursor } as state) { moveto, drawtos } =
    -- the foldlM and map List.reverse should be combined into a (working) traverse
    State.map2 SubPath (toAbsoluteMoveTo_ moveto) (State.map List.reverse << State.traverse toAbsoluteDrawTo_ << List.reverse <| drawtos)
        |> State.run state


toAbsoluteMoveTo_ : MoveTo -> State CursorState MoveTo
toAbsoluteMoveTo_ moveto =
    State.advance (\state -> toAbsoluteMoveTo state moveto)


toAbsoluteDrawTo_ : DrawTo -> State CursorState DrawTo
toAbsoluteDrawTo_ drawto =
    State.advance (\state -> toAbsoluteDrawTo state drawto)


toAbsoluteMoveTo : CursorState -> MoveTo -> ( MoveTo, CursorState )
toAbsoluteMoveTo { start, cursor } (MoveTo mode coordinate) =
    case mode of
        Absolute ->
            ( MoveTo Absolute coordinate, { start = coordinate, cursor = coordinate } )

        Relative ->
            let
                newCoordinate =
                    addCoordinates cursor coordinate
            in
                ( MoveTo Absolute newCoordinate, { start = newCoordinate, cursor = newCoordinate } )


mapCoordinates : (Mode -> Coordinate -> Coordinate) -> DrawTo -> DrawTo
mapCoordinates tagger drawto =
    case drawto of
        LineTo mode coordinates ->
            LineTo mode (List.map (tagger mode) coordinates)

        Horizontal mode xs ->
            Horizontal mode (List.map (Tuple.first << tagger mode << (\x -> ( x, 0 ))) xs)

        Vertical mode ys ->
            Vertical mode (List.map (Tuple.second << tagger mode << (\y -> ( 0, y ))) ys)

        _ ->
            drawto


toAbsoluteDrawTo : CursorState -> DrawTo -> ( DrawTo, CursorState )
toAbsoluteDrawTo ({ start, cursor } as state) drawto =
    case drawto of
        LineTo mode coordinates ->
            let
                absoluteCoordinates =
                    coordinatesToAbsolute mode (coordinateToAbsolute cursor) coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( LineTo Absolute [], state )

                    Just finalCoordinate ->
                        ( LineTo Absolute absoluteCoordinates
                        , { state | cursor = finalCoordinate }
                        )

        Horizontal mode xs ->
            let
                absoluteCoordinates =
                    xs
                        |> List.map (\x -> ( x, 0 ))
                        |> coordinatesToAbsolute mode (coordinateToAbsolute cursor)
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( Horizontal Absolute [], state )

                    Just finalCoordinate ->
                        ( Horizontal Absolute (List.map Tuple.first absoluteCoordinates)
                        , { state | cursor = finalCoordinate }
                        )

        CurveTo mode coordinates ->
            let
                coordinateToAbsolute ( p1, p2, p3 ) =
                    ( addCoordinates cursor p1
                    , addCoordinates cursor p1
                    , addCoordinates cursor p1
                    )

                absoluteCoordinates =
                    case mode of
                        Absolute ->
                            coordinates

                        Relative ->
                            List.map coordinateToAbsolute coordinates
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( CurveTo Absolute [], state )

                    Just ( _, _, target ) ->
                        ( CurveTo Absolute absoluteCoordinates, { state | cursor = target } )

        EllipticalArc mode arguments ->
            let
                argumentToAbsolute cursor argument =
                    { argument | target = addCoordinates cursor argument.target }

                absoluteArguments =
                    coordinatesToAbsolute mode (argumentToAbsolute cursor) arguments
            in
                case last absoluteArguments of
                    Nothing ->
                        ( EllipticalArc Absolute [], state )

                    Just { target } ->
                        ( EllipticalArc Absolute absoluteArguments, { state | cursor = target } )

        _ ->
            ( drawto, state )


coordinateToAbsolute =
    addCoordinates


coordinateToAbsolute2 cursor ( p1, p2 ) =
    ( addCoordinates cursor p1, addCoordinates cursor p2 )


coordinateToAbsolute3 cursor ( p1, p2, p3 ) =
    ( addCoordinates cursor p1, addCoordinates cursor p2, addCoordinates cursor p3 )


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
