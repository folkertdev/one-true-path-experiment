module ToAbsolute exposing (..)

{-| WIP convert relative instructions to absolute ones

-}

import State exposing (State)
import Path exposing (..)


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


traverseLeft : (a -> State s b) -> List a -> State s (List b)
traverseLeft f list =
    State.foldlM (\accum elem -> State.map2 (::) (f elem) (State.state accum)) [] list


toAbsoluteMoveTo_ : MoveTo -> State CursorState MoveTo
toAbsoluteMoveTo_ moveto =
    State.advance (\state -> toAbsoluteMoveTo state moveto)


toAbsoluteDrawTo_ : DrawTo -> State CursorState DrawTo
toAbsoluteDrawTo_ drawto =
    State.advance
        (\state ->
            let
                _ =
                    Debug.log "absolute drawto" ( drawto, state )
            in
                toAbsoluteDrawTo state drawto
        )


type alias CursorState =
    { start : Coordinate, cursor : Coordinate }


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


toAbsoluteCoordinates mode cursor coordinates =
    case mode of
        Absolute ->
            coordinates

        Relative ->
            List.map (addCoordinates cursor) coordinates


toAbsoluteDrawTo : CursorState -> DrawTo -> ( DrawTo, CursorState )
toAbsoluteDrawTo ({ start, cursor } as state) drawto =
    case drawto of
        LineTo mode coordinates ->
            let
                absoluteCoordinates =
                    toAbsoluteCoordinates mode cursor coordinates
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
                        |> toAbsoluteCoordinates mode cursor
            in
                case last absoluteCoordinates of
                    Nothing ->
                        ( Horizontal Absolute [], state )

                    Just finalCoordinate ->
                        ( Horizontal Absolute (List.map Tuple.first absoluteCoordinates)
                        , { state | cursor = finalCoordinate }
                        )

        _ ->
            ( drawto, state )


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
