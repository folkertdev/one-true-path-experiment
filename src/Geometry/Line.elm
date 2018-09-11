module Geometry.Line exposing (length, lengthParameterization)

import Vector2 as Vec2 exposing (..)


length : Float2 -> Float2 -> Float
length start end =
    Vec2.distance start end


lengthParameterization : Float2 -> Float2 -> Float -> Maybe ( Float, Float )
lengthParameterization start end s =
    let
        size =
            length start end
    in
    if s > size then
        Nothing

    else
        Just (Vec2.add start (Vec2.scale (s / size) (Vec2.sub end start)))
