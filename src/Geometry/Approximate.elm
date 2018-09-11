module Geometry.Approximate exposing (Config, approximate)


type alias Config a =
    { split : Float -> a -> ( a, a )
    , percentageError : Float
    , upperBound : a -> Float
    , lowerBound : a -> Float
    , baseCase : a -> Maybe ( Float, Float )
    , length : a -> Float
    }


approximate : Config a -> a -> Float -> Maybe ( Float, Float )
approximate config data s =
    let
        splitFurther data =
            let
                lower =
                    config.lowerBound data

                upper =
                    config.upperBound data

                average =
                    (lower + upper) / 2
            in
            (average - lower) / average > config.percentageError
    in
    if splitFurther data then
        let
            ( left, right ) =
                config.split 0.5 data
        in
        if s < config.lowerBound left then
            approximate config left s

        else if s > config.upperBound data then
            Nothing

        else if s > config.upperBound left then
            approximate config right (s - config.length left)

        else
            case approximate config left s of
                Just p ->
                    Just p

                Nothing ->
                    approximate config right (s - config.length left)

    else
        config.baseCase data
