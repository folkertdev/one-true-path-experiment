module SubPath exposing
    ( SubPath
    , with, empty
    , element, toString, toStringWith
    , Option, decimalPlaces, mergeAdjacent
    , reverse, compress
    , continue, connect, continueSmooth, close
    , translate, rotate, scale
    , mapCoordinate, mapWithCursorState
    , ArcLengthParameterized, arcLengthParameterized
    , arcLength, evenlySpaced, evenlySpacedWithEndpoints, evenlySpacedPoints
    , pointAlong, tangentAlong, parameterValueToArcLength, arcLengthToParameterValue
    , toSegments, fromSegments
    , fromLowLevel, toLowLevel, unwrap
    )

{-| `SubPath` is the fundamental type in this package.

In most cases it should be created with functions from the `Curve` module.

    import Curve
    import SubPath exposing (connect)
    import Svg
    import Svg.Attributes exposing (fill)

    right =
        Curve.linear [ ( 0, 0 ), ( 1, 0 ) ]

    down =
        Curve.linear [ ( 0, 0 ), ( 0, 1 ) ]

This module has several functions for composing subpaths

    topRightCorner =
        right
            |> connect down

    bottomLeftCorner
        down
            |> connect right

    square =
        topRightCorner
            |> connect (reverse bottomLeftCorner)
            |> close

And can generate svg elements

    view : Svg msg
    view =
        Svg.svg [] [ SubPath.element square [ fill "none" ] ]


## Types

@docs SubPath


## Construction

@docs with, empty


## Conversion

@docs element, toString, toStringWith
@docs Option, decimalPlaces, mergeAdjacent

@docs reverse, compress


## Composition

![composition of subpaths](https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/subpath-composition.svg)

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

@docs continue, connect, continueSmooth, close


## Mapping

@docs translate, rotate, scale
@docs mapCoordinate, mapWithCursorState


## Arc Length Parameterization

The arc length parameterization is a way of expressing a curve in terms of its arc length. For instance, `pointAlong` expects a distance, and returns the 2D coordinate reached
when walked that distance along the curve.

This is great for calculating the total length of your subpath (for instance to style based on the length) and to get evenly spaced points on the subpath.

@docs ArcLengthParameterized, arcLengthParameterized
@docs arcLength, evenlySpaced, evenlySpacedWithEndpoints, evenlySpacedPoints
@docs pointAlong, tangentAlong, parameterValueToArcLength, arcLengthToParameterValue


## Conversion

@docs toSegments, fromSegments
@docs fromLowLevel, toLowLevel, unwrap

-}

import Curve.ParameterValue as ParameterValue
import Deque exposing (Deque)
import List.Extra as List
import LowLevel.Command as Command exposing (CursorState, DrawTo(..), MoveTo(..))
import Path.LowLevel as LowLevel
import Point2d exposing (Point2d)
import Segment exposing (Segment)
import Svg
import Svg.Attributes
import Vector2d exposing (Vector2d)


{-| A subpath is one moveto command followed by an arbitrary number of drawto commands.

**Note:** Equality with the default `==` function in unreliable for `SubPath`. The easiest way to check for
equality is to use `SubPath.toString` on both arguments.

If you need more "fuzzy" equality use `toStringWith`, for instance:

    options : List Option
    options =
        [ decimalPlaces 3, mergeAdjacent ]

    equalSubpaths : SubPath -> SubPath -> Bool
    equalSubpaths a b =
        toStringWith options a == toStringWith options b

-}
type SubPath
    = SubPath Instructions
    | Empty


type alias Instructions =
    { moveto : MoveTo, drawtos : Deque DrawTo }


{-| Construct a subpath

**Always try to use a function from `Curve` over manual subpath construction!**

    import LowLevel.Command exposing (moveTo, lineTo)

    SubPath.with (moveTo (0,0)) [ lineTo [ (10,10), (10, 20) ] ]

-}
with : MoveTo -> List DrawTo -> SubPath
with moveto drawtos =
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
            Just { moveto = internal.moveto, drawtos = Deque.toList internal.drawtos }


{-| Formatting options
-}
type Option
    = DecimalPlaces Int
    | MergeAdjacent


type alias Config =
    { decimalPlaces : Maybe Int
    , mergeAdjacent : Bool
    }


defaultConfig : Config
defaultConfig =
    { decimalPlaces = Nothing, mergeAdjacent = False }


optionFolder : Option -> Config -> Config
optionFolder option config =
    case option of
        DecimalPlaces n ->
            { config | decimalPlaces = Just n }

        MergeAdjacent ->
            { config | mergeAdjacent = True }


{-| Set the maximum number of decimal places in the output

    import Curve

    line : SubPath
    line = Curve.linear [ (0, 0), (1/3, 1/7) ]

    SubPath.toString line
        --> "M0,0 L0.3333333333333333,0.14285714285714285"

    SubPath.toStringWith [ decimalPlaces 3 ] line
        --> "M0,0 L0.333,0.143"

-}
decimalPlaces : Int -> Option
decimalPlaces =
    DecimalPlaces


{-| Join adjacent instructions where possible
This can save a few characters, but more importantly makes
comparison of subpaths (based on the ouput string) more reliable.

    import Curve

    right : SubPath
    right = Curve.linear [ (0, 0), (1, 0) ]

    down : SubPath
    down = Curve.linear [ (0, 0), (0, 1) ]

    line : SubPath
    line =
        right
            |> continue down

    SubPath.toString line
        --> "M0,0 L1,0 L1,1"

    SubPath.toStringWith [ mergeAdjacent ] line
        --> "M0,0 L1,0 1,1"

-}
mergeAdjacent : Option
mergeAdjacent =
    MergeAdjacent


{-| Convert a subpath into SVG path notation

    import Curve

    line : SubPath
    line = Curve.linear [ (0,0), (10,10), (10, 20) ]

    SubPath.toString line --> "M0,0 L10,10 10,20"

-}
toString : SubPath -> String
toString subpath =
    toStringWith [] subpath


{-| toString with options
-}
toStringWith : List Option -> SubPath -> String
toStringWith options subpath =
    let
        config =
            List.foldl optionFolder defaultConfig options

        lowLevelOptions =
            case config.decimalPlaces of
                Nothing ->
                    []

                Just n ->
                    [ LowLevel.decimalPlaces n ]
    in
    subpath
        |> (if config.mergeAdjacent then
                compress

            else
                identity
           )
        |> toLowLevel
        |> Maybe.map (LowLevel.toStringWith lowLevelOptions << List.singleton)
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
                    Vector2d.difference
                        (Vector2d.fromComponents (finalPoint left))
                        (Vector2d.fromComponents (firstPoint right))

                mapper =
                    Vector2d.components << Vector2d.sum distance << Vector2d.fromComponents
            in
            unsafeConcatenate left (mapCoordinateInstructions mapper right)
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
mapCoordinate : (( Float, Float ) -> ( Float, Float )) -> SubPath -> SubPath
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


mapCoordinateInstructions : (( Float, Float ) -> ( Float, Float )) -> Instructions -> Instructions
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

    import Curve
    import Segment exposing (line)


    [ line (0,0) (10,10) , line (10, 10) (20, 10) ]
        |> fromSegments
        |> SubPath.toStringWith [ mergeAdjacent ]
        --> SubPath.toString <| Curve.linear [ (0,0), (10,10), (20, 10) ]

-}
fromSegments : List Segment -> SubPath
fromSegments segments =
    case segments of
        [] ->
            Empty

        segment :: rest ->
            with (Command.moveTo (Segment.firstPoint segment)) (List.map Segment.toDrawTo segments)


{-| Convert a subpath to its `Segment`s

    import Curve
    import Segment exposing (line)

    Curve.linear [ (0,0), (10,10), (20, 10) ]
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
Another use is in composing subpaths:

    arrowHead : ( Float, Float ) -> Float -> SubPath
    arrowHead location angle =
        let
            line =
                Curve.linear [ ( 0, 0 ), ( 10, 0 ) ]
                    |> SubPath.translate location

            a =
                SubPath.rotate (angle - (pi + pi / 4)) line

            b =
                SubPath.rotate (angle + (pi + pi / 4)) line
        in
        SubPath.reverse a
            |> SubPath.continue b

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
                |> (\( a, b ) -> (::) a b)
                |> List.reverse
                |> Deque.fromList


{-| Translate the subpath by a vector
-}
translate : ( Float, Float ) -> SubPath -> SubPath
translate vec subpath =
    mapCoordinate (Vector2d.components << Vector2d.sum (Vector2d.fromComponents vec) << Vector2d.fromComponents) subpath


{-| Rotate a subpath around its starting point by an angle (in radians).
-}
rotate : Float -> SubPath -> SubPath
rotate angle subpath =
    case subpath of
        Empty ->
            Empty

        SubPath { moveto, drawtos } ->
            let
                (MoveTo startPoint) =
                    moveto

                center =
                    Point2d.fromCoordinates startPoint

                -- attempt to round to "nice" floats for fuzz tests
                cleanFloat v =
                    round (v * 1.0e12)
                        |> toFloat
                        |> (\value -> value * 1.0e-12)

                cleanVec2 ( x, y ) =
                    ( cleanFloat x, cleanFloat y )

                transform point =
                    point
                        |> Point2d.fromCoordinates
                        |> Point2d.rotateAround center angle
                        |> Point2d.coordinates
            in
            mapCoordinate transform subpath


{-| Scale the subpath in the x and y direction

For more complex scaling operations, define a transformation matrix and use `mapCoordinate`.

-}
scale : ( Float, Float ) -> SubPath -> SubPath
scale ( scaleX, scaleY ) subpath =
    case subpath of
        Empty ->
            Empty

        SubPath { moveto, drawtos } ->
            let
                (MoveTo origin) =
                    moveto

                ds =
                    Deque.toList drawtos
                        |> List.map (Command.scaleDrawTo { origin = origin, scaleX = scaleX, scaleY = scaleY })
                        |> Deque.fromList
            in
            SubPath { moveto = moveto, drawtos = ds }



{-
   SubPath
       { moveto = moveto
       , drawtos =
           Deque.map (Command.scaleDrawTo { origin = origin, scaleX = scaleX, scaleY = scaleY }) drawtos
       }
-}


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
            with (MoveTo target) (Tuple.second <| Command.fromLowLevelDrawTos drawtos initialCursorState)


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



--


{-| The arc length parameterization as a binary tree of segments.
-}
type ArcLengthParameterized
    = Node { totalLength : Float, lengthAtSplit : Float, left : ArcLengthParameterized, right : ArcLengthParameterized, tolerance : Float }
    | Leaf { segment : Segment.ArcLengthParameterized, tolerance : Float }
    | None


{-| Build an arc length parameterization from a subpath.

For calculating the parameterization, approximations are used. To bound the error that approximations introduce,
you can supply a `tolerance`: Operations (arcLength, pointOn, ect.) are at most `tolerance` away from the truth.

    tolerance =
        1.0e-4

    parameterized =
        arcLengthParameterized tolerance mySubPath

> **Note**: keep the scale of your curve in mind. if the length of the curve is 100, then an
> `tolerance` of `0.1` is probably enough for the difference not to be visible.
>
> Using a much smaller `tolerance` can really slow down your page.

-}
arcLengthParameterized : Float -> SubPath -> ArcLengthParameterized
arcLengthParameterized tolerance subpath =
    arcLengthParameterizedHelper tolerance (toSegments subpath)


arcLengthParameterizedHelper : Float -> List Segment -> ArcLengthParameterized
arcLengthParameterizedHelper tolerance segments =
    case segments of
        [] ->
            None

        [ segment ] ->
            Leaf { segment = Segment.arcLengthParameterized tolerance segment, tolerance = tolerance }

        xs ->
            let
                ( leftSegments, rightSegments ) =
                    List.splitAt (ceiling <| toFloat (List.length segments) / 2) segments

                leftParameterized =
                    arcLengthParameterizedHelper tolerance leftSegments

                rightParameterized =
                    arcLengthParameterizedHelper tolerance rightSegments
            in
            case rightParameterized of
                None ->
                    leftParameterized

                _ ->
                    Node
                        { lengthAtSplit = arcLength leftParameterized
                        , totalLength = arcLength leftParameterized + arcLength rightParameterized
                        , left = leftParameterized
                        , right = rightParameterized
                        , tolerance = tolerance
                        }


{-| Find the total arc length of an elliptical arc. This will be accurate to within the tolerance given when calling arcLengthParameterized.

    import Curve

    Curve.linear [ (0,0), (100, 0) ]
        |> arcLengthParameterized 1e-4
        |> arcLength
        --> 100

-}
arcLength : ArcLengthParameterized -> Float
arcLength parameterized =
    case parameterized of
        None ->
            0

        Leaf { segment } ->
            Segment.arcLength segment

        Node { totalLength } ->
            totalLength


traverse : (Segment.ArcLengthParameterized -> Float -> Maybe a) -> ArcLengthParameterized -> Float -> Maybe a
traverse tagger parameterized t =
    let
        clamp totalLength tolerance length =
            if abs (length - totalLength) <= tolerance then
                totalLength

            else if abs length <= tolerance then
                0

            else
                length
    in
    case parameterized of
        None ->
            Nothing

        Leaf { segment, tolerance } ->
            let
                totalLength =
                    Segment.arcLength segment

                answer =
                    tagger segment (clamp totalLength tolerance t)
            in
            answer

        Node { totalLength, lengthAtSplit, left, right, tolerance } ->
            let
                clamped =
                    clamp totalLength tolerance t
            in
            if clamped <= lengthAtSplit then
                traverse tagger left clamped

            else
                traverse tagger right (clamped - lengthAtSplit)


{-| A point at some distance along the curve.

    import Curve

    parameterized : ArcLengthParameterized
    parameterized =
        Curve.quadraticBezier (0,0) [ ( (5,0), (10, 0) ) ]
            |> arcLengthParameterized 1e-4

    pointAlong parameterized (arcLength parameterized / 2)
        --> Just (5, 0)

-}
pointAlong : ArcLengthParameterized -> Float -> Maybe ( Float, Float )
pointAlong parameterized t =
    traverse Segment.pointAlong parameterized t


{-| The tangent along the curve

    import Curve

    parameterized : ArcLengthParameterized
    parameterized =
        Curve.quadraticBezier (0,0) [ ( (5,0), (10, 0) ) ]
            |> parameterized 1e-4

    tangentAlong parameterized (arcLength parameterized / 2)
        --> Just (1, 0)

-}
tangentAlong : ArcLengthParameterized -> Float -> Maybe ( Float, Float )
tangentAlong parameterized t =
    traverse Segment.tangentAlong parameterized t


{-| Find the arc length at some parameter value.
-}
parameterValueToArcLength : ArcLengthParameterized -> Float -> Maybe Float
parameterValueToArcLength parameterized t =
    traverse (\segment value -> Just (Segment.parameterValueToArcLength segment value)) parameterized t


{-| Find the parameter value at some arc length
-}
arcLengthToParameterValue : ArcLengthParameterized -> Float -> Maybe Float
arcLengthToParameterValue parameterized t =
    traverse Segment.arcLengthToParameterValue parameterized t


{-| Find `n` evenly spaced points on an arc length parameterized subpath
Includes the start and end point.

    import Curve

    curve : ArcLengthParameterized
    curve =
        Curve.linear [ (0,0), (10, 0) ]
            |> arcLengthParameterized 1e-4

    evenlySpacedPoints 1 curve
        --> [ (5, 0) ]

    evenlySpacedPoints 2 curve
        --> [ (0, 0), (10, 0) ]

    evenlySpacedPoints 5 curve
        --> [(0,0),(2.5,0),(5,0),(7.5,0),(10,0)]

-}
evenlySpacedPoints : Int -> ArcLengthParameterized -> List ( Float, Float )
evenlySpacedPoints count parameterized =
    evenlySpacedWithEndpoints count parameterized
        |> List.filterMap (pointAlong parameterized)


{-| Evenly splits the curve into `count` segments, giving their length along the curve
-}
evenlySpaced : Int -> ArcLengthParameterized -> List Float
evenlySpaced count parameterized =
    let
        length =
            arcLength parameterized
    in
    evenlySpacedParameterValues count
        |> List.map (\t -> t * length)


{-| Similar to `evenlySpaced`, but also gives the start and end point of the curve

    evenlySpacedPoints : Int -> ArcLengthParameterized -> List ( Float, Float )
    evenlySpacedPoints count parameterized =
        evenlySpacedWithEndpoints count parameterized
            |> List.filterMap (pointAlong parameterized)

-}
evenlySpacedWithEndpoints : Int -> ArcLengthParameterized -> List Float
evenlySpacedWithEndpoints count parameterized =
    let
        length =
            arcLength parameterized
    in
    evenlySpacedParameterValuesWithEndpoints count
        |> List.map (\t -> t * length)


evenlySpacedParameterValuesWithEndpoints : Int -> List Float
evenlySpacedParameterValuesWithEndpoints count =
    if count <= 0 then
        []

    else if count == 1 then
        [ 0.5 ]

    else
        let
            helper currentCount =
                List.repeat currentCount (1 / (1 + toFloat currentCount))
                    |> List.indexedMap (\i v -> (toFloat i + 1) * v)

            intermediate =
                helper (max 0 (count - 2))
        in
        0 :: intermediate ++ [ 1 ]


evenlySpacedParameterValues : Int -> List Float
evenlySpacedParameterValues count =
    List.repeat (count - 1) (1 / toFloat count)
        |> List.indexedMap (\i v -> (toFloat i + 1) * v)



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


firstPoint : Instructions -> ( Float, Float )
firstPoint { moveto } =
    case moveto of
        MoveTo p ->
            p


finalPoint : Instructions -> ( Float, Float )
finalPoint =
    finalCursorState >> .cursor


pushBack : DrawTo -> Instructions -> Instructions
pushBack drawto data =
    { data | drawtos = Deque.pushBack drawto data.drawtos }


unsafeConcatenate : Instructions -> Instructions -> Instructions
unsafeConcatenate a b =
    { a | drawtos = Deque.append a.drawtos b.drawtos }
