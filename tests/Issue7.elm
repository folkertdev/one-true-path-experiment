module Issue7 exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Path
import Segment
import SubPath
import Test exposing (..)


source =
    "M380,548c-10,96-75,176-105,176"


path =
    case Path.parse source of
        Ok [ subpath ] ->
            subpath

        _ ->
            Debug.todo ""


segmentEndPoint =
    case SubPath.toSegments path of
        [ segment ] ->
            Segment.at 1 segment

        _ ->
            Debug.todo ""


parameterized =
    SubPath.arcLengthParameterized 0.01 path


parameterizedEndPoint =
    case SubPath.pointAlong parameterized (SubPath.arcLength parameterized) of
        Just v ->
            v

        _ ->
            Debug.todo ""


tolerance =
    Absolute 0.1


suite =
    let
        ( segmentX, segmentY ) =
            segmentEndPoint

        ( paramX, paramY ) =
            parameterizedEndPoint
    in
    describe "end points are somewhat close"
        [ test "x position is withing tolerance" <|
            \_ ->
                Expect.within tolerance segmentX paramX
        , test "y position is withing tolerance" <|
            \_ ->
                Expect.within tolerance segmentY paramY
        ]
