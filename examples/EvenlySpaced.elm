module EvenlySpaced exposing (..)

import Svg
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Svg.Attributes exposing (width, height, fill, stroke)
import Html.Attributes
import CurveImages exposing (points, gridRect, svgGrid, nodes)
import Curve
import SubPath
import Path
import Geometry.Ellipse exposing (signedAngle)
import AnimationFrame
import Time exposing (Time)


mySubPath =
    Curve.catmullRom 0.5 points


parameterized =
    mySubPath
        |> SubPath.arcLengthParameterized 1.0e-2


type alias Model =
    { count : Int, offset : Float }


type Msg
    = Count String
    | Frame Time


floatModulo larger smaller =
    larger - smaller * toFloat (floor (larger / smaller))


update msg model =
    case msg of
        Count n ->
            ( { model | count = String.toInt n |> Result.withDefault model.count }
            , Cmd.none
            )

        Frame time ->
            ( { model
                | offset =
                    let
                        new =
                            model.offset + time / 20

                        size =
                            segmentLength model.count
                    in
                        floatModulo new size
              }
            , Cmd.none
            )


main =
    Html.program
        { view = view
        , update = update
        , init = ( { count = 20, offset = 0 }, Cmd.none )
        , subscriptions = \_ -> AnimationFrame.diffs Frame
        }


segmentLength count =
    SubPath.arcLength parameterized / toFloat (count - 1)


arrowHead location tangent =
    let
        line =
            Curve.linear [ ( 0, 0 ), ( 30, 0 ) ]
                |> SubPath.translate location

        angle =
            signedAngle ( 1, 0 ) tangent

        a =
            SubPath.rotate (angle - (pi + pi / 4)) line

        b =
            SubPath.rotate (angle + (pi + pi / 4)) line
    in
        SubPath.reverse a
            |> SubPath.continue b


view : Model -> Html.Html Msg
view { count, offset } =
    let
        ( locations, tangents ) =
            SubPath.evenlySpacedWithEndpoints count parameterized
                |> List.map (\distance -> offset + distance)
                |> List.filterMap (\d -> Maybe.map2 (,) (SubPath.pointAlong parameterized d) (SubPath.tangentAlong parameterized d))
                |> List.unzip

        arrowHeads =
            List.map2 arrowHead locations tangents
                |> flip Path.element [ fill "none", stroke "black" ]
    in
        Html.div
            []
            [ CurveImages.grid { width = 1000, height = 400 }
                []
                (SubPath.element mySubPath [ fill "none", stroke "black" ]
                    :: arrowHeads
                    :: (nodes "black" points ++ nodes "red" locations)
                )
            , Html.input [ Attributes.type_ "range", Attributes.min "0", Attributes.max "20", Events.onInput Count ] []
            ]
