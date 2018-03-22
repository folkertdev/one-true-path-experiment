module Main exposing (main)

import Curve
import Html.Attributes exposing (attribute)
import LowLevel.Command exposing (..)
import OpenSolid.Direction2d as Direction2d
import OpenSolid.EllipticalArc2d as EllipticalArc2d
import OpenSolid.Point2d as Point2d
import SubPath exposing (SubPath)
import Svg
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, transform, width, x, y)
import Vector2 as Vec2


epsilon : Float
epsilon =
    0.2


main =
    Svg.svg [ width "1000", height "1000" ]
        [ Svg.g [ transform "translate(300, 300)" ]
            [ myCircle
                |> flip SubPath.element [ strokeWidth "2", fill "none", stroke "black" ]
            , simplified
                |> Curve.linear
                |> flip SubPath.element [ strokeWidth "2", fill "none", stroke "black" ]
            , Svg.g [] (List.indexedMap renderLabel labelPositions)
            ]
        ]


renderCircle ( x, y ) =
    Svg.circle [ cx (toString x), cy (toString y), r "10", fill "red" ] []


renderLabel i ( xx, yy ) =
    Svg.text_
        [ Svg.Attributes.x (toString xx)
        , Svg.Attributes.y (toString yy)
        , attribute "text-anchor" "middle"
        , attribute "alignment-baseline" "middle"
        ]
        [ Svg.text (toString i) ]


circle : SubPath
circle =
    SubPath.subpath (moveTo ( 0, 1 ))
        [ EllipticalArc
            [ { target = ( 0, -1 ), radii = ( 1, 1 ), xAxisRotate = 0, direction = clockwise, arcFlag = largestArc }
            , { target = ( 0, 1 ), radii = ( 1, 1 ), xAxisRotate = 0, direction = clockwise, arcFlag = largestArc }
            ]
        ]



{-
   EllipticalArc2d { ellipse = Ellipse2d { axes = Frame2d { originPoint = Point2d (0,0), xDirection = Direction2d (1,0), yDirection = Direction2d (0,1) }, xRadius = 1, yRadius = 1 }, startAngle = -4.71238898038469, sweptAngle = 3.141592653589793 }

-}


myArc =
    EllipticalArc2d.with
        { xDirection = Direction2d.x
        , xRadius = 1
        , yRadius = 1
        , centerPoint = Point2d.fromCoordinates ( 0, 0 )
        , startAngle = -4.71238898038469
        , sweptAngle = pi
        }


myCircle =
    circle
        |> SubPath.scale ( 100, 100 )
        |> Debug.log "positions"


parameterized =
    myCircle
        |> SubPath.arcLengthParameterized epsilon


displace parameterized t =
    let
        tangent =
            SubPath.tangentAlong parameterized t
                |> Maybe.withDefault ( 0, 0 )
                |> Vec2.normalize
                |> (\( x, y ) -> ( y, -x ))
                |> Vec2.scale 20
                |> Debug.log "tangent"
    in
    SubPath.pointAlong parameterized t
        |> Maybe.withDefault ( 0, 0 )
        |> Vec2.add tangent


simplified =
    SubPath.evenlySpacedWithEndpoints 13 parameterized
        |> List.take 12
        |> List.filterMap (\t -> SubPath.pointAlong parameterized t)


labelPositions =
    SubPath.evenlySpacedWithEndpoints 13 parameterized
        |> List.drop 1
        |> List.map (displace parameterized)


x =
    labelPositions
        |> Debug.log "positions segment"
