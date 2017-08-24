module CurveImages exposing (..)

{-| Defines small views for the different curves. These views are rendered to svg files with elm-static-html
and then used in the documentations
-}

import Svg
import Svg.Attributes exposing (..)
import Html.Attributes
import Html
import Path
import SubPath exposing (subpath)
import LowLevel.Command as LowLevel exposing (moveTo, lineTo, closePath)
import Curve
import Color
import Color.Interpolate as Color exposing (Space(LAB))
import Color.Convert as Color
import Color.Manipulate as Color


composition : Svg.Svg msg
composition =
    let
        right =
            Curve.quadraticBezier ( 0, 0 ) [ ( ( 0.5, -0.5 ), ( 1.0, 0 ) ) ]
                |> SubPath.translate ( 50, 100 )
                |> SubPath.scale ( 100, 100 )

        down =
            Curve.linear [ ( 0, 0 ), ( 0, 1 ) ]
                |> SubPath.translate ( 250, 100 )
                |> SubPath.scale ( 100, 100 )

        start =
            let
                subpaths =
                    [ right
                    , down
                    ]
            in
                render "" subpaths

        connect =
            let
                subpaths =
                    [ right
                        |> SubPath.connect down
                    ]
            in
                render "Connect" subpaths

        continue =
            let
                subpaths =
                    [ right
                        |> SubPath.continue down
                    ]
            in
                render "Continue" subpaths

        continueSmooth =
            let
                subpaths =
                    [ right
                        |> SubPath.continueSmooth down
                    ]
            in
                render "Continue Smooth" subpaths

        render name subpaths =
            let
                path =
                    Path.element subpaths
                        [ stroke "black"
                        , strokeWidth "2"
                        , fill "none"
                        ]

                label =
                    Svg.text_ [ x "25", y "30", fontSize "20px", fontWeight "bolder", fontFamily "Verdana, sans-serif" ] [ Svg.text name ]
            in
                Svg.g
                    [ width "300"
                    , height "300"
                    ]
                    (label :: path :: [])

        gridRect =
            Svg.rect [ width "100%", height "100%", fill "url(#grid)" ] []
    in
        Svg.svg
            [ width "1200"
            , height "300"
            , Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg"
            , Html.Attributes.style [ ( "background-color", "#efefef" ) ]
            ]
            [ svgGrid
            , gridRect
            , start
            , Svg.g [ transform "translate(300, 0)" ] [ connect ]
            , Svg.g [ transform "translate(600, 0)" ] [ continue ]
            , Svg.g [ transform "translate(900, 0)" ] [ continueSmooth ]
            ]


(=>) =
    (,)


points =
    [ 50 => 0
    , 150 => 150
    , 225 => 200
    , 275 => 200
    , 350 => 50
    , 500 => 50
    , 600 => 150
    , 725 => 100
    , 825 => 50
    , 900 => 75
    , 975 => 0
    ]
        |> List.map (\( x, y ) -> ( x, 300 - y ))


monotoneYPoints =
    [ 50 => 0
    , 350 => 0
    , 425 => 50
    , 425 => 100
    , 375 => 150
    , 575 => 200
    , 975 => 200
    ]
        |> List.map (\( x, y ) -> ( x, 300 - y ))


points2 =
    [ ( 0, 85 ), ( 120, 45 ), ( 150, 89 ), ( 167, 42 ), ( 180, 23 ), ( 190, 23 ), ( 200, 89 ), ( 230, 200 ) ]
        |> List.map (\( x, y ) -> ( x * 2, y * 2 ))


smallGrid dim =
    let
        p =
            [ subpath (moveTo ( dim, 0 )) [ lineTo [ ( 0, 0 ), ( 0, dim ) ] ] ]
    in
        Svg.pattern [ id "smallGrid", width (Basics.toString dim), height (Basics.toString dim), patternUnits "userSpaceOnUse" ]
            [ Path.element p [ fill "none", stroke "white", strokeWidth "1" ] ]


grid dim =
    let
        p =
            [ subpath (moveTo ( dim, 0 )) [ lineTo [ ( 0, 0 ), ( 0, dim ) ] ] ]
    in
        Svg.pattern [ id "grid", width (Basics.toString dim), height (Basics.toString dim), patternUnits "userSpaceOnUse" ]
            [ Svg.rect [ width (Basics.toString dim), height (Basics.toString dim), fill "url(#smallGrid)" ] []
            , Path.element p [ fill "none", stroke "white", strokeWidth "1.5" ]
            ]


tau =
    2 * pi


radialPoints =
    [ ( 0, 80 )
    , ( pi * 0.25, 80 )
    , ( pi * 0.5, 30 )
    , ( pi * 0.75, 80 )
    , ( pi, 80 )
    , ( pi * 1.25, 80 )
    , ( pi * 1.5, 80 )
    , ( pi * 1.75, 80 )
    , ( pi * 2, 80 )
    ]


colorAt value =
    Color.interpolate LAB (Color.rgb 255 192 203) Color.purple value |> Color.saturate 0.5


stacked name toPath points =
    let
        values =
            [ 0, 0.25, 0.5, 0.75, 1 ]

        path value =
            Path.element (List.singleton <| toPath value points)
                [ stroke (colorAt value |> Color.colorToHex)
                , strokeWidth "2"
                , fill "none"
                ]

        paths =
            List.map path values

        gridRect =
            Svg.rect [ width "100%", height "100%", fill "url(#grid)" ] []

        label i value =
            Svg.text_
                [ x "25"
                , y (Basics.toString <| 30 + 22 * i)
                , fontSize "20px"
                , fontWeight "bolder"
                , fontFamily "Verdana, sans-serif"
                , fill (colorAt value |> Color.colorToHex)
                ]
                [ Svg.text (name ++ " " ++ Basics.toString value) ]

        labels =
            List.indexedMap label values
    in
        Svg.svg
            [ width "1000"
            , Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg"
            , height "400"
            , Html.Attributes.style [ ( "background-color", "#efefef" ) ]
            ]
            (svgGrid :: gridRect :: (labels ++ paths ++ nodes points))


nodes points =
    List.map
        (\( xx, yy ) ->
            Svg.circle [ fill "white", stroke "black", strokeWidth "2", cx (Basics.toString xx), cy (Basics.toString (yy)), r "4" ] []
        )
        points


svgGrid =
    Svg.defs []
        [ smallGrid 25
        , grid 50
        ]


diagram name toPath points =
    let
        path =
            Path.element (List.singleton <| toPath points)
                [ stroke "black"
                , strokeWidth "2"
                , fill "none"
                ]

        label =
            Svg.text_ [ x "25", y "30", fontSize "20px", fontWeight "bolder", fontFamily "Verdana, sans-serif" ] [ Svg.text name ]

        gridRect =
            Svg.rect [ width "100%", height "100%", fill "url(#grid)" ] []
    in
        Svg.svg
            [ width "1000"
            , height "400"
            , Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg"
            , Html.Attributes.style [ ( "background-color", "#efefef" ) ]
            ]
            (svgGrid :: gridRect :: label :: path :: nodes points)


linear =
    diagram "linear" Curve.linear points


linearClosed =
    diagram "linear closed" Curve.linearClosed points


step =
    diagram "step 0.5" (Curve.step 0.5) points


stepBefore =
    diagram "step before" Curve.stepBefore points


stepAfter =
    diagram "step after" Curve.stepAfter points


basis =
    diagram "basis" Curve.basis points


basisClosed =
    diagram "basis closed" Curve.basisClosed points


basisOpen =
    diagram "basis open" Curve.basisOpen points


bundle =
    stacked "bundle" Curve.bundle points


cardinal =
    stacked "cardinal" Curve.cardinal points


cardinalClosed =
    stacked "cardinal closed" Curve.cardinalClosed points


cardinalOpen =
    stacked "cardinal open" Curve.cardinalOpen points


catmullRom =
    stacked "catmull rom" Curve.catmullRom points


catmullRomClosed =
    stacked "catmull rom closed" Curve.catmullRomClosed points


catmullRomOpen =
    stacked "catmull rom open" Curve.catmullRomOpen points


monotoneX =
    diagram "monotoneX" Curve.monotoneX points


monotoneY =
    diagram "monotoneY" Curve.monotoneY monotoneYPoints


radial =
    diagram "radial" Curve.linear (Curve.toPolarWithCenter ( 500, 200 ) radialPoints)


natural =
    diagram "natural" Curve.natural points
