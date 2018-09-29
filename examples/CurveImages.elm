module CurveImages exposing (basis, basisClosed, basisClosedReversed, basisOpen, bundle, cardinal, cardinalClosed, cardinalOpen, catmullRom, catmullRomClosed, catmullRomOpen, colorAt, composition, diagram, grid, gridRect, largeGrid, linear, linearClosed, main, monotoneX, monotoneY, monotoneYPoints, natural, nodes, points2, radial, radialPoints, smallGrid, stacked, step, stepAfter, stepBefore, svgGrid, tau)

{-| Defines small views for the different curves. These views are rendered to svg files with elm-static-html
and then used in the documentations
-}

{-
   import Color.Convert as Color
   import Color.Interpolate as Color exposing (Space(..))
   import Color.Manipulate as Color
-}

import Color
import Curve
import Html
import Html.Attributes
import Path
import SubPath exposing (SubPath, with)
import Svg exposing (Attribute, Svg)
import Svg.Attributes exposing (..)


subpath =
    with


main =
    Html.div []
        [ linear
        , linearClosed

        -- , simplified
        , step
        , stepBefore
        , stepAfter
        , basis
        , basisClosed
        , basisOpen
        , bundle
        , cardinal
        , cardinalClosed
        , cardinalOpen
        , catmullRom
        , catmullRomClosed
        , catmullRomOpen
        , monotoneX
        , monotoneY
        , radial
        , natural
        , natural2
        ]


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
    in
    grid { width = 1200, height = 300 }
        []
        [ start
        , Svg.g [ transform "translate(300, 0)" ] [ connect ]
        , Svg.g [ transform "translate(600, 0)" ] [ continue ]
        , Svg.g [ transform "translate(900, 0)" ] [ continueSmooth ]
        ]


testPoints =
    [ ( 50, 0 )
    , ( 150, 150 )
    , ( 225, 200 )
    , ( 275, 200 )
    , ( 350, 50 )
    , ( 500, 50 )
    , ( 600, 150 )
    , ( 725, 100 )
    , ( 825, 50 )
    , ( 900, 75 )
    , ( 975, 0 )
    ]
        |> List.map (\( x, y ) -> ( x, 300 - y ))


monotoneYPoints =
    [ ( 50, 0 )
    , ( 350, 0 )
    , ( 425, 50 )
    , ( 425, 100 )
    , ( 375, 150 )
    , ( 575, 200 )
    , ( 975, 200 )
    ]
        |> List.map (\( x, y ) -> ( x, 300 - y ))


points2 =
    [ ( 0, 85 ), ( 120, 45 ), ( 150, 89 ), ( 167, 42 ), ( 180, 23 ), ( 190, 23 ), ( 200, 89 ), ( 230, 200 ) ]
        |> List.map (\( x, y ) -> ( x * 2, y * 2 ))


smallGrid dim =
    let
        p =
            Curve.linear [ ( dim, 0 ), ( 0, 0 ), ( 0, dim ) ]
    in
    Svg.pattern [ id "smallGrid", width (String.fromFloat dim), height (String.fromFloat dim), patternUnits "userSpaceOnUse" ]
        [ SubPath.element p [ fill "none", stroke "white", strokeWidth "1" ] ]


largeGrid dim =
    let
        p =
            Curve.linear [ ( dim, 0 ), ( 0, 0 ), ( 0, dim ) ]
    in
    Svg.pattern [ id "grid", width (String.fromFloat dim), height (String.fromFloat dim), patternUnits "userSpaceOnUse" ]
        [ Svg.rect [ width (String.fromFloat dim), height (String.fromFloat dim), fill "url(#smallGrid)" ] []
        , SubPath.element p [ fill "none", stroke "white", strokeWidth "1.5" ]
        ]


gridRect =
    Svg.rect [ width "100%", height "100%", fill "url(#grid)" ] []


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
    -- Color.interpolate LAB (Color.rgb 255 192 203) Color.purple value |> Color.saturate 0.5
    Color.rgb 255 102 203



{-
   simplified =
       stacked "simplified"
           (\factor points ->
               testPoints
                   |> Simplify.simplify factor
                   |> Debug.log "in simplified"
                   |> Curve.linear
           )
           points
-}


stacked : String -> (Float -> List ( Float, Float ) -> SubPath) -> List ( Float, Float ) -> Svg msg
stacked name toPath points =
    let
        values =
            [ 0, 0.25, 0.5, 0.75, 1 ]

        path value =
            SubPath.element (toPath value points)
                [ stroke (colorAt value |> Color.toCssString)
                , strokeWidth "2"
                , fill "none"
                ]

        paths =
            List.map path values

        label i value =
            Svg.text_
                [ x "25"
                , y (String.fromInt <| 30 + 22 * i)
                , fontSize "20px"
                , fontWeight "bolder"
                , fontFamily "Verdana, sans-serif"
                , fill (colorAt value |> Color.toCssString)
                ]
                [ Svg.text (name ++ " " ++ String.fromFloat value) ]

        labels =
            List.indexedMap label values
    in
    grid { width = 1000, height = 400 }
        []
        (labels ++ paths ++ nodes "black" points)


nodes strokeColor points =
    List.map
        (\( xx, yy ) ->
            Svg.circle [ fill "white", stroke strokeColor, strokeWidth "2", cx (String.fromFloat xx), cy (String.fromFloat yy), r "4" ] []
        )
        points


svgGrid =
    Svg.defs []
        [ smallGrid 25
        , largeGrid 50
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
    in
    grid { width = 1000, height = 400 } [] (label :: path :: nodes "black" points)


grid : { width : Float, height : Float } -> List (Attribute msg) -> List (Svg msg) -> Svg msg
grid canvas attributes children =
    Svg.svg
        [ width (String.fromFloat canvas.width)
        , height (String.fromFloat canvas.height)
        , Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg"
        , Html.Attributes.style "background-color" "#efefef"
        ]
        [ svgGrid, gridRect, Svg.g attributes children ]


linear =
    diagram "linear" Curve.linear testPoints


linearClosed =
    diagram "linear closed" Curve.linearClosed testPoints


step =
    diagram "step 0.5" (Curve.step 0.5) testPoints


stepBefore =
    diagram "step before" Curve.stepBefore testPoints


stepAfter =
    diagram "step after" Curve.stepAfter testPoints


basis =
    diagram "basis" Curve.basis testPoints


basisClosedReversed =
    let
        function =
            List.reverse
                >> Curve.basisClosed
                >> SubPath.reverse
                >> SubPath.compress
    in
    diagram "basis closed reversed" function testPoints


basisClosed =
    diagram "basis closed" Curve.basisClosed testPoints


basisOpen =
    diagram "basis open" Curve.basisOpen testPoints


bundle =
    stacked "bundle" Curve.bundle testPoints


cardinal =
    stacked "cardinal" Curve.cardinal testPoints


cardinalClosed =
    stacked "cardinal closed" Curve.cardinalClosed testPoints


cardinalOpen =
    stacked "cardinal open" Curve.cardinalOpen testPoints


catmullRom =
    stacked "catmull rom" Curve.catmullRom testPoints


catmullRomClosed =
    stacked "catmull rom closed" Curve.catmullRomClosed testPoints


catmullRomOpen =
    stacked "catmull rom open" Curve.catmullRomOpen testPoints


monotoneX =
    diagram "monotoneX" Curve.monotoneX testPoints


monotoneY =
    diagram "monotoneY" Curve.monotoneY monotoneYPoints


radial =
    diagram "radial" Curve.linear (Curve.toPolarWithCenter ( 500, 200 ) radialPoints)


natural =
    diagram "natural" Curve.natural testPoints


natural2 =
    let
        points =
            [ ( 94.5, 365 ), ( 139, 190 ), ( 205.75, 295 ), ( 250.25, 295 ), ( 317, 330 ), ( 450.5, 120 ), ( 584, 190 ), ( 717.5, 85 ), ( 806.5, 330 ), ( 895.5, 365 ) ]
    in
    diagram "natural 2" Curve.natural points
