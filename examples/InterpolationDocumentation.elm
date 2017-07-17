module InterpolationDocumentation exposing (..)

import Svg
import Svg.Attributes exposing (..)
import Html exposing (..)
import Html.Attributes
import Path exposing (subpath, moveTo, lineTo, closePath)
import Curve exposing (..)
import Color
import Color.Interpolate as Color exposing (Space(LAB))
import Color.Convert as Color
import Color.Manipulate as Color


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
            [ Path.svgPath p [ fill "none", stroke "white", strokeWidth "1" ] ]


grid dim =
    let
        p =
            [ subpath (moveTo ( dim, 0 )) [ lineTo [ ( 0, 0 ), ( 0, dim ) ] ] ]
    in
        Svg.pattern [ id "grid", width (Basics.toString dim), height (Basics.toString dim), patternUnits "userSpaceOnUse" ]
            [ Svg.rect [ width (Basics.toString dim), height (Basics.toString dim), fill "url(#smallGrid)" ] []
            , Path.svgPath p [ fill "none", stroke "white", strokeWidth "1.5" ]
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
            Path.svgPath (toPath value points)
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
        Svg.svg [ width "1000", height "400", Html.Attributes.style [ ( "background-color", "#efefef" ) ] ] (svgGrid :: gridRect :: (labels ++ paths ++ nodes points))


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
            Path.svgPath (toPath points)
                [ stroke "black"
                , strokeWidth "2"
                , fill "none"
                ]

        label =
            Svg.text_ [ x "25", y "30", fontSize "20px", fontWeight "bolder", fontFamily "Verdana, sans-serif" ] [ Svg.text name ]

        gridRect =
            Svg.rect [ width "100%", height "100%", fill "url(#grid)" ] []
    in
        Svg.svg [ width "1000", height "400", Html.Attributes.style [ ( "background-color", "#efefef" ) ] ] (svgGrid :: gridRect :: label :: path :: nodes points)


main =
    Html.div []
        [ diagram "linear" linear points
        , diagram "linear closed" linearClosed points
        , diagram "step" (step 0.5) points
        , diagram "step before" (step 0) points
        , diagram "step after" (step 1) points
        , diagram "basis" basis points
        , diagram "basis closed" basisClosed points
        , diagram "basis open" basisOpen points
        , stacked "bundle" bundle points
        , stacked "cardinal" cardinal points
        , stacked "cardinal closed" cardinalClosed points
        , stacked "cardinal open" cardinalOpen points
        , stacked "catmull rom" catmullRom points
        , stacked "catmull rom closed" catmullRomClosed points
        , stacked "catmull rom open" catmullRomOpen points
        , diagram "monotoneX" monotoneX points
        , diagram "monotoneY" monotoneY monotoneYPoints
        , diagram "radial" natural (toPolarWithCenter ( 500, 200 ) radialPoints)
        , diagram "natural" natural points
        ]
