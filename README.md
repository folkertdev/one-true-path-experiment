# One True Path 

A general-purpose library for working with curves and paths. 

The primary aim is SVG paths, but the types and functions in this package can also be 
used for animations or other graphics backends (webgl, canvas). 

Additionally, this package is meant to serve as an interchange format between packages. 

## Core Concepts 

* **Path:** A list of subpaths
* **SubPath:** A move command followed by a list of draw commands
* **Command:** A lowlevel instruction 
    - `MoveTo` moves the cursor
    - `DrawTo` draws a curve 
* **Segment:** A set of four drawing primitives, useful for mathematical operations


### Composition

Lines can be composed in two "obvious" ways: concatenation and layering. 

concatenation happens on the subpath level, using the functions


<img style="max-width: 100%;" src="https://rawgit.com/folkertdev/one-true-path-experiment/master/docs/subpath-composition.svg" /> 

* `connect:` draws a straight line connecting two subpaths (end to start)
* `continue:` make the start and end point of two subpaths coincide 
* `continueSmooth:` make the start and end point of two subpaths coincide, and rotate to make the transition smooth.

`SubPath`s can be layered by putting them in a list, forming a `Path`.

### Using this package 

Ideally, you can use the drawing functions in `Curve` and use the composition in `SubPath` and `Path`, then use either `SubPath.element` or `Path.element` to create svg. 

```elm
import SubPath exposing SubPath
import Curve
import Svg exposing (Svg)
import Svg.Attributes exposing (width, height, viewBox, fill, stroke)

hShape : SubPath 
hShape =
    Curve.linearClosed 
        [ ( 0.3, 0.2 )
        , ( 0.4, 0.2 )
        , ( 0.4, 0.45 )
        , ( 0.6, 0.45 )
        , ( 0.6, 0.2 )
        , ( 0.7, 0.2 )
        , ( 0.7, 0.8 )
        , ( 0.6, 0.8 )
        , ( 0.6, 0.55 )
        , ( 0.4, 0.55 )
        , ( 0.4, 0.8 )
        , ( 0.3, 0.8 )
        ]

logo : Svg msg 
logo = 
    Svg.svg [ width "50", height "50", viewBox "0 0 1 1" ] 
        [ SubPath.element hShape [ fill "none", stroke "black" ] ] 
```

Existing svg path strings can be parsed into a nice elm data structure

```elm
import Path 
import SubPath

pathAsString = 
    """
  M 213.1,6.7
  c -32.4-14.4-73.7,0-88.1,30.6
  C 110.6,4.9,67.5-9.5,36.9,6.7
  C 2.8,22.9-13.4,62.4,13.5,110.9
  C 33.3,145.1,67.5,170.3,125,217
  c 59.3-46.7,93.5-71.9,111.5-106.1
  C 263.4,64.2,247.2,22.9,213.1,6.7
  z
    """

myPath = 
    SubPath.subpath (MoveTo ( 213.1, 6.7 ))
        [ CurveTo
            [ ( ( 110.6, 4.9 ), ( 67.5, -9.5 ), ( 36.9, 6.7 ) )
            , ( ( 4.5, -7.7 ), ( -36.800000000000004, 6.7 ), ( -51.199999999999996, 37.300000000000004 ) )
            , ( ( 110.6, 4.9 ), ( 67.5, -9.5 ), ( 36.9, 6.7 ) )
            , ( ( 2.8, 22.9 ), ( -13.4, 62.4 ), ( 13.5, 110.9 ) )
            , ( ( 33.3, 145.1 ), ( 67.5, 170.3 ), ( 125, 217 ) )
            , ( ( 184.3, 170.3 ), ( 218.5, 145.1 ), ( 236.5, 110.9 ) )
            , ( ( 263.4, 64.2 ), ( 247.2, 22.9 ), ( 213.1, 6.7 ) )
            ]
        , ClosePath
        ]

Path.parse pathAsString
    |> Result.toMaybe
    |> Maybe.andThen List.head
    |> Maybe.withDefault SubPath.empty
    |> SubPath.compress
    --> myPath
```

The `Segment` module breaks down a line into four basic segment types, and exposes some mathematical functions (and the constructors, if you want to define your own fancy stuff). 

The `LowLevel.Command` module contains individual instructions. These should only be used for building other primitives! Making and combining curves should happen on the SubPath level.

## What about styling

That's not part of this package, but I'm looking into it. The julia [Compose.jl](https://github.com/GiovineItalia/Compose.jl) library has some interesting ideas. 
