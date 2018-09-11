# v4 changelog 

* remove `VerticalTo` and `HorizontalTo` constructors from `DrawTo`. 
    Relative instructions were removed early on in the development process, but these were kept. They are still relative in one coordinate. 
    With them gone, every `DrawTo` completely specifies its endpoint, making conversion to `Segment` easier.

    The conversion from `svg-path-lowlevel` to a `DrawTo` converts horizontal and vertical into equivalent `LineTo`s

* use `OpenSolid.EllipticalArc2d.EllipticalArc2d` for arcs in `Segment`.

* add `ArcLengthParameterization` in cooperation with Ian Mackenzie

* add `toStringWith` and `Option`: formatting options for the svg string output. 

* rename `Segment.arc` to `Segment.ellipticalArc`
* rename `SubPath.subpath` to `SubPath.with`

Fixed bugs

* Make curve functions tail-recursive (thanks @mitchellwrosen for the PR)
* The end point for cardinal curves was calculated incorrectly


Here is a diff of the api in 0.18 (diffing across versions doesn't work). 
The rename `arc -> ellipticalArc` was added later and isn't in here yet.

``` 
Comparing folkertdev/one-true-path-experiment 3.0.2 to local changes...
This is a MAJOR change.

------ Changes to module LowLevel.Command - MAJOR ------

    Removed:
        horizontalTo : List Float -> LowLevel.Command.DrawTo
        verticalTo : List Float -> LowLevel.Command.DrawTo

    Changed:
      - type DrawTo
            = ClosePath
            | CurveTo (List (Vector2.Vec2 Float, Vector2.Vec2 Float, Vector2.Vec2 Float))
            | EllipticalArc (List LowLevel.Command.EllipticalArcArgument)
            | Horizontal (List Float)
            | LineTo (List (Vector2.Vec2 Float))
            | QuadraticBezierCurveTo (List (Vector2.Vec2 Float, Vector2.Vec2 Float))
            | Vertical (List Float)
      + type DrawTo
            = ClosePath
            | CurveTo (List (Vector2.Vec2 Float, Vector2.Vec2 Float, Vector2.Vec2 Float))
            | EllipticalArc (List LowLevel.Command.EllipticalArcArgument)
            | LineTo (List (Vector2.Vec2 Float))
            | QuadraticBezierCurveTo (List (Vector2.Vec2 Float, Vector2.Vec2 Float))



------ Changes to module Segment - MAJOR ------

    Added:
        type ArcLengthParameterized
        arcLength : Segment.ArcLengthParameterized -> Float
        arcLengthParameterized : Float -> Segment.Segment -> Segment.ArcLengthParameterized
        arcLengthToParameterValue : Segment.ArcLengthParameterized -> Float -> Maybe.Maybe Float
        parameterValueToArcLength : Segment.ArcLengthParameterized -> Float -> Maybe.Maybe Float
        pointAlong : Segment.ArcLengthParameterized -> Float -> Maybe.Maybe (Float, Float)
        tangentAlong : Segment.ArcLengthParameterized -> Float -> Maybe.Maybe (Float, Float)

    Changed:
      - type Segment
            = Arc Geometry.Ellipse.EndpointParameterization
            | Cubic OpenSolid.CubicSpline2d.CubicSpline2d
            | LineSegment OpenSolid.LineSegment2d.LineSegment2d
            | Quadratic OpenSolid.QuadraticSpline2d.QuadraticSpline2d
      + type Segment
            = Arc OpenSolid.EllipticalArc2d.EllipticalArc2d
            | Cubic OpenSolid.CubicSpline2d.CubicSpline2d
            | LineSegment OpenSolid.LineSegment2d.LineSegment2d
            | Quadratic OpenSolid.QuadraticSpline2d.QuadraticSpline2d



------ Changes to module SubPath - MINOR ------

    Added:
        type ArcLengthParameterized
        type Option
        arcLength : SubPath.ArcLengthParameterized -> Float
        arcLengthParameterized : Float -> SubPath.SubPath -> SubPath.ArcLengthParameterized
        arcLengthToParameterValue : SubPath.ArcLengthParameterized -> Float -> Maybe.Maybe Float
        decimalPlaces : Int -> SubPath.Option
        evenlySpaced : Int -> SubPath.ArcLengthParameterized -> List Float
        evenlySpacedPoints : Int -> SubPath.ArcLengthParameterized -> List (Float, Float)
        evenlySpacedWithEndpoints : Int -> SubPath.ArcLengthParameterized -> List Float
        mergeAdjacent : SubPath.Option
        parameterValueToArcLength : SubPath.ArcLengthParameterized -> Float -> Maybe.Maybe Float
        pointAlong : SubPath.ArcLengthParameterized -> Float -> Maybe.Maybe (Float, Float)
        tangentAlong : SubPath.ArcLengthParameterized -> Float -> Maybe.Maybe (Float, Float)
        toStringWith : List SubPath.Option -> SubPath.SubPath -> String
```
