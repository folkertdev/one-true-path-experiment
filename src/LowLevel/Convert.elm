module LowLevel.Convert exposing (..)

{-| Conversion between the exposed absolute commands and internal mixed commands.

This is in a separate module to circumvent cyclic dependencies.

-}

import LowLevel.Command as Command exposing (..)
import Path.LowLevel as LowLevel


{-| Exposed for testing purposes
-}
toMixedMoveTo : MoveTo -> LowLevel.MoveTo
toMixedMoveTo (MoveTo coordinates) =
    LowLevel.MoveTo LowLevel.Absolute coordinates


{-| Exposed for testing purposes
-}
toMixedDrawTo : DrawTo -> LowLevel.DrawTo
toMixedDrawTo drawto =
    case drawto of
        LineTo coordinates ->
            LowLevel.LineTo LowLevel.Absolute coordinates

        Horizontal coordinates ->
            LowLevel.Horizontal LowLevel.Absolute coordinates

        Vertical coordinates ->
            LowLevel.Vertical LowLevel.Absolute coordinates

        CurveTo coordinates ->
            LowLevel.CurveTo LowLevel.Absolute coordinates

        SmoothCurveTo coordinates ->
            LowLevel.SmoothCurveTo LowLevel.Absolute coordinates

        QuadraticBezierCurveTo coordinates ->
            LowLevel.QuadraticBezierCurveTo LowLevel.Absolute coordinates

        SmoothQuadraticBezierCurveTo coordinates ->
            LowLevel.SmoothQuadraticBezierCurveTo LowLevel.Absolute coordinates

        EllipticalArc arguments ->
            LowLevel.EllipticalArc LowLevel.Absolute arguments

        ClosePath ->
            LowLevel.ClosePath


fromMixedMoveTo : LowLevel.MoveTo -> MoveTo
fromMixedMoveTo (LowLevel.MoveTo _ coordinate) =
    MoveTo coordinate


fromMixedDrawTo : LowLevel.AbstractDrawTo () -> DrawTo
fromMixedDrawTo drawto =
    case drawto of
        LowLevel.LineTo _ arg ->
            LineTo arg

        LowLevel.Horizontal _ arg ->
            Horizontal arg

        LowLevel.Vertical _ arg ->
            Vertical arg

        LowLevel.CurveTo _ arg ->
            CurveTo arg

        LowLevel.SmoothCurveTo _ arg ->
            SmoothCurveTo arg

        LowLevel.QuadraticBezierCurveTo _ arg ->
            QuadraticBezierCurveTo arg

        LowLevel.SmoothQuadraticBezierCurveTo _ arg ->
            SmoothQuadraticBezierCurveTo arg

        LowLevel.EllipticalArc _ arg ->
            EllipticalArc arg

        LowLevel.ClosePath ->
            ClosePath
