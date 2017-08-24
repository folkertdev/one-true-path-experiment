module LowLevel.Convert exposing (..)

{-| Conversion between the exposed absolute commands and internal mixed commands.

This is in a separate module to circumvent cyclic dependencies.
-}

import LowLevel.MixedCommand as MixedCommand exposing (AbstractDrawTo, AbstractMoveTo)
import LowLevel.Command as Command exposing (..)


{-| Exposed for testing purposes
-}
toMixedMoveTo : MoveTo -> MixedCommand.MoveTo
toMixedMoveTo (MoveTo coordinates) =
    MixedCommand.MoveTo MixedCommand.Absolute coordinates


{-| Exposed for testing purposes
-}
toMixedDrawTo : DrawTo -> MixedCommand.DrawTo
toMixedDrawTo drawto =
    case drawto of
        LineTo coordinates ->
            MixedCommand.LineTo MixedCommand.Absolute coordinates

        Horizontal coordinates ->
            MixedCommand.Horizontal MixedCommand.Absolute coordinates

        Vertical coordinates ->
            MixedCommand.Vertical MixedCommand.Absolute coordinates

        CurveTo coordinates ->
            MixedCommand.CurveTo MixedCommand.Absolute coordinates

        SmoothCurveTo coordinates ->
            MixedCommand.SmoothCurveTo MixedCommand.Absolute coordinates

        QuadraticBezierCurveTo coordinates ->
            MixedCommand.QuadraticBezierCurveTo MixedCommand.Absolute coordinates

        SmoothQuadraticBezierCurveTo coordinates ->
            MixedCommand.SmoothQuadraticBezierCurveTo MixedCommand.Absolute coordinates

        EllipticalArc arguments ->
            MixedCommand.EllipticalArc MixedCommand.Absolute arguments

        ClosePath ->
            MixedCommand.ClosePath


fromMixedMoveTo : AbstractMoveTo () -> MoveTo
fromMixedMoveTo (MixedCommand.MoveTo _ coordinate) =
    MoveTo coordinate


fromMixedDrawTo : MixedCommand.AbstractDrawTo () -> DrawTo
fromMixedDrawTo drawto =
    case drawto of
        MixedCommand.LineTo _ arg ->
            LineTo arg

        MixedCommand.Horizontal _ arg ->
            Horizontal arg

        MixedCommand.Vertical _ arg ->
            Vertical arg

        MixedCommand.CurveTo _ arg ->
            CurveTo arg

        MixedCommand.SmoothCurveTo _ arg ->
            SmoothCurveTo arg

        MixedCommand.QuadraticBezierCurveTo _ arg ->
            QuadraticBezierCurveTo arg

        MixedCommand.SmoothQuadraticBezierCurveTo _ arg ->
            SmoothQuadraticBezierCurveTo arg

        MixedCommand.EllipticalArc _ arg ->
            EllipticalArc arg

        MixedCommand.ClosePath ->
            ClosePath
