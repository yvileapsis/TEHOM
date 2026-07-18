namespace SlugDemo
open System.Numerics

open Nu
open Prime

[<RequireQualifiedAccess>]
module SlugDemoMorphing =

    let private sharedBounds =
        { Min = v2 -0.5f -0.35f
          Max = v2 0.5f 0.35f }

    let private canonicalTriangle =
        let commands =
            [| MoveTo (v2 -0.5f -0.35f)
               QuadraticCurveTo (v2 0.0f 0.0f, v2 0.5f -0.35f)
               QuadraticCurveTo (v2 0.25f 0.0f, v2 0.0f 0.35f)
               QuadraticCurveTo (v2 -0.25f 0.0f, v2 -0.5f -0.35f)
               CloseContour |]
        let source =
            SlugShapeRuntime.createSourceWithBounds
                (SlugShapeRuntime.fromContourCommands commands SlugFillNonzero 1.0e-3f).Contours
                sharedBounds
                SlugFillNonzero
                None
        let data = SlugShapeRuntime.pack [| source |]
        let layer =
            { SlugLayerState.defaultState 0 with
                Color = color 1.0f 0.5f 0.0f 1.0f
                EffectId = 21 }
        SlugShapeRuntime.createComposite data [| layer |]

    let private debugBounds =
        let inner = v2 0.488f 0.338f
        let commands =
            [| MoveTo sharedBounds.Min
               LineTo (v2 sharedBounds.Max.X sharedBounds.Min.Y)
               LineTo sharedBounds.Max
               LineTo (v2 sharedBounds.Min.X sharedBounds.Max.Y)
               CloseContour
               MoveTo -inner
               LineTo (v2 inner.X -inner.Y)
               LineTo inner
               LineTo (v2 -inner.X inner.Y)
               CloseContour |]
        let source =
            SlugShapeRuntime.createSourceWithBounds
                (SlugShapeRuntime.fromContourCommands commands SlugFillEvenOdd 1.0e-3f).Contours
                sharedBounds
                SlugFillEvenOdd
                None
        let data = SlugShapeRuntime.pack [| source |]
        let layer =
            { SlugLayerState.defaultState 0 with
                Color = color 0.12f 0.72f 0.10f 1.0f
                EffectId = 21 }
        SlugShapeRuntime.createComposite data [| layer |]

    let private animatedProjective seconds =
        let orbit = seconds * 0.32f
        let roll = System.MathF.Sin orbit * 0.30f
        let pitch = 0.40f + System.MathF.Sin (orbit * 0.73f) * 0.18f
        let zoom = 0.92f + System.MathF.Sin (orbit * 0.61f) * 0.10f
        let sinRoll = System.MathF.Sin roll
        let cosRoll = System.MathF.Cos roll
        let cosPitch = System.MathF.Cos pitch
        let mutable projective = Matrix4x4.Identity
        projective.M11 <- zoom * cosRoll
        projective.M12 <- zoom * sinRoll
        projective.M21 <- -zoom * cosPitch * sinRoll
        projective.M22 <- zoom * cosPitch * cosRoll
        projective.M14 <- System.MathF.Sin (orbit * 0.47f) * 0.0006f
        projective.M24 <- System.MathF.Sin pitch * 0.0012f
        projective

    let draw (world : World) =
        let center = v3 -65.0f -10.0f 0.0f
        let size = v3 250.0f 175.0f 0.0f
        let projective = animatedProjective (SlugDemo.clockSeconds world)
        SlugDemoContours.placeProjectiveComposite
            "MorphTriangle"
            canonicalTriangle
            center
            size
            Quaternion.Identity
            1.0f
            projective
            world
        SlugDemoContours.placeProjectiveComposite
            "MorphDebugBound"
            debugBounds
            center
            size
            Quaternion.Identity
            2.0f
            projective
            world
