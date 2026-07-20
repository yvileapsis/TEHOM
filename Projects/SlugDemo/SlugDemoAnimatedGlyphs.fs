namespace SlugDemo
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoAnimatedGlyphs =

    // The reference media was recorded from osgslug-font-animation's original
    // effect 6: a local em-space wave and subtle horizontal morph, not a pulse.
    let private horizontalAmplitude = 0.3f
    let private verticalAmplitude = 0.3f

    let private conservativeHorizontalRange minValue maxValue =
        let deform value phase = value + (value - 0.5f) * phase
        let a = deform minValue -horizontalAmplitude
        let b = deform minValue horizontalAmplitude
        let c = deform maxValue -horizontalAmplitude
        let d = deform maxValue horizontalAmplitude
        struct (min (min a b) (min c d), max (max a b) (max c d))

    // Match the recorded osgSlug demo: Ubuntu Mono, one aggregate drawable,
    // seven translated layers, and its historical effect 6 equations.
    let private fontFilePath =
        IO.Path.Combine (AppContext.BaseDirectory, "Assets", "Default", "UbuntuMono-Regular.ttf")
    let private animatedText =
        lazy
            (use loader = new SlugColorFontLoader (fontFilePath, fontScale = 1)
             let sources =
                 "osgSlug"
                 |> Seq.map (fun character ->
                     let glyphId = loader.GetGlyphId (uint32 character)
                     match loader.LoadGlyph (glyphId, foreground = Color.White) with
                     | SlugColorGlyph.Outline source -> source
                     | SlugColorGlyph.ColrV0 _
                     | SlugColorGlyph.ColrV1 _ ->
                         invalidOp "The animated mono-font demo requires outline glyphs.")
                 |> Seq.toArray
             // Derive one fixed all-time envelope for the local wave and morph
             // rather than relying on an undersized per-frame submission.
             let mutable minX = Single.PositiveInfinity
             let mutable minY = Single.PositiveInfinity
             let mutable maxX = Single.NegativeInfinity
             let mutable maxY = Single.NegativeInfinity
             for index in 0 .. sources.Length - 1 do
                 let sourceBounds = sources[index].Bounds
                 let glyphX = single index
                 let struct (sourceMinX, sourceMaxX) =
                     conservativeHorizontalRange sourceBounds.Min.X sourceBounds.Max.X
                 let sourceMinY = sourceBounds.Min.Y - verticalAmplitude
                 let sourceMaxY = sourceBounds.Max.Y + verticalAmplitude
                 minX <- min minX (glyphX + sourceMinX)
                 minY <- min minY sourceMinY
                 maxX <- max maxX (glyphX + sourceMaxX)
                 maxY <- max maxY sourceMaxY
             let animationBounds : SlugShapeBounds =
                 { Min = v2 minX minY
                   Max = v2 maxX maxY }
             let data = SlugShapeRuntime.pack sources
             let layers =
                 Array.init sources.Length (fun index ->
                     { SlugLayerState.defaultState index with
                         Transform = Matrix4x4.CreateTranslation (single index, 0.0f, 0.0f)
                         Origin = Vector2.Zero
                         Color = Color.White
                         EffectId = 20 })
             struct (SlugCompositeShape (data, layers), animationBounds))

    let draw (world : World) =
        let seconds = SlugDemo.clockSeconds world
        let glyphScale = 48.0f
        let struct (composite, animationBounds) = animatedText.Value
        let boundsSize = animationBounds.Max - animationBounds.Min
        let boundsCenter = (animationBounds.Min + animationBounds.Max) * 0.5f
        let transform =
            Transform.makeIntuitive
                false
                (v3 (-boundsCenter.X * glyphScale) (-boundsCenter.Y * glyphScale) 0.0f)
                (v3 glyphScale glyphScale 1.0f)
                // Offset aligns Transform.Perimeter with the source-space
                // envelope without changing the root affine transform.
                (v3 (boundsCenter.X / boundsSize.X) (boundsCenter.Y / boundsSize.Y) 0.0f)
                (v3 boundsSize.X boundsSize.Y 0.0f)
                Vector3.Zero
                0.0f
        World.renderSlugShape
            { Transform = transform
              ClipOpt = ValueNone
              Composite = composite
              Projective = Matrix4x4.Identity
              Seconds = seconds
              Delta = world.GameDelta.SecondsF
              Frame = uint32 world.UpdateTime
              Seed = uint32 (abs (hash "AnimatedGlyphs"))
              ComputeConfigOpt = None
              TextureSlots = [||] }
            world
