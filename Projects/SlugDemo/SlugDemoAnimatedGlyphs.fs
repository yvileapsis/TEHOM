namespace SlugDemo
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoAnimatedGlyphs =

    // These amplitudes mirror effect 20 in SlugShape.vert and the canonical
    // osgslug-font-animation.cpp vertex hook.
    let private waveAmplitude = 0.1f
    let private pulseAmplitude = 0.4f

    let private conservativePulseRange minValue maxValue =
        let minScale = 1.0f - pulseAmplitude
        let maxScale = 1.0f + pulseAmplitude
        let a = minValue * minScale
        let b = minValue * maxScale
        let c = maxValue * minScale
        let d = maxValue * maxScale
        struct (min (min a b) (min c d), max (max a b) (max c d))

    // Match osgslug-font-animation.cpp: one aggregate drawable with seven font
    // layers. A shared composite gives the vertex effect a continuous x domain
    // and one conservative submission instead of seven independently clipped
    // glyph drawables.
    let private animatedText =
        lazy
            (use loader = new SlugColorFontLoader (SlugDemo.fontFilePath, fontScale = 1)
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
             // Effect 20 first adds the wave and then pulses each glyph about
             // its layer origin. Derive one fixed all-time envelope from the
             // actual outlines rather than giving each glyph a clipping box.
             let mutable minX = Single.PositiveInfinity
             let mutable minY = Single.PositiveInfinity
             let mutable maxX = Single.NegativeInfinity
             let mutable maxY = Single.NegativeInfinity
             for index in 0 .. sources.Length - 1 do
                 let sourceBounds = sources[index].Bounds
                 let struct (sourceMinX, sourceMaxX) =
                     conservativePulseRange sourceBounds.Min.X sourceBounds.Max.X
                 let struct (sourceMinY, sourceMaxY) =
                     conservativePulseRange
                         (sourceBounds.Min.Y - waveAmplitude)
                         (sourceBounds.Max.Y + waveAmplitude)
                 let glyphX = single index
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
