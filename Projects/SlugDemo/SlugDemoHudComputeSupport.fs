namespace SlugDemo
open System
open System.Numerics
open Prime
open Nu

module SlugDemoHudComputeSupport =
    let private roundedCommands = SlugDemoContours.roundedRectCommands 0.08f
    let private smallRoundedCommands = SlugDemoContours.roundedRectCommands 0.14f
    let private circleCommands = SlugDemoContours.circleCommands

    let frameTessellation =
        SlugDemoContours.makeFilled
            roundedCommands
            (color 0.025f 0.050f 0.095f 1.0f)
            NonZero
            (color 0.12f 0.35f 0.52f 1.0f)
            1.3f
            (v2 566.0f 166.0f)


    let clockOuterTessellation =
        SlugDemoContours.makeFilled
            circleCommands
            (color 0.025f 0.18f 0.24f 1.0f)
            NonZero
            (color 0.28f 0.88f 0.96f 1.0f)
            1.2f
            (v2 116.0f 116.0f)

    let clockInnerTessellation =
        SlugDemoContours.makeFilled
            circleCommands
            (color 0.018f 0.065f 0.105f 1.0f)
            NonZero
            (color 0.08f 0.30f 0.38f 1.0f)
            0.7f
            (v2 102.0f 102.0f)

    let radarOuterTessellation =
        SlugDemoContours.makeFilled
            circleCommands
            (color 0.018f 0.11f 0.15f 1.0f)
            NonZero
            (color 0.20f 0.76f 0.68f 1.0f)
            1.0f
            (v2 130.0f 130.0f)

    let radarMidTessellation =
        SlugDemoContours.makeFilled
            circleCommands
            (color 0.018f 0.085f 0.12f 1.0f)
            NonZero
            (color 0.10f 0.43f 0.40f 1.0f)
            0.8f
            (v2 96.0f 96.0f)

    let radarInnerTessellation =
        SlugDemoContours.makeFilled
            circleCommands
            (color 0.015f 0.060f 0.090f 1.0f)
            NonZero
            (color 0.08f 0.30f 0.30f 1.0f)
            0.7f
            (v2 62.0f 62.0f)

    let radarDotTessellation =
        SlugDemoContours.makeFilled
            circleCommands
            (color 0.95f 0.92f 0.45f 1.0f)
            Positive
            (color 1.0f 0.98f 0.72f 1.0f)
            0.8f
            (v2 10.0f 10.0f)

    let gaugeTessellation =
        SlugDemoContours.makeFilled
            smallRoundedCommands
            (color 0.025f 0.060f 0.10f 1.0f)
            NonZero
            (color 0.18f 0.38f 0.54f 1.0f)
            1.0f
            (v2 158.0f 130.0f)

    let gaugeTrackTessellation =
        SlugDemoContours.makeFilled
            smallRoundedCommands
            (color 0.025f 0.09f 0.12f 1.0f)
            NonZero
            (color 0.08f 0.30f 0.34f 1.0f)
            0.8f
            (v2 126.0f 20.0f)


    let tickTessellation =
        SlugDemoContours.makeFilled
            smallRoundedCommands
            (color 0.30f 0.82f 0.86f 1.0f)
            NonZero
            (color 0.62f 0.98f 0.98f 1.0f)
            0.6f
            (v2 5.0f 17.0f)

    let majorTickTessellation =
        SlugDemoContours.makeFilled
            smallRoundedCommands
            (color 0.92f 0.70f 0.28f 1.0f)
            NonZero
            (color 1.0f 0.92f 0.58f 1.0f)
            0.7f
            (v2 7.0f 21.0f)

    let handTessellation =
        SlugDemoContours.makeFilled
            smallRoundedCommands
            (color 0.90f 0.95f 1.0f 1.0f)
            NonZero
            (color 0.60f 0.92f 1.0f 1.0f)
            0.8f
            (v2 6.0f 49.0f)

    let needleTessellation =
        SlugDemoContours.makeFilled
            smallRoundedCommands
            (color 1.0f 0.40f 0.28f 1.0f)
            NonZero
            (color 1.0f 0.72f 0.48f 1.0f)
            0.8f
            (v2 7.0f 78.0f)

    let sweepTessellation =
        let commands =
            [| MoveTo (v2 0.0f 0.0f)
               LineTo (v2 0.5f 0.08f)
               LineTo (v2 0.5f -0.08f)
               CloseContour |]
        SlugDemoContours.makeFilled
            commands
            (color 0.25f 0.95f 0.82f 0.58f)
            NonZero
            (color 0.40f 1.0f 0.90f 0.82f)
            0.7f
            (v2 122.0f 122.0f)

    let arcSliceTessellation =
        let commands =
            [| MoveTo (v2 0.10f 0.48f)
               LineTo (v2 0.24f 0.45f)
               LineTo (v2 0.38f 0.36f)
               LineTo (v2 0.47f 0.22f)
               LineTo (v2 0.49f 0.08f)
               LineTo (v2 0.38f 0.10f)
               LineTo (v2 0.31f 0.22f)
               LineTo (v2 0.19f 0.31f)
               LineTo (v2 0.07f 0.35f)
               CloseContour |]
        SlugDemoContours.makeFilled
            commands
            (color 0.32f 0.72f 1.0f 0.78f)
            NonZero
            (color 0.64f 0.90f 1.0f 0.95f)
            0.8f
            (v2 126.0f 126.0f)

    let crossbarTessellation =
        SlugDemoContours.makeFilled
            smallRoundedCommands
            (color 0.20f 0.65f 0.60f 0.82f)
            NonZero
            (color 0.42f 0.95f 0.82f 0.95f)
            0.6f
            (v2 108.0f 3.0f)

    let clockCenter = v3 -205.0f -13.0f 0.0f
    let radarCenter = v3 -32.0f -13.0f 0.0f
    let gaugeCenter = v3 176.0f -13.0f 0.0f

    let clockTickPlacements =
        Array.init 12 (fun index ->
            let angle = single index * MathF.PI * 2.0f / 12.0f
            let position =
                v3
                    (clockCenter.X + MathF.Sin angle * 48.0f)
                    (clockCenter.Y + MathF.Cos angle * 48.0f)
                    0.0f
            let rotation = Quaternion.CreateFromAxisAngle (Vector3.UnitZ, -angle)
            position, rotation)

    let radarTickPlacements =
        Array.init 8 (fun index ->
            let angle = single index * MathF.PI * 2.0f / 8.0f
            let position =
                v3
                    (radarCenter.X + MathF.Sin angle * 57.0f)
                    (radarCenter.Y + MathF.Cos angle * 57.0f)
                    0.0f
            let rotation = Quaternion.CreateFromAxisAngle (Vector3.UnitZ, -angle)
            position, rotation)


    let placeContour name tessellation position size rotation elevation world =
        SlugDemoContours.placeContour name tessellation position size rotation elevation world

    let placeSlug name text position size fontSize color elevation world =
        SlugDemo.slug name SlugDemo.font text position size fontSize color elevation world

    let placeSlugLeft name text position size fontSize color elevation world =
        SlugDemo.slugLeft
            name
            SlugDemo.font
            text
            position
            size
            fontSize
            color
            elevation
            TextDirectionLeftToRight
            None
            world

    let handAngle (seconds : single) =
        let dayPhase = seconds % 86400.0f
        dayPhase / 86400.0f * MathF.PI * 2.0f

    let private computeFieldCommands centerX centerY halfWidth halfHeight =
        [| MoveTo (v2 (centerX - halfWidth) (centerY - halfHeight))
           LineTo (v2 (centerX + halfWidth) (centerY - halfHeight))
           LineTo (v2 (centerX + halfWidth) (centerY + halfHeight))
           LineTo (v2 (centerX - halfWidth) (centerY + halfHeight))
           CloseContour |]

    let private makeComputeField (proceduralId : int) (baseColor : Color) =
        let columns = 13
        let rows = 3
        let sources =
            Array.init (columns * rows) (fun index ->
                let column = index % columns
                let row = index / columns
                let centerX = -0.48f + single column * 0.08f
                let centerY = 0.12f - single row * 0.12f
                SlugShapeRuntime.fromContourCommands
                    (computeFieldCommands centerX centerY 0.034f 0.045f)
                    SlugFillNonzero
                    1.0e-4f)
        let data = SlugShapeRuntime.pack sources
        let layers =
            Array.init sources.Length (fun index ->
                let column = index % columns
                let row = index / columns
                let centerX = -0.48f + single column * 0.08f
                let centerY = 0.12f - single row * 0.12f
                let shade = 0.72f + single ((column + row) % 5) * 0.06f
                { SlugLayerState.defaultState index with
                    Origin = v2 centerX centerY
                    Color = baseColor.MapR (fun value -> value * shade)
                    FillSource = SlugFillSource.Procedural proceduralId
                    EffectParameters = v4 1.0f 12.0f 9.0f 0.0f })
        SlugShapeRuntime.createComposite data layers

    let radialComputeField = makeComputeField 9 SlugDemo.cyan
    let spiralComputeField = makeComputeField 10 SlugDemo.magenta

    let radialComputeConfig : Vortice.Vulkan.SlugShape.SlugShapeComputeConfig =
        { LayerCount = 39
          Mode = Vortice.Vulkan.SlugShape.SlugShapeComputeMode.Radial
          Flags = 0u
          Seed = 0x51a9u
          Speed = 1.2f
          Amplitude = 0.24f
          Frequency = 8.0f
          Phase = 0.0f
          Params1 = Vector4.Zero }

    let spiralComputeConfig : Vortice.Vulkan.SlugShape.SlugShapeComputeConfig =
        { LayerCount = 39
          Mode = Vortice.Vulkan.SlugShape.SlugShapeComputeMode.Interference
          Flags = 0u
          Seed = 0x7c31u
          Speed = 0.9f
          Amplitude = 0.20f
          Frequency = 11.0f
          Phase = 1.7f
          Params1 = Vector4.Zero }
