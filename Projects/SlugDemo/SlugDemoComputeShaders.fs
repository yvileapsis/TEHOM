namespace SlugDemo
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoComputeShaders =
    // The osgSlug compute sample uses two normalized contours in one composite.
    // Keep the source coordinates in [0, 1] so both layers share one exact overlap.
    let private canonicalCircleCommands =
        let k = 0.5522847498f * 0.5f
        [| MoveTo (v2 0.5f 0.0f)
           CubicCurveTo (v2 0.5f k, v2 k 0.5f, v2 0.0f 0.5f)
           CubicCurveTo (v2 0.0f (0.5f + k), v2 (0.5f - k) 1.0f, v2 0.5f 1.0f)
           CubicCurveTo (v2 (0.5f + k) 1.0f, v2 1.0f (0.5f + k), v2 1.0f 0.5f)
           CubicCurveTo (v2 1.0f (0.5f - k), v2 (0.5f + k) 0.0f, v2 0.5f 0.0f)
           CloseContour |]

    let private canonicalPentagonCommands =
        [| MoveTo (v2 0.0f 0.0f)
           LineTo (v2 0.0f 0.5f)
           LineTo (v2 0.5f 1.0f)
           LineTo (v2 1.0f 0.5f)
           LineTo (v2 1.0f 0.0f)
           CloseContour |]

    let private canonicalComposite =
        let circle =
            SlugShapeRuntime.fromContourCommands
                canonicalCircleCommands
                SlugFillNonzero
                1.0e-3f
        let pentagon =
            SlugShapeRuntime.fromContourCommands
                canonicalPentagonCommands
                SlugFillNonzero
                1.0e-3f
        let data = SlugShapeRuntime.pack [| circle; pentagon |]
        let circleState =
            { SlugLayerState.defaultState 0 with
                Color = Color.White
                // Procedural 11 is the managed canonical raymarch-style fill.
                FillSource = SlugFillSource.Procedural 11 }
        let pentagonState =
            { SlugLayerState.defaultState 1 with
                // CanonicalColor replaces this RGB each dispatch and preserves alpha .5.
                Color = color 1.0f 1.0f 1.0f 0.5f
                FillSource = SlugFillSource.Solid }
        SlugShapeRuntime.createComposite data [| circleState; pentagonState |]

    let private canonicalComputeConfig : Vortice.Vulkan.SlugShape.SlugShapeComputeConfig =
        { LayerCount = 2
          Mode = Vortice.Vulkan.SlugShape.SlugShapeComputeMode.CanonicalColor
          Flags = 0u
          Seed = 0u
          Speed = 1.0f
          Amplitude = 0.0f
          Frequency = 0.0f
          Phase = 0.0f
          Params1 = Vector4.Zero }

    let draw (world : World) =
        SlugDemoContours.placeComposite
            "ComputeCanonicalShapes"
            canonicalComposite
            (v3 0.0f -12.0f 0.0f)
            (v3 220.0f 220.0f 0.0f)
            Quaternion.Identity
            0.0f
            (Some canonicalComputeConfig)
            world
