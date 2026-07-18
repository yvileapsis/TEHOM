namespace SlugDemo
open System.Numerics

open Nu
open Prime

[<RequireQualifiedAccess>]
module SlugDemoMorphing =

    let private canonicalTriangleCommands =
        [| MoveTo (v2 -0.5f -0.35f)
           QuadraticCurveTo (v2 0.0f 0.0f, v2 0.5f -0.35f)
           QuadraticCurveTo (v2 0.25f 0.0f, v2 0.0f 0.35f)
           QuadraticCurveTo (v2 -0.25f 0.0f, v2 -0.5f -0.35f)
           CloseContour |]

    let private canonicalTriangle =
        let source =
            SlugShapeRuntime.createSourceWithBounds
                (SlugShapeRuntime.fromContourCommands canonicalTriangleCommands SlugFillNonzero 1.0e-3f).Contours
                { Min = v2 -1.25f -1.25f
                  Max = v2 1.25f 1.25f }
                SlugFillNonzero
                None
        let data = SlugShapeRuntime.pack [|source|]
        let layer =
            { SlugLayerState.defaultState 0 with
                Color = color 1.0f 0.5f 0.0f 1.0f
                EffectId = 21 }
        AnalyticSlug (SlugShapeRuntime.createComposite data [|layer|], None)

    let private debugBound =
        SlugDemoContours.makeFilled
            [| MoveTo (v2 -1.25f 1.25f)
               LineTo (v2 1.25f 1.25f)
               LineTo (v2 1.25f -1.25f)
               LineTo (v2 -1.25f -1.25f)
               CloseContour |]
            Color.Zero
            NonZero
            (color 0.30f 0.86f 1.0f 0.82f)
            1.5f
            (v2 100.0f 100.0f)


    let draw (world : World) =
        let center = v3 -65.0f -10.0f 0.0f
        let boundSize = v3 250.0f 250.0f 0.0f
        SlugDemoContours.placeContour
            "MorphTriangle"
            canonicalTriangle
            center
            boundSize
            Quaternion.Identity
            1.0f
            world
        SlugDemoContours.placeContour
            "MorphDebugBound"
            debugBound
            center
            boundSize
            Quaternion.Identity
            2.0f
            world
