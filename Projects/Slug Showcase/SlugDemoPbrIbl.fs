namespace SlugShowcase
open System
open System.Numerics
open Nu
open SlugDemoProjectionSupport

[<RequireQualifiedAccess>]
module SlugDemoPbrIbl =

    // The original demo's badge is a single warm-gold circle. Keep its source as one
    // analytic contour so PbrMaterial shades the Slug coverage directly.
    let private badgeCircleCommands =
        let radius = 0.48f
        let k = radius * 0.5522847498f
        [| MoveTo (v2 radius 0.0f)
           CubicCurveTo (v2 radius k, v2 k radius, v2 0.0f radius)
           CubicCurveTo (v2 (-k) radius, v2 (-radius) k, v2 (-radius) 0.0f)
           CubicCurveTo (v2 (-radius) (-k), v2 (-k) (-radius), v2 0.0f (-radius))
           CubicCurveTo (v2 k (-radius), v2 radius (-k), v2 radius 0.0f)
           CloseContour |]

    let private badgeComposite =
        let source =
            SlugShapeRuntime.fromContourCommands
                badgeCircleCommands
                SlugFillNonzero
                1.0e-3f
        let data = SlugShapeRuntime.pack [| source |]
        let state =
            { SlugLayerState.defaultState 0 with
                Color = color 1.0f 0.86f 0.57f 1.0f
                FillSource = SlugFillSource.PbrMaterial 0
                MaterialValues = v4 1.0f 0.08f 0.20f 0.0f }
        SlugShapeRuntime.createComposite data [| state |]

    let private renderBadge (world : World) =
        let size = v3 210.0f 210.0f 0.0f
        let position = v3 0.0f -12.0f 0.0f
        let composite = badgeComposite
        let mutable minPoint = Vector2 (Single.PositiveInfinity, Single.PositiveInfinity)
        let mutable maxPoint = Vector2 (Single.NegativeInfinity, Single.NegativeInfinity)
        for metadata in composite.Data.Metadata do
            minPoint <- Vector2 (min minPoint.X metadata.Bounds.Min.X, min minPoint.Y metadata.Bounds.Min.Y)
            maxPoint <- Vector2 (max maxPoint.X metadata.Bounds.Max.X, max maxPoint.Y metadata.Bounds.Max.Y)
        let extent =
            Vector2
                (max 1.0e-6f (maxPoint.X - minPoint.X),
                 max 1.0e-6f (maxPoint.Y - minPoint.Y))
        let scale = Vector3 (size.X / extent.X, size.Y / extent.Y, 1.0f)
        let center = (minPoint + maxPoint) * 0.5f
        let rotatedCenter =
            Vector3.Transform (Vector3 (center.X * scale.X, center.Y * scale.Y, 0.0f), Quaternion.Identity)
        let transform =
            Transform.makeIntuitive
                false
                (position - rotatedCenter)
                scale
                Vector3.Zero
                size
                Vector3.Zero
                2.0f
        World.renderSlugShape
            { Transform = transform
              ClipOpt = ValueNone
              Composite = composite
              Projective = Matrix4x4.Identity
              Seconds = SlugDemo.clockSeconds world
              Delta = world.GameDelta.SecondsF
              Frame = uint32 world.UpdateTime
              Seed = uint32 (abs (hash "PbrIblBadge"))
              ComputeConfigOpt = None
              TextureSlots = pbrEnvironment }
            world

    let draw (world : World) =
        renderBadge world
