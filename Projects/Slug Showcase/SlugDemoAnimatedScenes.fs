namespace SlugShowcase

open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoAnimatedScenes =

    // The source shape is the single 0.8 x 0.1 rounded pill used by the
    // canonical CompositeShape example. Each layer translates that same
    // shape by one 0.15-em step; the vertex effect supplies width animation.
    let private animatedPillCommands =
        let left = 0.1f
        let right = 0.9f
        let bottom = 0.1f
        let top = 0.2f
        let centerY = (bottom + top) * 0.5f
        let radiusX = 0.1f
        let radiusY = (top - bottom) * 0.5f
        let kappa = 0.5522847498f
        let kx = radiusX * kappa
        let ky = radiusY * kappa
        [| MoveTo (v2 (left + radiusX) top)
           CubicCurveTo (
               v2 (left + radiusX - kx) top,
               v2 left (centerY + ky),
               v2 left centerY)
           CubicCurveTo (
               v2 left (centerY - ky),
               v2 (left + radiusX - kx) bottom,
               v2 (left + radiusX) bottom)
           LineTo (v2 (right - radiusX) bottom)
           CubicCurveTo (
               v2 (right - radiusX + kx) bottom,
               v2 right (centerY - ky),
               v2 right centerY)
           CubicCurveTo (
               v2 right (centerY + ky),
               v2 (right - radiusX + kx) top,
               v2 (right - radiusX) top)
           LineTo (v2 (left + radiusX) top)
           CloseContour |]

    let private animatedPill =
        let source =
            SlugShapeRuntime.fromContourCommands
                animatedPillCommands
                SlugFillNonzero
                1.0e-3f
        let data = SlugShapeRuntime.pack [|source|]
        let layers =
            Array.init 12 (fun index ->
                { SlugLayerState.defaultState 0 with
                    Color = Color.One
                    Transform =
                        Matrix4x4.CreateTranslation (
                            0.0f,
                            single index * 0.15f,
                            0.0f)
                    EffectId = 22
                    EffectParameters = v4 (single index) 0.0f 0.0f 0.0f })
        SlugShapeRuntime.createComposite data layers

    let draw (world : World) =

        SlugShowcaseContours.placeContour
            "AnimatedScenePills"
            (AnalyticSlug (animatedPill, None))
            (v3 0.0f -68.0f 0.0f)
            (v3 260.0f 8.0f 0.0f)
            Quaternion.Identity
            0.0f
            world
