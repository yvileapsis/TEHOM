namespace SlugShowcase

open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoLayerEffects =

    // osgSlug authors the logo in an 800-unit, y-down canvas and scales it by
    // 1 / 800 before the fragment hook sees emCoord. Nu is y-up, so source
    // points are reflected once here while retaining the canonical em domain.
    let private canvasScale = 1.0f / 800.0f
    let private tileExtent = 100.0f * canvasScale

    let private canvasPoint x y =
        v2 (x * canvasScale) (-y * canvasScale)

    let private tileCommands index =
        let left = 50.0f + single index * 120.0f
        let right = left + 100.0f
        [| MoveTo (canvasPoint left 50.0f)
           LineTo (canvasPoint right 50.0f)
           LineTo (canvasPoint right 150.0f)
           LineTo (canvasPoint left 150.0f)
           CloseContour |]

    let private roundedRectCommands x y width height radius =
        let k = radius * 0.5522847498f
        [| MoveTo (canvasPoint (x + radius) y)
           CubicCurveTo
               (canvasPoint (x + radius - k) y,
                canvasPoint x (y + radius - k),
                canvasPoint x (y + radius))
           LineTo (canvasPoint x (y + height - radius))
           CubicCurveTo
               (canvasPoint x (y + height - radius + k),
                canvasPoint (x + radius - k) (y + height),
                canvasPoint (x + radius) (y + height))
           LineTo (canvasPoint (x + width - radius) (y + height))
           CubicCurveTo
               (canvasPoint (x + width - radius + k) (y + height),
                canvasPoint (x + width) (y + height - radius + k),
                canvasPoint (x + width) (y + height - radius))
           LineTo (canvasPoint (x + width) (y + radius))
           CubicCurveTo
               (canvasPoint (x + width) (y + radius - k),
                canvasPoint (x + width - radius + k) y,
                canvasPoint (x + width - radius) y)
           CloseContour |]

    let private mapSource transformPoint (source : SlugShapeSource) =
        let contours =
            source.Contours
            |> Array.map (Array.map (fun curve ->
                { P1 = transformPoint curve.P1
                  P2 = transformPoint curve.P2
                  P3 = transformPoint curve.P3 }))
        let corners =
            [| source.Bounds.Min
               v2 source.Bounds.Min.X source.Bounds.Max.Y
               v2 source.Bounds.Max.X source.Bounds.Min.Y
               source.Bounds.Max |]
            |> Array.map transformPoint
        let mutable minPoint = Vector2 (Single.PositiveInfinity, Single.PositiveInfinity)
        let mutable maxPoint = Vector2 (Single.NegativeInfinity, Single.NegativeInfinity)
        for point in corners do
            minPoint <- Vector2 (min minPoint.X point.X, min minPoint.Y point.Y)
            maxPoint <- Vector2 (max maxPoint.X point.X, max maxPoint.Y point.Y)
        { source with
            Contours = contours
            Bounds = { Min = minPoint; Max = maxPoint } }

    let private glyphSource (loader : SlugColorFontLoader) (character : char) (position : Vector2) =
        let glyph = loader.LoadGlyph (loader.GetGlyphId (uint32 character))
        let source =
            match glyph with
            | SlugColorGlyph.Outline source -> source
            | SlugColorGlyph.ColrV0 layers when layers.Length > 0 -> layers[0].Source
            | SlugColorGlyph.ColrV1 tree when tree.FlattenedLayers.Length > 0 -> tree.FlattenedLayers[0].Source
            | _ -> invalidOp ("The AlphaPixel font has no outline for '" + string character + "'.")
        // HarfBuzz outlines are y-up. Subtracting the canonical y-down baseline
        // places ALPHA inside each tile and PIXEL beneath it after reflection.
        mapSource
            (fun point ->
                v2
                    ((point.X + position.X) * canvasScale)
                    ((point.Y - position.Y) * canvasScale))
            source

    let private expandedGlyphSource scale (source : SlugShapeSource) =
        let center = (source.Bounds.Min + source.Bounds.Max) * 0.5f
        mapSource (fun point -> center + (point - center) * scale) source

    let private alphaPixelComposite =
        lazy
            use loader = new SlugColorFontLoader (SlugDemo.fontFilePath, fontScale = 60)

            let tileSources =
                [| for index in 0 .. 4 ->
                       SlugShapeRuntime.fromContourCommands
                           (tileCommands index)
                           SlugFillNonzero
                           1.0e-5f |]
            let alphaSources =
                [| for index in 0 .. 4 do
                       let center = 50.0f + single index * 120.0f + 50.0f
                       let position = v2 (center - 15.0f) (50.0f + 50.0f + 60.0f * 0.35f)
                       yield glyphSource loader ("ALPHA").[index] position |]
            let pixelSources =
                [| for index in 0 .. 4 do
                       let center = 50.0f + single index * 120.0f + 50.0f
                       let position = v2 (center - 15.0f) 200.0f
                       yield glyphSource loader ("PIXEL").[index] position |]

            // The source preview uses a 20-unit round stroke around the tile
            // union and PIXEL row. Expanded analytic loops supply the same dark
            // silhouette without replacing Slug coverage with tessellation.
            let expandedTileSources =
                [| for index in 0 .. 4 do
                       let x = 50.0f + single index * 120.0f - 10.0f
                       yield
                           SlugShapeRuntime.fromContourCommands
                               (roundedRectCommands x 40.0f 120.0f 120.0f 10.0f)
                               SlugFillNonzero
                               1.0e-5f |]
            let tileOutlineSource =
                SlugShapeRuntime.createSource
                    (expandedTileSources |> Array.collect (fun source -> source.Contours))
                    SlugFillNonzero
                    None
            let pixelOutlineSources =
                pixelSources |> Array.map (expandedGlyphSource 1.32f)

            let sources =
                Array.concat
                    [| [| tileOutlineSource |]
                       pixelOutlineSources
                       tileSources
                       alphaSources
                       pixelSources |]
            let tileColors =
                [| color (140.0f / 255.0f) (170.0f / 255.0f) (200.0f / 255.0f) 1.0f
                   color (130.0f / 255.0f) (165.0f / 255.0f) (185.0f / 255.0f) 1.0f
                   color (120.0f / 255.0f) (160.0f / 255.0f) (170.0f / 255.0f) 1.0f
                   color (110.0f / 255.0f) (155.0f / 255.0f) (155.0f / 255.0f) 1.0f
                   color (100.0f / 255.0f) (150.0f / 255.0f) (140.0f / 255.0f) 1.0f |]
            let glyphColor = color 0.47f 0.47f 0.47f 1.0f
            let outlineColor = color 0.07f 0.07f 0.07f 1.0f
            let alphaColors =
                [| glyphColor
                   glyphColor
                   glyphColor
                   color 1.0f 0.0f 0.0f 0.5f
                   color 0.0f 0.0f 0.0f 0.75f |]

            let checkerScale = 300.0f
            let gridScale = 200.0f * tileExtent
            let waveFrequency = 6.28f * 3.0f
            let wavePhaseSpeed = 0.3f * waveFrequency
            let tileStates =
                [| for index in 0 .. 4 ->
                       let fillSource, parameters =
                           match index with
                           | 0 -> SlugFillSource.Solid, Vector4.Zero
                           | 1 ->
                               SlugFillSource.Procedural 0,
                               v4 checkerScale checkerScale 0.0f 0.0f
                           | 2 ->
                               // The managed grid equation uses normalized UV,
                               // so 200 em cells over a 100 / 800 tile is 25.
                               SlugFillSource.Procedural 1,
                               v4 gridScale gridScale 0.0f 0.0f
                           | 3 -> SlugFillSource.Texture 0, Vector4.Zero
                           | _ ->
                               // sin ((x + 0.3t) * (6.28 * 3))
                               SlugFillSource.Procedural 6,
                               v4 wavePhaseSpeed waveFrequency 0.0f 0.0f
                       { SlugLayerState.defaultState (6 + index) with
                           Color = tileColors[index]
                           FillSource = fillSource
                           EffectParameters = parameters } |]
            let tileOutlineState =
                { SlugLayerState.defaultState 0 with
                    Color = outlineColor }
            let pixelOutlineStates =
                [| for index in 0 .. 4 ->
                       { SlugLayerState.defaultState (1 + index) with
                           Color = outlineColor } |]
            let alphaStates =
                [| for index in 0 .. 4 ->
                       { SlugLayerState.defaultState (11 + index) with
                           Color = alphaColors[index] } |]
            let pixelStates =
                [| for index in 0 .. 4 ->
                       { SlugLayerState.defaultState (16 + index) with
                           Color = glyphColor } |]

            let data = SlugShapeRuntime.pack sources
            SlugShapeRuntime.createComposite
                data
                (Array.concat
                    [| [| tileOutlineState |]
                       pixelOutlineStates
                       tileStates
                       alphaStates
                       pixelStates |])

    let private alphaPixelSize =
        lazy
            let composite = alphaPixelComposite.Value
            let mutable minPoint = Vector2 (Single.PositiveInfinity, Single.PositiveInfinity)
            let mutable maxPoint = Vector2 (Single.NegativeInfinity, Single.NegativeInfinity)
            for metadata in composite.Data.Metadata do
                minPoint <- Vector2 (min minPoint.X metadata.Bounds.Min.X, min minPoint.Y metadata.Bounds.Min.Y)
                maxPoint <- Vector2 (max maxPoint.X metadata.Bounds.Max.X, max maxPoint.Y metadata.Bounds.Max.Y)
            let extent = maxPoint - minPoint
            let uniformScale = 460.0f / max extent.X 1.0e-6f
            v3 460.0f (extent.Y * uniformScale) 0.0f

    let draw (world : World) =
        SlugShowcaseContours.placeCompositeWithTextures
            "LayerEffectsAlphaPixel"
            alphaPixelComposite.Value
            (v3 0.0f -18.0f 0.0f)
            alphaPixelSize.Value
            Quaternion.Identity
            2.0f
            None
            [| Assets.Default.Brick |]
            world
