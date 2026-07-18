namespace SlugShowcase
open System
open System.Numerics
open Prime
open Nu

module SlugDemoTextSupport =

    let identityRotation = Quaternion.Identity
    let unitScale = v3 1.0f 1.0f 1.0f

    // Keep glyph lists and path geometry immutable: declarations below only update transforms.
    let animatedGlyphs = [| "S"; "L"; "U"; "G"; "H"; "O"; "R"; "N" |]
    let animatedXs = [| -210.0f; -150.0f; -90.0f; -30.0f; 30.0f; 90.0f; 150.0f; 210.0f |]
    let pathGlyphs = [| "S"; "L"; "U"; "G"; "V"; "E"; "C"; "T"; "O"; "R" |]
    let textPanels =
        [| (-150.0f, 53.0f, "TextLatinPanel")
           (150.0f, 53.0f, "TextCombiningPanel")
           (-150.0f, 1.0f, "TextArabicPanel")
           (150.0f, 1.0f, "TextHebrewPanel")
           (-150.0f, -51.0f, "TextDevanagariPanel")
           (150.0f, -51.0f, "TextMixedPanel") |]
    let mixedLetterXs = [| -108.0f; -36.0f; 108.0f |]
    let mixedLetters = [| "S"; "L"; "G" |]
    let effectRows = [| 58.0f; 28.0f; -2.0f; -32.0f; -62.0f |]

    let private triangleCommands =
        [| MoveTo (v2 0.0f 0.46f)
           LineTo (v2 -0.40f -0.40f)
           LineTo (v2 0.40f -0.40f)
           CloseContour |]

    let triangleTessellation =
        SlugShowcaseContours.makeFilled
            triangleCommands
            (color 0.25f 0.90f 0.80f 1.0f)
            NonZero
            (color 0.75f 1.0f 0.92f 1.0f)
            2.0f
            (v2 55.0f 64.0f)

    let private textPath =
        SlugPath (
            v2 -220.0f -20.0f,
            [| SlugPathSegment.Cubic (v2 -165.0f -72.0f, v2 -68.0f 34.0f, v2 0.0f -20.0f)
               SlugPathSegment.Cubic (v2 68.0f -74.0f, v2 165.0f 32.0f, v2 220.0f -20.0f) |],
            tolerance = 0.05f)

    // A narrow closed ribbon is used only as the visible guide in Text Along Path. It is
    // deliberately labelled as a Nu contour, rather than implying that Slug consumes it.
    let private pathGuideCommands =
        let sampleCount = 48
        let samples = Array.init sampleCount (fun index -> textPath.SampleNormalized (single index / single (sampleCount - 1)))
        let normals =
            samples
            |> Array.map (fun (_, tangent) ->
                let normal = Vector2 (-tangent.Y, tangent.X)
                if normal.LengthSquared () > 1.0e-8f then Vector2.Normalize normal else Vector2.UnitY)
        let top = Array.mapi (fun index (point, _) -> point + normals.[index] * 2.0f) samples
        let bottom = Array.init sampleCount (fun index ->
            let sourceIndex = sampleCount - index - 1
            let point, _ = samples.[sourceIndex]
            point - normals.[sourceIndex] * 2.0f)
        Array.concat
            [| [| MoveTo top.[0] |]
               top.[1..] |> Array.map LineTo
               bottom |> Array.map LineTo
               [| CloseContour |] |]

    let pathGuideTessellation =
        SlugShowcaseContours.makeFilled
            pathGuideCommands
            (color 0.05f 0.16f 0.23f 1.0f)
            NonZero
            (color 0.20f 0.70f 0.82f 1.0f)
            1.5f
            (v2 500.0f 80.0f)

    let pathPoint index =
        let amount = single index / single (pathGlyphs.Length - 1)
        let point, tangent = textPath.SampleNormalized amount
        let angle = MathF.Atan2 (tangent.Y, tangent.X)
        v3 point.X point.Y 0.0f, Quaternion.CreateFromAxisAngle (Vector3.UnitZ, angle)

    let slugCentered name slugFont text position size fontSize color elevation direction languageOpt world =
        World.doSlugText
            name
            [Entity.Position .= position
             Entity.Size .= size
             Entity.Elevation .= elevation
             Entity.SlugFont .= slugFont
             Entity.Text .= text
             Entity.FontSizing .= Some fontSize
             Entity.TextColor .= color
             Entity.TextDirection .= direction
             Entity.LanguageOpt .= languageOpt
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)]
            world

    let textEffectCharacters = [| 'S'; 'L'; 'U'; 'G' |]
    let textEffectXs = [| 67.0f; 113.0f; 159.0f; 205.0f |]

    let textEffectComposites =
        lazy
            use loader = new SlugColorFontLoader (SlugDemo.fontFilePath)
            Array.init effectRows.Length (fun row ->
                Array.init textEffectCharacters.Length (fun column ->
                    let glyphId = loader.GetGlyphId (uint32 textEffectCharacters.[column])
                    let composite = loader.LoadComposite (glyphId, foreground = Color.White)
                    let state = composite.Layers.Item 0
                    let bounds = composite.Data.Metadata.[state.ShapeIndex].Bounds
                    let origin = (bounds.Min + bounds.Max) * 0.5f
                    let state =
                        match row with
                        | 1 ->
                            { state with
                                Origin = origin
                                Color = SlugDemo.amber
                                FillSource = SlugFillSource.Procedural 0
                                EffectParameters = v4 10.0f 8.0f 0.0f 0.0f }
                        | 2 ->
                            { state with
                                Origin = origin
                                Color = SlugDemo.magenta
                                EffectId = 2
                                EffectParameters = v4 0.15f 2.2f 0.0f 0.0f }
                        | 3 ->
                            { state with
                                Origin = origin
                                Color = SlugDemo.green
                                EffectId = 4 }
                        | 4 ->
                            { state with
                                Origin = origin
                                Color = SlugDemo.cyan
                                FillSource = SlugFillSource.Procedural 8
                                EffectParameters = v4 1.2f 0.018f (single column * 0.8f) 0.0f
                                EffectParameters2 = v4 0.013f 1.0f 0.0f 0.0f }
                        | _ -> { state with Origin = origin; Color = SlugDemo.white }
                    composite.SetLayerState (0, state)
                    composite))
