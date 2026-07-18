namespace SlugShowcase
open System
open System.Numerics
open Prime
open Nu
open SlugDemoTextSupport

[<RequireQualifiedAccess>]
module SlugDemoTextEffects =

    let private configureGlyph (composite : SlugCompositeShape) fillSource effectId parameters parameters2 color =
        let state = composite.Layers.Item 0
        composite.SetLayerState
            (0,
             { state with
                 Color = color
                 FillSource = fillSource
                 EffectId = effectId
                 EffectParameters = parameters
                 EffectParameters2 = parameters2 })

    let draw (world : World) =
        let cards =
            [| (-150.0f, 45.0f)
               (150.0f, 45.0f)
               (-150.0f, -55.0f)
               (150.0f, -55.0f) |]
        let sourceCenterX = (textEffectXs.[0] + textEffectXs.[textEffectXs.Length - 1]) * 0.5f
        let glyphOffsets = textEffectXs |> Array.map (fun sourceX -> sourceX - sourceCenterX)
        let glyphs = textEffectComposites.Value

        // Keep the preview honest about what this renderer provides: Slug's analytic
        // coverage stays the source of every glyph, while the procedural fills and
        // layer effects supply the halo, outline, and cut-away treatments.
        for column in 0 .. textEffectCharacters.Length - 1 do
            configureGlyph glyphs.[0].[column] SlugFillSource.Solid 0 (v4 0.0f 0.0f 0.0f 0.0f) (v4 0.0f 0.0f 0.0f 0.0f) SlugDemo.white
            configureGlyph glyphs.[1].[column] (SlugFillSource.Procedural 4) 0 (v4 8.0f 0.0f 0.0f 0.0f) (v4 0.0f 0.0f 0.0f 0.0f) SlugDemo.amber
            configureGlyph glyphs.[2].[column] (SlugFillSource.Procedural 5) 0 (v4 8.0f 0.0f 0.0f 0.0f) (v4 0.0f 0.0f 0.0f 0.0f) SlugDemo.coral
            configureGlyph glyphs.[4].[column] SlugFillSource.Solid 0 (v4 0.0f 0.0f 0.0f 0.0f) (v4 0.0f 0.0f 0.0f 0.0f) SlugDemo.white
            let cutProgress = [| 0.12f; 0.18f; 0.24f; 0.30f |].[column]
            configureGlyph
                glyphs.[3].[column]
                SlugFillSource.Solid
                4
                (v4 cutProgress 0.0f 0.0f 0.0f)
                (v4 0.0f 0.0f 0.0f 0.0f)
                SlugDemo.cyan


        for cardIndex in 0 .. cards.Length - 1 do
            let centerX, centerY = cards.[cardIndex]
            SlugDemo.panel
                ("TextEffectsPanel" + string cardIndex)
                (v3 centerX centerY 0.0f)
                (v3 282.0f 84.0f 0.0f)
                (if cardIndex % 2 = 0 then SlugDemo.panelColor else SlugDemo.panelColorLight)
                0.0f
                world

            for column in 0 .. textEffectCharacters.Length - 1 do
                let x = centerX + glyphOffsets.[column]
                let y = centerY - 15.0f
                match cardIndex with
                | 0 ->
                    SlugShowcaseContours.placeComposite
                        ("TextEffectsInside" + string column)
                        glyphs.[0].[column]
                        (v3 x y 0.0f)
                        (v3 28.0f 29.0f 0.0f)
                        Quaternion.Identity
                        2.0f
                        None
                        world
                | 1 ->
                    // The enlarged analytic halo sits behind the normal glyph, so its
                    // procedural edge is visible on the outside rather than as a label.
                    SlugShowcaseContours.placeComposite
                        ("TextEffectsHalo" + string column)
                        glyphs.[1].[column]
                        (v3 x y 0.0f)
                        (v3 37.0f 35.0f 0.0f)
                        Quaternion.Identity
                        2.0f
                        None
                        world
                    SlugShowcaseContours.placeComposite
                        ("TextEffectsHaloFill" + string column)
                        glyphs.[4].[column]
                        (v3 x y 0.0f)
                        (v3 28.0f 29.0f 0.0f)
                        Quaternion.Identity
                        3.0f
                        None
                        world
                | 2 ->
                    SlugShowcaseContours.placeComposite
                        ("TextEffectsOutline" + string column)
                        glyphs.[2].[column]
                        (v3 x y 0.0f)
                        (v3 37.0f 35.0f 0.0f)
                        Quaternion.Identity
                        2.0f
                        None
                        world
                | _ ->
                    SlugShowcaseContours.placeComposite
                        ("TextEffectsCutAway" + string column)
                        glyphs.[3].[column]
                        (v3 x y 0.0f)
                        (v3 31.0f 31.0f 0.0f)
                        Quaternion.Identity
                        2.0f
                        None
                        world

