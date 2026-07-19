namespace SlugDemo

open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoMixedText =

    // osgslug-simple.cpp builds one left-aligned, three-line text block and replaces the
    // capital F in its first line with a manually injected analytic triangle.
    let private firstLine = "Line 0: ABCDEFGabcdefg"
    let private fIndex = firstLine.IndexOf 'F'
    let private firstLineBeforeF = firstLine.Substring (0, fIndex)
    let private firstLineAfterF = firstLine.Substring (fIndex + 1)
    let private secondLine = "Line 1: 1234568790"
    let private thirdLinePrefix = "You can also"
    let private thirdLineMix = " mix"
    let private thirdLineColors = " colors "
    let private thirdLineSuffix = "in the same line!"
    let private mixedFontSize = 24.0f
    let private mixedLineHeight = 58.0f
    let private firstLineCenterY = 62.0f
    let private secondLineCenterY = 4.0f
    let private thirdLineCenterY = -54.0f
    let private sourceOrange = color 1.0f 0.5f 0.0f 1.0f
    let private sourceWhite = color 1.0f 1.0f 1.0f 1.0f
    let private sourceBlue = color 0.5f 0.7f 0.9f 1.0f
    let private sourceGreen = color 0.7f 0.9f 0.5f 1.0f
    let private sourceSpinner = color 1.0f 1.0f 0.0f 0.5f

    let private mixedFontData =
        lazy
            match SlugFontRuntime.tryLoad SlugDemo.fontFilePath with
            | Some fontData -> fontData
            | None -> invalidOp "The Slug showcase font could not be loaded."

    // The advance and visible bounds come from the actual font F slot.  The replacement
    // therefore preserves the same pen movement while matching the neighboring glyphs'
    // baseline and cap-height.
    let private fGlyph =
        lazy
            use loader = new SlugColorFontLoader (SlugDemo.fontFilePath)
            let glyphId = loader.GetGlyphId (uint32 'F')
            let mutable glyph = Unchecked.defaultof<SlugFontGlyph>
            if mixedFontData.Value.Glyphs.TryGetValue (glyphId, &glyph) then glyph
            else invalidOp "The Slug font has no metadata for its F glyph."

    let private measureText text =
        let layout =
            SlugFontRuntime.layout
                text
                mixedFontData.Value
                (Some mixedFontSize)
                sourceOrange
                SlugFillNonzero
                None
                (MsdfTextJustified (MsdfTextJustifyLeft, MsdfTextJustifyMiddle))
                TextDirectionLeftToRight
                None
                (v2 4096.0f mixedLineHeight)
                1.0f
        layout.Size.X

    type private MixedLayout =
        { LineLeft : single
          FirstBeforeWidth : single
          FirstAfterWidth : single
          FAdvance : single
          SecondWidth : single
          ThirdPrefixWidth : single
          ThirdMixWidth : single
          ThirdColorsWidth : single
          ThirdSuffixWidth : single
          TriangleCenter : Vector3
          TriangleSize : Vector3 }

    let private mixedLayout =
        lazy
            let firstBeforeWidth = measureText firstLineBeforeF
            let firstAfterWidth = measureText firstLineAfterF
            let fAdvance = fGlyph.Value.Advance * mixedFontSize
            let firstWidth = firstBeforeWidth + fAdvance + firstAfterWidth
            let secondWidth = measureText secondLine
            let thirdPrefixWidth = measureText thirdLinePrefix
            let thirdMixWidth = measureText thirdLineMix
            let thirdColorsWidth = measureText thirdLineColors
            let thirdSuffixWidth = measureText thirdLineSuffix
            let thirdWidth = thirdPrefixWidth + thirdMixWidth + thirdColorsWidth + thirdSuffixWidth
            let lineLeft = -max firstWidth (max secondWidth thirdWidth) * 0.5f
            let metrics = mixedFontData.Value.Metrics
            let baseline = firstLineCenterY - (metrics.Ascender + metrics.Descender) * mixedFontSize * 0.5f
            let bounds = fGlyph.Value.Bounds
            let fWidth = max 1.0e-4f ((bounds.Right - bounds.Left) * mixedFontSize)
            let fHeight = max 1.0e-4f ((bounds.Top - bounds.Bottom) * mixedFontSize)
            let fSlotLeft = lineLeft + firstBeforeWidth
            let triangleCenter =
                v3
                    (fSlotLeft + bounds.Left * mixedFontSize + fWidth * 0.5f)
                    (baseline + (bounds.Bottom + bounds.Top) * mixedFontSize * 0.5f)
                    0.0f
            { LineLeft = lineLeft
              FirstBeforeWidth = firstBeforeWidth
              FirstAfterWidth = firstAfterWidth
              FAdvance = fAdvance
              SecondWidth = secondWidth
              ThirdPrefixWidth = thirdPrefixWidth
              ThirdMixWidth = thirdMixWidth
              ThirdColorsWidth = thirdColorsWidth
              ThirdSuffixWidth = thirdSuffixWidth
              TriangleCenter = triangleCenter
              TriangleSize = v3 fWidth fHeight 0.0f }

    // Exact osgslug-simple.cpp triangle: three quadratic segments, including the curved
    // right and left sides. This remains analytic Slug geometry all the way to the fragment.
    let private triangleData =
        lazy
            let source =
                SlugShapeRuntime.fromContourCommands
                    [| MoveTo (v2 0.0f 0.0f)
                       QuadraticCurveTo (v2 0.5f 0.35f, v2 1.0f 0.0f)
                       QuadraticCurveTo (v2 0.75f 0.35f, v2 0.5f 0.7f)
                       QuadraticCurveTo (v2 0.25f 0.35f, v2 0.0f 0.0f)
                       CloseContour |]
                    SlugFillNonzero
                    1.0e-3f
            SlugShapeRuntime.pack [|source|]

    let private makeTriangleShape triangleColor =
        let state =
            { SlugLayerState.defaultState 0 with
                Color = triangleColor }
        SlugShapeRuntime.createComposite triangleData.Value [|state|]

    let private replacementTriangleShape = lazy (makeTriangleShape sourceOrange)
    let private spinningTriangleShape = lazy (makeTriangleShape sourceSpinner)

    let private drawRun name text x y width textColor elevation world =
        SlugDemo.slugLeft
            name
            SlugDemo.font
            text
            (v3 x y 0.0f)
            (v3 width mixedLineHeight 0.0f)
            mixedFontSize
            textColor
            elevation
            TextDirectionLeftToRight
            None
            world

    let draw (world : World) =
        SlugDemo.panel
            "MixedTextPanel"
            (v3 0.0f 4.0f 0.0f)
            (v3 610.0f 198.0f 0.0f)
            SlugDemo.panelColor
            0.0f
            world
        let layout = mixedLayout.Value
        drawRun
            "MixedTextLine0BeforeF"
            firstLineBeforeF
            layout.LineLeft
            firstLineCenterY
            layout.FirstBeforeWidth
            sourceOrange
            3.0f
            world
        SlugDemoContours.placeComposite
            "MixedTextReplacementF"
            replacementTriangleShape.Value
            layout.TriangleCenter
            layout.TriangleSize
            Quaternion.Identity
            3.0f
            None
            world
        SlugDemoContours.placeComposite
            "MixedTextSpinningF"
            spinningTriangleShape.Value
            layout.TriangleCenter
            layout.TriangleSize
            (Quaternion.CreateFromAxisAngle (Vector3.UnitZ, single (SlugDemo.clockSeconds world)))
            4.0f
            None
            world
        drawRun
            "MixedTextLine0AfterF"
            firstLineAfterF
            (layout.LineLeft + layout.FirstBeforeWidth + layout.FAdvance)
            firstLineCenterY
            layout.FirstAfterWidth
            sourceOrange
            3.0f
            world
        drawRun
            "MixedTextLine1"
            secondLine
            layout.LineLeft
            secondLineCenterY
            layout.SecondWidth
            sourceWhite
            3.0f
            world
        let thirdMixX = layout.LineLeft + layout.ThirdPrefixWidth
        let thirdColorsX = thirdMixX + layout.ThirdMixWidth
        let thirdSuffixX = thirdColorsX + layout.ThirdColorsWidth
        drawRun
            "MixedTextLine2Prefix"
            thirdLinePrefix
            layout.LineLeft
            thirdLineCenterY
            layout.ThirdPrefixWidth
            sourceWhite
            3.0f
            world
        drawRun
            "MixedTextLine2Mix"
            thirdLineMix
            thirdMixX
            thirdLineCenterY
            layout.ThirdMixWidth
            sourceBlue
            3.0f
            world
        drawRun
            "MixedTextLine2Colors"
            thirdLineColors
            thirdColorsX
            thirdLineCenterY
            layout.ThirdColorsWidth
            sourceGreen
            3.0f
            world
        drawRun
            "MixedTextLine2Suffix"
            thirdLineSuffix
            thirdSuffixX
            thirdLineCenterY
            layout.ThirdSuffixWidth
            sourceWhite
            3.0f
            world
