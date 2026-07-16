// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Tests

open System
open System.IO
open System.Reflection
open NUnit.Framework
open Nu

module SlugFontTests =

    let private fontPath =
        Path.Combine (AppContext.BaseDirectory, "Assets", "Default", "Font.ttf")

    let private loadFont () =
        Assert.That (File.Exists fontPath, Is.True, "The default Slug test font was not copied to the test output.")
        match SlugFontRuntime.tryLoad fontPath with
        | Some fontData -> fontData
        | None ->
            Assert.Fail "The default Slug font could not be loaded."
            Unchecked.defaultof<_>

    let private justification =
        MsdfTextJustified (MsdfTextJustifyLeft, MsdfTextJustifyMiddle)

    let private layoutArgs fontData text direction =
        [|box text
          box fontData
          box (Some 28.0f)
          box (Unchecked.defaultof<System.Numerics.Color>)
          box SlugFillNonzero
          box (None : int option)
          box justification
          box direction
          box (None : string option)
          box (System.Numerics.Vector2 (1024.0f, 256.0f))
          box 1.0f|]

    let private sameGlyph (left : SlugTextGlyph) (right : SlugTextGlyph) =
        left.Position = right.Position &&
        left.Size = right.Size &&
        left.TexCoords = right.TexCoords &&
        left.BandLocation = right.BandLocation &&
        left.BandMax = right.BandMax &&
        left.BandTransform = right.BandTransform &&
        left.Color = right.Color &&
        left.FillRule = right.FillRule

    let [<Test>] ``Slug band atlas densely packs valid row-local lists.`` () =
        let fontData = loadFont ()
        Assert.That (fontData.Glyphs.Count, Is.GreaterThan 0)
        Assert.That (fontData.CurveTexels.Length, Is.GreaterThan 0)
        Assert.That (fontData.BandTexels.Length, Is.GreaterThan 0)
        // A return to one full band row per glyph would erase the dense-packing memory saving.
        Assert.That (fontData.BandTexels.Length, Is.LessThan (fontData.Glyphs.Count * fontData.BandTextureWidth))
        for glyph in fontData.Glyphs.Values do
            let horizontalBandCount = glyph.BandMax.X + 1
            let verticalBandCount = glyph.BandMax.Y + 1
            let headerCount = horizontalBandCount + verticalBandCount
            let glyphStart = glyph.BandLocation.Y * fontData.BandTextureWidth + glyph.BandLocation.X
            Assert.That (glyph.BandLocation.X + headerCount, Is.LessThanOrEqualTo fontData.BandTextureWidth)
            Assert.That (glyphStart, Is.GreaterThanOrEqualTo 0)
            Assert.That (glyphStart + headerCount, Is.LessThanOrEqualTo fontData.BandTexels.Length)
            for headerIndex in 0 .. headerCount - 1 do
                let header = fontData.BandTexels[glyphStart + headerIndex]
                let listCount = int header.X
                let listStart = glyphStart + int header.Y
                Assert.That (listStart, Is.GreaterThanOrEqualTo glyphStart)
                Assert.That (listStart + listCount, Is.LessThanOrEqualTo fontData.BandTexels.Length)
                Assert.That (listStart % fontData.BandTextureWidth + listCount, Is.LessThanOrEqualTo fontData.BandTextureWidth)
                for listIndex in 0 .. listCount - 1 do
                    let curveLocation = fontData.BandTexels[listStart + listIndex]
                    Assert.That (int curveLocation.X + 1, Is.LessThan fontData.CurveTextureWidth)
                    Assert.That (int curveLocation.Y, Is.LessThan fontData.CurveTextureHeight)

    let [<Test>] ``Retained Slug shaping matches stateless shaping for complex runs.`` () =
        let fontData = loadFont ()
        let runtimeType = typeof<SlugFontAssetData>.Assembly.GetType ("Nu.SlugFontRuntime", true)
        let flags = BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public
        let tryCreateShaper = runtimeType.GetMethod ("tryCreateShaper", flags)
        let layout = runtimeType.GetMethod ("layout", flags)
        let layoutWithShaper = runtimeType.GetMethod ("layoutWithShaper", flags)
        Assert.That (tryCreateShaper, Is.Not.Null)
        Assert.That (layout, Is.Not.Null)
        Assert.That (layoutWithShaper, Is.Not.Null)
        let shaperOpt = tryCreateShaper.Invoke (null, [|box fontPath|])
        Assert.That (shaperOpt, Is.Not.Null, "The retained HarfBuzz context could not be created.")
        let shaper = shaperOpt.GetType().GetProperty("Value").GetValue shaperOpt
        use _shaperLifetime = shaper :?> IDisposable
        let cases =
            [|"office affine ffi", TextDirectionLeftToRight
              "עברית 123 العربية 456", TextDirectionAuto
              "abc אבג 123 xyz", TextDirectionAuto
              "مرحبا بالعالم 2026", TextDirectionRightToLeft
              "A\u0301 e\u0301 कक्षा", TextDirectionAuto|]
        let verify (text, direction) =
            let args = layoutArgs fontData text direction
            let stateless = layout.Invoke (null, args) :?> SlugTextLayout
            let retained = layoutWithShaper.Invoke (null, Array.append [|shaper|] args) :?> SlugTextLayout
            Assert.That (retained.Size, Is.EqualTo stateless.Size)
            Assert.That (retained.Glyphs.Length, Is.EqualTo stateless.Glyphs.Length)
            Assert.That (retained.Glyphs.Length, Is.GreaterThan 0)
            Assert.That (Array.forall2 sameGlyph retained.Glyphs stateless.Glyphs, Is.True,
                         "Retained shaping changed glyph geometry for '" + text + "'.")
        for case in cases do verify case
        // Font scale is mutable native state; concurrent callers must remain equivalent through serialization.
        for _ in 1 .. 4 do cases |> Array.Parallel.iter verify
