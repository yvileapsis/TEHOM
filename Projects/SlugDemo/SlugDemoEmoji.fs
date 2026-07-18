namespace SlugDemo

open System
open System.IO
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoEmoji =

    type private EmojiGlyph =
        { CodePoint : uint32
          Composite : SlugCompositeShape
          Bounds : struct (Vector2 * Vector2)
          DrawSize : Vector3 }

    let private canonicalEmoji =
        [| 0x1f525u // flame
           0x1f308u // rainbow
           0x1f52eu // crystal ball
           0x1f48eu // diamond
           0x1f98bu // butterfly
           0x1f30au // wave
           0x1f409u // dragon
           0x1f386u // fireworks
           0x1f305u |] // sunrise

    let private cellSize = v3 88.0f 88.0f 0.0f

    let private fitBoundsInCell (bounds : struct (Vector2 * Vector2)) =
        let struct (minPoint, maxPoint) = bounds
        let extent =
            Vector2 (
                max 1.0e-6f (maxPoint.X - minPoint.X),
                max 1.0e-6f (maxPoint.Y - minPoint.Y))
        let uniformScale = min (cellSize.X / extent.X) (cellSize.Y / extent.Y)
        v3 (extent.X * uniformScale) (extent.Y * uniformScale) 0.0f

    // Resolve the bundled COLRv1 font from either the project tree or a deployed
    // Assets directory.  Every glyph in the grid must come from this one font.
    let private emojiFontPaths =
        [| "Noto-COLRv1.ttf" |]
        |> Array.collect (fun fileName ->
            [| Path.Combine ("Projects", "SlugDemo", "Assets", "Default", fileName)
               Path.Combine ("Assets", "Default", fileName)
               Path.Combine (AppContext.BaseDirectory, "Assets", "Default", fileName)
               Path.Combine (AppContext.BaseDirectory, "..", "..", "..", "Assets", "Default", fileName) |])
        |> Array.map Path.GetFullPath
        |> Array.distinct

    let private loadFontGlyphs fontPath =
        try
            use loader = new SlugColorFontLoader (fontPath)
            canonicalEmoji
            |> Array.choose (fun codePoint ->
                try
                    match loader.TryGetGlyphId codePoint with
                    | Some glyphId ->
                        try
                            let composite = loader.LoadComposite glyphId
                            let bounds = SlugDemoContours.getCompositeLayerBounds composite
                            Some
                                { CodePoint = codePoint
                                  Composite = composite
                                  Bounds = bounds
                                  DrawSize = if composite.LayerCount = 0 then cellSize else fitBoundsInCell bounds }
                        with _ -> None
                    | None -> None
                with _ -> None)
        with _ -> [||]

    let private realEmojiGrid =
        lazy
            match emojiFontPaths |> Array.tryFind File.Exists with
            | Some fontPath ->
                let availableGlyphs = loadFontGlyphs fontPath
                canonicalEmoji
                |> Array.map (fun codePoint ->
                    availableGlyphs
                    |> Array.tryFind (fun glyph -> glyph.CodePoint = codePoint))
            | None -> Array.create canonicalEmoji.Length None

    let draw (world : World) =
        let columns = [| -160.0f; 0.0f; 160.0f |]
        let rows = [| 85.0f; -15.0f; -115.0f |]
        let grid = realEmojiGrid.Value
        for index in 0 .. grid.Length - 1 do
            match grid[index] with
            | Some glyph ->
                let column = index % 3
                let row = index / 3
                SlugDemoContours.placeCompositeInBounds
                    (sprintf "EmojiGlyph%02d" index)
                    glyph.Composite
                    glyph.Bounds
                    (v3 columns[column] rows[row] 0.0f)
                    glyph.DrawSize
                    Quaternion.Identity
                    1.0f
                    None
                    world
            | None -> ()

