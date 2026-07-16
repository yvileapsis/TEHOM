// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Numerics
open System.Text.Json
open HarfBuzzSharp
open Prime

/// Text flow direction for shaped MSDF text.
type [<Struct>] TextDirection =
    | TextDirectionAuto
    | TextDirectionLeftToRight
    | TextDirectionRightToLeft

/// Horizontal justification for laid-out MSDF text.
type [<Struct>] MsdfTextJustificationHorizontal =
    | MsdfTextJustifyLeft
    | MsdfTextJustifyCenter
    | MsdfTextJustifyRight

/// Vertical justification for laid-out MSDF text.
type [<Struct>] MsdfTextJustificationVertical =
    | MsdfTextJustifyTop
    | MsdfTextJustifyMiddle
    | MsdfTextJustifyBottom

/// Justification for laid-out MSDF text.
type MsdfTextJustification =
    | MsdfTextUnjustified of Wrapped : bool
    | MsdfTextJustified of Horizontal : MsdfTextJustificationHorizontal * Vertical : MsdfTextJustificationVertical

/// Per-glyph shader controls for MSDF text.
type [<Struct>] MsdfTextShader =
    { EdgeOffset : single
      Softness : single
      OutlineColor : Color
      OutlineThickness : single
      OutlineSoftness : single }

    static member defaultShader =
        { EdgeOffset = 0.0f
          Softness = 1.0f
          OutlineColor = Color.Zero
          OutlineThickness = 0.0f
          OutlineSoftness = 1.0f }

/// Plane and atlas bounds for one MSDF glyph.
type [<Struct>] MsdfGlyphBounds =
    { Left : single
      Bottom : single
      Right : single
      Top : single }

/// Metadata for one MSDF glyph.
type [<NoEquality; NoComparison>] MsdfFontGlyph =
    { Index : uint32
      AtlasIndex : int
      Advance : single
      PlaneBoundsOpt : MsdfGlyphBounds option
      AtlasBoundsOpt : MsdfGlyphBounds option }

/// Metadata for one MSDF font atlas texture.
type [<NoEquality; NoComparison>] MsdfFontAtlas =
    { FilePath : string
      Width : int
      Height : int
      DistanceRange : single
      Size : single }

/// Font-wide MSDF metrics in em units.
type [<Struct>] MsdfFontMetrics =
    { LineHeight : single
      Ascender : single
      Descender : single }

/// Runtime metadata for an MSDF font asset.
type [<NoEquality; NoComparison>] MsdfFontAssetData =
    { MetadataFilePath : string
      AtlasFilePath : string
      Atlases : MsdfFontAtlas array
      FontFilePath : string
      AtlasWidth : int
      AtlasHeight : int
      DistanceRange : single
      Size : single
      Metrics : MsdfFontMetrics
      Glyphs : Dictionary<uint32, MsdfFontGlyph> }

/// One laid-out glyph quad for MSDF rendering.
type [<Struct>] MsdfTextGlyph =
    { Position : Vector2
      Size : Vector2
      TexCoords : Box2
      AtlasIndex : int
      Color : Color
      DistanceRange : single }

/// Laid-out MSDF text glyphs and their natural pixel bounds.
type [<NoEquality; NoComparison>] MsdfTextLayout =
    { Glyphs : MsdfTextGlyph array
      Size : Vector2 }

[<RequireQualifiedAccess>]
module MsdfFontRuntime =

    type private ShapedGlyph =
        { Index : uint32
          XAdvance : single
          YAdvance : single
          XOffset : single
          YOffset : single }

    type private ShapedLine =
        { Glyphs : ShapedGlyph array
          Width : single }

    let private compressedCellFontAdvanceThreshold = 0.55f
    let private glyphGridWidthScaleMax = 3.0f

    let private clampSingle minimum maximum value =
        max minimum (min maximum value)

    let private median (values : single array) =
        if values.Length = 0 then 0.0f
        else
            let values = Array.sort values
            let middle = values.Length / 2
            if values.Length % 2 = 0
            then (values[middle - 1] + values[middle]) * 0.5f
            else values[middle]

    let inferGlyphGridScale (fontData : MsdfFontAssetData) (cellSize : Vector2) =
        let advances =
            [|for glyphIndex in 33u .. 126u do
                let mutable glyph = Unchecked.defaultof<MsdfFontGlyph>
                if fontData.Glyphs.TryGetValue (glyphIndex, &glyph) && glyph.Advance > 0.0f then
                    yield glyph.Advance|]
        let medianAdvance = median advances
        if medianAdvance > 0.0f && medianAdvance <= compressedCellFontAdvanceThreshold then
            let lineHeight = max 0.001f (abs fontData.Metrics.LineHeight)
            let cellAspect = if cellSize.Y <> 0.0f then abs (cellSize.X / cellSize.Y) else 1.0f
            let widthScale = clampSingle 1.0f glyphGridWidthScaleMax (cellAspect * lineHeight / medianAdvance)
            v2 widthScale 1.0f
        else v2 1.0f 1.0f

    let private tryGetProperty (propertyName : string) (element : JsonElement) =
        let mutable property = Unchecked.defaultof<JsonElement>
        if element.ValueKind = JsonValueKind.Object && element.TryGetProperty (propertyName, &property)
        then Some property
        else None

    let private tryGetSingle propertyName element =
        match tryGetProperty propertyName element with
        | Some property when property.ValueKind = JsonValueKind.Number -> Some (single (property.GetDouble ()))
        | _ -> None

    let private tryGetInt propertyName element =
        match tryGetProperty propertyName element with
        | Some property when property.ValueKind = JsonValueKind.Number -> Some (property.GetInt32 ())
        | _ -> None

    let private tryGetString propertyName element =
        match tryGetProperty propertyName element with
        | Some property when property.ValueKind = JsonValueKind.String -> Some (property.GetString ())
        | _ -> None

    let private tryGetUInt propertyName element =
        match tryGetProperty propertyName element with
        | Some property when property.ValueKind = JsonValueKind.Number -> Some (property.GetUInt32 ())
        | _ -> None

    let private tryReadBounds element =
        match struct (tryGetSingle "left" element, tryGetSingle "bottom" element, tryGetSingle "right" element, tryGetSingle "top" element) with
        | struct (Some left, Some bottom, Some right, Some top) ->
            Some { Left = left; Bottom = bottom; Right = right; Top = top }
        | _ -> None

    let private tryReadGlyph defaultAtlasIndex (glyphElement : JsonElement) =
        let indexOpt =
            match tryGetUInt "index" glyphElement with
            | Some index -> Some index
            | None -> tryGetUInt "unicode" glyphElement
        match indexOpt with
        | Some index ->
            Some
                { Index = index
                  AtlasIndex = max 0 (defaultArg (tryGetInt "atlasIndex" glyphElement) defaultAtlasIndex)
                  Advance = defaultArg (tryGetSingle "advance" glyphElement) 0.0f
                  PlaneBoundsOpt = Option.bind tryReadBounds (tryGetProperty "planeBounds" glyphElement)
                  AtlasBoundsOpt = Option.bind tryReadBounds (tryGetProperty "atlasBounds" glyphElement) }
        | None -> None

    let private makeSidecarFilePath metadataFilePath suffix extension =
        let directory = PathF.GetDirectoryName metadataFilePath
        let fileName = PathF.GetFileNameWithoutExtension metadataFilePath + suffix + extension
        if String.IsNullOrEmpty directory then fileName
        else directory + "/" + fileName

    let private resolveSiblingFilePath metadataFilePath filePath =
        if String.IsNullOrEmpty filePath then filePath
        elif Path.IsPathRooted filePath then filePath
        else
            let directory = PathF.GetDirectoryName metadataFilePath
            if String.IsNullOrEmpty directory then filePath
            else directory + "/" + filePath

    let private inferFontSidecarPath metadataFilePath =
        let ttfFilePath = makeSidecarFilePath metadataFilePath ".mtsdfSource" ".ttf"
        if File.Exists ttfFilePath then ttfFilePath
        else makeSidecarFilePath metadataFilePath ".mtsdfSource" ".otf"

    let private readAtlas metadataFilePath defaultFilePathOpt (atlasElement : JsonElement) =
        let filePath =
            match tryGetString "file" atlasElement with
            | Some filePath -> resolveSiblingFilePath metadataFilePath filePath
            | None -> defaultArg defaultFilePathOpt (makeSidecarFilePath metadataFilePath ".mtsdfAtlas" ".png")
        { FilePath = filePath
          Width = defaultArg (tryGetInt "width" atlasElement) 0
          Height = defaultArg (tryGetInt "height" atlasElement) 0
          DistanceRange = defaultArg (tryGetSingle "distanceRange" atlasElement) (defaultArg (tryGetSingle "pxRange" atlasElement) 4.0f)
          Size = defaultArg (tryGetSingle "size" atlasElement) Constants.Render.FontSizeDefault }

    let tryLoad metadataFilePath =
        let metadataFilePath = Path.GetFullPath metadataFilePath
        try
            use document = JsonDocument.Parse (File.ReadAllText metadataFilePath)
            let root = document.RootElement
            let atlas = defaultArg (tryGetProperty "atlas" root) root
            let atlases =
                match tryGetProperty "atlases" root with
                | Some atlasesElement when atlasesElement.ValueKind = JsonValueKind.Array ->
                    [|for atlasElement in atlasesElement.EnumerateArray () do
                        yield readAtlas metadataFilePath None atlasElement|]
                | _ ->
                    [|readAtlas metadataFilePath (Some (makeSidecarFilePath metadataFilePath ".mtsdfAtlas" ".png")) atlas|]
            let atlases =
                if atlases.Length = 0
                then [|readAtlas metadataFilePath (Some (makeSidecarFilePath metadataFilePath ".mtsdfAtlas" ".png")) atlas|]
                else atlases
            let metricsElement = defaultArg (tryGetProperty "metrics" root) root
            let metrics =
                { LineHeight = defaultArg (tryGetSingle "lineHeight" metricsElement) 1.2f
                  Ascender = defaultArg (tryGetSingle "ascender" metricsElement) 1.0f
                  Descender = defaultArg (tryGetSingle "descender" metricsElement) -0.2f }
            let glyphs = Dictionary<uint32, MsdfFontGlyph> ()
            match tryGetProperty "glyphs" root with
            | Some glyphsElement when glyphsElement.ValueKind = JsonValueKind.Array ->
                for glyphElement in glyphsElement.EnumerateArray () do
                    match tryReadGlyph 0 glyphElement with
                    | Some glyph -> glyphs[glyph.Index] <- glyph
                    | None -> ()
            | _ -> ()
            if glyphs.Count = 0 then Log.warn ("MSDF font metadata '" + metadataFilePath + "' contains no glyphs.")
            let atlas = if atlases.Length > 0 then atlases[0] else readAtlas metadataFilePath (Some (makeSidecarFilePath metadataFilePath ".mtsdfAtlas" ".png")) atlas
            let fontFilePath = inferFontSidecarPath metadataFilePath
            Some
                { MetadataFilePath = metadataFilePath
                  AtlasFilePath = atlas.FilePath
                  Atlases = atlases
                  FontFilePath = fontFilePath
                  AtlasWidth = atlas.Width
                  AtlasHeight = atlas.Height
                  DistanceRange = atlas.DistanceRange
                  Size = atlas.Size
                  Metrics = metrics
                  Glyphs = glyphs }
        with exn ->
            Log.info ("Could not load MSDF font metadata '" + metadataFilePath + "' due to: " + scstring exn)
            None

    let private isRtlChar (ch : char) =
        let code = int ch
        (code >= 0x0590 && code <= 0x08FF) ||
        (code >= 0xFB1D && code <= 0xFDFF) ||
        (code >= 0xFE70 && code <= 0xFEFF) ||
        (code >= 0x10800 && code <= 0x10FFF)

    let private isStrongLtrChar (ch : char) =
        let category = CharUnicodeInfo.GetUnicodeCategory ch
        Char.IsLetter ch &&
        category <> UnicodeCategory.NonSpacingMark &&
        not (isRtlChar ch)

    let private inferParagraphDirection (paragraph : string) direction =
        match direction with
        | TextDirectionLeftToRight -> TextDirectionLeftToRight
        | TextDirectionRightToLeft -> TextDirectionRightToLeft
        | TextDirectionAuto ->
            let mutable result = TextDirectionLeftToRight
            let mutable searching = true
            let mutable i = 0
            while searching && i < paragraph.Length do
                let ch = paragraph[i]
                if isRtlChar ch then
                    result <- TextDirectionRightToLeft
                    searching <- false
                elif isStrongLtrChar ch then
                    result <- TextDirectionLeftToRight
                    searching <- false
                i <- inc i
            result

    let private segmentRuns (paragraph : string) direction =
        if String.IsNullOrEmpty paragraph then [||]
        else
            let runs = List<string * TextDirection> ()
            let mutable start = 0
            let mutable runDirection = inferParagraphDirection paragraph direction
            let mutable i = 0
            while i < paragraph.Length do
                let ch = paragraph[i]
                let charDirectionOpt =
                    if isRtlChar ch then Some TextDirectionRightToLeft
                    elif isStrongLtrChar ch then Some TextDirectionLeftToRight
                    else None
                match charDirectionOpt with
                | Some charDirection when charDirection <> runDirection ->
                    if i > start then runs.Add (paragraph.Substring (start, i - start), runDirection)
                    start <- i
                    runDirection <- charDirection
                | _ -> ()
                i <- inc i
            if paragraph.Length > start then runs.Add (paragraph.Substring start, runDirection)
            let paragraphDirection = inferParagraphDirection paragraph direction
            if paragraphDirection = TextDirectionRightToLeft
            then runs |> Seq.rev |> Seq.toArray
            else runs |> Seq.toArray

    let private toHarfBuzzDirection direction =
        match direction with
        | TextDirectionRightToLeft -> Direction.RightToLeft
        | TextDirectionAuto | TextDirectionLeftToRight -> Direction.LeftToRight

    let private shapeRun fontFilePath fontSize direction languageOpt (runText : string) =
        if String.IsNullOrEmpty runText then [||]
        else
            use blob = Blob.FromFile fontFilePath
            use face = new Face (blob, 0u)
            use font = new HarfBuzzSharp.Font (face)
            font.SetFunctionsOpenType ()
            let scale = max 1 (int (fontSize * 64.0f))
            font.SetScale (scale, scale)
            use buffer = new HarfBuzzSharp.Buffer ()
            buffer.AddUtf16 runText
            buffer.Direction <- toHarfBuzzDirection direction
            match languageOpt with
            | Some language when not (String.IsNullOrWhiteSpace language) -> buffer.Language <- Language language
            | _ -> ()
            buffer.GuessSegmentProperties ()
            font.Shape (buffer, [||])
            let glyphInfos = buffer.GlyphInfos
            let glyphPositions = buffer.GlyphPositions
            [|for i in 0 .. dec glyphInfos.Length do
                let glyphInfo = glyphInfos[i]
                let glyphPosition = glyphPositions[i]
                { Index = glyphInfo.Codepoint
                  XAdvance = single glyphPosition.XAdvance / 64.0f
                  YAdvance = single glyphPosition.YAdvance / 64.0f
                  XOffset = single glyphPosition.XOffset / 64.0f
                  YOffset = single glyphPosition.YOffset / 64.0f }|]

    let private shapeLine fontData fontSize direction languageOpt (text : string) =
        let paragraphDirection = inferParagraphDirection text direction
        let runs = segmentRuns text paragraphDirection
        let glyphs = List<ShapedGlyph> ()
        let mutable width = 0.0f
        for (runText, runDirection) in runs do
            let runGlyphs = shapeRun fontData.FontFilePath fontSize runDirection languageOpt runText
            for glyph in runGlyphs do
                glyphs.Add glyph
                width <- width + glyph.XAdvance
        { Glyphs = glyphs.ToArray (); Width = width }

    let private wrapParagraph fontData fontSize maxWidth direction languageOpt (paragraph : string) =
        if maxWidth <= 0.0f || String.IsNullOrEmpty paragraph then [|paragraph|]
        else
            let words = paragraph.Split [|' '|]
            let lines = List<string> ()
            let mutable current = ""
            for word in words do
                let candidate = if String.IsNullOrEmpty current then word else current + " " + word
                let candidateWidth = (shapeLine fontData fontSize direction languageOpt candidate).Width
                if candidateWidth > maxWidth && not (String.IsNullOrEmpty current) then
                    lines.Add current
                    current <- word
                else current <- candidate
            lines.Add current
            lines.ToArray ()

    let private applyCaret (caretOpt : int option) (text : string) =
        match caretOpt with
        | Some caret when DateTimeOffset.UtcNow.Millisecond / 250 % 2 = 0 ->
            if caret < 0 || caret >= text.Length then text + "_"
            elif caret < text.Length then String.take caret text + "_" + String.skip (inc caret) text
            else text
        | Some _ | None -> text

    let private atlasTexCoords atlasWidth atlasHeight (atlasBounds : MsdfGlyphBounds) =
        let atlasWidth = max 1 atlasWidth
        let atlasHeight = max 1 atlasHeight
        let invWidth = 1.0f / single atlasWidth
        let invHeight = 1.0f / single atlasHeight
        let left = min atlasBounds.Left atlasBounds.Right
        let right = max atlasBounds.Left atlasBounds.Right
        let top = min atlasBounds.Top atlasBounds.Bottom
        let bottom = max atlasBounds.Top atlasBounds.Bottom
        box2
            (v2 (left * invWidth) (bottom * invHeight))
            (v2 ((right - left) * invWidth) ((top - bottom) * invHeight))

    let layout (text : string) (fontData : MsdfFontAssetData) fontSizing color justification caretOpt direction languageOpt (perimeterSize : Vector2) displayScalar =
        let text = applyCaret caretOpt text
        if String.IsNullOrEmpty text then { Glyphs = [||]; Size = v2Zero }
        else
            let fontSize = defaultArg fontSizing Constants.Render.FontSizeDefault * displayScalar
            let paragraphs = text.Replace("\r\n", "\n").Replace('\r', '\n').Split([|'\n'|])
            let wrapped =
                [|for paragraph in paragraphs do
                    match justification with
                    | MsdfTextUnjustified true -> yield! wrapParagraph fontData fontSize perimeterSize.X direction languageOpt paragraph
                    | MsdfTextUnjustified false | MsdfTextJustified _ -> yield paragraph|]
            let shapedLines =
                [|for paragraph in wrapped do
                    shapeLine fontData fontSize (inferParagraphDirection paragraph direction) languageOpt paragraph|]
            let lineCount = max 1 shapedLines.Length
            let lineHeight = abs fontData.Metrics.LineHeight * fontSize
            let (ascender, descender) =
                if fontData.Metrics.Ascender < fontData.Metrics.Descender
                then (-fontData.Metrics.Ascender * fontSize, -fontData.Metrics.Descender * fontSize)
                else (fontData.Metrics.Ascender * fontSize, fontData.Metrics.Descender * fontSize)
            let naturalTop = ascender
            let naturalBottom = descender - single (lineCount - 1) * lineHeight
            let naturalHeight = naturalTop - naturalBottom
            let yOffset =
                match justification with
                | MsdfTextJustified (_, MsdfTextJustifyMiddle) -> (perimeterSize.Y - naturalHeight) * 0.5f - naturalBottom
                | MsdfTextJustified (_, MsdfTextJustifyBottom) -> -naturalBottom
                | MsdfTextJustified (_, MsdfTextJustifyTop)
                | MsdfTextUnjustified _ -> perimeterSize.Y - naturalTop
            let glyphs = List<MsdfTextGlyph> ()
            let mutable layoutWidth = 0.0f
            for lineIndex in 0 .. dec shapedLines.Length do
                let shapedLine = shapedLines[lineIndex]
                layoutWidth <- max layoutWidth shapedLine.Width
                let xOffset =
                    match justification with
                    | MsdfTextJustified (MsdfTextJustifyCenter, _) -> floor ((perimeterSize.X - shapedLine.Width) * 0.5f)
                    | MsdfTextJustified (MsdfTextJustifyRight, _) -> perimeterSize.X - shapedLine.Width
                    | MsdfTextJustified (MsdfTextJustifyLeft, _)
                    | MsdfTextUnjustified _ -> 0.0f
                let mutable pen = v2 xOffset (yOffset - single lineIndex * lineHeight)
                for shapedGlyph in shapedLine.Glyphs do
                    let glyph =
                        let mutable glyphValue = Unchecked.defaultof<MsdfFontGlyph>
                        if fontData.Glyphs.TryGetValue (shapedGlyph.Index, &glyphValue) then Some glyphValue
                        elif fontData.Glyphs.TryGetValue (0u, &glyphValue) then Some glyphValue
                        else None
                    match glyph with
                    | Some glyph ->
                        match struct (glyph.PlaneBoundsOpt, glyph.AtlasBoundsOpt) with
                        | struct (Some planeBounds, Some atlasBounds) ->
                            let planeLeft = min planeBounds.Left planeBounds.Right
                            let planeRight = max planeBounds.Left planeBounds.Right
                            let (planeBottom, planeTop) =
                                if planeBounds.Top < planeBounds.Bottom
                                then (-planeBounds.Bottom, -planeBounds.Top)
                                else (planeBounds.Bottom, planeBounds.Top)
                            let glyphMin =
                                v2
                                    (pen.X + shapedGlyph.XOffset + planeLeft * fontSize)
                                    (pen.Y + shapedGlyph.YOffset + planeBottom * fontSize)
                            let glyphSize =
                                v2
                                    ((planeRight - planeLeft) * fontSize)
                                    ((planeTop - planeBottom) * fontSize)
                            if glyphSize.X <> 0.0f && glyphSize.Y <> 0.0f then
                                let atlasIndex = if glyph.AtlasIndex < fontData.Atlases.Length then glyph.AtlasIndex else 0
                                let atlas = fontData.Atlases[atlasIndex]
                                glyphs.Add
                                    { Position = glyphMin
                                      Size = glyphSize
                                      TexCoords = atlasTexCoords atlas.Width atlas.Height atlasBounds
                                      AtlasIndex = atlasIndex
                                      Color = color
                                      DistanceRange = atlas.DistanceRange }
                        | _ -> ()
                    | None -> ()
                    pen <- pen + v2 shapedGlyph.XAdvance shapedGlyph.YAdvance
            { Glyphs = glyphs.ToArray ()
              Size = v2 layoutWidth naturalHeight }
