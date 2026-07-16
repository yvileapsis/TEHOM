// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Buffers.Binary
open System.Globalization
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open HarfBuzzSharp
open Prime

/// The fill rule used by Slug contour evaluation.
type [<Struct>] SlugFillRule =
    | SlugFillNonzero
    | SlugFillEvenOdd

/// Per-glyph controls for Slug text.
type [<Struct>] SlugTextShader =
    { FillRule : SlugFillRule }

    static member defaultShader =
        { FillRule = SlugFillNonzero }

/// The normalized bounds of one Slug glyph.
type [<Struct>] SlugGlyphBounds =
    { Left : single
      Bottom : single
      Right : single
      Top : single }

/// A 16-bit unsigned RGBA texel used by Slug's band index texture.
[<Struct; StructLayout (LayoutKind.Sequential, Pack = 2)>]
type SlugBandTexel =
    { X : uint16
      Y : uint16
      Z : uint16
      W : uint16 }

/// Metadata for one glyph in a Slug font.
type [<NoEquality; NoComparison>] SlugFontGlyph =
    { Index : uint32
      Advance : single
      Bounds : SlugGlyphBounds
      BandLocation : Vector2i
      BandMax : Vector2i
      BandTransform : Vector4 }

/// Runtime data for a Slug font, including curve and band texture payloads.
type [<NoEquality; NoComparison>] SlugFontAssetData =
    { FontFilePath : string
      UnitsPerEm : single
      Metrics : MsdfFontMetrics
      Glyphs : Dictionary<uint32, SlugFontGlyph>
      CurveTextureWidth : int
      CurveTextureHeight : int
      CurveTexels : Vector4 array
      BandTextureWidth : int
      BandTextureHeight : int
      BandTexels : SlugBandTexel array }

/// One laid-out Slug glyph.
type [<Struct>] SlugTextGlyph =
    { Position : Vector2
      Size : Vector2
      TexCoords : Vector4
      BandLocation : Vector2i
      BandMax : Vector2i
      BandTransform : Vector4
      Color : Color
      FillRule : SlugFillRule }

/// Laid-out Slug text and its natural bounds.
type [<NoEquality; NoComparison>] SlugTextLayout =
    { Glyphs : SlugTextGlyph array
      Size : Vector2 }

[<RequireQualifiedAccess>]
module SlugFontRuntime =

    [<Struct>]
    type private Curve =
        { P1 : Vector2
          P2 : Vector2
          P3 : Vector2 }

    [<Struct>]
    type private Point =
        { Position : Vector2
          OnCurve : bool }

    type private RawGlyph =
        { Index : uint32
          Advance : single
          Bounds : SlugGlyphBounds
          Contours : Curve array array }

    type private ShapedGlyph =
        { Index : uint32
          XAdvance : single
          YAdvance : single
          XOffset : single
          YOffset : single }

    type private ShapedLine =
        { Glyphs : ShapedGlyph array
          Width : single }

    let private textureWidth = 4096
    let private maxBandCount = 16
    let private bandEpsilon = 1.0f / 1024.0f
    let private maxCompositeDepth = 32

    let private readUInt16 (bytes : byte array) offset =
        BinaryPrimitives.ReadUInt16BigEndian (bytes.AsSpan (offset, 2))

    let private readInt16 (bytes : byte array) offset =
        BinaryPrimitives.ReadInt16BigEndian (bytes.AsSpan (offset, 2))

    let private readUInt32 (bytes : byte array) offset =
        BinaryPrimitives.ReadUInt32BigEndian (bytes.AsSpan (offset, 4))

    let private readTable (face : Face) c1 c2 c3 c4 =
        use blob = face.ReferenceTable (Tag (c1, c2, c3, c4))
        blob.AsSpan().ToArray ()

    let private getBounds (curves : Curve array array) =
        let points =
            [| for contour in curves do
                   for curve in contour do
                       yield curve.P1
                       yield curve.P2
                       yield curve.P3 |]
        if points.Length = 0 then
            { Left = 0.0f; Bottom = 0.0f; Right = 0.0f; Top = 0.0f }
        else
            { Left = points |> Array.map (fun p -> p.X) |> Array.min
              Bottom = points |> Array.map (fun p -> p.Y) |> Array.min
              Right = points |> Array.map (fun p -> p.X) |> Array.max
              Top = points |> Array.map (fun p -> p.Y) |> Array.max }

    let private makeContour (points : Point array) =
        if points.Length < 2 then [||]
        else
            let expanded = ResizeArray<Point> ()
            for i in 0 .. dec points.Length do
                let a = points[i]
                let b = points[(i + 1) % points.Length]
                expanded.Add a
                if not a.OnCurve && not b.OnCurve then
                    expanded.Add { Position = (a.Position + b.Position) * 0.5f; OnCurve = true }
            let startIndex =
                expanded
                |> Seq.tryFindIndex (fun point -> point.OnCurve)
            match startIndex with
            | None -> [||]
            | Some startIndex ->
                let rotated =
                    Array.init expanded.Count (fun i -> expanded[(startIndex + i) % expanded.Count])
                let curves = ResizeArray<Curve> ()
                let mutable current = rotated[0].Position
                let mutable previous = rotated[0]
                for i in 1 .. rotated.Length do
                    let point = rotated[i % rotated.Length]
                    if point.OnCurve then
                        let control = if previous.OnCurve then point.Position else previous.Position
                        curves.Add { P1 = current; P2 = control; P3 = point.Position }
                        current <- point.Position
                    previous <- point
                curves.ToArray ()

    let private readSimpleGlyph (glyf : byte array) glyphOffset numberOfContours unitsPerEm =
        let mutable cursor = glyphOffset + 10
        let endPoints = Array.zeroCreate<int> numberOfContours
        for i in 0 .. dec numberOfContours do
            endPoints[i] <- int (readUInt16 glyf cursor)
            cursor <- cursor + 2
        let instructionLength = int (readUInt16 glyf cursor)
        cursor <- cursor + 2 + instructionLength
        let pointCount = if numberOfContours = 0 then 0 else endPoints[numberOfContours - 1] + 1
        let flags = Array.zeroCreate<byte> pointCount
        let mutable pointIndex = 0
        while pointIndex < pointCount do
            let flag = glyf[cursor]
            cursor <- cursor + 1
            let repeatCount = if flag &&& 8uy <> 0uy then int glyf[cursor] else 0
            if repeatCount > 0 then cursor <- cursor + 1
            let endIndex = min (pointCount - 1) (pointIndex + repeatCount)
            for i in pointIndex .. endIndex do flags[i] <- flag
            pointIndex <- endIndex + 1
        let points = Array.zeroCreate<Point> pointCount
        let mutable x = 0
        for i in 0 .. dec pointCount do
            let flag = flags[i]
            let dx =
                if flag &&& 2uy <> 0uy then
                    let value = int glyf[cursor]
                    cursor <- cursor + 1
                    if flag &&& 16uy <> 0uy then value else -value
                elif flag &&& 16uy <> 0uy then 0
                else
                    let value = int (readInt16 glyf cursor)
                    cursor <- cursor + 2
                    value
            x <- x + dx
            points[i] <- { Position = v2 (single x / unitsPerEm) 0.0f; OnCurve = flag &&& 1uy <> 0uy }
        let mutable y = 0
        for i in 0 .. dec pointCount do
            let flag = flags[i]
            let dy =
                if flag &&& 4uy <> 0uy then
                    let value = int glyf[cursor]
                    cursor <- cursor + 1
                    if flag &&& 32uy <> 0uy then value else -value
                elif flag &&& 32uy <> 0uy then 0
                else
                    let value = int (readInt16 glyf cursor)
                    cursor <- cursor + 2
                    value
            y <- y + dy
            points[i] <- { points[i] with Position = v2 points[i].Position.X (single y / unitsPerEm) }
        let contours = ResizeArray<Curve array> ()
        let mutable first = 0
        for contourIndex in 0 .. dec numberOfContours do
            let last = endPoints[contourIndex]
            if last >= first then contours.Add (makeContour points[first .. last])
            first <- last + 1
        contours.ToArray ()

    let private transformCurve (matrix : Matrix3x2) (offset : Vector2) (curve : Curve) =
        let transform (point : Vector2) =
            v2
                (point.X * matrix.M11 + point.Y * matrix.M21 + matrix.M31 + offset.X)
                (point.X * matrix.M12 + point.Y * matrix.M22 + matrix.M32 + offset.Y)
        { P1 = transform curve.P1
          P2 = transform curve.P2
          P3 = transform curve.P3 }

    let private makeGlyphParser (glyf : byte array) (loca : uint32 array) unitsPerEm =
        let cache = Dictionary<uint32, Curve array array> ()
        let rec parseGlyph depth glyphIndex =
            if depth > maxCompositeDepth || glyphIndex >= uint32 loca.Length - 1u then [||]
            elif cache.ContainsKey glyphIndex then cache[glyphIndex]
            else
                let start = int loca[int glyphIndex]
                let finish = int loca[int glyphIndex + 1]
                let curves =
                    if start >= finish || start + 10 > glyf.Length then [||]
                    else
                        let contourCount = int (readInt16 glyf start)
                        if contourCount >= 0 then
                            readSimpleGlyph glyf start contourCount unitsPerEm
                        else
                            let mutable cursor = start + 10
                            let transformed = ResizeArray<Curve array> ()
                            let mutable moreComponents = true
                            while moreComponents && cursor + 4 <= finish do
                                let flags = readUInt16 glyf cursor
                                let componentIndex = uint32 (readUInt16 glyf (cursor + 2))
                                cursor <- cursor + 4
                                let argWords = flags &&& 1us <> 0us
                                let argsAreXY = flags &&& 2us <> 0us
                                let arg1, arg2 =
                                    if argWords then
                                        let a = readInt16 glyf cursor
                                        let b = readInt16 glyf (cursor + 2)
                                        cursor <- cursor + 4
                                        int a, int b
                                    else
                                        let a = int (sbyte glyf[cursor])
                                        let b = int (sbyte glyf[cursor + 1])
                                        cursor <- cursor + 2
                                        a, b
                                let dx, dy = if argsAreXY then single arg1 / unitsPerEm, single arg2 / unitsPerEm else 0.0f, 0.0f
                                let mutable a = 1.0f
                                let mutable b = 0.0f
                                let mutable c = 0.0f
                                let mutable d = 1.0f
                                if flags &&& 8us <> 0us then
                                    let scale = single (readInt16 glyf cursor) / 16384.0f
                                    cursor <- cursor + 2
                                    a <- scale; d <- scale
                                elif flags &&& 64us <> 0us then
                                    a <- single (readInt16 glyf cursor) / 16384.0f
                                    d <- single (readInt16 glyf (cursor + 2)) / 16384.0f
                                    cursor <- cursor + 4
                                elif flags &&& 128us <> 0us then
                                    a <- single (readInt16 glyf cursor) / 16384.0f
                                    b <- single (readInt16 glyf (cursor + 2)) / 16384.0f
                                    c <- single (readInt16 glyf (cursor + 4)) / 16384.0f
                                    d <- single (readInt16 glyf (cursor + 6)) / 16384.0f
                                    cursor <- cursor + 8
                                let matrix = Matrix3x2 (a, b, c, d, 0.0f, 0.0f)
                                for contour in parseGlyph (inc depth) componentIndex do
                                    transformed.Add [|for curve in contour do yield transformCurve matrix (v2 dx dy) curve|]
                                moreComponents <- flags &&& 32us <> 0us
                            if moreComponents then
                                if cursor + 2 <= finish then
                                    let instructionLength = int (readUInt16 glyf cursor)
                                    cursor <- cursor + 2 + instructionLength
                            transformed.ToArray ()
                cache[glyphIndex] <- curves
                curves
        parseGlyph

    let private makeRawGlyph (glyphIndex : uint32) (advances : single array) curves =
        { Index = glyphIndex
          Advance = advances[int glyphIndex]
          Bounds = getBounds curves
          Contours = curves }

    let private addCurveTexel (texels : ResizeArray<Vector4>) value =
        texels.Add value

    let private packCurveData (glyphs : RawGlyph array) =
        let texels = ResizeArray<Vector4> ()
        let locations = Dictionary<uint32, Vector2i array> ()
        let mutable x = 0
        let mutable y = 0
        let beginBlock count =
            if count > textureWidth then failwith "Slug glyph contour exceeds the curve texture row width."
            if x + count > textureWidth then
                x <- 0
                y <- inc y
            let start = Vector2i (x, y)
            x <- x + count
            start
        let ensureTexel index =
            while texels.Count <= index do texels.Add Vector4.Zero
        for glyph in glyphs do
            let glyphLocations = ResizeArray<Vector2i> ()
            for contour in glyph.Contours do
                if contour.Length > 0 then
                    let start = beginBlock (inc contour.Length)
                    for i in 0 .. dec contour.Length do
                        let location = Vector2i (start.X + i, start.Y)
                        glyphLocations.Add location
                        let curve = contour[i]
                        ensureTexel (location.Y * textureWidth + location.X)
                        texels[location.Y * textureWidth + location.X] <- Vector4 (curve.P1.X, curve.P1.Y, curve.P2.X, curve.P2.Y)
                        let nextLocation = Vector2i (location.X + 1, location.Y)
                        ensureTexel (nextLocation.Y * textureWidth + nextLocation.X)
                        let nextP2 = if i + 1 < contour.Length then contour[i + 1].P2 else Vector2.Zero
                        texels[nextLocation.Y * textureWidth + nextLocation.X] <- Vector4 (curve.P3.X, curve.P3.Y, nextP2.X, nextP2.Y)
            locations[glyph.Index] <- glyphLocations.ToArray ()
        let height = max 1 (inc y)
        let result = Array.zeroCreate<Vector4> (height * textureWidth)
        for i in 0 .. dec texels.Count do result[i] <- texels[i]
        result, height, locations

    let private makeBandTexel (x : int) (y : int) =
        { X = uint16 x; Y = uint16 y; Z = 0us; W = 0us }

    let private chooseBandCount curveCount span =
        if curveCount = 0 || span <= 0.0f then 1
        else max 1 (min maxBandCount (int (sqrt (single curveCount))))

    let private makeBandData (glyphs : RawGlyph array) (curveLocations : Dictionary<uint32, Vector2i array>) =
        let bandRows = max 1 glyphs.Length
        let bandTexels = Array.zeroCreate<SlugBandTexel> (bandRows * textureWidth)
        let glyphMetadata = Dictionary<uint32, SlugFontGlyph> ()
        for glyphIndex in 0 .. dec glyphs.Length do
            let glyph = glyphs[glyphIndex]
            let bounds = glyph.Bounds
            let curveList =
                [|for contour in glyph.Contours do yield! contour|]
            let locationList = curveLocations[glyph.Index]
            let xSpan = max 0.0001f (bounds.Right - bounds.Left)
            let ySpan = max 0.0001f (bounds.Top - bounds.Bottom)
            let makeLists horizontal bandCount =
                Array.init bandCount (fun bandIndex ->
                    let lower = if horizontal then bounds.Bottom + single bandIndex * ySpan / single bandCount else bounds.Left + single bandIndex * xSpan / single bandCount
                    let upper = if horizontal then bounds.Bottom + single (bandIndex + 1) * ySpan / single bandCount else bounds.Left + single (bandIndex + 1) * xSpan / single bandCount
                    [|for curveIndex in 0 .. dec curveList.Length do
                          let curve = curveList[curveIndex]
                          let minimum = if horizontal then min curve.P1.Y (min curve.P2.Y curve.P3.Y) else min curve.P1.X (min curve.P2.X curve.P3.X)
                          let maximum = if horizontal then max curve.P1.Y (max curve.P2.Y curve.P3.Y) else max curve.P1.X (max curve.P2.X curve.P3.X)
                          let isAxisAligned = abs (maximum - minimum) <= bandEpsilon
                          if not isAxisAligned && maximum >= lower - bandEpsilon && minimum <= upper + bandEpsilon then yield curveIndex|]
                    |> Array.sortByDescending (fun curveIndex -> if horizontal then max curveList[curveIndex].P1.X (max curveList[curveIndex].P2.X curveList[curveIndex].P3.X) else max curveList[curveIndex].P1.Y (max curveList[curveIndex].P2.Y curveList[curveIndex].P3.Y)))
            let mutable bandCount = chooseBandCount curveList.Length (max xSpan ySpan)
            let mutable horizontalLists = makeLists true bandCount
            let mutable verticalLists = makeLists false bandCount
            let mutable headerCount = horizontalLists.Length + verticalLists.Length
            let mutable totalListCount = (Array.sumBy Array.length horizontalLists) + (Array.sumBy Array.length verticalLists)
            while totalListCount > textureWidth - (2 * bandCount) && bandCount > 1 do
                bandCount <- dec bandCount
                horizontalLists <- makeLists true bandCount
                verticalLists <- makeLists false bandCount
                headerCount <- horizontalLists.Length + verticalLists.Length
                totalListCount <- (Array.sumBy Array.length horizontalLists) + (Array.sumBy Array.length verticalLists)
            let row = glyphIndex
            let glyphLocation = Vector2i (0, row)
            let mutable cursor = headerCount
            let writeList (list : int array) : int * int =
                let offset = cursor
                for curveIndex in list do
                    let location = locationList[curveIndex]
                    bandTexels[row * textureWidth + cursor] <- makeBandTexel location.X location.Y
                    cursor <- inc cursor
                list.Length, offset
            for bandIndex in 0 .. dec bandCount do
                let count, offset = writeList horizontalLists[bandIndex]
                bandTexels[row * textureWidth + bandIndex] <- makeBandTexel count offset
            for bandIndex in 0 .. dec bandCount do
                let count, offset = writeList verticalLists[bandIndex]
                bandTexels[row * textureWidth + bandCount + bandIndex] <- makeBandTexel count offset
            let bandTransform =
                Vector4
                    (single bandCount / xSpan,
                     single bandCount / ySpan,
                     -bounds.Left * single bandCount / xSpan,
                     -bounds.Bottom * single bandCount / ySpan)
            glyphMetadata[glyph.Index] <-
                { Index = glyph.Index
                  Advance = glyph.Advance
                  Bounds = bounds
                  BandLocation = glyphLocation
                  BandMax = Vector2i (dec bandCount, dec bandCount)
                  BandTransform = bandTransform }
        glyphMetadata, bandTexels, bandRows

    let private tryReadFont fontFilePath =
        let fontFilePath = Path.GetFullPath fontFilePath
        try
            use blob = Blob.FromFile fontFilePath
            use face = new Face (blob, 0u)
            let head = readTable face 'h' 'e' 'a' 'd'
            let maxp = readTable face 'm' 'a' 'x' 'p'
            let hhea = readTable face 'h' 'h' 'e' 'a'
            let hmtx = readTable face 'h' 'm' 't' 'x'
            let loca = readTable face 'l' 'o' 'c' 'a'
            let glyf = readTable face 'g' 'l' 'y' 'f'
            if head.Length < 54 || maxp.Length < 6 || hhea.Length < 36 || hmtx.Length < 4 || loca.Length = 0 || glyf.Length = 0 then
                None
            else
                let unitsPerEm = max 1.0f (single (readUInt16 head 18))
                let glyphCount = int (readUInt16 maxp 4)
                let numberOfHMetrics = min glyphCount (int (readUInt16 hhea 34))
                let advances = Array.zeroCreate<single> glyphCount
                let mutable lastAdvance = 0.0f
                for glyphIndex in 0 .. dec glyphCount do
                    let metricIndex = min glyphIndex (dec numberOfHMetrics)
                    let offset = metricIndex * 4
                    if offset + 2 <= hmtx.Length then lastAdvance <- single (readUInt16 hmtx offset) / unitsPerEm
                    advances[glyphIndex] <- lastAdvance
                let locaFormat = int (readInt16 head 50)
                let locaOffsets = Array.zeroCreate<uint32> (glyphCount + 1)
                for glyphIndex in 0 .. glyphCount do
                    locaOffsets[glyphIndex] <-
                        if locaFormat = 0 then uint32 (readUInt16 loca (glyphIndex * 2)) * 2u
                        else readUInt32 loca (glyphIndex * 4)
                let parser = makeGlyphParser glyf locaOffsets unitsPerEm
                let rawGlyphs =
                    [|for glyphIndex in 0 .. dec glyphCount do
                          let contours = parser 0 (uint32 glyphIndex)
                          yield makeRawGlyph (uint32 glyphIndex) advances contours
                          |]
                let curveTexels, curveHeight, curveLocations = packCurveData rawGlyphs
                let glyphMetadata, bandTexels, bandHeight = makeBandData rawGlyphs curveLocations
                let ascender = single (readInt16 hhea 4) / unitsPerEm
                let descender = single (readInt16 hhea 6) / unitsPerEm
                let lineGap = single (readInt16 hhea 8) / unitsPerEm
                let metrics =
                    { LineHeight = ascender - descender + lineGap
                      Ascender = ascender
                      Descender = descender }
                Some
                    { FontFilePath = fontFilePath
                      UnitsPerEm = unitsPerEm
                      Metrics = metrics
                      Glyphs = glyphMetadata
                      CurveTextureWidth = textureWidth
                      CurveTextureHeight = curveHeight
                      CurveTexels = curveTexels
                      BandTextureWidth = textureWidth
                      BandTextureHeight = bandHeight
                      BandTexels = bandTexels }
        with exn ->
            Log.info ("Could not load Slug font '" + fontFilePath + "' due to: " + scstring exn)
            None

    let private clampSingle minimum maximum value = max minimum (min maximum value)

    let private isRtlChar (ch : char) =
        let code = int ch
        (code >= 0x0590 && code <= 0x08FF) ||
        (code >= 0xFB1D && code <= 0xFDFF) ||
        (code >= 0xFE70 && code <= 0xFEFF) ||
        (code >= 0x10800 && code <= 0x10FFF)

    let private isStrongLtrChar (ch : char) =
        let category = CharUnicodeInfo.GetUnicodeCategory ch
        Char.IsLetter ch && category <> UnicodeCategory.NonSpacingMark && not (isRtlChar ch)

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
                if isRtlChar ch then result <- TextDirectionRightToLeft; searching <- false
                elif isStrongLtrChar ch then searching <- false
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
                let charDirectionOpt = if isRtlChar ch then Some TextDirectionRightToLeft elif isStrongLtrChar ch then Some TextDirectionLeftToRight else None
                match charDirectionOpt with
                | Some charDirection when charDirection <> runDirection ->
                    if i > start then runs.Add (paragraph.Substring (start, i - start), runDirection)
                    start <- i
                    runDirection <- charDirection
                | _ -> ()
                i <- inc i
            if paragraph.Length > start then runs.Add (paragraph.Substring (start, paragraph.Length - start), runDirection)
            let paragraphDirection = inferParagraphDirection paragraph direction
            if paragraphDirection = TextDirectionRightToLeft then runs |> Seq.rev |> Seq.toArray else runs.ToArray ()

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
            match languageOpt with Some language when not (String.IsNullOrWhiteSpace language) -> buffer.Language <- new Language (language) | _ -> ()
            buffer.GuessSegmentProperties ()
            font.Shape (buffer, [||])
            let glyphInfos = buffer.GlyphInfos
            let glyphPositions = buffer.GlyphPositions
            [|for i in 0 .. dec glyphInfos.Length do
                  let info = glyphInfos[i]
                  let position = glyphPositions[i]
                  yield { Index = info.Codepoint; XAdvance = single position.XAdvance / 64.0f; YAdvance = single position.YAdvance / 64.0f; XOffset = single position.XOffset / 64.0f; YOffset = single position.YOffset / 64.0f }|]

    let private shapeLine fontData fontSize direction languageOpt text =
        let glyphs = ResizeArray<ShapedGlyph> ()
        let mutable width = 0.0f
        for runText, runDirection in segmentRuns text direction do
            for glyph in shapeRun fontData.FontFilePath fontSize runDirection languageOpt runText do
                glyphs.Add glyph
                width <- width + glyph.XAdvance
        { Glyphs = glyphs.ToArray (); Width = width }

    let private wrapParagraph fontData fontSize maxWidth direction languageOpt (paragraph : string) =
        if maxWidth <= 0.0f || String.IsNullOrEmpty paragraph then [|paragraph|]
        else
            let lines = ResizeArray<string> ()
            let mutable current = ""
            for word in paragraph.Split [|' '|] do
                let candidate = if String.IsNullOrEmpty current then word else current + " " + word
                if (shapeLine fontData fontSize direction languageOpt candidate).Width > maxWidth && not (String.IsNullOrEmpty current) then
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

    /// Try to load a Slug font from a TTF-compatible file.
    let tryLoad fontFilePath = tryReadFont fontFilePath

    /// Lay out shaped Slug text using the prepared contour data.
    let layout (text : string) (fontData : SlugFontAssetData) fontSizing color fillRule caretOpt justification direction languageOpt (perimeterSize : Vector2) displayScalar =
        let text = applyCaret caretOpt text
        if String.IsNullOrEmpty text then { Glyphs = [||]; Size = v2Zero }
        else
            let fontSize = defaultArg fontSizing Constants.Render.FontSizeDefault * displayScalar
            let paragraphs = text.Replace("\r\n", "\n").Replace('\r', '\n').Split([|'\n'|])
            let wrapped = [|for paragraph in paragraphs do match justification with MsdfTextUnjustified true -> yield! wrapParagraph fontData fontSize perimeterSize.X direction languageOpt paragraph | MsdfTextUnjustified false | MsdfTextJustified _ -> yield paragraph|]
            let shapedLines = [|for paragraph in wrapped do yield shapeLine fontData fontSize (inferParagraphDirection paragraph direction) languageOpt paragraph|]
            let lineCount = max 1 shapedLines.Length
            let lineHeight = abs fontData.Metrics.LineHeight * fontSize
            let ascender = fontData.Metrics.Ascender * fontSize
            let descender = fontData.Metrics.Descender * fontSize
            let naturalTop = ascender
            let naturalBottom = descender - single (lineCount - 1) * lineHeight
            let naturalHeight = naturalTop - naturalBottom
            let yOffset =
                match justification with
                | MsdfTextJustified (_, MsdfTextJustifyMiddle) -> (perimeterSize.Y - naturalHeight) * 0.5f - naturalBottom
                | MsdfTextJustified (_, MsdfTextJustifyBottom) -> -naturalBottom
                | MsdfTextJustified (_, MsdfTextJustifyTop)
                | MsdfTextUnjustified _ -> perimeterSize.Y - naturalTop
            let glyphs = ResizeArray<SlugTextGlyph> ()
            let mutable layoutWidth = 0.0f
            for lineIndex in 0 .. dec shapedLines.Length do
                let line = shapedLines[lineIndex]
                layoutWidth <- max layoutWidth line.Width
                let xOffset =
                    match justification with
                    | MsdfTextJustified (MsdfTextJustifyCenter, _) -> floor ((perimeterSize.X - line.Width) * 0.5f)
                    | MsdfTextJustified (MsdfTextJustifyRight, _) -> perimeterSize.X - line.Width
                    | MsdfTextJustified (MsdfTextJustifyLeft, _)
                    | MsdfTextUnjustified _ -> 0.0f
                let mutable pen = v2 xOffset (yOffset - single lineIndex * lineHeight)
                for shapedGlyph in line.Glyphs do
                    let mutable glyph = Unchecked.defaultof<SlugFontGlyph>
                    if fontData.Glyphs.TryGetValue (shapedGlyph.Index, &glyph) || fontData.Glyphs.TryGetValue (0u, &glyph) then
                        let bounds = glyph.Bounds
                        let glyphSize = v2 ((bounds.Right - bounds.Left) * fontSize) ((bounds.Top - bounds.Bottom) * fontSize)
                        if glyphSize.X > 0.0f && glyphSize.Y > 0.0f then
                            glyphs.Add
                                { Position = v2 (pen.X + shapedGlyph.XOffset + bounds.Left * fontSize) (pen.Y + shapedGlyph.YOffset + bounds.Bottom * fontSize)
                                  Size = glyphSize
                                  TexCoords = Vector4 (bounds.Left, bounds.Bottom, bounds.Right - bounds.Left, bounds.Top - bounds.Bottom)
                                  BandLocation = glyph.BandLocation
                                  BandMax = glyph.BandMax
                                  BandTransform = glyph.BandTransform
                                  Color = color
                                  FillRule = fillRule }
                    pen <- pen + v2 shapedGlyph.XAdvance shapedGlyph.YAdvance
            { Glyphs = glyphs.ToArray (); Size = v2 layoutWidth naturalHeight }
