namespace SlugShowcase
open System
open System.Numerics
open HarfBuzzSharp
open Prime
open Nu
open SlugDemoTextSupport

[<RequireQualifiedAccess>]
module SlugDemoTextAlongPath =

    // Fit osgSlug's 800x600 path presentation into Nu's 640x360 virtual viewport
    // while reserving the top strip for global navigation.
    let private pathScale = 0.75f
    let private pathOffset = v2 0.0f -12.0f
    let private fitPoint (point : Vector2) = Vector2.Multiply (point, pathScale) + pathOffset
    let private pathStart = fitPoint (v2 -270.0f -20.0f)
    let private pathSegments =
        [| SlugPathSegment.Cubic (fitPoint (v2 -252.0f -98.0f), fitPoint (v2 -190.0f -118.0f), fitPoint (v2 -132.0f -73.0f))
           SlugPathSegment.Cubic (fitPoint (v2 -72.0f -28.0f), fitPoint (v2 -12.0f 64.0f), fitPoint (v2 52.0f 124.0f))
           SlugPathSegment.Cubic (fitPoint (v2 108.0f 176.0f), fitPoint (v2 177.0f 127.0f), fitPoint (v2 278.0f 99.0f)) |]
    let private textPath = SlugPath (pathStart, pathSegments, tolerance = 0.05f)

    let private pathCommands =
        [| MoveTo pathStart
           for segment in pathSegments do
               match segment with
               | SlugPathSegment.Cubic (control1, control2, endpoint) ->
                   CubicCurveTo (control1, control2, endpoint)
               | SlugPathSegment.Quadratic (control, endpoint) ->
                   QuadraticCurveTo (control, endpoint)
               | SlugPathSegment.Line endpoint ->
                   LineTo endpoint |]

    let private pathStroke =
        ContourTessellation.make
            pathCommands
            ContourFill.none
            (ContourStroke.antiAliased (Color (228uy, 122uy, 18uy, 255uy)) 1.15f)
            (v2 1.0f 1.0f)

    let private phrase = "This is some text that follows a Path..."

    type private PathGlyph =
        { Text : string
          Advance : single }

    // Shape each source character with HarfBuzz at the same size used by the Slug entities.
    // Spaces remain in this sequence so their real advances move the following glyphs.
    let private shapedPhrase =
        lazy
            (use blob = Blob.FromFile SlugDemo.fontFilePath
             use face = new Face (blob, 0u)
             use font = new HarfBuzzSharp.Font (face)
             font.SetFunctionsOpenType ()
             let fontSize = 24.0f
             let scale = max 1 (int (fontSize * 64.0f))
             font.SetScale (scale, scale)
             phrase
             |> Seq.map string
             |> Seq.map (fun glyphText ->
                 use buffer = new HarfBuzzSharp.Buffer ()
                 buffer.AddUtf16 glyphText
                 buffer.Direction <- Direction.LeftToRight
                 buffer.GuessSegmentProperties ()
                 font.Shape (buffer, [||])
                 let positions = buffer.GlyphPositions
                 let advance =
                     if positions.Length = 0 then 0.0f
                     else single positions.[0].XAdvance / 64.0f
                 { Text = glyphText; Advance = advance })
             |> Seq.toArray)

    let draw (world : World) =
        SlugShowcaseContours.placeContour
            "TextPath"
            (TessellatedNu pathStroke)
            (v3 0.0f 0.0f 0.0f)
            (v3 1.0f 1.0f 0.0f)
            identityRotation
            1.0f
            world

        let mutable distance = 0.0f
        let fontSize = 24.0f
        let glyphScale = 0.86f
        let glyphs = shapedPhrase.Value
        for index in 0 .. glyphs.Length - 1 do
            let glyph = glyphs.[index]
            let advance = glyph.Advance * glyphScale
            let nextDistance = distance + advance
            if nextDistance > textPath.Length then
                ()
            elif Char.IsWhiteSpace glyph.Text.[0] then
                distance <- nextDistance
            else
                let point, tangent = textPath.Sample distance
                let angle = MathF.Atan2 (tangent.Y, tangent.X)
                let normal = v2 -tangent.Y tangent.X
                let center = point + tangent * (advance * 0.5f) + normal * (fontSize * glyphScale * 0.34f)
                SlugDemo.slugDynamic
                    ("TextPathGlyph" + string index)
                    SlugDemo.font
                    glyph.Text
                    (v3 center.X center.Y 0.0f)
                    (v3 (max 18.0f (glyph.Advance + 8.0f)) 44.0f 0.0f)
                    fontSize
                    SlugDemo.white
                    5.0f
                    (Quaternion.CreateFromAxisAngle (Vector3.UnitZ, angle))
                    (v3 glyphScale glyphScale 1.0f)
                    world
                distance <- nextDistance
