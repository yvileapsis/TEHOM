namespace SlugDemo
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
    let private pathScale = v2 1.0f 0.75f
    let private pathOffset = v2 0.0f -12.0f
    let private fitPoint (point : Vector2) = Vector2.Multiply (point, pathScale) + pathOffset
    let private sourcePathStart = v2 -270.0f -20.0f
    let private sourcePathSegments =
        [| SlugPathSegment.Cubic (v2 -252.0f -98.0f, v2 -190.0f -118.0f, v2 -132.0f -73.0f)
           SlugPathSegment.Cubic (v2 -72.0f -28.0f, v2 -12.0f 64.0f, v2 52.0f 124.0f)
           SlugPathSegment.Cubic (v2 108.0f 176.0f, v2 177.0f 127.0f, v2 278.0f 99.0f) |]
    let private pathStart = fitPoint sourcePathStart
    let private pathSegments =
        sourcePathSegments
        |> Array.map (function
            | SlugPathSegment.Cubic (control1, control2, endpoint) ->
                SlugPathSegment.Cubic (fitPoint control1, fitPoint control2, fitPoint endpoint)
            | SlugPathSegment.Quadratic (control, endpoint) ->
                SlugPathSegment.Quadratic (fitPoint control, fitPoint endpoint)
            | SlugPathSegment.Line endpoint ->
                SlugPathSegment.Line (fitPoint endpoint))
    let private textPath = SlugPath (sourcePathStart, sourcePathSegments, tolerance = 0.05f)

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
        SlugDemoContours.makeFilled
            pathCommands
            Color.Zero
            NonZero
            (Color (228uy, 122uy, 18uy, 255uy))
            1.15f

    let private phrase = "This is some text that follows a Path..."

    type private PathGlyph =
        { LayerIndex : int option
          Advance : single }

    // osgSlug places each glyph rigidly in the path's source space. Nu then fits
    // the complete presentation into its shorter viewport. Applying that same
    // anisotropic fit to the placed glyph—not only to the orange path—makes its
    // apparent width and height vary with the path tangent.
    let private pathText =
        lazy
            (use loader = new SlugColorFontLoader (SlugDemo.fontFilePath, fontScale = 1)
             use blob = Blob.FromFile SlugDemo.fontFilePath
             use face = new Face (blob, 0u)
             use font = new HarfBuzzSharp.Font (face)
             font.SetFunctionsOpenType ()
             let fontSize = 28.0f
             let scale = max 1 (int (fontSize * 64.0f))
             font.SetScale (scale, scale)
             let sources = ResizeArray<SlugShapeSource> ()
             let glyphs =
                 phrase
                 |> Seq.map (fun character ->
                     use buffer = new HarfBuzzSharp.Buffer ()
                     buffer.AddUtf16 (string character)
                     buffer.Direction <- Direction.LeftToRight
                     buffer.GuessSegmentProperties ()
                     font.Shape (buffer, [||])
                     let positions = buffer.GlyphPositions
                     if positions.Length <> 1 then
                         invalidOp "The text-path demo expects one shaped glyph per character."
                     let layerIndex =
                         if Char.IsWhiteSpace character then None
                         else
                             let glyphId = loader.GetGlyphId (uint32 character)
                             let source =
                                 match loader.LoadGlyph (glyphId, foreground = Color.White) with
                                 | SlugColorGlyph.Outline source -> source
                                 | SlugColorGlyph.ColrV0 _
                                 | SlugColorGlyph.ColrV1 _ ->
                                     invalidOp "The text-path demo requires outline glyphs."
                             let layerIndex = sources.Count
                             sources.Add source
                             Some layerIndex
                     { LayerIndex = layerIndex
                       Advance = single positions.[0].XAdvance / 64.0f })
                 |> Seq.toArray
             let data = SlugShapeRuntime.pack (sources.ToArray ())
             let layers =
                 Array.init sources.Count (fun sourceIndex ->
                     { SlugLayerState.defaultState sourceIndex with Color = Color.White })
             let composite = SlugShapeRuntime.createComposite data layers
             let presentationScale = Matrix4x4.CreateScale (pathScale.X, pathScale.Y, 1.0f)
             let mutable distance = 0.0f
             let mutable placing = true
             for glyph in glyphs do
                 if placing then
                     let nextDistance = distance + glyph.Advance
                     if nextDistance > textPath.Length then placing <- false
                     else
                         match glyph.LayerIndex with
                         | Some layerIndex ->
                             let point, tangent = textPath.Sample distance
                             let angle = MathF.Atan2 (tangent.Y, tangent.X)
                             let mutable sourceTransform =
                                 Matrix4x4.CreateRotationZ angle *
                                 Matrix4x4.CreateScale fontSize
                             sourceTransform.Translation <- v3 point.X point.Y 0.0f
                             let mutable fittedTransform = sourceTransform * presentationScale
                             fittedTransform.Translation <-
                                 fittedTransform.Translation +
                                 v3 pathOffset.X pathOffset.Y 0.0f
                             composite.SetLayerTransform (layerIndex, fittedTransform)
                         | None -> ()
                         distance <- nextDistance
             composite)

    let draw (world : World) =
        SlugDemoContours.placeContour
            pathStroke
            (v3 0.0f 0.0f 0.0f)
            (v3 1.0f 1.0f 0.0f)
            identityRotation
            1.0f
            world

        let composite = pathText.Value
        let struct (minPoint, maxPoint) = SlugDemoContours.getCompositeLayerBounds composite
        let extent = maxPoint - minPoint
        let center = (minPoint + maxPoint) * 0.5f
        SlugDemoContours.placeCompositeInBounds
            "TextPathGlyphs"
            composite
            (struct (minPoint, maxPoint))
            (v3 center.X center.Y 0.0f)
            (v3 extent.X extent.Y 0.0f)
            identityRotation
            5.0f
            None
            world
