namespace SlugDemo

open System
open System.Numerics
open Prime
open Nu
open SlugDemoVectorSupport

[<RequireQualifiedAccess>]
module SlugDemoMasking =

    // This is the active osgSlug mask composition: four independently authored
    // quadrants share one centered circular mask in composite coordinates.
    let private quadrantCommands =
        [| [| MoveTo (v2 0.0f 0.0f)
              LineTo (v2 0.5f 0.0f)
              LineTo (v2 0.5f 0.5f)
              LineTo (v2 0.0f 0.5f)
              CloseContour |]
           [| MoveTo (v2 0.5f 0.0f)
              LineTo (v2 1.0f 0.0f)
              LineTo (v2 1.0f 0.5f)
              LineTo (v2 0.5f 0.5f)
              CloseContour |]
           [| MoveTo (v2 0.0f 0.5f)
              LineTo (v2 0.5f 0.5f)
              LineTo (v2 0.5f 1.0f)
              LineTo (v2 0.0f 1.0f)
              CloseContour |]
           [| MoveTo (v2 0.5f 0.5f)
              LineTo (v2 1.0f 0.5f)
              LineTo (v2 1.0f 1.0f)
              LineTo (v2 0.5f 1.0f)
              CloseContour |] |]

    let private quadrantColors =
        [| color 1.0f 0.6f 0.1f 1.0f
           color 0.9f 0.2f 0.6f 1.0f
           color 0.1f 0.8f 0.9f 1.0f
           color 1.0f 1.0f 0.1f 1.0f |]

    let private makeMaskedQuadrants invert =
        let sources =
            quadrantCommands
            |> Array.map (fun commands ->
                SlugShapeRuntime.fromContourCommands commands SlugFillNonzero 1.0e-3f)
        let masks =
            [| { Kind = SlugMaskKind.Circle
                 Parameters = v4 0.5f 0.5f 0.25f 0.0f
                 Parameters2 = Vector4.Zero
                 Invert = invert } |]
        let data = SlugShapeRuntime.packWithResources sources [||] [||] masks [||]
        let layers =
            quadrantColors
            |> Array.mapi (fun index fillColor ->
                { SlugLayerState.defaultState index with
                    Color = fillColor
                    MaskIndex = 0
                    MaterialValues = Vector4.UnitX })
        SlugShapeRuntime.createComposite data layers

    let private canonicalQuadrants = makeMaskedQuadrants false
    let private invertedQuadrants = makeMaskedQuadrants true

    let draw (world : World) =
        SlugDemoContours.placeComposite
            "MaskCanonicalQuadrants"
            canonicalQuadrants
            (v3 -74.0f -24.0f 0.0f)
            (v3 210.0f 210.0f 0.0f)
            Quaternion.Identity
            0.0f
            None
            world
        SlugDemoContours.placeComposite
            "MaskInvertedQuadrants"
            invertedQuadrants
            (v3 156.0f -24.0f 0.0f)
            (v3 126.0f 126.0f 0.0f)
            Quaternion.Identity
            0.0f
            None
            world
