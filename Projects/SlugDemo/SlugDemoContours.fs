// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace SlugDemo
open System
open System.Numerics
open Prime
open Nu

/// Renders a contour tessellation supplied by the ImSim declaration without rebuilding it.
type PrecomputedContourFacet () =
    inherit Facet (false, false, false)

    override this.Render (_, entity, world) =
        World.renderContour
            { Transform = entity.GetTransform world
              ClipOpt = entity.GetClipOpt world |> Option.toValueOption
              Tessellation = entity.GetTessellation world }
            world

/// An ImSim contour entity whose geometry is immutable after module initialization.
type PrecomputedContourDispatcher () =
    inherit Contour2dDispatcher (false, false, false)

    static member Facets =
        [typeof<PrecomputedContourFacet>]

    static member Properties =
        [define Entity.OverflowAbsolute true
         define Entity.Size Constants.Engine.Entity2dSizeDefault
         define Entity.ClipOpt None
         nonPersistent Entity.Tessellation ContourTessellation.empty]

/// A showcase path rendered directly from Slug curves when its fill rule is representable.
/// Its optional Nu tessellation contains only the anti-aliased stroke, which SlugShape does not yet encode.
type ShowcaseContour =
    | AnalyticSlug of Composite : SlugCompositeShape * Stroke : ContourTessellation option
    | TessellatedNu of ContourTessellation

[<RequireQualifiedAccess>]
module SlugDemoContours =

    let private cardFill = color 0.035f 0.060f 0.105f 1.0f
    let private cardStroke = color 0.16f 0.28f 0.40f 1.0f
    let private panelFill = color 0.055f 0.090f 0.145f 1.0f
    let private panelStroke = color 0.15f 0.25f 0.35f 1.0f
    let private axolotlFill = color 0.15f 0.72f 0.63f 1.0f
    let private axolotlStroke = color 0.57f 1.0f 0.88f 1.0f
    let private eyeFill = color 0.98f 0.80f 0.30f 1.0f
    let private eyeStroke = color 1.0f 0.94f 0.72f 1.0f
    let private sameFill = color 0.98f 0.38f 0.48f 1.0f
    let private oppositeFill = color 0.38f 0.70f 1.0f 1.0f
    let private windingStroke = color 0.70f 0.82f 0.94f 1.0f

    let roundedRectCommands radius =
        let kappa = 0.5522847498f
        let k = radius * kappa
        [| MoveTo (v2 (-0.5f + radius) 0.5f)
           CubicCurveTo (v2 (-0.5f + radius - k) 0.5f, v2 -0.5f (0.5f - radius + k), v2 -0.5f (0.5f - radius))
           LineTo (v2 -0.5f (-0.5f + radius))
           CubicCurveTo (v2 -0.5f (-0.5f + radius - k), v2 (-0.5f + radius - k) -0.5f, v2 (-0.5f + radius) -0.5f)
           LineTo (v2 (0.5f - radius) -0.5f)
           CubicCurveTo (v2 (0.5f - radius + k) -0.5f, v2 0.5f (-0.5f + radius - k), v2 0.5f (-0.5f + radius))
           LineTo (v2 0.5f (0.5f - radius))
           CubicCurveTo (v2 0.5f (0.5f - radius + k), v2 (0.5f - radius + k) 0.5f, v2 (0.5f - radius) 0.5f)
           CloseContour|]

    let circleCommands =
        let k = 0.5522847498f / 2.0f
        [| MoveTo (v2 0.5f 0.0f)
           CubicCurveTo (v2 0.5f k, v2 k 0.5f, v2 0.0f 0.5f)
           CubicCurveTo (v2 -k 0.5f, v2 -0.5f k, v2 -0.5f 0.0f)
           CubicCurveTo (v2 -0.5f -k, v2 -k -0.5f, v2 0.0f -0.5f)
           CubicCurveTo (v2 k -0.5f, v2 0.5f -k, v2 0.5f 0.0f)
           CloseContour|]

    // This silhouette uses one continuous closed path so the future Slug contour asset can
    // compare the exact same source outline against this tessellated fallback.
    let private axolotlCommands =
        [| MoveTo (v2 -0.47f 0.02f)
           CubicCurveTo (v2 -0.46f -0.23f, v2 -0.27f -0.39f, v2 -0.02f -0.36f)
           CubicCurveTo (v2 0.19f -0.40f, v2 0.34f -0.28f, v2 0.43f -0.10f)
           LineTo (v2 0.73f 0.02f)
           LineTo (v2 0.43f 0.16f)
           CubicCurveTo (v2 0.35f 0.32f, v2 0.20f 0.43f, v2 0.00f 0.40f)
           CubicCurveTo (v2 -0.26f 0.43f, v2 -0.47f 0.28f, v2 -0.47f 0.02f)
           CloseContour|]

    let makeFilled commands fillColor winding strokeColor strokeThickness scale =
        match winding with
        | NonZero
        | EvenOdd ->
            let fillRule = if winding = EvenOdd then SlugFillEvenOdd else SlugFillNonzero
            let source = SlugShapeRuntime.fromContourCommands commands fillRule 1.0e-3f
            let data = SlugShapeRuntime.pack [|source|]
            let state =
                { SlugLayerState.defaultState 0 with
                    Color = fillColor }
            let stroke =
                if strokeThickness <= 0.0f then None
                else
                    Some
                        (ContourTessellation.make
                            commands
                            (ContourFill.ofColorWinding Color.Zero winding)
                            (ContourStroke.antiAliased strokeColor strokeThickness)
                            scale)
            AnalyticSlug (SlugShapeRuntime.createComposite data [|state|], stroke)
        | _ ->
            TessellatedNu
                (ContourTessellation.make
                    commands
                    (ContourFill.ofColorWinding fillColor winding)
                    (ContourStroke.antiAliased strokeColor strokeThickness)
                    scale)

    let private cardTessellation =
        makeFilled (roundedRectCommands 0.09f) cardFill NonZero cardStroke 2.0f (v2 240.0f 200.0f)

    let private panelTessellation =
        makeFilled (roundedRectCommands 0.12f) panelFill NonZero panelStroke 1.5f (v2 115.0f 30.0f)

    let private axolotlTessellation =
        makeFilled axolotlCommands axolotlFill NonZero axolotlStroke 2.0f (v2 140.0f 91.0f)

    let private eyeTessellation =
        makeFilled circleCommands eyeFill Positive eyeStroke 1.5f (v2 10.0f 10.0f)

    // A clockwise outer loop and a clockwise inner loop demonstrate how the five Nu winding
    // modes classify a nested shape. The opposite-direction version reverses only the inner loop.
    let private windingOuterCommands =
        roundedRectCommands 0.22f

    let private windingInnerCommands =
        [| MoveTo (v2 -0.28f 0.22f)
           LineTo (v2 -0.28f -0.22f)
           LineTo (v2 0.28f -0.22f)
           LineTo (v2 0.28f 0.22f)
           CloseContour |]

    let private reverseClosedContour commands =
        let points =
            commands
            |> Array.choose (function
                | MoveTo point -> Some point
                | LineTo point -> Some point
                | _ -> None)
        Array.append
            [| MoveTo points.[points.Length - 1] |]
            (Array.append
                (Array.sub points 0 (points.Length - 1)
                 |> Array.rev
                 |> Array.map LineTo)
                [| CloseContour |])

    let private windingSameCommands =
        Array.append windingOuterCommands windingInnerCommands

    let private windingOppositeCommands =
        Array.append windingOuterCommands (reverseClosedContour windingInnerCommands)

    let private windingModes =
        [| EvenOdd; NonZero; Positive; Negative; AbsGeqTwo |]

    let private windingSameTessellations =
        windingModes
        |> Array.map (fun winding ->
            makeFilled windingSameCommands sameFill winding windingStroke 1.5f (v2 95.0f 24.0f))

    let private windingOppositeTessellations =
        windingModes
        |> Array.map (fun winding ->
            makeFilled windingOppositeCommands oppositeFill winding windingStroke 1.5f (v2 95.0f 24.0f))

    let getCompositeLayerBounds (composite : SlugCompositeShape) =
        if composite.LayerCount = 0 then struct (Vector2.Zero, Vector2.Zero)
        else
            let mutable minPoint = Vector2 (Single.PositiveInfinity, Single.PositiveInfinity)
            let mutable maxPoint = Vector2 (Single.NegativeInfinity, Single.NegativeInfinity)
            let includePoint point =
                minPoint <- Vector2.Min (minPoint, point)
                maxPoint <- Vector2.Max (maxPoint, point)
            for layerIndex in 0 .. composite.LayerCount - 1 do
                let layer = composite.Layers[layerIndex]
                let bounds = composite.Data.Metadata[layer.ShapeIndex].Bounds
                let transform = layer.Transform
                includePoint (Vector2.Transform (Vector2 (bounds.Min.X, bounds.Min.Y), transform))
                includePoint (Vector2.Transform (Vector2 (bounds.Max.X, bounds.Min.Y), transform))
                includePoint (Vector2.Transform (Vector2 (bounds.Max.X, bounds.Max.Y), transform))
                includePoint (Vector2.Transform (Vector2 (bounds.Min.X, bounds.Max.Y), transform))
            struct (minPoint, maxPoint)

    let private placeCompositeWithinBounds
        (name : string)
        (composite : SlugCompositeShape)
        (bounds : struct (Vector2 * Vector2))
        (position : Vector3)
        (size : Vector3)
        (rotation : Quaternion)
        (elevation : single)
        (computeConfigOpt : Vortice.Vulkan.SlugShape.SlugShapeComputeConfig option)
        (projective : Matrix4x4)
        (textureSlots : Image AssetTag array)
        (world : World) =
        let struct (minPoint, maxPoint) = bounds
        let extent = Vector2 (max 1.0e-6f (maxPoint.X - minPoint.X), max 1.0e-6f (maxPoint.Y - minPoint.Y))
        let scale = Vector3 (size.X / extent.X, size.Y / extent.Y, 1.0f)
        let center = (minPoint + maxPoint) * 0.5f
        let rotatedCenter = Vector3.Transform (Vector3 (center.X * scale.X, center.Y * scale.Y, 0.0f), rotation)
        let mutable transform =
            Transform.makeIntuitive false (position - rotatedCenter) scale Vector3.Zero size Vector3.Zero elevation
        transform.Rotation <- rotation
        World.renderSlugShape
            { Transform = transform
              ClipOpt = ValueNone
              Composite = composite
              Projective = projective
              Seconds = single (world.DateTime.TimeOfDay.TotalSeconds % 10000.0)
              Delta = world.GameDelta.SecondsF
              Frame = uint32 world.UpdateTime
              Seed = uint32 (abs (hash name))
              ComputeConfigOpt = computeConfigOpt
              TextureSlots = textureSlots }
            world

    let placeCompositeWithProjectiveAndTextures
        (name : string)
        (composite : SlugCompositeShape)
        (position : Vector3)
        (size : Vector3)
        (rotation : Quaternion)
        (elevation : single)
        (computeConfigOpt : Vortice.Vulkan.SlugShape.SlugShapeComputeConfig option)
        (projective : Matrix4x4)
        (textureSlots : Image AssetTag array)
        (world : World) =
        let bounds =
            if composite.Data.Metadata.Length = 0 then struct (Vector2.Zero, Vector2.Zero)
            else
                let mutable minPoint = Vector2 (Single.PositiveInfinity, Single.PositiveInfinity)
                let mutable maxPoint = Vector2 (Single.NegativeInfinity, Single.NegativeInfinity)
                for metadata in composite.Data.Metadata do
                    minPoint <- Vector2.Min (minPoint, metadata.Bounds.Min)
                    maxPoint <- Vector2.Max (maxPoint, metadata.Bounds.Max)
                struct (minPoint, maxPoint)
        placeCompositeWithinBounds
            name composite bounds position size rotation elevation computeConfigOpt projective textureSlots world

    let placeCompositeInBounds
        (name : string)
        (composite : SlugCompositeShape)
        (bounds : struct (Vector2 * Vector2))
        (position : Vector3)
        (size : Vector3)
        (rotation : Quaternion)
        (elevation : single)
        (computeConfigOpt : Vortice.Vulkan.SlugShape.SlugShapeComputeConfig option)
        (world : World) =
        placeCompositeWithinBounds
            name composite bounds position size rotation elevation computeConfigOpt Matrix4x4.Identity [||] world

    let placeCompositeWithTextures name composite position size rotation elevation computeConfigOpt textureSlots world =
        placeCompositeWithProjectiveAndTextures
            name composite position size rotation elevation computeConfigOpt Matrix4x4.Identity textureSlots world

    let placeProjectiveComposite name composite position size rotation elevation projective world =
        let centeredProjective =
            Matrix4x4.CreateTranslation (-position) *
            projective *
            Matrix4x4.CreateTranslation position
        placeCompositeWithProjectiveAndTextures
            name composite position size rotation elevation None centeredProjective [||] world

    let placeComposite name composite position size rotation elevation computeConfigOpt world =
        placeCompositeWithProjectiveAndTextures
            name composite position size rotation elevation computeConfigOpt Matrix4x4.Identity [||] world

    let placeContour name shape position size rotation elevation (world : World) =
        let placeTessellation suffix tessellation strokeElevation =
            World.doEntity<PrecomputedContourDispatcher>
                (name + suffix)
                [Entity.Position .= position
                 Entity.Size .= size
                 Entity.Rotation @= rotation
                 Entity.Elevation .= strokeElevation
                 Entity.Tessellation .= tessellation]
                world
        match shape with
        | AnalyticSlug (composite, strokeOpt) ->
            placeComposite name composite position size rotation elevation None world
            match strokeOpt with
            | Some stroke -> placeTessellation "Stroke" stroke (elevation + 0.001f)
            | None -> ()
        | TessellatedNu tessellation ->
            placeTessellation String.Empty tessellation elevation

    let private placeLabel name text (position : Vector3) (size : Vector3) color elevation world =
        World.doEntity<SlugTextDispatcher>
            name
            [Entity.Position .= position + v3 (size.X * 0.5f) 0.0f 0.0f
             Entity.Size .= size
             Entity.Elevation .= elevation
             Entity.FontSizing .= Some (if size.Y <= 16.0f then 7.0f elif size.Y <= 18.0f then 8.0f else 14.0f)
             Entity.TextColor .= color
             Entity.Text @= text]
            world

    let drawGallery (time : single) (world : World) : unit =
        let phase = time * 0.72f
        let wobble = MathF.Sin phase
        let cardRotation = Quaternion.CreateFromAxisAngle (Vector3.UnitZ, wobble * 0.018f)
        let axolotlRotation = Quaternion.CreateFromAxisAngle (Vector3.UnitZ, wobble * 0.045f)

        // The note is deliberately a SlugTextDispatcher label: the arbitrary paths below are
        // Nu's tessellated Contour renderer, not the font-only Slug text path.
        placeLabel
            "ContoursRendererNote"
            "ARBITRARY PATHS: Nu tessellated Contour renderer (not font-only Slug path)"
            (v3 -300.0f -157.0f 0.0f)
            (v3 600.0f 18.0f 0.0f)
            (color 0.60f 0.78f 0.90f 1.0f)
            20.0f
            world

        placeContour
            "ContoursCard"
            cardTessellation
            (v3 -185.0f -30.0f 0.0f)
            (v3 240.0f 200.0f 0.0f)
            cardRotation
            0.0f
            world

        placeContour
            "ContoursAxolotl"
            axolotlTessellation
            (v3 -185.0f -12.0f 0.0f)
            (v3 140.0f 91.0f 0.0f)
            axolotlRotation
            2.0f
            world

        placeContour
            "ContoursAxolotlEyeLeft"
            eyeTessellation
            (v3 -216.0f 1.0f 0.0f)
            (v3 10.0f 10.0f 0.0f)
            Quaternion.Identity
            3.0f
            world

        placeContour
            "ContoursAxolotlEyeRight"
            eyeTessellation
            (v3 -181.0f 1.0f 0.0f)
            (v3 10.0f 10.0f 0.0f)
            Quaternion.Identity
            3.0f
            world

        for index in 0 .. dec windingModes.Length do
            let rowY = 53.0f - single index * 34.0f
            let mode = windingModes.[index]
            let same = windingSameTessellations.[index]
            let opposite = windingOppositeTessellations.[index]
            let modeName =
                match mode with
                | EvenOdd -> "EVEN-ODD"
                | NonZero -> "NONZERO"
                | Positive -> "POSITIVE"
                | Negative -> "NEGATIVE"
                | AbsGeqTwo -> "ABS>=2"
            placeContour
                ("ContoursSamePanel" + string index)
                panelTessellation
                (v3 75.0f rowY 0.0f)
                (v3 115.0f 30.0f 0.0f)
                Quaternion.Identity
                0.0f
                world
            placeContour
                ("ContoursOppositePanel" + string index)
                panelTessellation
                (v3 235.0f rowY 0.0f)
                (v3 115.0f 30.0f 0.0f)
                Quaternion.Identity
                0.0f
                world
            placeContour
                ("ContoursSame" + string index)
                same
                (v3 75.0f rowY 0.0f)
                (v3 95.0f 24.0f 0.0f)
                Quaternion.Identity
                1.0f
                world
            placeContour
                ("ContoursOpposite" + string index)
                opposite
                (v3 235.0f rowY 0.0f)
                (v3 95.0f 24.0f 0.0f)
                Quaternion.Identity
                1.0f
                world
            placeLabel
                ("ContoursMode" + string index)
                modeName
                (v3 136.0f rowY 0.0f)
                (v3 38.0f 16.0f 0.0f)
                (color 0.58f 0.70f 0.82f 1.0f)
                12.0f
                world

        placeLabel
            "ContoursSameCaption"
            "same direction"
            (v3 18.0f 77.0f 0.0f)
            (v3 114.0f 16.0f 0.0f)
            (color 0.98f 0.55f 0.62f 1.0f)
            12.0f
            world
        placeLabel
            "ContoursOppositeCaption"
            "opposite direction"
            (v3 178.0f 77.0f 0.0f)
            (v3 114.0f 16.0f 0.0f)
            (color 0.55f 0.78f 1.0f 1.0f)
            12.0f
            world
