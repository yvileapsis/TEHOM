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


type SlugDemoContour =
    { Commands : ContourCommand array
      Bounds : Box2
      Fill : ContourFill
      Stroke : ContourStroke }

[<RequireQualifiedAccess>]
module SlugDemoContours =


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


    let private getCommandBounds commands =
        let mutable minPoint = Vector2 (Single.PositiveInfinity, Single.PositiveInfinity)
        let mutable maxPoint = Vector2 (Single.NegativeInfinity, Single.NegativeInfinity)
        let includePoint point =
            minPoint <- Vector2.Min (minPoint, point)
            maxPoint <- Vector2.Max (maxPoint, point)
        for command in commands do
            match command with
            | MoveTo point
            | LineTo point ->
                includePoint point
            | QuadraticCurveTo (control, point) ->
                includePoint control
                includePoint point
            | CubicCurveTo (control1, control2, point) ->
                includePoint control1
                includePoint control2
                includePoint point
            | CloseContour -> ()
        Box2 (minPoint, maxPoint - minPoint)


    let makeFilled commands fillColor winding strokeColor strokeThickness =
        { Commands = commands
          Bounds = getCommandBounds commands
          Fill = ContourFill.ofColorWinding fillColor winding
          Stroke = ContourStroke.ofColorThickness strokeColor strokeThickness }


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
        (computeConfigOpt : Nu.Vulkan.SlugShape.SlugShapeComputeConfig option)
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
        (computeConfigOpt : Nu.Vulkan.SlugShape.SlugShapeComputeConfig option)
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
        (computeConfigOpt : Nu.Vulkan.SlugShape.SlugShapeComputeConfig option)
        (world : World) =
        placeCompositeWithinBounds
            name composite bounds position size rotation elevation computeConfigOpt Matrix4x4.Identity [||] world

    let placeCompositeInBoundsWithTextures
        name
        composite
        bounds
        position
        size
        rotation
        elevation
        computeConfigOpt
        textureSlots
        world =
        placeCompositeWithinBounds
            name composite bounds position size rotation elevation computeConfigOpt Matrix4x4.Identity textureSlots world

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

    let placeContour contour position (size : Vector3) (rotation : Quaternion) elevation (world : World) =
        let extent = contour.Bounds.Size
        let scale = v2 (size.X / extent.X) (size.Y / extent.Y)
        let center = contour.Bounds.Min + extent * 0.5f
        let rotatedCenter =
            Vector3.Transform (v3 (center.X * scale.X) (center.Y * scale.Y) 0.0f, rotation)
        let mutable transform =
            Transform.makeIntuitive false (position - rotatedCenter) v3One Vector3.Zero size Vector3.Zero elevation
        transform.Rotation <- rotation
        World.renderContour
            { Transform = transform
              ClipOpt = ValueNone
              Contour = Contour.make contour.Fill contour.Stroke contour.Commands scale }
            world

