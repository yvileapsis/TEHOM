// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Vortice.Vulkan
open Prime
open Nu

[<Struct; StructLayout (LayoutKind.Explicit, Size = 288)>]
type VoxelFaceInstanceStruct =
    [<FieldOffset(0)>] val mutable modelViewProjection : Matrix4x4
    [<FieldOffset(64)>] val mutable voxelOrigin : Vector4
    [<FieldOffset(80)>] val mutable voxelSize : Vector4
    [<FieldOffset(96)>] val mutable albedo : Vector4
    [<FieldOffset(112)>] val mutable material : Vector4
    [<FieldOffset(128)>] val mutable heightPlus : Vector4
    [<FieldOffset(144)>] val mutable subsurfacePlus : Vector4
    [<FieldOffset(160)>] val mutable clearCoatPlus : Vector4
    [<FieldOffset(176)>] val mutable normalX : Vector4
    [<FieldOffset(192)>] val mutable normalY : Vector4
    [<FieldOffset(208)>] val mutable normalZ : Vector4
    [<FieldOffset(224)>] val mutable clipPlaneLocal : Vector4
    [<FieldOffset(240)>] val mutable ambientLight : Vector4
    [<FieldOffset(256)>] val mutable keyLightDirection : Vector4
    [<FieldOffset(272)>] val mutable keyLightColor : Vector4

/// A voxel model resident as packed, neighbor-exposed GPU faces.
type VoxelModelGpu =
    { Id : uint64
      Bounds : Box3
      VoxelSize : Vector3
      Origin : Vector3
      FaceCount : int
      CompactFaces : bool
      FaceBuffer : VulkanBuffer
      PaletteBuffer : VulkanBuffer }

/// Material values written by a voxel into the physically-based G-buffer.
type VoxelMaterial =
    { Albedo : Color
      Roughness : single
      Metallic : single
      AmbientOcclusion : single
      Emission : single
      Height : single
      IgnoreLightMaps : bool
      FinenessOffset : single
      ScatterType : single
      ClearCoat : single
      ClearCoatRoughness : single }

type VoxelPipeline =
    { InstanceUniform : VulkanBuffer
      IndexBuffer : VulkanBuffer
      Pipeline : Pipeline
      FirstPersonPipeline : Pipeline }

type VoxelPass =
    { ViewProjection : Matrix4x4
      AmbientLight : Vector3
      KeyLightDirection : Vector3
      KeyLightColor : Vector3 }

[<RequireQualifiedAccess>]
module Voxel =

    let private writeStorageDescriptor binding (buffer : VulkanBuffer) vkDescriptorSet =
        let mutable info = VkDescriptorBufferInfo ()
        info.buffer <- buffer.VkBuffer
        info.range <- Vulkan.VK_WHOLE_SIZE
        let mutable write = VkWriteDescriptorSet ()
        write.dstSet <- vkDescriptorSet
        write.dstBinding <- uint binding
        write.descriptorCount <- 1u
        write.descriptorType <- VkDescriptorType.StorageBuffer
        write.pBufferInfo <- &&info
        DeviceApi.vkUpdateDescriptorSets (1u, &&write, 0u, nullPtr)

    let private writeUniformDescriptor binding (buffer : VulkanBuffer) vkDescriptorSet =
        let mutable info = VkDescriptorBufferInfo ()
        info.buffer <- buffer.VkBuffer
        info.range <- Vulkan.VK_WHOLE_SIZE
        let mutable write = VkWriteDescriptorSet ()
        write.dstSet <- vkDescriptorSet
        write.dstBinding <- uint binding
        write.descriptorCount <- 1u
        write.descriptorType <- VkDescriptorType.UniformBuffer
        write.pBufferInfo <- &&info
        DeviceApi.vkUpdateDescriptorSets (1u, &&write, 0u, nullPtr)

    let private packGridValues indexBits volume (values : uint array) =
        let indicesPerWord = 32 / indexBits
        let packed = Array.zeroCreate<uint> ((volume + dec indicesPerWord) / indicesPerWord)
        let mask = if indexBits = 8 then 0xFFu else 0xFFFFu
        for i in 0 .. dec volume do
            packed[i / indicesPerWord] <-
                packed[i / indicesPerWord] |||
                ((values[i] &&& mask) <<< ((i % indicesPerWord) * indexBits))
        packed

    let private makeGridFromSplats (descriptor : VoxelModelDescriptor) =
        let size =
            v3i
                (int (MathF.Round (descriptor.Bounds.Size.X / descriptor.VoxelSize.X)))
                (int (MathF.Round (descriptor.Bounds.Size.Y / descriptor.VoxelSize.Y)))
                (int (MathF.Round (descriptor.Bounds.Size.Z / descriptor.VoxelSize.Z)))
        if size.X <= 0 || size.Y <= 0 || size.Z <= 0 || size.X > 64 || size.Y > 64 || size.Z > 64 then
            invalidArg (nameof descriptor) "Voxel grid dimensions must each be between 1 and 64."
        let volume = size.X * size.Y * size.Z
        let origin = descriptor.Bounds.Min + descriptor.VoxelSize * 0.5f
        let paletteIndices = Dictionary<Color, int> ()
        let palette = ResizeArray<Color> ()
        let values = Array.zeroCreate<uint> volume
        let quantize position origin voxelSize =
            let coordinate = int (MathF.Round ((position - origin) / voxelSize))
            let reconstructed = origin + single coordinate * voxelSize
            let tolerance = max 0.0001f (MathF.Abs voxelSize * 0.01f)
            if coordinate < 0 || coordinate > 63 || MathF.Abs (reconstructed - position) > tolerance then
                invalidArg (nameof descriptor) "Voxel splats must lie on the descriptor's 64-cubed voxel grid."
            coordinate
        for splat in descriptor.Splats do
            let mutable paletteIndex = 0
            if not (paletteIndices.TryGetValue (splat.Albedo, &paletteIndex)) then
                if palette.Count >= 65535 then invalidArg (nameof descriptor) "A voxel grid palette cannot exceed 65535 colors."
                paletteIndex <- palette.Count
                paletteIndices[splat.Albedo] <- paletteIndex
                palette.Add splat.Albedo
            let x = quantize splat.Position.X origin.X descriptor.VoxelSize.X
            let y = quantize splat.Position.Y origin.Y descriptor.VoxelSize.Y
            let z = quantize splat.Position.Z origin.Z descriptor.VoxelSize.Z
            values[x + size.X * (y + size.Y * z)] <- uint (inc paletteIndex)
        let indexBits = if palette.Count <= 255 then 8 else 16
        { Size = size
          Origin = origin
          IndexBits = indexBits
          Indices = packGridValues indexBits volume values
          Palette = palette.ToArray () }

    let private validateGrid (descriptor : VoxelModelDescriptor) (grid : VoxelGridDescriptor) =
        if grid.Size.X <= 0 || grid.Size.Y <= 0 || grid.Size.Z <= 0 ||
           grid.Size.X > 64 || grid.Size.Y > 64 || grid.Size.Z > 64 then
            invalidArg (nameof descriptor) "Voxel grid dimensions must each be between 1 and 64."
        if grid.IndexBits <> 8 && grid.IndexBits <> 16 then
            invalidArg (nameof descriptor) "Voxel grid indices must contain either 8 or 16 bits."
        let volume = grid.Size.X * grid.Size.Y * grid.Size.Z
        let indicesPerWord = 32 / grid.IndexBits
        let expectedLength = (volume + dec indicesPerWord) / indicesPerWord
        if grid.Indices.Length <> expectedLength then
            invalidArg (nameof descriptor) "Voxel grid index storage does not match its dimensions and index width."
        let paletteLimit = if grid.IndexBits = 8 then 255 else 65535
        if grid.Palette.Length > paletteLimit then
            invalidArg (nameof descriptor) "Voxel grid palette exceeds its packed index width."
        let mask = if grid.IndexBits = 8 then 0xFFu else 0xFFFFu
        let readIndex linearIndex =
            (grid.Indices[linearIndex / indicesPerWord] >>> ((linearIndex % indicesPerWord) * grid.IndexBits)) &&& mask
        let mutable occupiedCount = 0
        for linearIndex in 0 .. dec volume do
            let value = readIndex linearIndex
            if value > uint grid.Palette.Length then
                invalidArg (nameof descriptor) "Voxel grid contains an index outside its palette."
            if value <> 0u then occupiedCount <- inc occupiedCount
        let halfSize = descriptor.VoxelSize * 0.5f
        let gridMin = grid.Origin - halfSize
        let gridMax =
            grid.Origin +
            v3
                (single (dec grid.Size.X) * descriptor.VoxelSize.X)
                (single (dec grid.Size.Y) * descriptor.VoxelSize.Y)
                (single (dec grid.Size.Z) * descriptor.VoxelSize.Z) +
            halfSize
        let tolerance =
            Vector3
                (max 0.0001f (MathF.Abs descriptor.VoxelSize.X * 0.01f),
                 max 0.0001f (MathF.Abs descriptor.VoxelSize.Y * 0.01f),
                 max 0.0001f (MathF.Abs descriptor.VoxelSize.Z * 0.01f))
        if gridMin.X < descriptor.Bounds.Min.X - tolerance.X || gridMin.Y < descriptor.Bounds.Min.Y - tolerance.Y || gridMin.Z < descriptor.Bounds.Min.Z - tolerance.Z ||
           gridMax.X > descriptor.Bounds.Max.X + tolerance.X || gridMax.Y > descriptor.Bounds.Max.Y + tolerance.Y || gridMax.Z > descriptor.Bounds.Max.Z + tolerance.Z then
            invalidArg (nameof descriptor) "Voxel grid lies outside the descriptor bounds."
        if occupiedCount = 0 then invalidArg (nameof descriptor) "A voxel model must contain at least one occupied voxel."
        occupiedCount

    type private VoxelModelData =
        { Bounds : Box3
          VoxelSize : Vector3
          Origin : Vector3
          FaceCount : int
          CompactFaces : bool
          PackedFaces : uint array
          Palette : Vector4 array }

    let private makeModelData (descriptor : VoxelModelDescriptor) =
        if descriptor.VoxelSize.X <= 0.0f || descriptor.VoxelSize.Y <= 0.0f || descriptor.VoxelSize.Z <= 0.0f then
            invalidArg (nameof descriptor) "Voxel sizes must be positive."
        let inferredGridSize =
            v3i
                (int (MathF.Round (descriptor.Bounds.Size.X / descriptor.VoxelSize.X)))
                (int (MathF.Round (descriptor.Bounds.Size.Y / descriptor.VoxelSize.Y)))
                (int (MathF.Round (descriptor.Bounds.Size.Z / descriptor.VoxelSize.Z)))
        let gridOpt =
            match descriptor.Grid with
            | Some grid -> Some grid
            | None when
                inferredGridSize.X > 0 && inferredGridSize.X <= 64 &&
                inferredGridSize.Y > 0 && inferredGridSize.Y <= 64 &&
                inferredGridSize.Z > 0 && inferredGridSize.Z <= 64 ->
                Some (makeGridFromSplats descriptor)
            | None -> None
        gridOpt |> Option.iter (fun grid -> validateGrid descriptor grid |> ignore<int>)
        let paletteIndices = Dictionary<Color, int> ()
        let paletteColors = ResizeArray<Color> ()
        let addPaletteColor color =
            let mutable paletteIndex = 0
            if paletteIndices.TryGetValue (color, &paletteIndex) then paletteIndex
            else
                if paletteColors.Count >= 0x04000000 then
                    invalidArg (nameof descriptor) "A voxel palette cannot exceed 67108864 colors."
                paletteIndex <- paletteColors.Count
                paletteIndices[color] <- paletteIndex
                paletteColors.Add color
                paletteIndex
        match gridOpt with
        | Some grid ->
            for color in grid.Palette do addPaletteColor color |> ignore<int>
        | None -> ()
        for splat in descriptor.Splats do addPaletteColor splat.Albedo |> ignore<int>
        let palette =
            paletteColors
            |> Seq.map (fun color ->
                Vector4
                    (MathF.Pow (Math.Clamp (color.R, 0.0f, 1.0f), 2.2f),
                     MathF.Pow (Math.Clamp (color.G, 0.0f, 1.0f), 2.2f),
                     MathF.Pow (Math.Clamp (color.B, 0.0f, 1.0f), 2.2f),
                     color.A))
            |> Seq.toArray
        let faceCount =
            descriptor.Splats
            |> Array.sumBy (fun splat -> BitOperations.PopCount (uint splat.Faces &&& uint VoxelFaces.AllFaces))
        let compactFaces = Option.isSome gridOpt && palette.Length <= 2048
        let faceStride = if compactFaces then 1 else 2
        let packedFaces = Array.zeroCreate<uint> (max faceStride (faceCount * faceStride))
        let voxelOrigin = descriptor.Bounds.Min + descriptor.VoxelSize * 0.5f
        let quantize position origin voxelSize =
            let coordinate = int (MathF.Round ((position - origin) / voxelSize))
            let reconstructed = origin + single coordinate * voxelSize
            let tolerance = max 0.0001f (MathF.Abs voxelSize * 0.01f)
            if coordinate < 0 || coordinate > 1023 || MathF.Abs (reconstructed - position) > tolerance then
                invalidArg (nameof descriptor) "Voxel splats must lie on the descriptor's 1024-cubed voxel grid."
            coordinate
        let mutable faceIndex = 0
        for splat in descriptor.Splats do
            let paletteIndex = paletteIndices[splat.Albedo]
            let x = quantize splat.Position.X voxelOrigin.X descriptor.VoxelSize.X
            let y = quantize splat.Position.Y voxelOrigin.Y descriptor.VoxelSize.Y
            let z = quantize splat.Position.Z voxelOrigin.Z descriptor.VoxelSize.Z
            let packedPosition = uint x ||| (uint y <<< 10) ||| (uint z <<< 20)
            if compactFaces && (x > 63 || y > 63 || z > 63) then
                invalidArg (nameof descriptor) "Compact voxel faces must lie on the descriptor's 64-cubed voxel grid."
            for face in 0 .. 5 do
                if int splat.Faces &&& (1 <<< face) <> 0 then
                    if compactFaces then
                        let offset = faceIndex
                        packedFaces[offset] <-
                            uint x |||
                            (uint y <<< 6) |||
                            (uint z <<< 12) |||
                            (uint face <<< 18) |||
                            (uint paletteIndex <<< 21)
                    else
                        let offset = faceIndex * 2
                        packedFaces[offset] <- packedPosition
                        packedFaces[offset + 1] <- uint face ||| (uint paletteIndex <<< 3)
                    faceIndex <- inc faceIndex
        { Bounds = descriptor.Bounds
          VoxelSize = descriptor.VoxelSize
          Origin = voxelOrigin
          FaceCount = faceCount
          CompactFaces = compactFaces
          PackedFaces = packedFaces
          Palette = palette }

    let private createModelFromData data context =
        let faceBuffer = VulkanBuffer.create Storage (data.PackedFaces.Length * sizeof<uint>) context
        VulkanBuffer.uploadArray data.PackedFaces faceBuffer context
        let paletteBuffer = VulkanBuffer.create Storage (max sizeof<Vector4> (data.Palette.Length * sizeof<Vector4>)) context
        VulkanBuffer.uploadArray data.Palette paletteBuffer context
        { Id = Gen.id64
          Bounds = data.Bounds
          VoxelSize = data.VoxelSize
          Origin = data.Origin
          FaceCount = data.FaceCount
          CompactFaces = data.CompactFaces
          FaceBuffer = faceBuffer
          PaletteBuffer = paletteBuffer }

    let createModel descriptor context =
        createModelFromData (makeModelData descriptor) context

    /// Update reusable voxel GPU buffers in place, reallocating only when the new model exceeds their capacities.
    let updateModel descriptor (model : VoxelModelGpu) context =
        let data = makeModelData descriptor
        let canUpdateInPlace =
            data.PackedFaces.Length * sizeof<uint> <= model.FaceBuffer.Size &&
            data.Palette.Length * sizeof<Vector4> <= model.PaletteBuffer.Size
        if canUpdateInPlace then
            VulkanBuffer.uploadArray data.PackedFaces model.FaceBuffer context
            VulkanBuffer.uploadArray data.Palette model.PaletteBuffer context
            let updated =
                { model with
                    Bounds = data.Bounds
                    VoxelSize = data.VoxelSize
                    Origin = data.Origin
                    FaceCount = data.FaceCount
                    CompactFaces = data.CompactFaces }
            struct (updated, false)
        else struct (createModelFromData data context, true)

    let destroyModel (model : VoxelModelGpu) (context : VulkanContext) =
        VulkanBuffer.destroy model.FaceBuffer context
        VulkanBuffer.destroy model.PaletteBuffer context

    let private getColorAttachmentFormats (attachments : PhysicallyBasedAttachments) =
        let (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, z) = attachments.GeometryAttachments
        [|depth.VkFormat
          albedo.VkFormat
          material.VkFormat
          normalPlus.VkFormat
          subdermalPlus.VkFormat
          scatterPlus.VkFormat
          clearCoatPlus.VkFormat|], z

    let createPipeline attachments context =
        let instanceUniform = VulkanBuffer.create Uniform sizeof<VoxelFaceInstanceStruct> context
        let quadIndices = [|0u; 1u; 2u; 0u; 2u; 3u|]
        let indexBuffer = VulkanBuffer.createIndexStagedFromArray quadIndices context
        let colorAttachmentFormats, z = getColorAttachmentFormats attachments
        let descriptorDefinitions : DescriptorSetDefinition array =
            [|(Pipeline.descriptorSet<uint64>
                [|Pipeline.descriptor 0 StorageBuffer VertexStage 1
                  Pipeline.descriptor 1 StorageBuffer VertexStage 1|] :> DescriptorSetDefinition)
              (Pipeline.descriptorSet<int>
                [|Pipeline.descriptor 0 UniformBuffer VertexAndFragmentStage 1|] :> DescriptorSetDefinition)|]
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredVoxelShaderFilePath
                [|VulkanUnblended|]
                [|true|]
                [||]
                descriptorDefinitions
                [||]
                colorAttachmentFormats
                (Some z.VkFormat)
                [|instanceUniform|]
        let firstPersonPipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedFirstPersonVoxelShaderFilePath
                [|VulkanUnblended|]
                [|true|]
                [||]
                descriptorDefinitions
                [||]
                [|attachments.CompositionAttachment.VkFormat|]
                (Some z.VkFormat)
                [|instanceUniform|]
        { InstanceUniform = instanceUniform
          IndexBuffer = indexBuffer
          Pipeline = pipeline
          FirstPersonPipeline = firstPersonPipeline }

    let beginFrame (pipeline : VoxelPipeline) =
        Pipeline.beginFrame pipeline.Pipeline
        Pipeline.beginFrame pipeline.FirstPersonPipeline

    let beginDeferred
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (colorAttachments : VkImageView array)
        (depthAttachment : Texture)
        (resolution : Vector2i)
        (context : VulkanContext) =
        let projection = projectionUnflipped.Flipped
        let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
        let mutable viewport = Hl.makeViewport false renderArea
        Hl.withRenderingInfo colorAttachments (Some depthAttachment.ImageView) renderArea None $ fun renderingInfo ->
            let mutable renderingInfo = renderingInfo
            DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
        DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&viewport)
        DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)
        { ViewProjection = view * projection
          AmbientLight = v3Zero
          KeyLightDirection = v3Forward
          KeyLightColor = v3Zero }

    let beginFirstPerson
        (projectionUnflipped : Matrix4x4)
        (ambientLight : Vector3)
        (keyLightDirection : Vector3)
        (keyLightColor : Vector3)
        (colorAttachment : Texture)
        (depthAttachment : Texture)
        (resolution : Vector2i)
        (context : VulkanContext) =
        let projection = projectionUnflipped.Flipped
        let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
        let mutable viewport = Hl.makeViewport false renderArea
        let mutable renderingInfo =
            Hl.makeRenderingInfoWithDepthClear
                [|colorAttachment.ImageView|] depthAttachment.ImageView renderArea
        DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
        DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&viewport)
        DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)
        { ViewProjection = projection
          AmbientLight = ambientLight
          KeyLightDirection = keyLightDirection
          KeyLightColor = keyLightColor }

    let private bindDescriptorSets modelDescriptorSet instanceDescriptorSet (pipeline : Pipeline) (context : VulkanContext) =
        let mutable modelDescriptorSet = modelDescriptorSet
        let mutable instanceDescriptorSet = instanceDescriptorSet
        DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 0u, 1u, &&modelDescriptorSet, 0u, nullPtr)
        DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 1u, 1u, &&instanceDescriptorSet, 0u, nullPtr)

    let private normalizedOr fallback (value : Vector3) =
        if value.LengthSquared () > 0.0000001f then value.Normalized else fallback

    let private draw
        (modelMatrix : Matrix4x4)
        depthCutoff
        (materialProperties : VoxelMaterial)
        (clipPlaneLocal : Vector4)
        (model : VoxelModelGpu)
        (pass : VoxelPass)
        (renderPipeline : Pipeline)
        (pipeline : VoxelPipeline)
        (context : VulkanContext) =
        if model.FaceCount > 0 then
            match Pipeline.tryGetVkPipeline VulkanUnblended true renderPipeline with
            | Some vkPipeline ->
                let modelDescriptorSet = Pipeline.specifyDescriptorSet 0 model.Id renderPipeline $ fun vkSet ->
                    writeStorageDescriptor 0 model.FaceBuffer vkSet
                    writeStorageDescriptor 1 model.PaletteBuffer vkSet
                let axisX = Vector3.TransformNormal (v3Right, modelMatrix)
                let axisY = Vector3.TransformNormal (v3Up, modelMatrix)
                let axisZ = Vector3.TransformNormal (v3Forward, modelMatrix)
                let mutable normalX = normalizedOr v3Right (Vector3.Cross (axisY, axisZ))
                let mutable normalY = normalizedOr v3Up (Vector3.Cross (axisZ, axisX))
                let mutable normalZ = normalizedOr v3Forward (Vector3.Cross (axisX, axisY))
                if Vector3.Dot (normalX, axisX) < 0.0f then normalX <- -normalX
                if Vector3.Dot (normalY, axisY) < 0.0f then normalY <- -normalY
                if Vector3.Dot (normalZ, axisZ) < 0.0f then normalZ <- -normalZ
                let ignoreLightMaps = if materialProperties.IgnoreLightMaps then 1.0f else 0.0f
                let instance =
                    VoxelFaceInstanceStruct
                        (modelViewProjection = modelMatrix * pass.ViewProjection,
                         voxelOrigin = Vector4 (model.Origin, 0.0f),
                         voxelSize = Vector4 (model.VoxelSize, if model.CompactFaces then 1.0f else -1.0f),
                         albedo = Vector4 (materialProperties.Albedo.R, materialProperties.Albedo.G, materialProperties.Albedo.B, materialProperties.Albedo.A),
                         material = Vector4 (materialProperties.Roughness, materialProperties.Metallic, materialProperties.AmbientOcclusion, materialProperties.Emission),
                         heightPlus = Vector4 (materialProperties.Height, ignoreLightMaps, depthCutoff, 0.0f),
                         subsurfacePlus = Vector4 (materialProperties.FinenessOffset, materialProperties.ScatterType, 0.0f, 0.0f),
                         clearCoatPlus = Vector4 (materialProperties.ClearCoat, materialProperties.ClearCoatRoughness, 0.0f, 0.0f),
                         normalX = Vector4 (normalX, 0.0f),
                         normalY = Vector4 (normalY, 0.0f),
                         normalZ = Vector4 (normalZ, 0.0f),
                         clipPlaneLocal = clipPlaneLocal,
                         ambientLight = Vector4 (pass.AmbientLight, 0.0f),
                         keyLightDirection = Vector4 (pass.KeyLightDirection, 0.0f),
                         keyLightColor = Vector4 (pass.KeyLightColor, 0.0f))
                let instanceDescriptorSet = Pipeline.specifyDescriptorSet 1 renderPipeline.DrawIndex renderPipeline $ fun vkSet ->
                    VulkanBuffer.uploadValue instance pipeline.InstanceUniform context
                    Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.InstanceUniform vkSet
                DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                DeviceApi.vkCmdSetDepthTestEnable (context.RenderCommandBuffer, true)
                DeviceApi.vkCmdSetDepthCompareOp (context.RenderCommandBuffer, VkCompareOp.Less)
                bindDescriptorSets modelDescriptorSet instanceDescriptorSet renderPipeline context
                DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, pipeline.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)
                DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, 6u, uint model.FaceCount, 0u, 0, 0u)
                Hl.reportDrawCall model.FaceCount false
                Pipeline.advance renderPipeline
            | None -> Log.warnOnce "Cannot draw voxel faces because the Vulkan voxel face pipeline does not exist."

    let drawDeferred modelMatrix depthCutoff materialProperties (clipPlane : Vector4) model pass pipeline context =
        let clipPlaneLocal = Vector4.Transform (clipPlane, Matrix4x4.Transpose modelMatrix)
        draw modelMatrix depthCutoff materialProperties clipPlaneLocal model pass pipeline.Pipeline pipeline context

    let drawFirstPerson modelMatrix materialProperties model pass pipeline context =
        draw modelMatrix Single.MaxValue materialProperties Vector4.UnitW model pass pipeline.FirstPersonPipeline pipeline context

    let endDeferred (context : VulkanContext) =
        DeviceApi.vkCmdEndRendering context.RenderCommandBuffer
        Hl.reportDrawScope ()
        VulkanContext.advanceRenderCommandBuffer context

    let reloadShaders (pipeline : VoxelPipeline) context =
        Pipeline.reloadShaders pipeline.Pipeline context
        Pipeline.reloadShaders pipeline.FirstPersonPipeline context

    let destroyPipeline (pipeline : VoxelPipeline) context =
        Pipeline.destroy pipeline.Pipeline context
        Pipeline.destroy pipeline.FirstPersonPipeline context
        VulkanBuffer.destroy pipeline.IndexBuffer context
        VulkanBuffer.destroy pipeline.InstanceUniform context
