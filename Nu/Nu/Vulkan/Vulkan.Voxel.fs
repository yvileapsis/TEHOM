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

[<Struct; StructLayout (LayoutKind.Explicit, Size = 208)>]
type VoxelInstanceStruct =
    [<FieldOffset(0)>] val mutable model : Matrix4x4
    [<FieldOffset(64)>] val mutable voxelOrigin : Vector4
    [<FieldOffset(80)>] val mutable voxelSize : Vector4
    [<FieldOffset(96)>] val mutable albedo : Vector4
    [<FieldOffset(112)>] val mutable material : Vector4
    [<FieldOffset(128)>] val mutable heightPlus : Vector4
    [<FieldOffset(144)>] val mutable subsurfacePlus : Vector4
    [<FieldOffset(160)>] val mutable clearCoatPlus : Vector4
    [<FieldOffset(176)>] val mutable viewport : Vector4
    [<FieldOffset(192)>] val mutable clipPlane : Vector4

/// A voxel splat model resident in GPU storage buffers.
type VoxelModelGpu =
    { Id : uint64
      Bounds : Box3
      VoxelSize : Vector3
      Origin : Vector3
      SplatCount : int
      SplatBuffer : VulkanBuffer
      PaletteBuffer : VulkanBuffer }

/// Material values written by a voxel splat into the physically-based G-buffer.
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

/// Pipeline state for direct voxel splatting into the physically-based G-buffer.
type VoxelPipeline =
    { EyeUniform : VulkanBuffer
      InstanceUniform : VulkanBuffer
      Pipeline : Pipeline }

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

    let createModel (descriptor : VoxelModelDescriptor) context =
        if descriptor.Splats.Length = 0 then invalidArg (nameof descriptor) "A voxel model must contain at least one splat."
        if descriptor.VoxelSize.X <= 0.0f || descriptor.VoxelSize.Y <= 0.0f || descriptor.VoxelSize.Z <= 0.0f then
            invalidArg (nameof descriptor) "Voxel sizes must be positive."
        let origin = descriptor.Bounds.Min + descriptor.VoxelSize * 0.5f
        let paletteIndices = Dictionary<Color, int> ()
        let palette = ResizeArray<Vector4> ()
        let packedSplats = Array.zeroCreate<uint> descriptor.Splats.Length
        let quantize position origin size =
            let coordinate = int (MathF.Round ((position - origin) / size))
            let reconstructed = origin + single coordinate * size
            let tolerance = max 0.0001f (size * 0.01f)
            if coordinate < 0 || coordinate > 63 || MathF.Abs (reconstructed - position) > tolerance then
                invalidArg (nameof descriptor) "Voxel splats must lie on the descriptor's 64-cubed voxel grid."
            coordinate
        for i in 0 .. dec descriptor.Splats.Length do
            let splat = descriptor.Splats[i]
            let mutable paletteIndex = 0
            if not (paletteIndices.TryGetValue (splat.Albedo, &paletteIndex)) then
                if palette.Count >= 0x4000 then invalidArg (nameof descriptor) "A voxel model palette cannot exceed 16384 colors."
                paletteIndex <- palette.Count
                paletteIndices[splat.Albedo] <- paletteIndex
                palette.Add (Vector4 (splat.Albedo.R, splat.Albedo.G, splat.Albedo.B, splat.Albedo.A))
            let x = quantize splat.Position.X origin.X descriptor.VoxelSize.X
            let y = quantize splat.Position.Y origin.Y descriptor.VoxelSize.Y
            let z = quantize splat.Position.Z origin.Z descriptor.VoxelSize.Z
            packedSplats[i] <- uint x ||| (uint y <<< 6) ||| (uint z <<< 12) ||| (uint paletteIndex <<< 18)
        let splatBuffer = VulkanBuffer.create Storage (packedSplats.Length * sizeof<uint>) context
        VulkanBuffer.uploadArray packedSplats splatBuffer context
        let paletteBuffer = VulkanBuffer.create Storage (palette.Count * sizeof<Vector4>) context
        VulkanBuffer.uploadArray (palette.ToArray ()) paletteBuffer context
        { Id = Gen.id64
          Bounds = descriptor.Bounds
          VoxelSize = descriptor.VoxelSize
          Origin = origin
          SplatCount = packedSplats.Length
          SplatBuffer = splatBuffer
          PaletteBuffer = paletteBuffer }

    let destroyModel (model : VoxelModelGpu) (context : VulkanContext) =
        VulkanBuffer.destroy model.SplatBuffer context
        VulkanBuffer.destroy model.PaletteBuffer context

    let createPipeline (attachments : PhysicallyBasedAttachments) (context : VulkanContext) =
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let instanceUniform = VulkanBuffer.create Uniform sizeof<VoxelInstanceStruct> context
        let (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, z) = attachments.GeometryAttachments
        let colorAttachmentFormats =
            [|depth.VkFormat
              albedo.VkFormat
              material.VkFormat
              normalPlus.VkFormat
              subdermalPlus.VkFormat
              scatterPlus.VkFormat
              clearCoatPlus.VkFormat|]
        let descriptorDefinitions : DescriptorSetDefinition array =
            [|(Pipeline.descriptorSet<int>
                [|Pipeline.descriptor 0 UniformBuffer VertexAndFragmentStage 1|] :> DescriptorSetDefinition)
              (Pipeline.descriptorSet<uint64>
                [|Pipeline.descriptor 0 StorageBuffer VertexStage 1
                  Pipeline.descriptor 1 StorageBuffer VertexStage 1|] :> DescriptorSetDefinition)
              (Pipeline.descriptorSet<int>
                [|Pipeline.descriptor 0 UniformBuffer VertexAndFragmentStage 1|] :> DescriptorSetDefinition)|]
        let pipeline =
            Pipeline.create
                Constants.Paths.PhysicallyBasedDeferredVoxelShaderFilePath
                [|VulkanUnblended|]
                [|false|]
                [||]
                descriptorDefinitions
                [||]
                colorAttachmentFormats
                (Some z.VkFormat)
                [|eyeUniform; instanceUniform|]
        { EyeUniform = eyeUniform
          InstanceUniform = instanceUniform
          Pipeline = pipeline }

    let beginFrame (pipeline : VoxelPipeline) =
        Pipeline.beginFrame pipeline.Pipeline

    let beginDeferred
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (colorAttachments : VkImageView array)
        (depthAttachment : Texture)
        (resolution : Vector2i)
        (renderPassIndex : int)
        (pipeline : VoxelPipeline)
        (context : VulkanContext) =
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection
        let mutable eyeDescriptorSet = Pipeline.specifyDescriptorSet 0 renderPassIndex pipeline.Pipeline $ fun vkSet ->
            let eye =
                EyeStruct
                    (center = eyeCenter,
                     view = view,
                     viewInverse = viewInverse,
                     projection = projection,
                     projectionInverse = projectionInverse,
                     viewProjection = viewProjection)
            VulkanBuffer.uploadValue eye pipeline.EyeUniform context
            Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet
        let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
        let mutable viewport = Hl.makeViewport false renderArea
        let mutable renderingInfo = Hl.makeRenderingInfo colorAttachments (Some depthAttachment.ImageView) renderArea None
        DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
        DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&viewport)
        DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)
        eyeDescriptorSet

    let drawDeferred
        (modelMatrix : Matrix4x4)
        (depthCutoff : single)
        (materialProperties : VoxelMaterial)
        (resolution : Vector2i)
        (clipPlane : Vector4)
        (model : VoxelModelGpu)
        (eyeDescriptorSet : VkDescriptorSet)
        (pipeline : VoxelPipeline)
        (context : VulkanContext) =

        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->
            let modelDescriptorSet = Pipeline.specifyDescriptorSet 1 model.Id pipeline.Pipeline $ fun vkSet ->
                writeStorageDescriptor 0 model.SplatBuffer vkSet
                writeStorageDescriptor 1 model.PaletteBuffer vkSet
            let ignoreLightMaps = if materialProperties.IgnoreLightMaps then 1.0f else 0.0f
            let instance =
                VoxelInstanceStruct
                    (model = modelMatrix,
                     voxelOrigin = Vector4 (model.Origin, 0.0f),
                     voxelSize = Vector4 (model.VoxelSize, 0.0f),
                     albedo = Vector4 (materialProperties.Albedo.R, materialProperties.Albedo.G, materialProperties.Albedo.B, materialProperties.Albedo.A),
                     material = Vector4 (materialProperties.Roughness, materialProperties.Metallic, materialProperties.AmbientOcclusion, materialProperties.Emission),
                     heightPlus = Vector4 (materialProperties.Height, ignoreLightMaps, depthCutoff, 0.0f),
                     subsurfacePlus = Vector4 (materialProperties.FinenessOffset, materialProperties.ScatterType, 0.0f, 0.0f),
                     clearCoatPlus = Vector4 (materialProperties.ClearCoat, materialProperties.ClearCoatRoughness, 0.0f, 0.0f),
                     viewport = Vector4 (0.0f, 0.0f, single resolution.X, single resolution.Y),
                     clipPlane = clipPlane)
            let instanceDescriptorSet = Pipeline.specifyDescriptorSet 2 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                VulkanBuffer.uploadValue instance pipeline.InstanceUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.InstanceUniform vkSet
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
            DeviceApi.vkCmdSetDepthTestEnable (context.RenderCommandBuffer, true)
            DeviceApi.vkCmdSetDepthCompareOp (context.RenderCommandBuffer, VkCompareOp.Less)
            let mutable eyeDescriptorSet = eyeDescriptorSet
            let mutable modelDescriptorSet = modelDescriptorSet
            let mutable instanceDescriptorSet = instanceDescriptorSet
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&eyeDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&modelDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, &&instanceDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdDraw (context.RenderCommandBuffer, 6u, uint model.SplatCount, 0u, 0u)
            Hl.reportDrawCall model.SplatCount false
            Pipeline.advance pipeline.Pipeline
        | None -> Log.warnOnce "Cannot draw voxel splats because the Vulkan voxel pipeline does not exist."

    let endDeferred (context : VulkanContext) =
        DeviceApi.vkCmdEndRendering context.RenderCommandBuffer
        Hl.reportDrawScope ()
        VulkanContext.advanceRenderCommandBuffer context

    let reloadShaders (pipeline : VoxelPipeline) context =
        Pipeline.reloadShaders pipeline.Pipeline context

    let destroyPipeline (pipeline : VoxelPipeline) context =
        Pipeline.destroy pipeline.Pipeline context
