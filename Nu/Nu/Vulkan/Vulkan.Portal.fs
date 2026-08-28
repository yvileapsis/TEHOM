// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Vortice.Vulkan
open Prime
open Nu

[<Struct; StructLayout (LayoutKind.Explicit, Size = 160)>]
type PortalStruct =
    [<FieldOffset(0)>] val mutable viewProjection : Matrix4x4
    [<FieldOffset(64)>] val mutable model : Matrix4x4
    [<FieldOffset(128)>] val mutable viewport : Vector4
    [<FieldOffset(144)>] val mutable tint : Vector4

/// Pipeline state for projecting recursive portal views onto portal apertures.
type PortalPipeline =
    { Uniform : VulkanBuffer
      Pipeline : Pipeline }

[<RequireQualifiedAccess>]
module Portal =

    let createPipeline colorFormat depthFormat (context : VulkanContext) =
        let uniform = VulkanBuffer.create Uniform sizeof<PortalStruct> context
        let descriptorDefinitions : DescriptorSetDefinition array =
            [|(Pipeline.descriptorSet<int>
                [|Pipeline.descriptor 0 UniformBuffer VertexAndFragmentStage 1
                  Pipeline.descriptor 1 CombinedImageSampler FragmentStage 1|] :> DescriptorSetDefinition)|]
        let pipeline =
            Pipeline.create
                Constants.Paths.PortalShaderFilePath
                [|VulkanUnblended|]
                [|false|]
                [||]
                descriptorDefinitions
                [||]
                [|colorFormat|]
                (Some depthFormat)
                [|uniform|]
        { Uniform = uniform; Pipeline = pipeline }

    let beginFrame pipeline =
        Pipeline.beginFrame pipeline.Pipeline

    let draw
        (viewProjection : Matrix4x4)
        (resolution : Vector2i)
        (model : Matrix4x4)
        (tint : Color)
        fillOnly
        (portalTexture : Texture)
        (sampler : Sampler)
        (colorAttachment : Texture)
        (depthAttachment : Texture)
        (pipeline : PortalPipeline)
        (context : VulkanContext) =
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->
            let fillOnlyValue = if fillOnly then 1.0f else 0.0f
            let portal =
                PortalStruct
                    (viewProjection = viewProjection,
                     model = model,
                     viewport = Vector4 (single resolution.X, single resolution.Y, fillOnlyValue, 0.0f),
                     tint = tint.V4)
            let mutable descriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                VulkanBuffer.uploadValue portal pipeline.Uniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.Uniform vkSet
                Pipeline.writeDescriptorCombinedTextureSampler 1 0 portalTexture sampler vkSet
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable viewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] (Some depthAttachment.ImageView) renderArea None $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&viewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
            DeviceApi.vkCmdSetDepthTestEnable (context.RenderCommandBuffer, true)
            DeviceApi.vkCmdSetDepthCompareOp (context.RenderCommandBuffer, VkCompareOp.LessOrEqual)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&descriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdDraw (context.RenderCommandBuffer, 6u, 1u, 0u, 0u)
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer
            Hl.reportDrawCall 1 false
            Hl.reportDrawScope ()
            Pipeline.advance pipeline.Pipeline
            VulkanContext.advanceRenderCommandBuffer context
        | None -> Log.warnOnce "Cannot draw a portal because the Vulkan portal pipeline does not exist."

    let reloadShaders pipeline context =
        Pipeline.reloadShaders pipeline.Pipeline context

    let destroyPipeline pipeline context =
        Pipeline.destroy pipeline.Pipeline context
