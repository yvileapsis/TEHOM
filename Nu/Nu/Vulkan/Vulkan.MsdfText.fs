// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Vortice.Vulkan
open Prime
open Nu

[<RequireQualifiedAccess>]
module MsdfText =

    [<Struct; StructLayout (LayoutKind.Explicit)>]
    type Glyph =
        [<FieldOffset(0)>] val mutable perimeter : Vector4
        [<FieldOffset(16)>] val mutable texCoords : Vector4
        [<FieldOffset(32)>] val mutable color : Vector4
        [<FieldOffset(48)>] val mutable outlineColor : Vector4
        [<FieldOffset(64)>] val mutable shader : Vector4
        [<FieldOffset(80)>] val mutable shader2 : Vector4

    [<Struct; StructLayout (LayoutKind.Explicit)>]
    type ViewProjection =
        [<FieldOffset(0)>] val mutable viewProjection : Matrix4x4

    type [<Struct>] private MsdfTextBatchState =
        { Absolute : bool
          ClipOpt : Box2 voption
          Blend : VulkanBlend
          TextureOpt : Texture voption }

        static member inline changed state state2 =
            state.Absolute <> state2.Absolute ||
            (match struct (state.ClipOpt, state2.ClipOpt) with
             | struct (ValueSome _, ValueNone) -> true
             | struct (ValueNone, ValueSome _) -> true
             | struct (ValueNone, ValueNone) -> false
             | struct (ValueSome c, ValueSome c2) -> c <> c2) ||
            state.Blend <> state2.Blend ||
            (match struct (state.TextureOpt, state2.TextureOpt) with
             | struct (ValueSome _, ValueNone) -> true
             | struct (ValueNone, ValueSome _) -> true
             | struct (ValueNone, ValueNone) -> false
             | struct (ValueSome t, ValueSome t2) -> t <> t2)

        static member inline make absolute clipOpt blend texture =
            { Absolute = absolute
              ClipOpt = clipOpt
              Blend = blend
              TextureOpt = ValueSome texture }

        static member defaultState =
            { Absolute = false
              ClipOpt = ValueNone
              Blend = VulkanTransparent
              TextureOpt = ValueNone }

    /// The environment that contains the internal state required for batching MSDF text.
    type [<ReferenceEquality>] MsdfTextBatchEnv =
        private
            { mutable GlyphIndex : int
              mutable ViewProjection2dAbsolute : Matrix4x4
              mutable ViewProjection2dRelative : Matrix4x4
              mutable ViewProjectionClipAbsolute : Matrix4x4
              mutable ViewProjectionClipRelative : Matrix4x4
              VulkanContext : VulkanContext
              Pipeline : Pipeline
              FilteredSampler : Sampler
              GlyphUniform : VulkanBuffer
              ViewProjectionUniform : VulkanBuffer
              Glyphs : Glyph array
              mutable State : MsdfTextBatchState }

    let private CreateMsdfTextBatchPipeline (context : VulkanContext) =
        let glyphUniform = VulkanBuffer.create Storage (Constants.Render.SpriteBatchSize * sizeof<Glyph>) context
        let viewProjectionUniform = VulkanBuffer.create Storage sizeof<ViewProjection> context
        let pipeline =
            Pipeline.create
                Constants.Paths.MsdfTextShaderFilePath
                [|VulkanTransparent; VulkanAdditive; VulkanOverwrite|]
                [|true|]
                [||]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer VertexStage 1
                      Pipeline.descriptor 1 StorageBuffer VertexStage 1
                      Pipeline.descriptor 2 SampledImage FragmentStage 1|]
                  Pipeline.descriptorSet<Sampler>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||]
                [|context.SwapFormat|]
                None
                [|glyphUniform; viewProjectionUniform|]
        glyphUniform, viewProjectionUniform, pipeline

    /// Reload the shaders used by the environment.
    let ReloadShaders env context =
        Pipeline.reloadShaders env.Pipeline context

    let private BeginMsdfTextBatch state env =
        env.State <- state

    let private EndMsdfTextBatch (viewport : Viewport) env =
        match env.State.TextureOpt with
        | ValueSome texture when env.GlyphIndex > 0 ->
            let context = env.VulkanContext
            let pixelDensity = Hl.getWindowPixelDensity context.Window
            let renderAreaLogical =
                VkRect2D
                    (viewport.Inner.Min.X,
                     viewport.Outer.Max.Y - viewport.Inner.Max.Y,
                     uint viewport.Inner.Size.X,
                     uint viewport.Inner.Size.Y)
            let mutable renderArea = Hl.scaleRectForPixelDensity pixelDensity renderAreaLogical
            let mutable vkViewport = Hl.makeViewport true renderArea
            let mutable scissor = renderArea
            match env.State.ClipOpt with
            | ValueSome clip ->
                let viewProjection = if env.State.Absolute then env.ViewProjectionClipAbsolute else env.ViewProjectionClipRelative
                let minClip4 = System.Numerics.Vector4.Transform (System.Numerics.Vector4 (clip.Min.X, clip.Max.Y, 0.0f, 1.0f), viewProjection)
                let minClip = System.Numerics.Vector2 (minClip4.X, minClip4.Y)
                let minNdc = minClip * single viewport.DisplayScalar
                let viewportSize = System.Numerics.Vector2 (single viewport.Inner.Size.X, single viewport.Inner.Size.Y)
                let minScissor = (minNdc + v2One) * 0.5f * viewportSize
                let sizeClip4 = System.Numerics.Vector4.Transform (System.Numerics.Vector4 (clip.Size, 0.0f, 1.0f), viewProjection)
                let sizeClip = System.Numerics.Vector2 (sizeClip4.X, sizeClip4.Y)
                let sizeNdc = sizeClip * single viewport.DisplayScalar
                let sizeScissor = sizeNdc * 0.5f * viewportSize
                let offset = v2i viewport.Inner.Min.X (viewport.Outer.Max.Y - viewport.Inner.Max.Y)
                let scissorLogical =
                    VkRect2D
                        ((minScissor.X |> round |> int) + offset.X,
                         (single renderAreaLogical.extent.height - minScissor.Y |> round |> int) + offset.Y,
                         uint sizeScissor.X,
                         uint sizeScissor.Y)
                scissor <- Hl.scaleRectForPixelDensity pixelDensity scissorLogical
                scissor <- Hl.clipRect renderArea scissor
            | ValueNone -> ()
            if Hl.validateRect scissor then
                match Pipeline.tryGetVkPipeline env.State.Blend true env.Pipeline with
                | Some vkPipeline ->
                    let mutable mainDescriptorSet =
                        Pipeline.specifyDescriptorSet 0 env.Pipeline.DrawIndex env.Pipeline $ fun vkSet ->
                            VulkanBuffer.uploadArrayCount env.GlyphIndex env.Glyphs env.GlyphUniform context
                            Pipeline.writeDescriptorStorageBuffer 0 0 env.GlyphUniform vkSet
                            let mutable viewProjection = ViewProjection ()
                            viewProjection.viewProjection <- if env.State.Absolute then env.ViewProjection2dAbsolute else env.ViewProjection2dRelative
                            VulkanBuffer.uploadValue viewProjection env.ViewProjectionUniform context
                            Pipeline.writeDescriptorStorageBuffer 1 0 env.ViewProjectionUniform vkSet
                            Pipeline.writeDescriptorSampledTexture 2 0 texture vkSet
                    let mutable samplerDescriptorSet =
                        Pipeline.specifyDescriptorSet 1 env.FilteredSampler env.Pipeline $ fun vkSet ->
                            Pipeline.writeDescriptorSampler 0 0 env.FilteredSampler vkSet
                    let mutable renderingInfo = Hl.makeRenderingInfo [|context.SwapchainImageView|] None renderArea None
                    DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
                    DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
                    DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&scissor)
                    DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                    DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 0u, 1u, &&mainDescriptorSet, 0u, nullPtr)
                    DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 1u, 1u, &&samplerDescriptorSet, 0u, nullPtr)
                    DeviceApi.vkCmdDraw (context.RenderCommandBuffer, uint (6 * env.GlyphIndex), 1u, 0u, 0u)
                    DeviceApi.vkCmdEndRendering context.RenderCommandBuffer
                    Hl.reportDrawCall env.GlyphIndex true
                    Pipeline.advance env.Pipeline
                    VulkanContext.advanceRenderCommandBuffer context
                | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."
            env.GlyphIndex <- 0
        | ValueSome _ | ValueNone -> ()

    let private RestartMsdfTextBatch state viewport env =
        EndMsdfTextBatch viewport env
        BeginMsdfTextBatch state env

    /// Begin a new MSDF text batch frame.
    let BeginMsdfTextBatchFrame
        (viewProjection2dAbsolute : Matrix4x4 inref,
         viewProjection2dRelative : Matrix4x4 inref,
         viewProjectionClipAbsolute : Matrix4x4 inref,
         viewProjectionClipRelative : Matrix4x4 inref,
         env) =
        Pipeline.beginFrame env.Pipeline
        env.GlyphIndex <- 0
        env.ViewProjection2dAbsolute <- viewProjection2dAbsolute
        env.ViewProjection2dRelative <- viewProjection2dRelative
        env.ViewProjectionClipAbsolute <- viewProjectionClipAbsolute
        env.ViewProjectionClipRelative <- viewProjectionClipRelative
        BeginMsdfTextBatch MsdfTextBatchState.defaultState env

    /// End the current MSDF text batch frame, if any.
    let EndMsdfTextBatchFrame viewport env =
        EndMsdfTextBatch viewport env

    /// Forcibly end the current MSDF text batch frame, if any, run the given fn, then restart the batch frame.
    let InterruptMsdfTextBatchFrame fn viewport env =
        let state = env.State
        EndMsdfTextBatch viewport env
        fn ()
        BeginMsdfTextBatch state env

    let
#if !DEBUG
        inline
#endif
        private PopulateMsdfTextBatchGlyph (position : Vector2) (size : Vector2) (texCoords : Box2) (color : Color) distanceRange (shader : MsdfTextShader) env =
        let mutable glyph = Glyph ()
        glyph.perimeter <- v4 position.X position.Y size.X size.Y
        glyph.texCoords <- v4 texCoords.Min.X texCoords.Min.Y texCoords.Size.X texCoords.Size.Y
        glyph.color <- color.V4
        glyph.outlineColor <- shader.OutlineColor.V4
        glyph.shader <- v4 distanceRange shader.EdgeOffset shader.Softness shader.OutlineThickness
        glyph.shader2 <- v4 shader.OutlineSoftness 0.0f 0.0f 0.0f
        env.Glyphs[env.GlyphIndex] <- glyph

    /// Submit a glyph to the appropriate MSDF text batch.
    let SubmitMsdfTextBatchGlyph (absolute, position, size, texCoords : Box2 inref, clipOpt : Box2 voption inref, color : Color inref, blend, distanceRange, shader, texture : Texture, viewport, env) =
        let state = MsdfTextBatchState.make absolute clipOpt blend texture
        if MsdfTextBatchState.changed state env.State || env.GlyphIndex = Constants.Render.SpriteBatchSize then
            RestartMsdfTextBatch state viewport env
        PopulateMsdfTextBatchGlyph position size texCoords color distanceRange shader env
        env.GlyphIndex <- inc env.GlyphIndex

    /// Create an MSDF text batch environment.
    let CreateMsdfTextBatchEnv filteredSampler context =
        let glyphUniform, viewProjectionUniform, pipeline = CreateMsdfTextBatchPipeline context
        { GlyphIndex = 0
          ViewProjection2dAbsolute = m4Identity
          ViewProjection2dRelative = m4Identity
          ViewProjectionClipAbsolute = m4Identity
          ViewProjectionClipRelative = m4Identity
          VulkanContext = context
          Pipeline = pipeline
          FilteredSampler = filteredSampler
          GlyphUniform = glyphUniform
          ViewProjectionUniform = viewProjectionUniform
          Glyphs = Array.zeroCreate Constants.Render.SpriteBatchSize
          State = MsdfTextBatchState.defaultState }

    /// Destroy the given MSDF text batch environment.
    let DestroyMsdfTextBatchEnv env =
        Pipeline.destroy env.Pipeline env.VulkanContext
