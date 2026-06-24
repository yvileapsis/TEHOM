// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module MsdfText =

    [<Struct; StructLayout(LayoutKind.Explicit)>]
    type Glyph =
        [<FieldOffset(0)>] val mutable perimeter : Vector4
        [<FieldOffset(16)>] val mutable texCoords : Vector4
        [<FieldOffset(32)>] val mutable color : Vector4
        [<FieldOffset(48)>] val mutable outlineColor : Vector4
        [<FieldOffset(64)>] val mutable shader : Vector4
        [<FieldOffset(80)>] val mutable shader2 : Vector4

    [<Struct; StructLayout(LayoutKind.Explicit)>]
    type ViewProjection =
        [<FieldOffset(0)>] val mutable viewProjection : Matrix4x4

    type [<Struct>] private MsdfTextBatchState =
        { Absolute : bool
          ClipOpt : Box2 voption
          Blend : Pipeline.Blend
          TextureOpt : Texture.Texture voption }

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
            { Absolute = absolute; ClipOpt = clipOpt; Blend = blend; TextureOpt = ValueSome texture }

        static member defaultState =
            { Absolute = false; ClipOpt = ValueNone; Blend = Pipeline.Transparent; TextureOpt = ValueNone }

    /// The environment that contains the internal state required for batching MSDF text.
    type [<ReferenceEquality>] MsdfTextBatchEnv =
        private
            { mutable DrawIndex : int
              mutable GlyphIndex : int
              mutable ViewProjection2dAbsolute : Matrix4x4
              mutable ViewProjection2dRelative : Matrix4x4
              mutable ViewProjectionClipAbsolute : Matrix4x4
              mutable ViewProjectionClipRelative : Matrix4x4
              VulkanContext : Hl.VulkanContext
              Pipeline : Pipeline.Pipeline
              FilteredSampler : Texture.Sampler
              GlyphUniform : Buffer.Buffer
              ViewProjectionUniform : Buffer.Buffer
              Perimeters : Vector4 array
              TexCoordses : Vector4 array
              Colors : Vector4 array
              OutlineColors : Vector4 array
              Shaders : Vector4 array
              Shader2s : Vector4 array
              mutable State : MsdfTextBatchState }

    let private CreateMsdfTextBatchPipeline (vkc : Hl.VulkanContext) =
        let pipeline =
            Pipeline.Pipeline.create
                Constants.Paths.MsdfTextShaderFilePath
                Constants.Render.SpriteBatchesMax
                [|Pipeline.Transparent; Pipeline.Additive; Pipeline.Overwrite|] [|true|] [||]
                [|Pipeline.descriptorSet Hl.BulkSetIndexed 1
                    [|Pipeline.descriptor 0 Hl.StorageBuffer Hl.VertexStage 1
                      Pipeline.descriptor 1 Hl.StorageBuffer Hl.VertexStage 1
                      Pipeline.descriptor 2 Hl.SampledImage Hl.FragmentStage 1|]
                  Pipeline.descriptorSet Hl.BulkNone 1
                    [|Pipeline.descriptor 0 Hl.Sampler Hl.FragmentStage 1|]|]
                [||] [|vkc.SwapFormat|] None vkc
        let glyphUniform = Buffer.Buffer.create (Constants.Render.SpriteBatchSize * sizeof<Glyph>) Buffer.Storage vkc
        let viewProjectionUniform = Buffer.Buffer.create sizeof<ViewProjection> Buffer.Storage vkc
        (glyphUniform, viewProjectionUniform, pipeline)

    /// Reload the shaders used by the environment.
    let ReloadShaders env vkc =
        Pipeline.Pipeline.reloadShaders env.Pipeline vkc

    let private BeginMsdfTextBatch state env =
        env.State <- state

    let private EndMsdfTextBatch (viewport : Viewport) env =
        match env.State.TextureOpt with
        | ValueSome texture when env.GlyphIndex > 0 ->
            if env.DrawIndex < env.Pipeline.BulkDrawLimit then
                let vkc = env.VulkanContext
                let glyphUniform = env.GlyphUniform
                let viewProjectionUniform = env.ViewProjectionUniform
                for i in 0 .. dec env.GlyphIndex do
                    let mutable glyph = Glyph ()
                    glyph.perimeter <- env.Perimeters.[i]
                    glyph.texCoords <- env.TexCoordses.[i]
                    glyph.color <- env.Colors.[i]
                    glyph.outlineColor <- env.OutlineColors.[i]
                    glyph.shader <- env.Shaders.[i]
                    glyph.shader2 <- env.Shader2s.[i]
                    Buffer.Buffer.uploadValue env.DrawIndex (i * sizeof<Glyph>) 0 glyph glyphUniform vkc
                    Pipeline.Pipeline.writeDescriptorStorageBuffer 0 0 env.DrawIndex 0 glyphUniform.[env.DrawIndex] env.Pipeline vkc
                let mutable viewProjection = ViewProjection ()
                viewProjection.viewProjection <- if env.State.Absolute then env.ViewProjection2dAbsolute else env.ViewProjection2dRelative
                Buffer.Buffer.uploadValue env.DrawIndex 0 0 viewProjection viewProjectionUniform vkc
                Pipeline.Pipeline.writeDescriptorStorageBuffer 0 1 env.DrawIndex 0 viewProjectionUniform.[env.DrawIndex] env.Pipeline vkc
                Pipeline.Pipeline.writeDescriptorSampledImage 0 2 env.DrawIndex 0 texture env.Pipeline vkc
                Pipeline.Pipeline.writeDescriptorSampler 1 0 0 0 env.FilteredSampler env.Pipeline vkc

                let pixelDensity = Hl.getWindowPixelDensity vkc.Window
                let renderAreaLogical = VkRect2D (viewport.Inner.Min.X, viewport.Outer.Max.Y - viewport.Inner.Max.Y, uint viewport.Inner.Size.X, uint viewport.Inner.Size.Y)
                let mutable renderArea = Hl.scaleRectForPixelDensity pixelDensity renderAreaLogical
                let mutable vkViewport = Hl.makeViewport true renderArea
                let mutable scissor = renderArea
                match env.State.ClipOpt with
                | ValueSome clip ->
                    let viewProjection = if env.State.Absolute then env.ViewProjectionClipAbsolute else env.ViewProjectionClipRelative
                    let minClip = Vector4.Transform(Vector4 (clip.Min.X, clip.Max.Y, 0.0f, 1.0f), viewProjection).V2
                    let minNdc = minClip * single viewport.DisplayScalar
                    let minScissor = (minNdc + v2One) * 0.5f * viewport.Inner.Size.V2
                    let sizeClip = Vector4.Transform(Vector4 (clip.Size, 0.0f, 1.0f), viewProjection).V2
                    let sizeNdc = sizeClip * single viewport.DisplayScalar
                    let sizeScissor = sizeNdc * 0.5f * viewport.Inner.Size.V2
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
                    match Pipeline.Pipeline.tryGetVkPipeline env.State.Blend true env.Pipeline with
                    | Some vkPipeline ->
                        let cb = vkc.RenderCommandBuffer
                        let mutable rendering = Hl.makeRenderingInfo [|vkc.SwapchainImageView|] None renderArea None
                        Vulkan.vkCmdBeginRendering (cb, asPointer &rendering)
                        Vulkan.vkCmdBindPipeline (cb, VkPipelineBindPoint.Graphics, vkPipeline)
                        Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
                        Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)
                        let mutable mainDescriptorSet = env.Pipeline.VkDescriptorSet 0 env.DrawIndex
                        let mutable samplerDescriptorSet = env.Pipeline.VkDescriptorSet 1 0
                        Vulkan.vkCmdBindDescriptorSets (cb, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 0u, 1u, asPointer &mainDescriptorSet, 0u, nullPtr)
                        Vulkan.vkCmdBindDescriptorSets (cb, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 1u, 1u, asPointer &samplerDescriptorSet, 0u, nullPtr)
                        Vulkan.vkCmdDraw (cb, uint (6 * env.GlyphIndex), 1u, 0u, 0u)
                        Hl.reportDrawCall env.GlyphIndex
                        Vulkan.vkCmdEndRendering cb
                    | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."
            else Log.warnOnce "Draw operations aborted because bulk draw limit has been reached. Increase relevant bulk draw limit as necessary for current application."
            env.DrawIndex <- inc env.DrawIndex
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
        env.DrawIndex <- 0
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
        env.Perimeters.[env.GlyphIndex] <- v4 position.X position.Y size.X size.Y
        env.TexCoordses.[env.GlyphIndex] <- v4 texCoords.Min.X texCoords.Min.Y texCoords.Size.X texCoords.Size.Y
        env.Colors.[env.GlyphIndex] <- color.V4
        env.OutlineColors.[env.GlyphIndex] <- shader.OutlineColor.V4
        env.Shaders.[env.GlyphIndex] <- v4 distanceRange shader.EdgeOffset shader.Softness shader.OutlineThickness
        env.Shader2s.[env.GlyphIndex] <- v4 shader.OutlineSoftness 0.0f 0.0f 0.0f

    /// Submit a glyph to the appropriate MSDF text batch.
    let SubmitMsdfTextBatchGlyph (absolute, position, size, texCoords : Box2 inref, clipOpt : Box2 voption inref, color : Color inref, blend, distanceRange, shader, texture : Texture.Texture, viewport, env) =
        let state = MsdfTextBatchState.make absolute clipOpt blend texture
        if MsdfTextBatchState.changed state env.State || env.GlyphIndex = Constants.Render.SpriteBatchSize then
            RestartMsdfTextBatch state viewport env
        PopulateMsdfTextBatchGlyph position size texCoords color distanceRange shader env
        env.GlyphIndex <- inc env.GlyphIndex

    /// Create an MSDF text batch environment.
    let CreateMsdfTextBatchEnv filteredSampler vkc =
        let (glyphUniform, viewProjectionUniform, pipeline) = CreateMsdfTextBatchPipeline vkc
        { DrawIndex = 0
          GlyphIndex = 0
          ViewProjection2dAbsolute = m4Identity
          ViewProjection2dRelative = m4Identity
          ViewProjectionClipAbsolute = m4Identity
          ViewProjectionClipRelative = m4Identity
          VulkanContext = vkc
          Pipeline = pipeline
          FilteredSampler = filteredSampler
          GlyphUniform = glyphUniform
          ViewProjectionUniform = viewProjectionUniform
          Perimeters = Array.zeroCreate Constants.Render.SpriteBatchSize
          TexCoordses = Array.zeroCreate Constants.Render.SpriteBatchSize
          Colors = Array.zeroCreate Constants.Render.SpriteBatchSize
          OutlineColors = Array.zeroCreate Constants.Render.SpriteBatchSize
          Shaders = Array.zeroCreate Constants.Render.SpriteBatchSize
          Shader2s = Array.zeroCreate Constants.Render.SpriteBatchSize
          State = MsdfTextBatchState.defaultState }

    /// Destroy the given MSDF text batch environment.
    let DestroyMsdfTextBatchEnv env =
        let vkc = env.VulkanContext
        Pipeline.Pipeline.destroy env.Pipeline vkc
        Buffer.Buffer.destroy env.GlyphUniform vkc
        Buffer.Buffer.destroy env.ViewProjectionUniform vkc
