// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugText =

    [<Struct; StructLayout(LayoutKind.Explicit)>]
    type Glyph =
        [<FieldOffset(0)>] val mutable perimeter : Vector4
        [<FieldOffset(16)>] val mutable texCoords : Vector4
        [<FieldOffset(32)>] val mutable jacobian : Vector4
        [<FieldOffset(48)>] val mutable banding : Vector4
        [<FieldOffset(64)>] val mutable bandLocationX : uint32
        [<FieldOffset(68)>] val mutable bandLocationY : uint32
        [<FieldOffset(72)>] val mutable bandMaxX : uint32
        [<FieldOffset(76)>] val mutable bandMaxYAndFlags : uint32
        [<FieldOffset(80)>] val mutable color : Vector4
        [<FieldOffset(96)>] val mutable transform : Matrix4x4

    [<Struct; StructLayout(LayoutKind.Explicit)>]
    type ViewProjection =
        [<FieldOffset(0)>] val mutable viewProjection : Matrix4x4
        [<FieldOffset(64)>] val mutable viewport : Vector4

    type [<Struct>] private SlugTextBatchState =
        { Absolute : bool
          ClipOpt : Box2 voption
          Blend : Pipeline.Blend
          TextureOpt : (Texture.Texture * Texture.Texture) voption }

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
             | struct (ValueSome (c, b), ValueSome (c2, b2)) -> c <> c2 || b <> b2)

        static member inline make absolute clipOpt blend curveTexture bandTexture =
            { Absolute = absolute
              ClipOpt = clipOpt
              Blend = blend
              TextureOpt = ValueSome (curveTexture, bandTexture) }

        static member defaultState =
            { Absolute = false
              ClipOpt = ValueNone
              Blend = Pipeline.Transparent
              TextureOpt = ValueNone }

    type [<ReferenceEquality>] SlugTextBatchEnv =
        private
            { mutable DrawIndex : int
              mutable GlyphIndex : int
              mutable ViewProjection2dAbsolute : Matrix4x4
              mutable ViewProjection2dRelative : Matrix4x4
              mutable ViewProjectionClipAbsolute : Matrix4x4
              mutable ViewProjectionClipRelative : Matrix4x4
              VulkanContext : Hl.VulkanContext
              Pipeline : Pipeline.Pipeline
              UnfilteredSampler : Texture.Sampler
              GlyphUniform : Buffer.Buffer
              ViewProjectionUniform : Buffer.Buffer
              Glyphs : Glyph array
              mutable State : SlugTextBatchState }

    let private CreateSlugTextBatchPipeline (vkc : Hl.VulkanContext) =
        let pipeline =
            Pipeline.Pipeline.create
                Constants.Paths.SlugTextShaderFilePath
                Constants.Render.SpriteBatchesMax
                [|Pipeline.Transparent; Pipeline.Additive; Pipeline.Overwrite|] [|true|] [||]
                [|Pipeline.descriptorSet Hl.BulkSetIndexed 1
                    [|Pipeline.descriptor 0 Hl.StorageBuffer Hl.VertexStage 1
                      Pipeline.descriptor 1 Hl.StorageBuffer Hl.VertexStage 1
                      Pipeline.descriptor 2 Hl.CombinedImageSampler Hl.FragmentStage 1
                      Pipeline.descriptor 3 Hl.CombinedImageSampler Hl.FragmentStage 1|]|]
                [||] [|vkc.SwapFormat|] None vkc
        let glyphUniform = Buffer.Buffer.create (Constants.Render.SpriteBatchSize * sizeof<Glyph>) Buffer.Storage vkc
        let viewProjectionUniform = Buffer.Buffer.create sizeof<ViewProjection> Buffer.Storage vkc
        glyphUniform, viewProjectionUniform, pipeline

    let ReloadShaders env vkc =
        Pipeline.Pipeline.reloadShaders env.Pipeline vkc

    let private BeginSlugTextBatch state env = env.State <- state

    let private EndSlugTextBatch (viewport : Viewport) env =
        match env.State.TextureOpt with
        | ValueSome (curveTexture, bandTexture) when env.GlyphIndex > 0 ->
            if env.DrawIndex < env.Pipeline.BulkDrawLimit then
                let vkc = env.VulkanContext
                // Preserve the existing Slug batch and ordering boundaries, but copy and flush its
                // populated CPU prefix once instead of issuing one mapped-memory flush per glyph.
                Buffer.Buffer.uploadArrayCount env.DrawIndex 0 0 env.GlyphIndex env.Glyphs env.GlyphUniform vkc
                Pipeline.Pipeline.writeDescriptorStorageBuffer 0 0 env.DrawIndex 0 env.GlyphUniform.[env.DrawIndex] env.Pipeline vkc
                let mutable viewProjection = ViewProjection ()
                viewProjection.viewProjection <- if env.State.Absolute then env.ViewProjection2dAbsolute else env.ViewProjection2dRelative
                let pixelDensity = Hl.getWindowPixelDensity vkc.Window
                let renderAreaLogical = VkRect2D (viewport.Inner.Min.X, viewport.Outer.Max.Y - viewport.Inner.Max.Y, uint viewport.Inner.Size.X, uint viewport.Inner.Size.Y)
                let renderArea = Hl.scaleRectForPixelDensity pixelDensity renderAreaLogical
                viewProjection.viewport <- Vector4 (single renderArea.extent.width, single renderArea.extent.height, 0.0f, 0.0f)
                Buffer.Buffer.uploadValue env.DrawIndex 0 0 viewProjection env.ViewProjectionUniform vkc
                Pipeline.Pipeline.writeDescriptorStorageBuffer 0 1 env.DrawIndex 0 env.ViewProjectionUniform.[env.DrawIndex] env.Pipeline vkc
                Pipeline.Pipeline.writeDescriptorCombinedImageSampler 0 2 env.DrawIndex 0 curveTexture env.UnfilteredSampler env.Pipeline vkc
                Pipeline.Pipeline.writeDescriptorCombinedImageSampler 0 3 env.DrawIndex 0 bandTexture env.UnfilteredSampler env.Pipeline vkc
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
                    let scissorLogical = VkRect2D ((minScissor.X |> round |> int) + offset.X, (single renderAreaLogical.extent.height - minScissor.Y |> round |> int) + offset.Y, uint sizeScissor.X, uint sizeScissor.Y)
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
                        Vulkan.vkCmdBindDescriptorSets (cb, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 0u, 1u, asPointer &mainDescriptorSet, 0u, nullPtr)
                        Vulkan.vkCmdDraw (cb, uint (6 * env.GlyphIndex), 1u, 0u, 0u)
                        Hl.reportDrawCall env.GlyphIndex
                        Vulkan.vkCmdEndRendering cb
                    | None -> Log.warnOnce "Cannot draw Slug text because VkPipeline does not exist."
            else Log.warnOnce "Slug text draw operations aborted because the bulk draw limit has been reached."
            env.DrawIndex <- inc env.DrawIndex
            env.GlyphIndex <- 0
        | ValueSome _ | ValueNone -> ()

    let private RestartSlugTextBatch state viewport env =
        EndSlugTextBatch viewport env
        BeginSlugTextBatch state env

    let BeginSlugTextBatchFrame (viewProjection2dAbsolute : Matrix4x4 inref, viewProjection2dRelative : Matrix4x4 inref, viewProjectionClipAbsolute : Matrix4x4 inref, viewProjectionClipRelative : Matrix4x4 inref, env) =
        env.DrawIndex <- 0
        env.GlyphIndex <- 0
        env.ViewProjection2dAbsolute <- viewProjection2dAbsolute
        env.ViewProjection2dRelative <- viewProjection2dRelative
        env.ViewProjectionClipAbsolute <- viewProjectionClipAbsolute
        env.ViewProjectionClipRelative <- viewProjectionClipRelative
        BeginSlugTextBatch SlugTextBatchState.defaultState env

    let EndSlugTextBatchFrame viewport env = EndSlugTextBatch viewport env

    let InterruptSlugTextBatchFrame fn viewport env =
        let state = env.State
        EndSlugTextBatch viewport env
        fn ()
        BeginSlugTextBatch state env

    let private PopulateSlugTextBatchGlyph (glyph : SlugTextGlyph) fontSize (transform : Matrix4x4 inref) env =
        let i = env.GlyphIndex
        let mutable gpuGlyph = Glyph ()
        gpuGlyph.perimeter <- v4 glyph.Position.X glyph.Position.Y glyph.Size.X glyph.Size.Y
        gpuGlyph.texCoords <- glyph.TexCoords
        gpuGlyph.jacobian <- v4 (1.0f / max fontSize 1.0e-5f) 0.0f 0.0f (1.0f / max fontSize 1.0e-5f)
        gpuGlyph.banding <- glyph.BandTransform
        gpuGlyph.bandLocationX <- uint32 glyph.BandLocation.X
        gpuGlyph.bandLocationY <- uint32 glyph.BandLocation.Y
        gpuGlyph.bandMaxX <- uint32 glyph.BandMax.X
        gpuGlyph.bandMaxYAndFlags <- uint32 glyph.BandMax.Y ||| (match glyph.FillRule with SlugFillNonzero -> 0u | SlugFillEvenOdd -> 0x1000u)
        gpuGlyph.color <- glyph.Color.V4
        gpuGlyph.transform <- transform
        env.Glyphs.[i] <- gpuGlyph

    let SubmitSlugTextBatchGlyph (absolute, glyph : SlugTextGlyph, fontSize, transform : Matrix4x4 inref, clipOpt : Box2 voption inref, blend, curveTexture, bandTexture, viewport, env) =
        let state = SlugTextBatchState.make absolute clipOpt blend curveTexture bandTexture
        if SlugTextBatchState.changed state env.State || env.GlyphIndex = Constants.Render.SpriteBatchSize then RestartSlugTextBatch state viewport env
        PopulateSlugTextBatchGlyph glyph fontSize &transform env
        env.GlyphIndex <- inc env.GlyphIndex

    let CreateSlugTextBatchEnv unfilteredSampler vkc =
        let glyphUniform, viewProjectionUniform, pipeline = CreateSlugTextBatchPipeline vkc
        { DrawIndex = 0
          GlyphIndex = 0
          ViewProjection2dAbsolute = m4Identity
          ViewProjection2dRelative = m4Identity
          ViewProjectionClipAbsolute = m4Identity
          ViewProjectionClipRelative = m4Identity
          VulkanContext = vkc
          Pipeline = pipeline
          UnfilteredSampler = unfilteredSampler
          GlyphUniform = glyphUniform
          ViewProjectionUniform = viewProjectionUniform
          Glyphs = Array.zeroCreate Constants.Render.SpriteBatchSize
          State = SlugTextBatchState.defaultState }

    let DestroySlugTextBatchEnv env =
        let vkc = env.VulkanContext
        Pipeline.Pipeline.destroy env.Pipeline vkc
        Buffer.Buffer.destroy env.GlyphUniform vkc
        Buffer.Buffer.destroy env.ViewProjectionUniform vkc
