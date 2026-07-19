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
module SlugText =

    [<Struct; StructLayout (LayoutKind.Explicit)>]
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

    [<Struct; StructLayout (LayoutKind.Explicit)>]
    type ViewProjection =
        [<FieldOffset(0)>] val mutable viewProjection : Matrix4x4
        [<FieldOffset(64)>] val mutable viewport : Vector4

    type [<Struct>] private SlugTextBatchState =
        { Absolute : bool
          ClipOpt : Box2 voption
          Blend : VulkanBlend
          TextureOpt : (Texture * Texture) voption }

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
              Blend = VulkanTransparent
              TextureOpt = ValueNone }

    type [<ReferenceEquality>] SlugTextBatchEnv =
        private
            { mutable GlyphIndex : int
              mutable ViewProjection2dAbsolute : Matrix4x4
              mutable ViewProjection2dRelative : Matrix4x4
              mutable ViewProjectionClipAbsolute : Matrix4x4
              mutable ViewProjectionClipRelative : Matrix4x4
              VulkanContext : VulkanContext
              Pipeline : Pipeline
              UnfilteredSampler : Sampler
              GlyphUniform : VulkanBuffer
              ViewProjectionUniform : VulkanBuffer
              Glyphs : Glyph array
              mutable State : SlugTextBatchState }

    let private CreateSlugTextBatchPipeline (context : VulkanContext) =
        let glyphUniform = VulkanBuffer.create Storage (Constants.Render.SpriteBatchSize * sizeof<Glyph>) context
        let viewProjectionUniform = VulkanBuffer.create Storage sizeof<ViewProjection> context
        let pipeline =
            Pipeline.create
                Constants.Paths.SlugTextShaderFilePath
                [|VulkanTransparent; VulkanAdditive; VulkanOverwrite|]
                [|true|]
                [||]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer VertexStage 1
                      Pipeline.descriptor 1 StorageBuffer VertexStage 1
                      Pipeline.descriptor 2 CombinedImageSampler FragmentStage 1
                      Pipeline.descriptor 3 CombinedImageSampler FragmentStage 1|]|]
                [||]
                [|context.SwapFormat|]
                None
                [|glyphUniform; viewProjectionUniform|]
        glyphUniform, viewProjectionUniform, pipeline

    let ReloadShaders env context =
        Pipeline.reloadShaders env.Pipeline context

    let private BeginSlugTextBatch state env =
        env.State <- state

    let private EndSlugTextBatch (viewport : Viewport) env =
        match env.State.TextureOpt with
        | ValueSome (curveTexture, bandTexture) when env.GlyphIndex > 0 ->
            let context = env.VulkanContext
            let pixelDensity = Hl.getWindowPixelDensity context.Window
            let renderAreaLogical =
                VkRect2D
                    (viewport.Inner.Min.X,
                     viewport.Outer.Max.Y - viewport.Inner.Max.Y,
                     uint viewport.Inner.Size.X,
                     uint viewport.Inner.Size.Y)
            let renderArea = Hl.scaleRectForPixelDensity pixelDensity renderAreaLogical
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
                    let mutable descriptorSet =
                        Pipeline.specifyDescriptorSet 0 env.Pipeline.DrawIndex env.Pipeline $ fun vkSet ->
                            VulkanBuffer.uploadArrayCount env.GlyphIndex env.Glyphs env.GlyphUniform context
                            Pipeline.writeDescriptorStorageBuffer 0 0 env.GlyphUniform vkSet
                            let mutable viewProjection = ViewProjection ()
                            viewProjection.viewProjection <- if env.State.Absolute then env.ViewProjection2dAbsolute else env.ViewProjection2dRelative
                            viewProjection.viewport <- Vector4 (single renderArea.extent.width, single renderArea.extent.height, 0.0f, 0.0f)
                            VulkanBuffer.uploadValue viewProjection env.ViewProjectionUniform context
                            Pipeline.writeDescriptorStorageBuffer 1 0 env.ViewProjectionUniform vkSet
                            Pipeline.writeDescriptorCombinedTextureSampler 2 0 curveTexture env.UnfilteredSampler vkSet
                            Pipeline.writeDescriptorCombinedTextureSampler 3 0 bandTexture env.UnfilteredSampler vkSet
                    let mutable renderingInfo = Hl.makeRenderingInfo [|context.SwapchainImageView|] None renderArea None
                    DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
                    DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
                    DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&scissor)
                    DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                    DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 0u, 1u, &&descriptorSet, 0u, nullPtr)
                    DeviceApi.vkCmdDraw (context.RenderCommandBuffer, uint (6 * env.GlyphIndex), 1u, 0u, 0u)
                    DeviceApi.vkCmdEndRendering context.RenderCommandBuffer
                    Hl.reportDrawCall env.GlyphIndex true
                    Pipeline.advance env.Pipeline
                    VulkanContext.advanceRenderCommandBuffer context
                | None -> Log.warnOnce "Cannot draw Slug text because VkPipeline does not exist."
            env.GlyphIndex <- 0
        | ValueSome _ | ValueNone -> ()

    let private RestartSlugTextBatch state viewport env =
        EndSlugTextBatch viewport env
        BeginSlugTextBatch state env

    let BeginSlugTextBatchFrame
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
        BeginSlugTextBatch SlugTextBatchState.defaultState env

    let EndSlugTextBatchFrame viewport env =
        EndSlugTextBatch viewport env

    let InterruptSlugTextBatchFrame fn viewport env =
        let state = env.State
        EndSlugTextBatch viewport env
        fn ()
        BeginSlugTextBatch state env

    let private PopulateSlugTextBatchGlyph (glyph : SlugTextGlyph) fontSize (transform : Matrix4x4 inref) env =
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
        env.Glyphs[env.GlyphIndex] <- gpuGlyph

    let SubmitSlugTextBatchGlyph (absolute, glyph : SlugTextGlyph, fontSize, transform : Matrix4x4 inref, clipOpt : Box2 voption inref, blend, curveTexture, bandTexture, viewport, env) =
        let state = SlugTextBatchState.make absolute clipOpt blend curveTexture bandTexture
        if SlugTextBatchState.changed state env.State || env.GlyphIndex = Constants.Render.SpriteBatchSize then
            RestartSlugTextBatch state viewport env
        PopulateSlugTextBatchGlyph glyph fontSize &transform env
        env.GlyphIndex <- inc env.GlyphIndex

    let CreateSlugTextBatchEnv unfilteredSampler context =
        let glyphUniform, viewProjectionUniform, pipeline = CreateSlugTextBatchPipeline context
        { GlyphIndex = 0
          ViewProjection2dAbsolute = m4Identity
          ViewProjection2dRelative = m4Identity
          ViewProjectionClipAbsolute = m4Identity
          ViewProjectionClipRelative = m4Identity
          VulkanContext = context
          Pipeline = pipeline
          UnfilteredSampler = unfilteredSampler
          GlyphUniform = glyphUniform
          ViewProjectionUniform = viewProjectionUniform
          Glyphs = Array.zeroCreate Constants.Render.SpriteBatchSize
          State = SlugTextBatchState.defaultState }

    let DestroySlugTextBatchEnv env =
        Pipeline.destroy env.Pipeline env.VulkanContext
