// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan

open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Prime
open Nu

/// Vulkan implementation of the analytic SlugShape curve renderer.  The module owns
/// only the resources it creates; samplers supplied to CreateSlugShapeEnv remain
/// owned by the caller.
[<RequireQualifiedAccess>]
module SlugShape =

    [<Struct; StructLayout(LayoutKind.Explicit, Size = 16)>]
    type private Uvec4 =
        [<FieldOffset(0)>] val mutable X : uint32
        [<FieldOffset(4)>] val mutable Y : uint32
        [<FieldOffset(8)>] val mutable Z : uint32
        [<FieldOffset(12)>] val mutable W : uint32

    let private makeUvec4 a b c d =
        let mutable value = Uvec4 ()
        value.X <- a
        value.Y <- b
        value.Z <- c
        value.W <- d
        value

    [<Struct; StructLayout(LayoutKind.Explicit, Size = 112)>]
    type private ViewGpu =
        [<FieldOffset(0)>] val mutable ViewProjection : Matrix4x4
        [<FieldOffset(64)>] val mutable Viewport : Vector4
        [<FieldOffset(80)>] val mutable Time : Vector4
        [<FieldOffset(96)>] val mutable Counts : Uvec4

    [<Struct; StructLayout(LayoutKind.Explicit, Size = 240)>]
    type private LayerGpu =
        [<FieldOffset(0)>] val mutable Transform : Matrix4x4
        [<FieldOffset(64)>] val mutable Color : Vector4
        [<FieldOffset(80)>] val mutable Origin : Vector4
        [<FieldOffset(96)>] val mutable StateIds : Uvec4
        [<FieldOffset(112)>] val mutable ResourceIds : Uvec4
        [<FieldOffset(128)>] val mutable EffectParams : Vector4
        [<FieldOffset(144)>] val mutable EffectParams2 : Vector4
        [<FieldOffset(160)>] val mutable GradientTransform : Matrix4x4
        [<FieldOffset(224)>] val mutable MaskMaterial : Vector4

    [<Struct; StructLayout(LayoutKind.Explicit, Size = 64)>]
    type private ShapeGpu =
        [<FieldOffset(0)>] val mutable Bounds : Vector4
        [<FieldOffset(16)>] val mutable BandTransform : Vector4
        [<FieldOffset(32)>] val mutable BandAddress : Uvec4
        [<FieldOffset(48)>] val mutable BandFlags : Uvec4

    [<Struct; StructLayout(LayoutKind.Explicit, Size = 48)>]
    type private GradientGpu =
        [<FieldOffset(0)>] val mutable StopRange : Uvec4
        [<FieldOffset(16)>] val mutable Params : Vector4
        [<FieldOffset(32)>] val mutable Params2 : Vector4

    [<Struct; StructLayout(LayoutKind.Explicit, Size = 32)>]
    type private StopGpu =
        [<FieldOffset(0)>] val mutable Color : Vector4
        [<FieldOffset(16)>] val mutable Offset : Vector4

    [<Struct; StructLayout(LayoutKind.Explicit, Size = 48)>]
    type private MaskGpu =
        [<FieldOffset(0)>] val mutable Params : Vector4
        [<FieldOffset(16)>] val mutable Params2 : Vector4
        [<FieldOffset(32)>] val mutable State : Uvec4

    [<Struct; StructLayout(LayoutKind.Explicit, Size = 48)>]
    type private ComputeConfigGpu =
        [<FieldOffset(0)>] val mutable Dispatch : Uvec4
        [<FieldOffset(16)>] val mutable Params0 : Vector4
        [<FieldOffset(32)>] val mutable Params1 : Vector4

    /// The modes implemented by SlugShape.comp.
    type SlugShapeComputeMode =
        | Radial
        | Spiral
        | Interference
        | CanonicalColor

    /// Per-draw compute parameters.  A config is supplied to RenderSlugShape to
    /// request a real dispatch; None leaves the layer SSBO untouched by compute.
    [<Struct>]
    type SlugShapeComputeConfig =
        { LayerCount : int
          Mode : SlugShapeComputeMode
          Flags : uint32
          Seed : uint32
          Speed : single
          Amplitude : single
          Frequency : single
          Phase : single
          Params1 : Vector4 }

    type private CompositeGpu =
        { Id : Guid
          Revision : int64
          CurveTexture : Texture.Texture
          BandTexture : Texture.Texture
          ViewBuffer : Buffer.Buffer
          LayerBuffer : Buffer.Buffer
          ShapeBuffer : Buffer.Buffer
          GradientBuffer : Buffer.Buffer
          StopBuffer : Buffer.Buffer
          MaskBuffer : Buffer.Buffer
          ConfigBuffer : Buffer.Buffer
          Layers : LayerGpu array
          Masks : MaskGpu array
          mutable CpuLayerRevision : int64
          StaticInitialized : bool array
          mutable DrawIndex : int }

    type [<ReferenceEquality>] SlugShapeEnv =
        private
            { VulkanContext : Hl.VulkanContext
              UnfilteredSampler : Texture.Sampler
              FilteredSampler : Texture.Sampler
              GraphicsPipeline : Pipeline.Pipeline
              ComputePipeline : Pipeline.ComputePipeline
              FallbackTexture : Texture.Texture
              Composites : Dictionary<SlugCompositeShape, CompositeGpu>
              mutable DrawIndex : int }
    let private shaderPath = Constants.Paths.SlugShapeShaderFilePath
    let private maxFillTextures = 8

    let private uintVector4 a b c d = makeUvec4 a b c d

    let private fillIdAndResources (data : SlugCompositeShapeData) (source : SlugFillSource) =
        match source with
        | SlugFillSource.Solid -> 0u, 0u, 0u, 0u
        | SlugFillSource.Gradient index when index >= 0 && index < data.Gradients.Length ->
            let id =
                match data.Gradients[index].Kind with
                | SlugGradientKind.Linear _ -> 1u
                | SlugGradientKind.Radial _ -> 2u
                | SlugGradientKind.FocalRadial _ -> 2u
                | SlugGradientKind.Sweep _ -> 3u
                | SlugGradientKind.SweepRange _ -> 3u
            id, uint32 index, 0u, 0u
        | SlugFillSource.Texture slot -> 4u, 0u, uint32 (max 0 slot), uint32 (max 0 slot)
        | SlugFillSource.Procedural effectId ->
            // Procedural IDs 0..10 map to shader fill IDs 5..15.  ID 11 is
            // reserved for the canonical compute-demo raymarched circle fill;
            // it intentionally skips material fill ID 16.
            let fillId = if effectId = 11 then 17u else uint32 (5 + max 0 (min 10 effectId))
            fillId, 0u, 0u, 0u
        | SlugFillSource.PbrMaterial index -> 16u, 0u, uint32 (max 0 index), uint32 (max 0 index)
        | _ -> 0u, 0u, 0u, 0u

    let private maskType kind =
        match kind with
        | SlugMaskKind.MsdfLayer _ -> 0u
        | SlugMaskKind.Circle -> 1u
        | SlugMaskKind.Rectangle -> 2u
        | SlugMaskKind.Capsule -> 3u
        | SlugMaskKind.Arc -> 4u
        | SlugMaskKind.ArcBand -> 5u
        | SlugMaskKind.Hexagon -> 6u
        | SlugMaskKind.Octagon -> 7u
        | SlugMaskKind.Star -> 8u
        | SlugMaskKind.Shape _ -> 9u
        | SlugMaskKind.None -> UInt32.MaxValue

    let private makeMaskGpu (mask : SlugMaskState) =
        let mutable gpu = MaskGpu ()
        let resourceIndex =
            match mask.Kind with
            | SlugMaskKind.MsdfLayer index when index >= 0 -> uint32 index
            | SlugMaskKind.Shape index when index >= 0 -> uint32 index
            | _ -> UInt32.MaxValue
        gpu.Params <- mask.Parameters
        gpu.Params2 <- mask.Parameters2
        gpu.State <- uintVector4 (maskType mask.Kind) resourceIndex (if mask.Invert then 1u else 0u) 0u
        gpu

    let private makeGradientGpu (gradient : SlugGradient) firstStop =
        let mutable gpu = GradientGpu ()
        let kind, parameters, parameters2 =
            match gradient.Kind with
            | SlugGradientKind.Linear (startPoint, endPoint) ->
                0u, Vector4 (startPoint.X, startPoint.Y, endPoint.X - startPoint.X, endPoint.Y - startPoint.Y), Vector4.Zero
            | SlugGradientKind.Radial (center, radius) ->
                1u, Vector4 (center.X, center.Y, radius.X, radius.Y), Vector4.Zero
            | SlugGradientKind.FocalRadial (center0, radius0, center1, radius1) ->
                3u, Vector4 (center0.X, center0.Y, radius0, radius1), Vector4 (center1.X, center1.Y, 0.0f, 0.0f)
            | SlugGradientKind.Sweep (center, startAngle) ->
                2u, Vector4 (center.X, center.Y, startAngle, 1.0f), Vector4.Zero
            | SlugGradientKind.SweepRange (center, startAngle, endAngle) ->
                let span = endAngle - startAngle
                let scale = if abs span < 1.0e-6f then 1.0f else 2.0f * MathF.PI / span
                2u, Vector4 (center.X, center.Y, startAngle, scale), Vector4.Zero
        let extend =
            match gradient.Extend with
            | SlugGradientPad -> 0u
            | SlugGradientRepeat -> 1u
            | SlugGradientReflect -> 2u
        let stopRange = uintVector4 (uint32 firstStop) (uint32 gradient.Stops.Length) kind extend
        gpu.StopRange <- stopRange
        gpu.Params <- parameters
        gpu.Params2 <- parameters2
        gpu

    let private makeShapeGpu (data : SlugCompositeShapeData) (metadata : SlugShapeMetadata) =
        let mutable gpu = ShapeGpu ()
        gpu.Bounds <- Vector4 (metadata.Bounds.Min.X, metadata.Bounds.Min.Y, metadata.Bounds.Max.X, metadata.Bounds.Max.Y)
        gpu.BandTransform <- metadata.BandTransform
        // packBands stores list offsets relative to the shape's header origin.  The
        // shader adds each header offset to this origin before texelFetch.
        gpu.BandAddress <- uintVector4 (uint32 metadata.BandLocation.X) (uint32 metadata.BandLocation.Y) (uint32 metadata.BandLocation.X) (uint32 metadata.BandLocation.Y)
        let fillFlags = if metadata.FillRule = SlugFillEvenOdd then 1u else metadata.Flags
        gpu.BandFlags <- uintVector4 (uint32 metadata.BandMax.X) (uint32 metadata.BandMax.Y) (uint32 data.BandTextureWidth) fillFlags
        gpu

    let private makeLayerGpu (data : SlugCompositeShapeData) layerIndex (layer : SlugLayerState) =
        let fillId, gradientIndex, textureSlot, materialIndex = fillIdAndResources data layer.FillSource
        let maskIndex = if layer.MaskIndex >= 0 then uint32 layer.MaskIndex else UInt32.MaxValue
        let mutable gpu = LayerGpu ()
        gpu.Transform <- layer.Transform
        gpu.Color <- layer.Color.V4
        gpu.Origin <- Vector4 (layer.Origin.X, layer.Origin.Y, 0.0f, 0.0f)
        gpu.StateIds <- uintVector4 (uint32 (max 0 layer.ShapeIndex)) fillId (uint32 (max 0 layer.EffectId)) maskIndex
        // resourceIds.w is the HarfBuzz composite operator numeric ID.  It is
        // deliberately preserved even while all blend grouping remains caller-owned.
        gpu.ResourceIds <- uintVector4 gradientIndex textureSlot materialIndex (uint32 layer.CompositeMode)
        gpu.EffectParams <- layer.EffectParameters
        gpu.EffectParams2 <- layer.EffectParameters2
        gpu.GradientTransform <- layer.GradientTransform
        gpu.MaskMaterial <- Vector4 (layer.MaterialValues.X, layer.MaterialValues.Y, layer.MaterialValues.Z, textureSlot |> single)
        gpu

    let private makeComputeGpu (config : SlugShapeComputeConfig) =
        let mutable gpu = ComputeConfigGpu ()
        let mode =
            match config.Mode with
            | SlugShapeComputeMode.Radial -> 0u
            | SlugShapeComputeMode.Spiral -> 1u
            | SlugShapeComputeMode.Interference -> 2u
            | SlugShapeComputeMode.CanonicalColor -> 3u
        gpu.Dispatch <- uintVector4 (uint32 (max 0 config.LayerCount)) mode config.Flags config.Seed
        gpu.Params0 <- Vector4 (config.Speed, config.Amplitude, config.Frequency, config.Phase)
        gpu.Params1 <- config.Params1
        gpu


    let private descriptorDefinitions stage bulkMode =
        [| Pipeline.descriptorSet bulkMode 1
               [| Pipeline.descriptor 0 Hl.StorageBuffer stage 1
                  Pipeline.descriptor 1 Hl.StorageBuffer stage 1
                  Pipeline.descriptor 2 Hl.StorageBuffer stage 1
                  Pipeline.descriptor 3 Hl.CombinedImageSampler stage 1
                  Pipeline.descriptor 4 Hl.CombinedImageSampler stage 1
                  Pipeline.descriptor 5 Hl.StorageBuffer stage 1
                  Pipeline.descriptor 6 Hl.StorageBuffer stage 1
                  Pipeline.descriptor 7 Hl.StorageBuffer stage 1
                  Pipeline.descriptor 8 Hl.CombinedImageSampler stage maxFillTextures
                  Pipeline.descriptor 9 Hl.StorageBuffer stage 1 |] |]

    let private createGraphicsPipeline (vkc : Hl.VulkanContext) =
        Pipeline.Pipeline.create
            shaderPath
            Constants.Render.SpriteBatchesMax
            [| Pipeline.Transparent; Pipeline.Additive; Pipeline.Overwrite |]
            // COLRv1 and user-authored layer transforms may reflect the quad, so both windings must rasterize.
            [| false |]
            [||]
            (descriptorDefinitions Hl.VertexFragmentStage Hl.BulkSetIndexed)
            [||]
            [| vkc.SwapFormat |]
            None
            vkc

    let private createComputePipeline (vkc : Hl.VulkanContext) =
        Pipeline.ComputePipeline.create
            shaderPath
            Constants.Render.SpriteBatchesMax
            (descriptorDefinitions Hl.ComputeStage Hl.BulkSetIndexed)
            [||]
            vkc

    let private makeRenderArea (viewport : Viewport) (vkc : Hl.VulkanContext) =
        let pixelDensity = Hl.getWindowPixelDensity vkc.Window
        let renderAreaLogical = VkRect2D (viewport.Inner.Min.X, viewport.Outer.Max.Y - viewport.Inner.Max.Y, uint viewport.Inner.Size.X, uint viewport.Inner.Size.Y)
        Hl.scaleRectForPixelDensity pixelDensity renderAreaLogical

    let private makeScissor (clipOpt : Box2 voption) (effectiveViewProjection : Matrix4x4) (viewport : Viewport) (renderArea : VkRect2D) (vkc : Hl.VulkanContext) =
        let pixelDensity = Hl.getWindowPixelDensity vkc.Window
        match clipOpt with
        | ValueNone -> renderArea
        | ValueSome clip ->
            let minClip = Vector4.Transform(Vector4 (clip.Min.X, clip.Max.Y, 0.0f, 1.0f), effectiveViewProjection).V2
            let minNdc = minClip * single viewport.DisplayScalar
            let minScissor = (minNdc + v2One) * 0.5f * viewport.Inner.Size.V2
            let sizeClip = Vector4.Transform(Vector4 (clip.Size, 0.0f, 1.0f), effectiveViewProjection).V2
            let sizeNdc = sizeClip * single viewport.DisplayScalar
            let sizeScissor = sizeNdc * 0.5f * viewport.Inner.Size.V2
            let offset = v2i viewport.Inner.Min.X (viewport.Outer.Max.Y - viewport.Inner.Max.Y)
            let logical = VkRect2D ((minScissor.X |> round |> int) + offset.X, (single renderArea.extent.height - minScissor.Y |> round |> int) + offset.Y, uint (max 0 (sizeScissor.X |> round |> int)), uint (max 0 (sizeScissor.Y |> round |> int)))
            Hl.clipRect renderArea (Hl.scaleRectForPixelDensity pixelDensity logical)

    let private destroyComposite (gpu : CompositeGpu) vkc =
        gpu.CurveTexture.Destroy vkc
        gpu.BandTexture.Destroy vkc
        Buffer.Buffer.destroy gpu.ViewBuffer vkc
        Buffer.Buffer.destroy gpu.LayerBuffer vkc
        Buffer.Buffer.destroy gpu.ShapeBuffer vkc
        Buffer.Buffer.destroy gpu.GradientBuffer vkc
        Buffer.Buffer.destroy gpu.StopBuffer vkc
        Buffer.Buffer.destroy gpu.MaskBuffer vkc
        Buffer.Buffer.destroy gpu.ConfigBuffer vkc

    let private populateBaseLayers (gpu : CompositeGpu) (composite : SlugCompositeShape) =
        let data = composite.Data
        let mutable capturedRevision = -1L
        composite.ConsumeLayers (Action<int64, SlugLayerState array> (fun revision source ->
            for layerIndex in 0 .. dec source.Length do
                let layer = source[layerIndex]
                gpu.Layers[layerIndex] <- makeLayerGpu data layerIndex layer
                if layer.MaskIndex < 0 && layer.MaskKind <> SlugMaskKind.None then
                    let directMaskIndex = data.Masks.Length + layerIndex
                    gpu.Masks[directMaskIndex] <-
                        makeMaskGpu
                            { Kind = layer.MaskKind
                              Parameters = layer.MaskParameters
                              Parameters2 = layer.MaskParameters2
                              Invert = layer.MaskInvert }
                    gpu.Layers[layerIndex].StateIds <-
                        uintVector4
                            gpu.Layers[layerIndex].StateIds.X
                            gpu.Layers[layerIndex].StateIds.Y
                            gpu.Layers[layerIndex].StateIds.Z
                            (uint32 directMaskIndex)
            capturedRevision <- revision))
        capturedRevision

    let private uploadStatic (gpu : CompositeGpu) (data : SlugCompositeShapeData) (vkc : Hl.VulkanContext) =
        let shapes = data.Metadata |> Array.map (makeShapeGpu data)
        Buffer.Buffer.uploadArray 0 0 0 shapes gpu.ShapeBuffer vkc
        if data.Gradients.Length > 0 then
            let gradients = Array.zeroCreate<GradientGpu> data.Gradients.Length
            let mutable stopCursor = 0
            for i in 0 .. dec gradients.Length do
                gradients[i] <- makeGradientGpu data.Gradients[i] stopCursor
                stopCursor <- stopCursor + data.Gradients[i].Stops.Length
            Buffer.Buffer.uploadArray 0 0 0 gradients gpu.GradientBuffer vkc
        if data.GradientStops.Length > 0 then
            let stops =
                data.GradientStops
                |> Array.map (fun stop ->
                    let mutable gpuStop = StopGpu ()
                    gpuStop.Color <- stop.Color.V4
                    gpuStop.Offset <- Vector4 (stop.Offset, 0.0f, 0.0f, 0.0f)
                    gpuStop)
            Buffer.Buffer.uploadArray 0 0 0 stops gpu.StopBuffer vkc
        if gpu.Masks.Length > 0 then Buffer.Buffer.uploadArray 0 0 0 gpu.Masks gpu.MaskBuffer vkc

    let private createCompositeGpu (composite : SlugCompositeShape) (vkc : Hl.VulkanContext) =
        let data = composite.Data
        let curveTexels = if data.CurveTexels.Length = 0 then Array.zeroCreate<Vector4> (data.CurveTextureWidth * data.CurveTextureHeight) else data.CurveTexels
        let bandTexels = if data.BandTexels.Length = 0 then Array.zeroCreate<SlugShapeBandTexel> (data.BandTextureWidth * data.BandTextureHeight) else data.BandTexels
        let curveTexture = Texture.EagerTexture (Texture.Texture.createFromArray data.CurveTextureWidth data.CurveTextureHeight Hl.Rgba32f Hl.Rgba curveTexels Texture.RenderThread vkc)
        let bandTexture = Texture.EagerTexture (Texture.Texture.createFromArray data.BandTextureWidth data.BandTextureHeight Hl.Rgba16ui Hl.Rgba bandTexels Texture.RenderThread vkc)
        let layerCount = max 1 composite.LayerCount
        let shapeCount = max 1 data.ShapeCount
        let gradientCount = max 1 data.Gradients.Length
        let stopCount = max 1 data.GradientStops.Length
        let maskCount = max 1 (data.Masks.Length + composite.LayerCount)
        let layers = Array.zeroCreate<LayerGpu> composite.LayerCount
        let masks = Array.zeroCreate<MaskGpu> maskCount
        for i in 0 .. dec data.Masks.Length do masks[i] <- makeMaskGpu data.Masks[i]
        let viewBuffer = Buffer.Buffer.create (max 1 (sizeof<ViewGpu>)) Buffer.Storage vkc
        let layerBuffer = Buffer.Buffer.create (max 1 (sizeof<LayerGpu>) * layerCount) Buffer.Storage vkc
        let shapeBuffer = Buffer.Buffer.create (max 1 (sizeof<ShapeGpu>) * shapeCount) Buffer.Storage vkc
        let gradientBuffer = Buffer.Buffer.create (max 1 (sizeof<GradientGpu>) * gradientCount) Buffer.Storage vkc
        let stopBuffer = Buffer.Buffer.create (max 1 (sizeof<StopGpu>) * stopCount) Buffer.Storage vkc
        let maskBuffer = Buffer.Buffer.create (max 1 (sizeof<MaskGpu>) * maskCount) Buffer.Storage vkc
        let configBuffer = Buffer.Buffer.create (max 1 (sizeof<ComputeConfigGpu>)) Buffer.Storage vkc
        let gpu =
            { Id = data.Id
              Revision = data.Revision
              CurveTexture = curveTexture
              BandTexture = bandTexture
              ViewBuffer = viewBuffer
              LayerBuffer = layerBuffer
              ShapeBuffer = shapeBuffer
              GradientBuffer = gradientBuffer
              StopBuffer = stopBuffer
              MaskBuffer = maskBuffer
              ConfigBuffer = configBuffer
              Layers = layers
              Masks = masks
              CpuLayerRevision = -1L
              StaticInitialized = Array.zeroCreate Constants.Vulkan.MaxFramesInFlight
              DrawIndex = 0 }
        gpu.CpuLayerRevision <- populateBaseLayers gpu composite
        gpu

    let private uploadBaseLayerBuffers bufferIndex (gpu : CompositeGpu) (vkc : Hl.VulkanContext) =
        Buffer.Buffer.uploadArray bufferIndex 0 0 gpu.Layers gpu.LayerBuffer vkc
        Buffer.Buffer.uploadArray 0 0 0 gpu.Masks gpu.MaskBuffer vkc

    let CreateSlugShapeEnv (unfilteredSampler : Texture.Sampler) (filteredSampler : Texture.Sampler) (vkc : Hl.VulkanContext) =
        let fallback = Texture.EagerTexture (Texture.Texture.createFromArray 1 1 Hl.Rgba8 Hl.Rgba [| 255uy; 255uy; 255uy; 255uy |] Texture.RenderThread vkc)
        { VulkanContext = vkc
          UnfilteredSampler = unfilteredSampler
          FilteredSampler = filteredSampler
          GraphicsPipeline = createGraphicsPipeline vkc
          ComputePipeline = createComputePipeline vkc
          FallbackTexture = fallback
          Composites = Dictionary<SlugCompositeShape, CompositeGpu> HashIdentity.Reference
          DrawIndex = 0 }
    let private ensureComposite (composite : SlugCompositeShape) (env : SlugShapeEnv) (vkc : Hl.VulkanContext) =
        let data = composite.Data
        match env.Composites.TryGetValue composite with
        | true, gpu when gpu.Revision = data.Revision -> gpu
        | true, gpu ->
            Hl.Queue.waitIdle vkc.RenderQueue
            destroyComposite gpu vkc
            let created = createCompositeGpu composite vkc
            env.Composites[composite] <- created
            created
        | false, _ ->
            let created = createCompositeGpu composite vkc
            env.Composites.Add (composite, created)
            created

    let private writeDescriptors (drawIndex : int) bufferIndex (gpu : CompositeGpu) (fillTextures : Texture.Texture array) (env : SlugShapeEnv) (vkc : Hl.VulkanContext) =
        let pipeline = env.GraphicsPipeline
        let fillTexture index =
            if not (isNull fillTextures) && index < fillTextures.Length && not (obj.ReferenceEquals (fillTextures[index], null)) then fillTextures[index]
            else env.FallbackTexture
        Pipeline.Pipeline.writeDescriptorStorageBuffer 0 0 drawIndex 0 gpu.ViewBuffer[bufferIndex] pipeline vkc
        Pipeline.Pipeline.writeDescriptorStorageBuffer 0 1 drawIndex 0 gpu.LayerBuffer[bufferIndex] pipeline vkc
        Pipeline.Pipeline.writeDescriptorStorageBuffer 0 2 drawIndex 0 gpu.ShapeBuffer[0] pipeline vkc
        Pipeline.Pipeline.writeDescriptorCombinedImageSampler 0 3 drawIndex 0 gpu.CurveTexture env.UnfilteredSampler pipeline vkc
        Pipeline.Pipeline.writeDescriptorCombinedImageSampler 0 4 drawIndex 0 gpu.BandTexture env.UnfilteredSampler pipeline vkc
        Pipeline.Pipeline.writeDescriptorStorageBuffer 0 5 drawIndex 0 gpu.GradientBuffer[0] pipeline vkc
        Pipeline.Pipeline.writeDescriptorStorageBuffer 0 6 drawIndex 0 gpu.StopBuffer[0] pipeline vkc
        Pipeline.Pipeline.writeDescriptorStorageBuffer 0 7 drawIndex 0 gpu.MaskBuffer[0] pipeline vkc
        for i in 0 .. maxFillTextures - 1 do Pipeline.Pipeline.writeDescriptorCombinedImageSampler 0 8 drawIndex i (fillTexture i) env.FilteredSampler pipeline vkc
        Pipeline.Pipeline.writeDescriptorStorageBuffer 0 9 drawIndex 0 gpu.ConfigBuffer[bufferIndex] pipeline vkc

    let private writeComputeDescriptors (drawIndex : int) bufferIndex (gpu : CompositeGpu) (env : SlugShapeEnv) (vkc : Hl.VulkanContext) =
        let pipeline = env.ComputePipeline
        Pipeline.ComputePipeline.writeDescriptorStorageBuffer 0 0 drawIndex 0 gpu.ViewBuffer[bufferIndex] pipeline vkc
        Pipeline.ComputePipeline.writeDescriptorStorageBuffer 0 1 drawIndex 0 gpu.LayerBuffer[bufferIndex] pipeline vkc
        Pipeline.ComputePipeline.writeDescriptorStorageBuffer 0 2 drawIndex 0 gpu.ShapeBuffer[0] pipeline vkc
        Pipeline.ComputePipeline.writeDescriptorStorageBuffer 0 5 drawIndex 0 gpu.GradientBuffer[0] pipeline vkc
        Pipeline.ComputePipeline.writeDescriptorStorageBuffer 0 6 drawIndex 0 gpu.StopBuffer[0] pipeline vkc
        Pipeline.ComputePipeline.writeDescriptorStorageBuffer 0 7 drawIndex 0 gpu.MaskBuffer[0] pipeline vkc
        Pipeline.ComputePipeline.writeDescriptorStorageBuffer 0 9 drawIndex 0 gpu.ConfigBuffer[bufferIndex] pipeline vkc

    /// Reset per-render-frame descriptor allocation.
    let BeginSlugShapeFrame (env : SlugShapeEnv) =
        env.DrawIndex <- 0
        for gpu in env.Composites.Values do gpu.DrawIndex <- 0

    let RenderSlugShape rootTransform (clipOpt : Box2 voption) (composite : SlugCompositeShape) (seconds : single) (delta : single) (frame : uint32) (seed : uint32) (computeConfigOpt : SlugShapeComputeConfig option) (fillTextures : Texture.Texture array) (viewProjection : Matrix4x4) (viewport : Viewport) blend env vkc =
        if obj.ReferenceEquals (composite, null) then nullArg (nameof composite)
        if env.DrawIndex >= env.GraphicsPipeline.BulkDrawLimit then
            Log.warnOnce "SlugShape draw limit reached for this frame."
        else
            let gpu = ensureComposite composite env vkc
            let bufferIndex = gpu.DrawIndex
            let currentFrame = Hl.CurrentFrame
            let layerRevision = composite.LayerRevision
            if gpu.CpuLayerRevision <> layerRevision then
                gpu.CpuLayerRevision <- populateBaseLayers gpu composite
            if not gpu.StaticInitialized[currentFrame] then
                uploadStatic gpu composite.Data vkc
                gpu.StaticInitialized[currentFrame] <- true
            uploadBaseLayerBuffers bufferIndex gpu vkc
            let effectiveViewProjection = rootTransform * viewProjection
            let renderArea = makeRenderArea viewport vkc
            let mutable scissor = makeScissor clipOpt effectiveViewProjection viewport renderArea vkc
            if Hl.validateRect scissor then
                let mutable view = ViewGpu ()
                let pixelWidth = single renderArea.extent.width
                let pixelHeight = single renderArea.extent.height
                view.ViewProjection <- effectiveViewProjection
                view.Viewport <- Vector4 (pixelWidth, pixelHeight, 1.0f / max pixelWidth 1.0f, 1.0f / max pixelHeight 1.0f)
                view.Time <- Vector4 (seconds, delta, single frame, single seed)
                view.Counts <- uintVector4 (uint32 composite.LayerCount) (uint32 composite.Data.ShapeCount) 0u 0u
                Buffer.Buffer.uploadValue bufferIndex 0 0 view gpu.ViewBuffer vkc
                let mutable computeEnabled = false
                match computeConfigOpt with
                | Some config ->
                    let configGpu = makeComputeGpu { config with LayerCount = if config.LayerCount > 0 then min config.LayerCount composite.LayerCount else composite.LayerCount }
                    Buffer.Buffer.uploadValue bufferIndex 0 0 configGpu gpu.ConfigBuffer vkc
                    computeEnabled <- true
                | None ->
                    let configGpu = makeComputeGpu { LayerCount = composite.LayerCount; Mode = SlugShapeComputeMode.Radial; Flags = 0u; Seed = seed; Speed = 0.0f; Amplitude = 0.0f; Frequency = 0.0f; Phase = 0.0f; Params1 = Vector4.Zero }
                    Buffer.Buffer.uploadValue bufferIndex 0 0 configGpu gpu.ConfigBuffer vkc
                writeDescriptors env.DrawIndex bufferIndex gpu fillTextures env vkc
                if computeEnabled then writeComputeDescriptors env.DrawIndex bufferIndex gpu env vkc
                let cb = vkc.RenderCommandBuffer
                if computeEnabled then
                    Hl.recordHostWritesToCompute cb (fst gpu.ViewBuffer[bufferIndex])
                    Hl.recordHostWritesToCompute cb (fst gpu.LayerBuffer[bufferIndex])
                    Hl.recordHostWritesToCompute cb (fst gpu.ConfigBuffer[bufferIndex])
                    Pipeline.ComputePipeline.bind cb env.ComputePipeline
                    Pipeline.ComputePipeline.bindDescriptorSet cb 0 env.DrawIndex env.ComputePipeline
                    let groups = max 1u (uint32 ((composite.LayerCount + 63) / 64))
                    Pipeline.ComputePipeline.dispatch cb groups 1u 1u env.ComputePipeline
                    Hl.recordComputeWritesToGraphics cb (fst gpu.LayerBuffer[bufferIndex])
                let mutable vkViewport = Hl.makeViewport true renderArea
                let mutable rendering = Hl.makeRenderingInfo [| vkc.SwapchainImageView |] None renderArea None
                Vulkan.vkCmdBeginRendering (cb, asPointer &rendering)
                match Pipeline.Pipeline.tryGetVkPipeline blend false env.GraphicsPipeline with
                | Some vkPipeline ->
                    Vulkan.vkCmdBindPipeline (cb, VkPipelineBindPoint.Graphics, vkPipeline)
                    Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
                    Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)
                    let mutable descriptorSet = env.GraphicsPipeline.VkDescriptorSet 0 env.DrawIndex
                    Vulkan.vkCmdBindDescriptorSets (cb, VkPipelineBindPoint.Graphics, env.GraphicsPipeline.PipelineLayout, 0u, 1u, asPointer &descriptorSet, 0u, nullPtr)
                    Vulkan.vkCmdDraw (cb, 6u, uint composite.LayerCount, 0u, 0u)
                    Hl.reportDrawCall composite.LayerCount
                | None -> Log.warnOnce "Cannot draw SlugShape because VkPipeline does not exist."
                Vulkan.vkCmdEndRendering cb
                gpu.DrawIndex <- inc gpu.DrawIndex
                env.DrawIndex <- inc env.DrawIndex

    let ReloadShaders env vkc =
        Pipeline.Pipeline.reloadShaders env.GraphicsPipeline vkc
        Pipeline.ComputePipeline.reload env.ComputePipeline vkc

    let DestroySlugShapeComposite (id : Guid) (env : SlugShapeEnv) (vkc : Hl.VulkanContext) =
        let matching = ResizeArray<SlugCompositeShape> ()
        for pair in env.Composites do
            if pair.Key.Data.Id = id then matching.Add pair.Key
        if matching.Count > 0 then
            Hl.Queue.waitIdle vkc.RenderQueue
            for composite in matching do
                destroyComposite env.Composites[composite] vkc
                env.Composites.Remove composite |> ignore

    let DestroySlugShapeEnv (env : SlugShapeEnv) (vkc : Hl.VulkanContext) =
        Hl.Queue.waitIdle vkc.RenderQueue
        for pair in env.Composites do destroyComposite pair.Value vkc
        env.Composites.Clear ()
        Pipeline.Pipeline.destroy env.GraphicsPipeline vkc
        Pipeline.ComputePipeline.destroy env.ComputePipeline vkc
        env.FallbackTexture.Destroy vkc
