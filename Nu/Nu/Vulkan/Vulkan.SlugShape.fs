// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan

open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Prime
open Nu
open Vortice.Vulkan

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
          CurveTexture : Texture
          BandTexture : Texture
          ViewBuffer : VulkanBuffer
          LayerBuffer : VulkanBuffer
          ShapeBuffer : VulkanBuffer
          GradientBuffer : VulkanBuffer
          StopBuffer : VulkanBuffer
          MaskBuffer : VulkanBuffer
          ConfigBuffer : VulkanBuffer
          Layers : LayerGpu array
          Masks : MaskGpu array
          mutable CpuLayerRevision : int64 }

    type [<ReferenceEquality>] SlugShapeEnv =
        private
            { VulkanContext : VulkanContext
              UnfilteredSampler : Sampler
              FilteredSampler : Sampler
              GraphicsPipeline : Pipeline
              ComputePipeline : ComputePipeline
              FallbackTexture : Texture
              Composites : Dictionary<SlugCompositeShape, CompositeGpu> }
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


    let private descriptorDefinitions stage : DescriptorSetDefinition array =
        [|(Pipeline.descriptorSet<int>
            [|Pipeline.descriptor 0 StorageBuffer stage 1
              Pipeline.descriptor 1 StorageBuffer stage 1
              Pipeline.descriptor 2 StorageBuffer stage 1
              Pipeline.descriptor 3 CombinedImageSampler stage 1
              Pipeline.descriptor 4 CombinedImageSampler stage 1
              Pipeline.descriptor 5 StorageBuffer stage 1
              Pipeline.descriptor 6 StorageBuffer stage 1
              Pipeline.descriptor 7 StorageBuffer stage 1
              Pipeline.descriptor 8 CombinedImageSampler stage maxFillTextures
              Pipeline.descriptor 9 StorageBuffer stage 1|] :> DescriptorSetDefinition)|]

    let private createGraphicsPipeline (context : VulkanContext) =
        Pipeline.create
            shaderPath
            [|VulkanTransparent; VulkanAdditive; VulkanOverwrite|]
            [|false|]
            [||]
            (descriptorDefinitions VertexAndFragmentStage)
            [||]
            [|context.SwapFormat|]
            None
            [||]

    let private createComputePipeline () =
        ComputePipeline.create
            shaderPath
            (descriptorDefinitions ComputeStage)
            [||]

    let private makeRenderArea (viewport : Viewport) =
        VkRect2D (viewport.Inner.Min.X, viewport.Outer.Max.Y - viewport.Inner.Max.Y, uint viewport.Inner.Size.X, uint viewport.Inner.Size.Y)

    let private makeScissor (clipOpt : Box2 voption) (effectiveViewProjection : Matrix4x4) (viewport : Viewport) (renderArea : VkRect2D) =
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
            let scissor = VkRect2D ((minScissor.X |> round |> int) + offset.X, (single renderArea.extent.height - minScissor.Y |> round |> int) + offset.Y, uint (max 0 (sizeScissor.X |> round |> int)), uint (max 0 (sizeScissor.Y |> round |> int)))
            Hl.clipRect renderArea scissor

    let private destroyComposite (gpu : CompositeGpu) context =
        Texture.destroy gpu.CurveTexture context
        Texture.destroy gpu.BandTexture context
        VulkanBuffer.destroy gpu.ViewBuffer context
        VulkanBuffer.destroy gpu.LayerBuffer context
        VulkanBuffer.destroy gpu.ShapeBuffer context
        VulkanBuffer.destroy gpu.GradientBuffer context
        VulkanBuffer.destroy gpu.StopBuffer context
        VulkanBuffer.destroy gpu.MaskBuffer context
        VulkanBuffer.destroy gpu.ConfigBuffer context

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

    let private uploadStatic (gpu : CompositeGpu) (data : SlugCompositeShapeData) context =
        let shapes = data.Metadata |> Array.map (makeShapeGpu data)
        VulkanBuffer.uploadArray shapes gpu.ShapeBuffer context
        if data.Gradients.Length > 0 then
            let gradients = Array.zeroCreate<GradientGpu> data.Gradients.Length
            let mutable stopCursor = 0
            for i in 0 .. dec gradients.Length do
                gradients[i] <- makeGradientGpu data.Gradients[i] stopCursor
                stopCursor <- stopCursor + data.Gradients[i].Stops.Length
            VulkanBuffer.uploadArray gradients gpu.GradientBuffer context
        if data.GradientStops.Length > 0 then
            let stops =
                data.GradientStops
                |> Array.map (fun stop ->
                    let mutable gpuStop = StopGpu ()
                    gpuStop.Color <- stop.Color.V4
                    gpuStop.Offset <- Vector4 (stop.Offset, 0.0f, 0.0f, 0.0f)
                    gpuStop)
            VulkanBuffer.uploadArray stops gpu.StopBuffer context
        if gpu.Masks.Length > 0 then VulkanBuffer.uploadArray gpu.Masks gpu.MaskBuffer context

    let private createCompositeGpu (composite : SlugCompositeShape) context =
        let data = composite.Data
        let curveTexels = if data.CurveTexels.Length = 0 then Array.zeroCreate<Vector4> (data.CurveTextureWidth * data.CurveTextureHeight) else data.CurveTexels
        let bandTexels = if data.BandTexels.Length = 0 then Array.zeroCreate<SlugShapeBandTexel> (data.BandTextureWidth * data.BandTextureHeight) else data.BandTexels
        let curveTexture = EagerTexture (Texture.createFromArray data.CurveTextureWidth data.CurveTextureHeight Rgba32f Rgba curveTexels RenderThread context)
        let bandTexture = EagerTexture (Texture.createFromArray data.BandTextureWidth data.BandTextureHeight Rgba16ui Rgba bandTexels RenderThread context)
        let layerCount = max 1 composite.LayerCount
        let shapeCount = max 1 data.ShapeCount
        let gradientCount = max 1 data.Gradients.Length
        let stopCount = max 1 data.GradientStops.Length
        let maskCount = max 1 (data.Masks.Length + composite.LayerCount)
        let layers = Array.zeroCreate<LayerGpu> composite.LayerCount
        let masks = Array.zeroCreate<MaskGpu> maskCount
        for i in 0 .. dec data.Masks.Length do masks[i] <- makeMaskGpu data.Masks[i]
        let viewBuffer = VulkanBuffer.create Storage (max 1 sizeof<ViewGpu>) context
        let layerBuffer = VulkanBuffer.create Storage (max 1 (sizeof<LayerGpu> * layerCount)) context
        let shapeBuffer = VulkanBuffer.create Storage (max 1 (sizeof<ShapeGpu> * shapeCount)) context
        let gradientBuffer = VulkanBuffer.create Storage (max 1 (sizeof<GradientGpu> * gradientCount)) context
        let stopBuffer = VulkanBuffer.create Storage (max 1 (sizeof<StopGpu> * stopCount)) context
        let maskBuffer = VulkanBuffer.create Storage (max 1 (sizeof<MaskGpu> * maskCount)) context
        let configBuffer = VulkanBuffer.create Storage (max 1 sizeof<ComputeConfigGpu>) context
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
              CpuLayerRevision = -1L }
        gpu.CpuLayerRevision <- populateBaseLayers gpu composite
        uploadStatic gpu data context
        gpu

    let private uploadBaseLayerBuffers (gpu : CompositeGpu) context =
        VulkanBuffer.uploadArray gpu.Layers gpu.LayerBuffer context
        VulkanBuffer.uploadArray gpu.Masks gpu.MaskBuffer context

    let CreateSlugShapeEnv (unfilteredSampler : Sampler) (filteredSampler : Sampler) (context : VulkanContext) =
        let fallback = EagerTexture (Texture.createFromArray 1 1 Rgba8 Rgba [|255uy; 255uy; 255uy; 255uy|] RenderThread context)
        { VulkanContext = context
          UnfilteredSampler = unfilteredSampler
          FilteredSampler = filteredSampler
          GraphicsPipeline = createGraphicsPipeline context
          ComputePipeline = createComputePipeline ()
          FallbackTexture = fallback
          Composites = Dictionary<SlugCompositeShape, CompositeGpu> HashIdentity.Reference }
    let private ensureComposite (composite : SlugCompositeShape) (env : SlugShapeEnv) (context : VulkanContext) =
        let data = composite.Data
        match env.Composites.TryGetValue composite with
        | true, gpu when gpu.Revision = data.Revision -> gpu
        | true, gpu ->
            ConcurrentCommandQueue.waitIdle context.RenderQueue
            destroyComposite gpu context
            let created = createCompositeGpu composite context
            env.Composites[composite] <- created
            created
        | false, _ ->
            let created = createCompositeGpu composite context
            env.Composites.Add (composite, created)
            created

    let private writeStorageDescriptor binding descriptorIndex (buffer : VulkanBuffer) vkDescriptorSet =
        let mutable info = VkDescriptorBufferInfo ()
        info.buffer <- buffer.VkBuffer
        info.range <- Vulkan.VK_WHOLE_SIZE
        let mutable write = VkWriteDescriptorSet ()
        write.dstSet <- vkDescriptorSet
        write.dstBinding <- uint binding
        write.dstArrayElement <- uint descriptorIndex
        write.descriptorCount <- 1u
        write.descriptorType <- VkDescriptorType.StorageBuffer
        write.pBufferInfo <- &&info
        DeviceApi.vkUpdateDescriptorSets (1u, &&write, 0u, nullPtr)

    let private writeDescriptors (gpu : CompositeGpu) (fillTextures : Texture array) (env : SlugShapeEnv) vkDescriptorSet =
        let fillTexture index =
            if not (isNull fillTextures) && index < fillTextures.Length then fillTextures[index]
            else env.FallbackTexture
        writeStorageDescriptor 0 0 gpu.ViewBuffer vkDescriptorSet
        writeStorageDescriptor 1 0 gpu.LayerBuffer vkDescriptorSet
        writeStorageDescriptor 2 0 gpu.ShapeBuffer vkDescriptorSet
        Pipeline.writeDescriptorCombinedTextureSampler 3 0 gpu.CurveTexture env.UnfilteredSampler vkDescriptorSet
        Pipeline.writeDescriptorCombinedTextureSampler 4 0 gpu.BandTexture env.UnfilteredSampler vkDescriptorSet
        writeStorageDescriptor 5 0 gpu.GradientBuffer vkDescriptorSet
        writeStorageDescriptor 6 0 gpu.StopBuffer vkDescriptorSet
        writeStorageDescriptor 7 0 gpu.MaskBuffer vkDescriptorSet
        for i in 0 .. maxFillTextures - 1 do
            Pipeline.writeDescriptorCombinedTextureSampler 8 i (fillTexture i) env.FilteredSampler vkDescriptorSet
        writeStorageDescriptor 9 0 gpu.ConfigBuffer vkDescriptorSet

    /// Reset per-render-frame descriptor allocation.
    let BeginSlugShapeFrame (env : SlugShapeEnv) =
        Pipeline.beginFrame env.GraphicsPipeline
        ComputePipeline.beginFrame env.ComputePipeline
        for gpu in env.Composites.Values do
            VulkanBuffer.beginFrame gpu.ViewBuffer
            VulkanBuffer.beginFrame gpu.LayerBuffer
            VulkanBuffer.beginFrame gpu.ShapeBuffer
            VulkanBuffer.beginFrame gpu.GradientBuffer
            VulkanBuffer.beginFrame gpu.StopBuffer
            VulkanBuffer.beginFrame gpu.MaskBuffer
            VulkanBuffer.beginFrame gpu.ConfigBuffer

    let RenderSlugShape rootTransform (clipOpt : Box2 voption) (composite : SlugCompositeShape) (seconds : single) (delta : single) (frame : uint32) (seed : uint32) (computeConfigOpt : SlugShapeComputeConfig option) (fillTextures : Texture array) (viewProjection : Matrix4x4) (viewport : Viewport) blend env context =
        if obj.ReferenceEquals (composite, null) then nullArg (nameof composite)
        let gpu = ensureComposite composite env context
        let layerRevision = composite.LayerRevision
        if gpu.CpuLayerRevision <> layerRevision then
            gpu.CpuLayerRevision <- populateBaseLayers gpu composite
        uploadBaseLayerBuffers gpu context
        let effectiveViewProjection = rootTransform * viewProjection
        let renderArea = makeRenderArea viewport
        let mutable scissor = makeScissor clipOpt effectiveViewProjection viewport renderArea
        if Hl.validateRect scissor then
            match Pipeline.tryGetVkPipeline blend false env.GraphicsPipeline with
            | Some vkPipeline ->
                let mutable view = ViewGpu ()
                let pixelWidth = single renderArea.extent.width
                let pixelHeight = single renderArea.extent.height
                view.ViewProjection <- effectiveViewProjection
                view.Viewport <- Vector4 (pixelWidth, pixelHeight, 1.0f / max pixelWidth 1.0f, 1.0f / max pixelHeight 1.0f)
                view.Time <- Vector4 (seconds, delta, single frame, single seed)
                view.Counts <- uintVector4 (uint32 composite.LayerCount) (uint32 composite.Data.ShapeCount) 0u 0u
                VulkanBuffer.uploadValue view gpu.ViewBuffer context
                let computeEnabled, configGpu =
                    match computeConfigOpt with
                    | Some config ->
                        true,
                        makeComputeGpu
                            { config with
                                LayerCount =
                                    if config.LayerCount > 0
                                    then min config.LayerCount composite.LayerCount
                                    else composite.LayerCount }
                    | None ->
                        false,
                        makeComputeGpu
                            { LayerCount = composite.LayerCount
                              Mode = SlugShapeComputeMode.Radial
                              Flags = 0u
                              Seed = seed
                              Speed = 0.0f
                              Amplitude = 0.0f
                              Frequency = 0.0f
                              Phase = 0.0f
                              Params1 = Vector4.Zero }
                VulkanBuffer.uploadValue configGpu gpu.ConfigBuffer context
                let viewBuffer = gpu.ViewBuffer.VkBuffer
                let layerBuffer = gpu.LayerBuffer.VkBuffer
                let configBuffer = gpu.ConfigBuffer.VkBuffer
                let mutable descriptorSet =
                    Pipeline.specifyDescriptorSet 0 env.GraphicsPipeline.DrawIndex env.GraphicsPipeline $ fun vkSet ->
                        writeDescriptors gpu fillTextures env vkSet
                let commandBuffer = context.RenderCommandBuffer
                if computeEnabled then
                    Hl.recordHostWritesToCompute commandBuffer viewBuffer
                    Hl.recordHostWritesToCompute commandBuffer layerBuffer
                    Hl.recordHostWritesToCompute commandBuffer configBuffer
                    ComputePipeline.bind commandBuffer env.ComputePipeline
                    ComputePipeline.bindDescriptorSet commandBuffer 0u descriptorSet env.ComputePipeline
                    let groups = max 1u (uint32 ((composite.LayerCount + 63) / 64))
                    ComputePipeline.dispatch commandBuffer groups 1u 1u env.ComputePipeline
                    Hl.recordComputeWritesToGraphics commandBuffer layerBuffer
                let mutable vkViewport = Hl.makeViewport true renderArea
                let mutable renderingInfo = Hl.makeRenderingInfo [|context.SwapchainImageView|] None renderArea None
                DeviceApi.vkCmdBeginRendering (commandBuffer, &&renderingInfo)
                DeviceApi.vkCmdSetViewport (commandBuffer, 0u, 1u, &&vkViewport)
                DeviceApi.vkCmdSetScissor (commandBuffer, 0u, 1u, &&scissor)
                DeviceApi.vkCmdBindPipeline (commandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                DeviceApi.vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, env.GraphicsPipeline.PipelineLayout, 0u, 1u, &&descriptorSet, 0u, nullPtr)
                DeviceApi.vkCmdDraw (commandBuffer, 6u, uint composite.LayerCount, 0u, 0u)
                DeviceApi.vkCmdEndRendering commandBuffer
                Hl.reportDrawCall composite.LayerCount true
                Pipeline.advance env.GraphicsPipeline
                VulkanContext.advanceRenderCommandBuffer context
                VulkanBuffer.advance gpu.ViewBuffer
                VulkanBuffer.advance gpu.LayerBuffer
                VulkanBuffer.advance gpu.MaskBuffer
                VulkanBuffer.advance gpu.ConfigBuffer
            | None -> Log.warnOnce "Cannot draw SlugShape because VkPipeline does not exist."

    let ReloadShaders env context =
        Pipeline.reloadShaders env.GraphicsPipeline context
        ComputePipeline.reload env.ComputePipeline context

    let DestroySlugShapeComposite (id : Guid) (env : SlugShapeEnv) (context : VulkanContext) =
        let matching = ResizeArray<SlugCompositeShape> ()
        for pair in env.Composites do
            if pair.Key.Data.Id = id then matching.Add pair.Key
        if matching.Count > 0 then
            ConcurrentCommandQueue.waitIdle context.RenderQueue
            for composite in matching do
                destroyComposite env.Composites[composite] context
                env.Composites.Remove composite |> ignore

    let DestroySlugShapeEnv (env : SlugShapeEnv) (context : VulkanContext) =
        ConcurrentCommandQueue.waitIdle context.RenderQueue
        for pair in env.Composites do destroyComposite pair.Value context
        env.Composites.Clear ()
        Pipeline.destroy env.GraphicsPipeline context
        ComputePipeline.destroy env.ComputePipeline
        Texture.destroy env.FallbackTexture context
