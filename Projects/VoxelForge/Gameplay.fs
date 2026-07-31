namespace VoxelForge
open System
open System.Collections.Generic
open System.Numerics
open System.Threading.Tasks
open Prime
open Nu

type GameplayState =
    | Playing
    | Quit

type VoxelAimPick =
    { Position : Vector3
      Normal : Vector3
      DestroyBlockCoord : Vector3i
      PlaceBlockCoordOpt : Vector3i option }

type [<ReferenceEquality>] Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      VoxelModelReady : bool
      VoxelLevelOpt : VoxelLevel option
      VoxelChunks : VoxelChunk array
      OcclusionBlockCoords : Set<Vector3i>
      StreamCenterChunkCoordOpt : Vector3i option
      AimPickOpt : VoxelAimPick option
      SelectedBlockIndex : int
      SelectedBlockPreviewPositionOpt : Vector3 option
      PortalPair : PortalPair
      PortalPlayerTracking : PortalPlayerTracking }

    static member val empty =
        { GameplayTime = 0L
          GameplayState = Quit
          VoxelModelReady = false
          VoxelLevelOpt = None
          VoxelChunks = [||]
          OcclusionBlockCoords = Set.empty
          StreamCenterChunkCoordOpt = None
          AimPickOpt = None
          SelectedBlockIndex = 0
          SelectedBlockPreviewPositionOpt = None
          PortalPair = PortalLogic.defaultPair
          PortalPlayerTracking = PortalPlayerTracking.empty }

    static member val initial =
        { Gameplay.empty with
            GameplayState = Playing }

type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | TimeUpdate
    | TryDestroyBlock
    | TryPlaceBlock
    interface Message

type GameplayCommand =
    | EnsureVoxelModel
    | UseGeneratedWorld of GeneratedWorldPackage
    | DestroyVoxelModel of VoxelChunk array * PlaceableBlock array * VoxelLevel option
    | DestroyBlock of VoxelAimPick
    | PlaceBlock of VoxelAimPick
    | StreamVoxelChunks
    | ResolvePortalTraversal
    | StartQuitting
    interface Command

[<NoEquality; NoComparison>]
type VoxelChunkSyncDelta =
    { ChunksAdded : VoxelChunk array
      ChunksUpdated : VoxelChunk array
      ChunksRemoved : VoxelChunk array
      VoxelAssetsToDestroy : VoxelChunk array }

    static member val Empty =
        { ChunksAdded = [||]
          ChunksUpdated = [||]
          ChunksRemoved = [||]
          VoxelAssetsToDestroy = [||] }

[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.QuitEvent = Events.QuitEvent --> this

[<RequireQualifiedAccess>]
module VoxelChunkVisibility =

    let private depthWidth = 160
    let private depthHeight = 90
    let private depthBias = 0.00075f
    let private occlusionCoveragePadding = 1
    let private occlusionCandidateAreaMin = 9
    let private occluderAreaMin = 6
    let private portalEyeRecursionLimitMax = 8

    [<Struct>]
    type private ProjectedBounds =
        { MinX : int
          MaxX : int
          MinY : int
          MaxY : int
          NearDepth : single
          FarDepth : single }

        member this.Area =
            (this.MaxX - this.MinX + 1) * (this.MaxY - this.MinY + 1)

    type private VoxelChunkDistanceComparer () =

        member val EyeCenter = v3Zero with get, set

        interface IComparer<VoxelChunk> with
            member this.Compare (left, right) =
                let leftDistance = Vector3.DistanceSquared (this.EyeCenter, left.ChunkCenter)
                let rightDistance = Vector3.DistanceSquared (this.EyeCenter, right.ChunkCenter)
                if leftDistance < rightDistance then -1
                elif leftDistance > rightDistance then 1
                else
                    let coordCompareZ = compare left.ChunkCoord.Z right.ChunkCoord.Z
                    if coordCompareZ <> 0 then coordCompareZ
                    else
                        let coordCompareY = compare left.ChunkCoord.Y right.ChunkCoord.Y
                        if coordCompareY <> 0 then coordCompareY
                        else compare left.ChunkCoord.X right.ChunkCoord.X

    let chunkBounds (voxelChunk : VoxelChunk) =
        box3 (voxelChunk.ChunkCenter - voxelChunk.ChunkSize * 0.5f) voxelChunk.ChunkSize

    let private portalBounds (portal : PortalSurface) =
        let right = portal.Rotation.Right * portal.HalfExtents.X
        let up = portal.Rotation.Up * portal.HalfExtents.Y
        let forward = portal.Rotation.Forward * 0.05f
        let mutable min = v3Dup Single.PositiveInfinity
        let mutable max = v3Dup Single.NegativeInfinity
        let includePoint point =
            min <- Vector3.Min (min, point)
            max <- Vector3.Max (max, point)
        includePoint (portal.Center - right - up - forward)
        includePoint (portal.Center - right - up + forward)
        includePoint (portal.Center - right + up - forward)
        includePoint (portal.Center - right + up + forward)
        includePoint (portal.Center + right - up - forward)
        includePoint (portal.Center + right - up + forward)
        includePoint (portal.Center + right + up - forward)
        includePoint (portal.Center + right + up + forward)
        box3 min (max - min)

    let private portalInView (portal : PortalSurface) (world : World) =
        World.boundsInView3d false Exterior (portalBounds portal) world

    let private tryProjectBounds (viewProjection : Matrix4x4) (bounds : Box3) =
        let mutable minX = Single.PositiveInfinity
        let mutable maxX = Single.NegativeInfinity
        let mutable minY = Single.PositiveInfinity
        let mutable maxY = Single.NegativeInfinity
        let mutable nearDepth = Single.PositiveInfinity
        let mutable farDepth = Single.NegativeInfinity
        let mutable valid = true
        let projectCorner (corner : Vector3) =
            let clip = Vector4.Transform (Vector4 (corner, 1.0f), viewProjection)
            if clip.W <= 0.001f then valid <- false
            else
                let invW = 1.0f / clip.W
                let ndcX = clip.X * invW
                let ndcY = clip.Y * invW
                let ndcZ = clip.Z * invW
                minX <- min minX ndcX
                maxX <- max maxX ndcX
                minY <- min minY ndcY
                maxY <- max maxY ndcY
                nearDepth <- min nearDepth ndcZ
                farDepth <- max farDepth ndcZ
        let min = bounds.Min
        let max = bounds.Min + bounds.Size
        projectCorner (v3 min.X min.Y min.Z)
        projectCorner (v3 min.X min.Y max.Z)
        projectCorner (v3 max.X min.Y max.Z)
        projectCorner (v3 max.X min.Y min.Z)
        projectCorner (v3 max.X max.Y max.Z)
        projectCorner (v3 min.X max.Y max.Z)
        projectCorner (v3 min.X max.Y min.Z)
        projectCorner (v3 max.X max.Y min.Z)
        if not valid || maxX < -1.0f || minX > 1.0f || maxY < -1.0f || minY > 1.0f then ValueNone
        else
            let screenMinX = Math.Clamp (int (MathF.Floor (((minX * 0.5f) + 0.5f) * single depthWidth)), 0, dec depthWidth)
            let screenMaxX = Math.Clamp (int (MathF.Ceiling (((maxX * 0.5f) + 0.5f) * single depthWidth)), 0, dec depthWidth)
            let screenMinY = Math.Clamp (int (MathF.Floor ((1.0f - ((maxY * 0.5f) + 0.5f)) * single depthHeight)), 0, dec depthHeight)
            let screenMaxY = Math.Clamp (int (MathF.Ceiling ((1.0f - ((minY * 0.5f) + 0.5f)) * single depthHeight)), 0, dec depthHeight)
            let screenWidth = screenMaxX - screenMinX + 1
            let screenHeight = screenMaxY - screenMinY + 1
            if screenMinX > screenMaxX || screenMinY > screenMaxY || screenWidth <= 1 || screenHeight <= 1 || Single.IsNaN nearDepth || Single.IsNaN farDepth
            then ValueNone
            else ValueSome { MinX = screenMinX; MaxX = screenMaxX; MinY = screenMinY; MaxY = screenMaxY; NearDepth = nearDepth; FarDepth = farDepth }

    let private isCovered (depths : single array) (bounds : ProjectedBounds) =
        if bounds.Area < occlusionCandidateAreaMin then false
        else
            let minX = max 0 (bounds.MinX - occlusionCoveragePadding)
            let maxX = min (dec depthWidth) (bounds.MaxX + occlusionCoveragePadding)
            let minY = max 0 (bounds.MinY - occlusionCoveragePadding)
            let maxY = min (dec depthHeight) (bounds.MaxY + occlusionCoveragePadding)
            let mutable covered = true
            let mutable y = minY
            while covered && y <= maxY do
                let row = y * depthWidth
                let mutable x = minX
                while covered && x <= maxX do
                    let depth = depths[row + x]
                    covered <- depth < Single.PositiveInfinity && bounds.NearDepth > depth + depthBias
                    x <- inc x
                y <- inc y
            covered

    let private rasterize (depths : single array) (bounds : ProjectedBounds) =
        if bounds.Area >= occluderAreaMin && not (Single.IsNaN bounds.FarDepth) then
            let mutable y = bounds.MinY
            while y <= bounds.MaxY do
                let row = y * depthWidth
                let mutable x = bounds.MinX
                while x <= bounds.MaxX do
                    let i = row + x
                    if bounds.FarDepth < depths[i] then depths[i] <- bounds.FarDepth
                    x <- inc x
                y <- inc y

    let private iteratePortalEyePoses (pair : PortalPair) (eyeCenter : Vector3) (eyeRotation : Quaternion) action =
        let recursionLimit = Math.Clamp (pair.RecursionLimit, 0, portalEyeRecursionLimitMax)
        action eyeCenter eyeRotation
        let processPortal portal =
            let mutable source = portal
            let mutable currentEyeCenter = eyeCenter
            let mutable currentEyeRotation = eyeRotation
            for _ in 1 .. recursionLimit do
                let destination = PortalLogic.pairedPortal pair source
                currentEyeCenter <- PortalLogic.transferPosition source destination currentEyeCenter
                currentEyeRotation <- PortalLogic.transferRotation source destination currentEyeRotation
                action currentEyeCenter currentEyeRotation
                source <- destination
        processPortal pair.Blue
        processPortal pair.Orange

    let private candidateChunks = ResizeArray<VoxelChunk> ()
    let private depthBuffer = Array.zeroCreate<single> (depthWidth * depthHeight)
    let private distanceComparer = VoxelChunkDistanceComparer ()

    let private isChunkNearEye (eyeCenter : Vector3) (chunk : VoxelChunk) =
        let size = chunk.ChunkSize
        let nearDistanceSquared = size.LengthSquared () * 2.0f
        Vector3.DistanceSquared (eyeCenter, chunk.ChunkCenter) <= nearDistanceSquared

    let private computeVisibleChunksUncached (gameplay : Gameplay) (world : World) =
        candidateChunks.Clear ()
        match gameplay.VoxelLevelOpt with
        | Some level ->
            VoxelWorld.beginChunkManifestVisiblePass level.ChunkManifest |> ignore<int>
            let markVisible (chunk : VoxelChunk) =
                match VoxelWorld.tryChunkCoordToIndex level.ChunkManifest.ChunkCounts chunk.ChunkCoord with
                | ValueSome chunkIndex -> VoxelWorld.markChunkManifestVisible level.ChunkManifest chunkIndex
                | ValueNone -> ()
            for chunk in gameplay.VoxelChunks do
                if  (Option.isSome chunk.VoxelModelOpt || chunk.OpaqueOccluderBoxes.Length > 0) &&
                    World.boundsInView3d false Exterior (chunkBounds chunk) world then
                    candidateChunks.Add chunk
            let portalViewsVisible =
                gameplay.PortalPair.RecursionLimit > 0 &&
                (portalInView gameplay.PortalPair.Blue world || portalInView gameplay.PortalPair.Orange world)
            let mutable eyeIndex = 0
            let processEye eyeCenter eyeRotation =
                let viewProjection = Viewport.getViewProjection3d eyeCenter eyeRotation world.Eye3dFieldOfView world.WindowViewport
                if eyeIndex = 0 then
                    Array.Fill (depthBuffer, Single.PositiveInfinity)
                    distanceComparer.EyeCenter <- eyeCenter
                    candidateChunks.Sort distanceComparer
                    for chunk in candidateChunks do
                        let renderable = Option.isSome chunk.VoxelModelOpt && chunk.SplatCount > 0
                        let bounds = chunkBounds chunk
                        match tryProjectBounds viewProjection bounds with
                        | ValueSome projectedBounds ->
                            let occluded = isCovered depthBuffer projectedBounds
                            if not occluded then
                                if renderable then markVisible chunk
                                if chunk.FullOpaqueChunk then rasterize depthBuffer projectedBounds
                                else
                                    for occluderBox in chunk.OpaqueOccluderBoxes do
                                        match tryProjectBounds viewProjection occluderBox with
                                        | ValueSome projectedOccluder -> rasterize depthBuffer projectedOccluder
                                        | ValueNone -> ()
                        | ValueNone ->
                            if renderable then markVisible chunk
                else
                    for chunk in gameplay.VoxelChunks do
                        if Option.isSome chunk.VoxelModelOpt && chunk.SplatCount > 0 then
                            match tryProjectBounds viewProjection (chunkBounds chunk) with
                            | ValueSome _ -> markVisible chunk
                            | ValueNone ->
                                if isChunkNearEye eyeCenter chunk then markVisible chunk
                eyeIndex <- inc eyeIndex
            if portalViewsVisible then iteratePortalEyePoses gameplay.PortalPair world.Eye3dCenter world.Eye3dRotation processEye
            else processEye world.Eye3dCenter world.Eye3dRotation
        | None -> ()

    let mutable private visibleChunksCacheValid = false
    let mutable private visibleChunksCacheEyeCenter = v3Zero
    let mutable private visibleChunksCacheEyeRotation = quatIdentity
    let mutable private visibleChunksCacheEyeFieldOfView = 0.0f
    let mutable private visibleChunksCacheWindowViewport = Unchecked.defaultof<Viewport>
    let mutable private visibleChunksCachePortalPair = Unchecked.defaultof<PortalPair>
    let mutable private visibleChunksCacheChunks = Unchecked.defaultof<VoxelChunk array>

    let getVisibleChunks (gameplay : Gameplay) (world : World) =
        let eyeCenter = world.Eye3dCenter
        let eyeRotation = world.Eye3dRotation
        let eyeFieldOfView = world.Eye3dFieldOfView
        let windowViewport = world.WindowViewport
        let portalPair = gameplay.PortalPair
        let chunks = gameplay.VoxelChunks
        if  visibleChunksCacheValid &&
            visibleChunksCacheEyeCenter = eyeCenter &&
            visibleChunksCacheEyeRotation = eyeRotation &&
            visibleChunksCacheEyeFieldOfView = eyeFieldOfView &&
            visibleChunksCacheWindowViewport = windowViewport &&
            visibleChunksCachePortalPair = portalPair &&
            Object.ReferenceEquals (visibleChunksCacheChunks, chunks) then
            ()
        else
            computeVisibleChunksUncached gameplay world
            visibleChunksCacheValid <- true
            visibleChunksCacheEyeCenter <- eyeCenter
            visibleChunksCacheEyeRotation <- eyeRotation
            visibleChunksCacheEyeFieldOfView <- eyeFieldOfView
            visibleChunksCacheWindowViewport <- windowViewport
            visibleChunksCachePortalPair <- portalPair
            visibleChunksCacheChunks <- chunks

    let isChunkVisible (gameplay : Gameplay) chunkCoord (world : World) =
        getVisibleChunks gameplay world
        match gameplay.VoxelLevelOpt with
        | Some level ->
            match VoxelWorld.tryChunkCoordToIndex level.ChunkManifest.ChunkCounts chunkCoord with
            | ValueSome chunkIndex -> VoxelWorld.isChunkManifestVisible level.ChunkManifest chunkIndex
            | ValueNone -> false
        | None -> false

[<AutoOpen>]
module VoxelChunkFacetExtensions =
    type Entity with
        member this.GetVoxelChunkCoord world : Vector3i = this.Get (nameof this.VoxelChunkCoord) world
        member this.SetVoxelChunkCoord (value : Vector3i) world = this.Set (nameof this.VoxelChunkCoord) value world
        member this.VoxelChunkCoord = lens (nameof this.VoxelChunkCoord) this this.GetVoxelChunkCoord this.SetVoxelChunkCoord
        member this.GetVoxelModelOpt world : VoxelModel AssetTag option = this.Get (nameof this.VoxelModelOpt) world
        member this.SetVoxelModelOpt (value : VoxelModel AssetTag option) world = this.Set (nameof this.VoxelModelOpt) value world
        member this.VoxelModelOpt = lens (nameof this.VoxelModelOpt) this this.GetVoxelModelOpt this.SetVoxelModelOpt

type VoxelChunkFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.Size (v3Dup 1.0f)
         define Entity.Presence Exterior
         define Entity.Static true
         define Entity.AlwaysRender false
         define Entity.MaterialProperties MaterialProperties.empty
         define Entity.VoxelChunkCoord v3iZero
         define Entity.VoxelModelOpt (None : VoxelModel AssetTag option)]

    override this.Render (renderPass, entity, world) =
        match entity.GetVoxelModelOpt world with
        | Some voxelModel ->
            let visibleByGameplay =
                if renderPass.IsNormalPass && Simulants.Gameplay.GetExists world then
                    VoxelChunkVisibility.isChunkVisible (Simulants.Gameplay.GetGameplay world) (entity.GetVoxelChunkCoord world) world
                else true
            if visibleByGameplay then
                let mutable transform = entity.GetTransform world
                let castShadow = (World.getRenderer3dConfig world).LightShadowingEnabled && transform.CastShadow
                if transform.Visible && (not renderPass.IsShadowPass || castShadow) then
                    let affineMatrix = transform.AffineMatrix
                    let presence = transform.Presence
                    let properties = entity.GetMaterialProperties world
                    World.renderVoxelModelFast (&affineMatrix, castShadow, presence, &properties, voxelModel, renderPass, world)
        | None -> ()

    override this.GetAttributesInferred (entity, world) =
        let size = entity.GetSize world
        AttributesInferred.important size v3Zero

    override this.RayCast (ray, entity, world) =
        let intersectionOpt = ray.Intersects (entity.GetBounds world)
        [|Intersection.ofNullable intersectionOpt|]

type VoxelChunkDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static member Facets =
        [typeof<VoxelChunkFacet>
         typeof<RigidBodyFacet>]

[<RequireQualifiedAccess>]
module GameplayLogic =

    let private defaultWorldSettings = WorldGenSettings.defaultSettings
    let private fallbackWorldSettings =
        { defaultWorldSettings with
            WorldSizeBlocks = v3i 32 16 32
            ActiveBlockOrigin = v3i 0 0 0
            ChunkCounts = v3i 8 4 8 }
    let private sourceVoxelSize = defaultWorldSettings.VoxelSize
    let private editReach = 6.0f
    let private editEpsilon = 0.01f
    let private portalRayPadding = 0.02f
    let private portalRaySurfaceOffset = 0.02f
    let private portalRayRecursionLimitMax = 8
    let private aimBlockHighlightPadding = 0.01f
    let private aimBlockHighlightThickness = 0.0125f
    let private streamChunkRadius = 8
    let private streamInitialBuildLimit = 96
    let private streamBuildsPerUpdate = 24
    let private streamBuildJobsMax = 96
    let private farTerrainLodBlockStride = 8
    let private farTerrainLodVerticalOffset = -0.08f

    type private FarTerrainSurfaceBuilder =
        { Positions : ResizeArray<Vector3>
          TexCoordses : ResizeArray<Vector2>
          Normals : ResizeArray<Vector3>
          Indices : ResizeArray<int>
          mutable BoundsMin : Vector3
          mutable BoundsMax : Vector3 }

    type private StreamBuildJob =
        { ChunkCoord : Vector3i
          EditRevision : int
          BuildTask : Task<VoxelRuntime.VoxelChunkBuild option> }

    let private streamBuildJobs = Dictionary<int, StreamBuildJob> ()
    let private streamBuildJobsLock = obj ()
    let private streamIndicesToRemoveBuffer = ResizeArray<int> ()
    let private streamCompletedIndicesBuffer = ResizeArray<int> ()
    let private streamCompletedBuildsBuffer = ResizeArray<VoxelRuntime.VoxelChunkBuild> ()
    let private streamChunksToKeepBuffer = ResizeArray<VoxelChunk> ()
    let private streamChunksToDestroyBuffer = ResizeArray<VoxelChunk> ()
    let private desiredStreamChunkIndicesBuffer = ResizeArray<int> ()

    let private voxelChunkMaterialProperties =
        { MaterialProperties.empty with
            RoughnessOpt = ValueSome 0.92f
            MetallicOpt = ValueSome 0.0f
            AmbientOcclusionOpt = ValueSome 1.0f
            EmissionOpt = ValueSome 0.0f
            ClearCoatOpt = ValueSome 0.0f
            ClearCoatRoughnessOpt = ValueSome 1.0f }

    let private makeFarTerrainSurfaceBuilder () =
        { Positions = ResizeArray<Vector3> ()
          TexCoordses = ResizeArray<Vector2> ()
          Normals = ResizeArray<Vector3> ()
          Indices = ResizeArray<int> ()
          BoundsMin = v3Dup Single.PositiveInfinity
          BoundsMax = v3Dup Single.NegativeInfinity }

    let private includeFarTerrainBounds (point : Vector3) (builder : FarTerrainSurfaceBuilder) =
        builder.BoundsMin <-
            v3
                (min builder.BoundsMin.X point.X)
                (min builder.BoundsMin.Y point.Y)
                (min builder.BoundsMin.Z point.Z)
        builder.BoundsMax <-
            v3
                (max builder.BoundsMax.X point.X)
                (max builder.BoundsMax.Y point.Y)
                (max builder.BoundsMax.Z point.Z)

    let private addFarTerrainQuad (builder : FarTerrainSurfaceBuilder) (p0 : Vector3) (p1 : Vector3) (p2 : Vector3) (p3 : Vector3) =
        let baseIndex = builder.Positions.Count
        builder.Positions.Add p0
        builder.Positions.Add p1
        builder.Positions.Add p2
        builder.Positions.Add p3
        builder.TexCoordses.Add (v2 0.0f 0.0f)
        builder.TexCoordses.Add (v2 1.0f 0.0f)
        builder.TexCoordses.Add (v2 1.0f 1.0f)
        builder.TexCoordses.Add (v2 0.0f 1.0f)
        for _ in 0 .. 3 do
            builder.Normals.Add v3Up
        builder.Indices.Add baseIndex
        builder.Indices.Add (baseIndex + 1)
        builder.Indices.Add (baseIndex + 2)
        builder.Indices.Add baseIndex
        builder.Indices.Add (baseIndex + 2)
        builder.Indices.Add (baseIndex + 3)
        includeFarTerrainBounds p0 builder
        includeFarTerrainBounds p1 builder
        includeFarTerrainBounds p2 builder
        includeFarTerrainBounds p3 builder

    let private averageTemplateAlbedo fallback (template : VoxelBlockTemplate) =
        if template.Voxels.Length = 0 then fallback
        else
            let mutable r = 0.0f
            let mutable g = 0.0f
            let mutable b = 0.0f
            let mutable a = 0.0f
            for struct (_, cell) in template.Voxels do
                r <- r + cell.Albedo.R
                g <- g + cell.Albedo.G
                b <- b + cell.Albedo.B
                a <- a + cell.Albedo.A
            let scalar = 1.0f / single template.Voxels.Length
            color (r * scalar) (g * scalar) (b * scalar) (a * scalar)

    let private farTerrainMaterialColor (generation : VoxelGeneration) material =
        match material with
        | Grass -> averageTemplateAlbedo (color 0.34f 0.58f 0.24f 1.0f) generation.Templates.Grass
        | Dirt -> averageTemplateAlbedo (color 0.38f 0.26f 0.16f 1.0f) generation.Templates.Dirt
        | Stone -> averageTemplateAlbedo (color 0.48f 0.48f 0.46f 1.0f) generation.Templates.Stone
        | Sand -> averageTemplateAlbedo (color 0.72f 0.66f 0.42f 1.0f) generation.Templates.Sand
        | Wood -> averageTemplateAlbedo (color 0.38f 0.25f 0.13f 1.0f) generation.Templates.Log
        | Leaves -> averageTemplateAlbedo (color 0.22f 0.43f 0.16f 1.0f) generation.Templates.Leaves
        | Glass -> color 0.74f 0.9f 1.0f 1.0f
        | Water -> averageTemplateAlbedo (color 0.12f 0.32f 0.72f 1.0f) generation.Templates.Water
        | Lava -> averageTemplateAlbedo (color 1.0f 0.24f 0.02f 1.0f) generation.Templates.Lava
        | Ore -> averageTemplateAlbedo (color 0.72f 0.45f 0.28f 1.0f) generation.Templates.Ore
        | Brick -> color 0.52f 0.17f 0.12f 1.0f
        | Crafted -> color 0.58f 0.58f 0.58f 1.0f

    let private farTerrainSurfaceProperties albedo =
        { Vulkan.PhysicallyBasedMaterialProperties.empty with
            Albedo = albedo
            Roughness = 0.96f
            Metallic = 0.0f
            AmbientOcclusion = 1.0f
            Emission = 0.0f
            Height = 1.0f
            IgnoreLightMaps = true
            OpaqueDistance = Constants.Render.OpaqueDistanceDefault
            FinenessOffset = 0.0f
            ScatterType = NoScatter
            SpecularScalar = 0.28f
            SubsurfaceCutoff = Constants.Render.SubsurfaceCutoffDefault
            SubsurfaceCutoffMargin = Constants.Render.SubsurfaceCutoffMarginDefault
            RefractiveIndex = Constants.Render.RefractiveIndexDefault
            ClearCoat = 0.0f
            ClearCoatRoughness = 1.0f }

    let private farTerrainDefaultImage name =
        asset<Image> "Default" name

    let private makeFarTerrainSurfaceDescriptor generation material (builder : FarTerrainSurfaceBuilder) =
        let bounds = box3 builder.BoundsMin (builder.BoundsMax - builder.BoundsMin)
        { Positions = builder.Positions.ToArray ()
          TexCoordses = builder.TexCoordses.ToArray ()
          Normals = builder.Normals.ToArray ()
          Indices = builder.Indices.ToArray ()
          ModelMatrix = Matrix4x4.Identity
          Bounds = bounds
          MaterialProperties = farTerrainSurfaceProperties (farTerrainMaterialColor generation material)
          IgnoreLightMaps = true
          AlbedoImage = farTerrainDefaultImage "MaterialAlbedo"
          RoughnessImage = farTerrainDefaultImage "MaterialRoughness"
          MetallicImage = farTerrainDefaultImage "MaterialMetallic"
          AmbientOcclusionImage = farTerrainDefaultImage "MaterialAmbientOcclusion"
          EmissionImage = farTerrainDefaultImage "MaterialEmission"
          NormalImage = farTerrainDefaultImage "MaterialNormal"
          HeightImage = farTerrainDefaultImage "MaterialHeight"
          SubdermalImage = farTerrainDefaultImage "MaterialSubdermal"
          FinenessImage = farTerrainDefaultImage "MaterialFineness"
          ScatterImage = farTerrainDefaultImage "MaterialScatter"
          ClearCoatImage = farTerrainDefaultImage "MaterialClearCoat"
          ClearCoatRoughnessImage = farTerrainDefaultImage "MaterialClearCoatRoughness"
          ClearCoatNormalImage = farTerrainDefaultImage "MaterialClearCoatNormal"
          TwoSided = true
          Clipped = false }

    let private tryBuildFarTerrainLod (level : VoxelLevel) =
        match level.GenerationOpt with
        | Some generation ->
            let macroCounts = VoxelWorld.generatedBlockCounts level
            if macroCounts.X <= 0 || macroCounts.Z <= 0 then None
            else
                let side = max 1 level.BlockSideVoxels
                let stride = min farTerrainLodBlockStride (max 1 (min macroCounts.X macroCounts.Z))
                let surfaceBuilders = Dictionary<VoxelMaterialKind, FarTerrainSurfaceBuilder> ()
                let getBuilder material =
                    let mutable builder = Unchecked.defaultof<FarTerrainSurfaceBuilder>
                    if surfaceBuilders.TryGetValue (material, &builder) then builder
                    else
                        builder <- makeFarTerrainSurfaceBuilder ()
                        surfaceBuilders.Add (material, builder)
                        builder
                let blockEdgeX x =
                    level.Bounds.Min.X + level.LevelOffset.X + single (level.BlockGridOffsetVoxels.X + x * side) * level.VoxelSize.X
                let blockEdgeZ z =
                    level.Bounds.Min.Z + level.LevelOffset.Z + single (level.BlockGridOffsetVoxels.Z + z * side) * level.VoxelSize.Z
                let blockTopY y =
                    level.Bounds.Min.Y + level.LevelOffset.Y + single (level.BlockGridOffsetVoxels.Y + (inc y) * side) * level.VoxelSize.Y + farTerrainLodVerticalOffset
                let heightAt x z =
                    VoxelWorld.generatedHeightAt
                        level
                        generation
                        (Math.Clamp (x, 0, dec macroCounts.X))
                        (Math.Clamp (z, 0, dec macroCounts.Z))
                let surfaceMaterialForHeight height =
                    if height <= generation.SeaLevelBlocks + 1 then Sand else Grass
                for z in 0 .. stride .. dec macroCounts.Z do
                    let z1 = min macroCounts.Z (z + stride)
                    for x in 0 .. stride .. dec macroCounts.X do
                        let x1 = min macroCounts.X (x + stride)
                        if x1 > x && z1 > z then
                            let height00 = heightAt x z
                            let height10 = heightAt x1 z
                            let height11 = heightAt x1 z1
                            let height01 = heightAt x z1
                            let centerHeight = heightAt ((x + x1) / 2) ((z + z1) / 2)
                            let x0f = blockEdgeX x
                            let x1f = blockEdgeX x1
                            let z0f = blockEdgeZ z
                            let z1f = blockEdgeZ z1
                            let p0 = v3 x0f (blockTopY height00) z0f
                            let p1 = v3 x1f (blockTopY height10) z0f
                            let p2 = v3 x1f (blockTopY height11) z1f
                            let p3 = v3 x0f (blockTopY height01) z1f
                            addFarTerrainQuad (getBuilder (surfaceMaterialForHeight centerHeight)) p0 p1 p2 p3
                            if centerHeight < generation.SeaLevelBlocks then
                                let waterY = blockTopY generation.SeaLevelBlocks + 0.02f
                                addFarTerrainQuad
                                    (getBuilder Water)
                                    (v3 x0f waterY z0f)
                                    (v3 x1f waterY z0f)
                                    (v3 x1f waterY z1f)
                                    (v3 x0f waterY z1f)
                let surfaceDescriptors =
                    [|for entry in surfaceBuilders do
                        if entry.Value.Positions.Count > 0 then
                            makeFarTerrainSurfaceDescriptor generation entry.Key entry.Value|]
                if surfaceDescriptors.Length = 0 then None
                else
                    let mutable boundsMin = v3Dup Single.PositiveInfinity
                    let mutable boundsMax = v3Dup Single.NegativeInfinity
                    for descriptor in surfaceDescriptors do
                        boundsMin <-
                            v3
                                (min boundsMin.X descriptor.Bounds.Min.X)
                                (min boundsMin.Y descriptor.Bounds.Min.Y)
                                (min boundsMin.Z descriptor.Bounds.Min.Z)
                        boundsMax <-
                            v3
                                (max boundsMax.X descriptor.Bounds.Max.X)
                                (max boundsMax.Y descriptor.Bounds.Max.Y)
                                (max boundsMax.Z descriptor.Bounds.Max.Z)
                    let bounds = box3 boundsMin (boundsMax - boundsMin)
                    Some struct (surfaceDescriptors, bounds)
        | None -> None

    let destroyFarTerrainLod (world : World) =
        World.destroyUserDefinedStaticModel Assets.Voxels.FarTerrainLod world

    let createFarTerrainLod (level : VoxelLevel) (world : World) =
        match tryBuildFarTerrainLod level with
        | Some (struct (surfaceDescriptors, bounds)) ->
            World.createUserDefinedStaticModel surfaceDescriptors bounds Assets.Voxels.FarTerrainLod world
            let tileCount = surfaceDescriptors |> Array.sumBy (fun descriptor -> descriptor.Indices.Length / 6)
            Log.info ("VoxelForge created far terrain LOD with " + scstring tileCount + " coarse tiles across " + scstring surfaceDescriptors.Length + " surfaces.")
        | None -> ()

    let voxelChunkEntity (voxelChunk : VoxelChunk) =
        Simulants.VoxelLevelChunk voxelChunk.ChunkCoord.X voxelChunk.ChunkCoord.Y voxelChunk.ChunkCoord.Z

    let setVoxelChunkEntityProperties (voxelChunk : VoxelChunk) (entity : Entity) (world : World) =
        entity.SetPosition voxelChunk.ChunkCenter world
        entity.SetSize voxelChunk.ChunkSize world
        entity.SetVoxelChunkCoord voxelChunk.ChunkCoord world
        entity.SetVoxelModelOpt voxelChunk.VoxelModelOpt world
        entity.SetVisible (Option.isSome voxelChunk.VoxelModelOpt) world
        entity.SetPresence Exterior world
        entity.SetAlwaysRender false world
        entity.SetCastShadow false world
        entity.SetBodyEnabled true world
        entity.SetBodyType Static world
        entity.SetBodyShape voxelChunk.BodyShape world
        entity.SetCollisionCategories "10" world
        entity.SetStatic true world
        entity.SetMaterialProperties voxelChunkMaterialProperties world

    let createOrUpdateVoxelChunkEntity (voxelChunk : VoxelChunk) (world : World) =
        let entity = voxelChunkEntity voxelChunk
        if Simulants.GameplayScene.GetExists world then
            if not (entity.GetExists world) || entity.GetDestroying world then
                World.createEntity<VoxelChunkDispatcher> (Some Address.parent) DefaultOverlay (Some entity.Surnames) entity.Group world |> ignore<Entity>
                entity.SetProtection ManualProtection world
                entity.SetPersistent false world
            setVoxelChunkEntityProperties voxelChunk entity world

    let removeVoxelChunkEntity (voxelChunk : VoxelChunk) (world : World) =
        let entity = voxelChunkEntity voxelChunk
        if entity.GetExists world then
            entity.SetVoxelModelOpt None world
            entity.SetVisible false world
            entity.SetBodyEnabled false world
            World.destroyEntity entity world

    let applyVoxelChunkSyncDelta (delta : VoxelChunkSyncDelta) (world : World) =
        for chunk in delta.ChunksRemoved do
            removeVoxelChunkEntity chunk world
        for chunk in delta.ChunksAdded do
            createOrUpdateVoxelChunkEntity chunk world
        for chunk in delta.ChunksUpdated do
            createOrUpdateVoxelChunkEntity chunk world
        if delta.VoxelAssetsToDestroy.Length > 0 then
            VoxelRuntime.destroyVoxelChunks delta.VoxelAssetsToDestroy world

    let private clearStreamBuildJobs () =
        lock streamBuildJobsLock (fun () ->
            streamBuildJobs.Clear ())

    let private hasStreamBuildJobs () =
        lock streamBuildJobsLock (fun () -> streamBuildJobs.Count > 0)

    let private markStreamEditedChunks (level : VoxelLevel) (chunkCoords : Vector3i array) =
        lock streamBuildJobsLock (fun () ->
            for chunkCoord in chunkCoords do
                match VoxelWorld.tryChunkCoordToIndex level.ChunkManifest.ChunkCounts chunkCoord with
                | ValueSome chunkIndex ->
                    level.ChunkManifest.EmptyFlags[chunkIndex] <- false
                    level.ChunkManifest.EditedFlags[chunkIndex] <- true
                    streamBuildJobs.Remove chunkIndex |> ignore<bool>
                | ValueNone -> ())

    let private removeStreamBuildJobsOutside (level : VoxelLevel) =
        lock streamBuildJobsLock (fun () ->
            let indicesToRemove = streamIndicesToRemoveBuffer
            indicesToRemove.Clear ()
            for entry in streamBuildJobs do
                if not (VoxelWorld.isChunkManifestDesired level.ChunkManifest entry.Key) then
                    indicesToRemove.Add entry.Key
            for chunkIndex in indicesToRemove do
                streamBuildJobs.Remove chunkIndex |> ignore<bool>
            indicesToRemove.Clear ())

    let private tryQueueStreamBuild (level : VoxelLevel) (snapshot : VoxelEditSnapshot) useCache chunkIndex =
        lock streamBuildJobsLock (fun () ->
            if  streamBuildJobs.Count >= streamBuildJobsMax ||
                streamBuildJobs.ContainsKey chunkIndex ||
                level.ChunkManifest.EmptyFlags[chunkIndex] then false
            else
                let chunkCoord = level.ChunkManifest.ChunkStatics[chunkIndex].ChunkCoord
                let buildTask =
                    Task.Run<VoxelRuntime.VoxelChunkBuild option>
                        (Func<VoxelRuntime.VoxelChunkBuild option>
                            (fun () ->
                                VoxelRuntime.tryBuildChunkWithEditSnapshotCached
                                    useCache
                                    level
                                    snapshot
                                    chunkCoord))
                streamBuildJobs[chunkIndex] <- { ChunkCoord = chunkCoord; EditRevision = snapshot.Revision; BuildTask = buildTask }
                true)

    let private collectCompletedStreamBuilds (level : VoxelLevel) buildLimit =
        let currentEditRevision = VoxelWorld.getEditRevision level
        lock streamBuildJobsLock (fun () ->
            let completedIndices = streamCompletedIndicesBuffer
            let completedBuilds = streamCompletedBuildsBuffer
            completedIndices.Clear ()
            completedBuilds.Clear ()
            let mutable count = 0
            let limit = max 0 buildLimit
            for entry in streamBuildJobs do
                if count < limit && entry.Value.BuildTask.IsCompleted then
                    completedIndices.Add entry.Key
                    count <- inc count
            for chunkIndex in completedIndices do
                match streamBuildJobs.TryGetValue chunkIndex with
                | (true, job) ->
                    streamBuildJobs.Remove chunkIndex |> ignore<bool>
                    if job.EditRevision = currentEditRevision && job.BuildTask.Status = TaskStatus.RanToCompletion then
                        match job.BuildTask.Result with
                        | Some chunkBuild ->
                            VoxelWorld.updateChunkManifestFromBuild currentEditRevision level chunkBuild
                            if  VoxelWorld.isChunkManifestDesired level.ChunkManifest chunkIndex &&
                                not (VoxelWorld.isChunkManifestLoaded level.ChunkManifest chunkIndex) then
                                completedBuilds.Add chunkBuild
                        | None ->
                            VoxelWorld.markChunkManifestEmpty currentEditRevision level job.ChunkCoord
                | (false, _) -> ()
            let completedBuildsArray = completedBuilds.ToArray ()
            completedIndices.Clear ()
            completedBuilds.Clear ()
            completedBuildsArray)

    let private queueMissingStreamBuilds (level : VoxelLevel) (desiredChunkIndices : int array) buildLimit =
        let mutable queued = 0
        let mutable i = 0
        let mutable snapshotCaptured = false
        let mutable snapshot = Unchecked.defaultof<VoxelEditSnapshot>
        while queued < buildLimit && i < desiredChunkIndices.Length do
            let chunkIndex = desiredChunkIndices[i]
            if  not (VoxelWorld.isChunkManifestLoaded level.ChunkManifest chunkIndex) &&
                not level.ChunkManifest.EmptyFlags[chunkIndex] then
                if not snapshotCaptured then
                    snapshot <- VoxelWorld.snapshotEdits level
                    snapshotCaptured <- true
                let useCache = not level.ChunkManifest.EditedFlags[chunkIndex]
                if tryQueueStreamBuild level snapshot useCache chunkIndex then
                    queued <- inc queued
            i <- inc i

    let private wrapIndex count index =
        if count <= 0 then 0
        else
            let index = index % count
            if index < 0 then index + count else index

    let updateSelectedBlockFromInput (gameplay : Gameplay) (world : World) =
        match gameplay.VoxelLevelOpt with
        | Some level when world.Advancing && level.PlaceableBlocks.Length > 0 ->
            let scroll = World.getMouseScrolled world
            let delta =
                if scroll > 0.0f then -1
                elif scroll < 0.0f then 1
                else 0
            if delta <> 0 then
                { gameplay with SelectedBlockIndex = wrapIndex level.PlaceableBlocks.Length (gameplay.SelectedBlockIndex + delta) }
            else gameplay
        | Some _ | None -> gameplay

    let private selectedBlockPreviewPosition (world : World) =
        let rotation = world.Eye3dRotation
        world.Eye3dCenter + rotation.Forward * 1.25f + rotation.Right * 0.55f + rotation.Down * 0.35f

    let tryGetSelectedBlock (gameplay : Gameplay) =
        match gameplay.VoxelLevelOpt with
        | Some level when level.PlaceableBlocks.Length > 0 ->
            Some level.PlaceableBlocks[wrapIndex level.PlaceableBlocks.Length gameplay.SelectedBlockIndex]
        | Some _ | None -> None

    let computeOcclusionBlockCoords (voxelChunks : VoxelChunk array) =
        voxelChunks
        |> Seq.collect (fun (chunk : VoxelChunk) -> chunk.OpaqueBlockCoords)
        |> Set.ofSeq

    let private replaceOcclusionBlockCoords (current : Set<Vector3i>) (chunksToDestroy : VoxelChunk array) (chunksToAdd : VoxelChunk array) =
        let removed =
            chunksToDestroy
            |> Seq.collect (fun (chunk : VoxelChunk) -> chunk.OpaqueBlockCoords)
            |> Set.ofSeq
        let added = computeOcclusionBlockCoords chunksToAdd
        Set.union (Set.difference current removed) added

    let mutable private streamChunkOffsetsCacheValid = false
    let mutable private streamChunkOffsetsCacheChunkCounts = v3iZero
    let mutable private streamChunkOffsetsCacheRadius = 0
    let mutable private streamChunkOffsetsCache = [||]

    let private streamChunkOffsets (level : VoxelLevel) =
        if  not streamChunkOffsetsCacheValid ||
            streamChunkOffsetsCacheChunkCounts <> level.ChunkCounts ||
            streamChunkOffsetsCacheRadius <> streamChunkRadius then
            streamChunkOffsetsCache <-
                [|for dz in -streamChunkRadius .. streamChunkRadius do
                    for dy in -(dec level.ChunkCounts.Y) .. dec level.ChunkCounts.Y do
                        for dx in -streamChunkRadius .. streamChunkRadius do
                            v3i dx dy dz|]
                |> Array.sortBy (fun offset ->
                    struct (offset.X * offset.X + offset.Z * offset.Z, abs offset.Y, offset.Z, offset.Y, offset.X))
            streamChunkOffsetsCacheValid <- true
            streamChunkOffsetsCacheChunkCounts <- level.ChunkCounts
            streamChunkOffsetsCacheRadius <- streamChunkRadius
        streamChunkOffsetsCache

    let mutable private desiredStreamChunkIndicesCacheValid = false
    let mutable private desiredStreamChunkIndicesCacheCenter = v3iZero
    let mutable private desiredStreamChunkIndicesCacheChunkCounts = v3iZero
    let mutable private desiredStreamChunkIndicesCacheRadius = 0
    let mutable private desiredStreamChunkIndicesCacheManifest = Unchecked.defaultof<VoxelChunkManifest>
    let mutable private desiredStreamChunkIndicesCache = [||]

    let private desiredStreamChunkIndices (level : VoxelLevel) (center : Vector3i) =
        if  not desiredStreamChunkIndicesCacheValid ||
            desiredStreamChunkIndicesCacheCenter <> center ||
            desiredStreamChunkIndicesCacheChunkCounts <> level.ChunkCounts ||
            desiredStreamChunkIndicesCacheRadius <> streamChunkRadius ||
            not (Object.ReferenceEquals (desiredStreamChunkIndicesCacheManifest, level.ChunkManifest)) then
            VoxelWorld.beginChunkManifestDesiredPass level.ChunkManifest |> ignore<int>
            let indices = desiredStreamChunkIndicesBuffer
            indices.Clear ()
            let offsets = streamChunkOffsets level
            for offset in offsets do
                let chunkCoord = center + offset
                match VoxelWorld.tryChunkCoordToIndex level.ChunkManifest.ChunkCounts chunkCoord with
                | ValueSome chunkIndex ->
                    VoxelWorld.markChunkManifestDesired level.ChunkManifest chunkIndex
                    indices.Add chunkIndex
                | ValueNone -> ()
            desiredStreamChunkIndicesCacheValid <- true
            desiredStreamChunkIndicesCacheCenter <- center
            desiredStreamChunkIndicesCacheChunkCounts <- level.ChunkCounts
            desiredStreamChunkIndicesCacheRadius <- streamChunkRadius
            desiredStreamChunkIndicesCacheManifest <- level.ChunkManifest
            desiredStreamChunkIndicesCache <- indices.ToArray ()
            indices.Clear ()
        desiredStreamChunkIndicesCache

    let private desiredStreamEmptyChunkCount (level : VoxelLevel) (desiredChunkIndices : int array) =
        let mutable count = 0
        for chunkIndex in desiredChunkIndices do
            if level.ChunkManifest.EmptyFlags[chunkIndex] then
                count <- inc count
        count

    let private getStreamingPosition (gameplay : Gameplay) (world : World) =
        if Simulants.GameplayPlayer.GetExists world then Simulants.GameplayPlayer.GetPosition world
        else
            match gameplay.VoxelLevelOpt with
            | Some level -> level.SpawnPosition
            | None -> world.Eye3dCenter

    let streamChunksAroundPosition buildLimit position (gameplay : Gameplay) (world : World) =
        match gameplay.VoxelLevelOpt with
        | Some level ->
            match VoxelWorld.tryWorldToChunkCoord level position with
            | Some centerChunkCoord ->
                let centerChanged = gameplay.StreamCenterChunkCoordOpt <> Some centerChunkCoord
                let desiredChunkIndices = desiredStreamChunkIndices level centerChunkCoord
                VoxelWorld.beginChunkManifestLoadedPass level.ChunkManifest |> ignore<int>
                for chunk in gameplay.VoxelChunks do
                    match VoxelWorld.tryChunkCoordToIndex level.ChunkManifest.ChunkCounts chunk.ChunkCoord with
                    | ValueSome chunkIndex -> VoxelWorld.markChunkManifestLoaded level.ChunkManifest chunkIndex
                    | ValueNone -> ()
                removeStreamBuildJobsOutside level
                let chunksToKeep = streamChunksToKeepBuffer
                let chunksToDestroy = streamChunksToDestroyBuffer
                chunksToKeep.Clear ()
                chunksToDestroy.Clear ()
                for chunk in gameplay.VoxelChunks do
                    match VoxelWorld.tryChunkCoordToIndex level.ChunkManifest.ChunkCounts chunk.ChunkCoord with
                    | ValueSome chunkIndex when VoxelWorld.isChunkManifestDesired level.ChunkManifest chunkIndex ->
                        chunksToKeep.Add chunk
                    | ValueSome _
                    | ValueNone -> chunksToDestroy.Add chunk
                let completedBuilds = collectCompletedStreamBuilds level buildLimit
                let chunksToAdd =
                    if completedBuilds.Length > 0 then
                        let chunksToAdd = Array.zeroCreate<VoxelChunk> completedBuilds.Length
                        for i in 0 .. dec completedBuilds.Length do
                            let chunk = VoxelRuntime.realizeChunk level completedBuilds[i] world
                            chunksToAdd[i] <- chunk
                            match VoxelWorld.tryChunkCoordToIndex level.ChunkManifest.ChunkCounts chunk.ChunkCoord with
                            | ValueSome chunkIndex -> VoxelWorld.markChunkManifestLoaded level.ChunkManifest chunkIndex
                            | ValueNone -> ()
                        chunksToAdd
                    else Array.empty
                queueMissingStreamBuilds level desiredChunkIndices buildLimit
                let chunksToDestroyArray =
                    if chunksToDestroy.Count > 0 then chunksToDestroy.ToArray ()
                    else Array.empty
                let chunksChanged = chunksToDestroyArray.Length > 0 || chunksToAdd.Length > 0
                let voxelChunks =
                    if chunksChanged then
                        let voxelChunks = Array.zeroCreate<VoxelChunk> (chunksToKeep.Count + chunksToAdd.Length)
                        chunksToKeep.CopyTo (voxelChunks, 0)
                        Array.Copy (chunksToAdd, 0, voxelChunks, chunksToKeep.Count, chunksToAdd.Length)
                        VoxelRuntime.sortVoxelChunks voxelChunks
                    else gameplay.VoxelChunks
                let syncDelta =
                    if chunksChanged then
                        { VoxelChunkSyncDelta.Empty with
                            ChunksAdded = chunksToAdd
                            ChunksRemoved = chunksToDestroyArray
                            VoxelAssetsToDestroy = chunksToDestroyArray }
                    else VoxelChunkSyncDelta.Empty
                let result =
                    if chunksChanged || centerChanged then
                        struct
                            ({ gameplay with
                                VoxelChunks = voxelChunks
                                OcclusionBlockCoords =
                                    if chunksChanged then
                                        replaceOcclusionBlockCoords gameplay.OcclusionBlockCoords chunksToDestroyArray chunksToAdd
                                    else gameplay.OcclusionBlockCoords
                                StreamCenterChunkCoordOpt = Some centerChunkCoord },
                             syncDelta,
                             true)
                    else struct (gameplay, VoxelChunkSyncDelta.Empty, false)
                chunksToKeep.Clear ()
                chunksToDestroy.Clear ()
                result
            | None -> struct (gameplay, VoxelChunkSyncDelta.Empty, false)
        | None -> struct (gameplay, VoxelChunkSyncDelta.Empty, false)

    let streamChunksForCurrentPosition buildLimit (gameplay : Gameplay) (world : World) =
        streamChunksAroundPosition buildLimit (getStreamingPosition gameplay world) gameplay world

    let streamInitialChunksAroundPosition position (gameplay : Gameplay) (_world : World) =
        clearStreamBuildJobs ()
        match gameplay.VoxelLevelOpt with
        | Some level ->
            match VoxelWorld.tryWorldToChunkCoord level position with
            | Some centerChunkCoord -> { gameplay with StreamCenterChunkCoordOpt = Some centerChunkCoord }
            | None -> gameplay
        | None -> gameplay

    let streamChunksForCurrentPositionDefault (gameplay : Gameplay) (world : World) =
        let buildLimit =
            match gameplay.StreamCenterChunkCoordOpt with
            | Some _ -> streamBuildsPerUpdate
            | None -> streamInitialBuildLimit
        streamChunksForCurrentPosition buildLimit gameplay world

    let shouldStreamVoxelChunks (gameplay : Gameplay) (world : World) =
        world.Advancing &&
        match gameplay.VoxelLevelOpt with
        | Some level ->
            match VoxelWorld.tryWorldToChunkCoord level (getStreamingPosition gameplay world) with
            | Some centerChunkCoord ->
                if gameplay.StreamCenterChunkCoordOpt <> Some centerChunkCoord then true
                elif hasStreamBuildJobs () then true
                else false
            | None -> false
        | None -> false

    let getAimBlockHighlightFace (bounds : Box3) (faceIndex : int) =
        let bounds = box3 (bounds.Min - v3Dup aimBlockHighlightPadding) (bounds.Size + v3Dup (aimBlockHighlightPadding * 2.0f))
        match faceIndex with
        | 0 -> struct (bounds.Center + v3Forward * bounds.Depth * 0.5f, quatIdentity, v3 bounds.Width bounds.Height aimBlockHighlightThickness)
        | 1 -> struct (bounds.Center + v3Back * bounds.Depth * 0.5f, Quaternion.CreateFromAxisAngle (v3Up, MathF.PI), v3 bounds.Width bounds.Height aimBlockHighlightThickness)
        | 2 -> struct (bounds.Center + v3Left * bounds.Width * 0.5f, Quaternion.CreateFromAxisAngle (v3Up, MathF.PI_OVER_2), v3 bounds.Depth bounds.Height aimBlockHighlightThickness)
        | 3 -> struct (bounds.Center + v3Right * bounds.Width * 0.5f, Quaternion.CreateFromAxisAngle (v3Up, -MathF.PI_OVER_2), v3 bounds.Depth bounds.Height aimBlockHighlightThickness)
        | 4 -> struct (bounds.Center + v3Up * bounds.Height * 0.5f, Quaternion.CreateFromAxisAngle (v3Right, MathF.PI_OVER_2), v3 bounds.Width bounds.Depth aimBlockHighlightThickness)
        | 5 -> struct (bounds.Center + v3Down * bounds.Height * 0.5f, Quaternion.CreateFromAxisAngle (v3Right, -MathF.PI_OVER_2), v3 bounds.Width bounds.Depth aimBlockHighlightThickness)
        | _ -> failwithumf ()

    let private boxesIntersect (box : Box3) (box2 : Box3) =
        box.Min.X < box2.Max.X && box.Max.X > box2.Min.X &&
        box.Min.Y < box2.Max.Y && box.Max.Y > box2.Min.Y &&
        box.Min.Z < box2.Max.Z && box.Max.Z > box2.Min.Z

    let private blockIntersectsPlayer (level : VoxelLevel) (blockCoord : Vector3i) (world : World) =
        if Simulants.GameplayPlayer.GetExists world then
            let position = Simulants.GameplayPlayer.GetPosition world
            let playerBounds = box3 (position + v3 -0.45f 0.0f -0.45f) (v3 0.9f 1.9f 0.9f)
            boxesIntersect (VoxelWorld.blockBounds level blockCoord) playerBounds
        else false

    let tryGetAimBlockBounds (gameplay : Gameplay) =
        match gameplay.VoxelLevelOpt, gameplay.AimPickOpt with
        | Some level, Some pick when VoxelWorld.blockContainsCell level pick.DestroyBlockCoord ->
            Some (VoxelWorld.blockBounds level pick.DestroyBlockCoord)
        | Some _, Some _ | Some _, None | None, _ ->
            None

    let private rebuildChunks (chunkCoords : Vector3i seq) (gameplay : Gameplay) (world : World) =
        match gameplay.VoxelLevelOpt with
        | Some level ->
            let chunkCoords = chunkCoords |> Seq.toArray
            markStreamEditedChunks level chunkCoords
            let loadedChunkCoordExists chunkCoord =
                let mutable exists = false
                let mutable i = 0
                while not exists && i < gameplay.VoxelChunks.Length do
                    exists <- gameplay.VoxelChunks[i].ChunkCoord = chunkCoord
                    i <- inc i
                exists
            let chunkCoords =
                chunkCoords
                |> Array.filter loadedChunkCoordExists
            if chunkCoords.Length = 0 then struct (gameplay, VoxelChunkSyncDelta.Empty)
            else
                let isTargetChunk chunkCoord =
                    let mutable found = false
                    let mutable i = 0
                    while not found && i < chunkCoords.Length do
                        found <- chunkCoords[i] = chunkCoord
                        i <- inc i
                    found
                let struct (voxelChunks, chunksReplaced, voxelAssetsToDestroy) = VoxelRuntime.rebuildChunks chunkCoords level gameplay.VoxelChunks world
                let chunksToAdd =
                    voxelChunks
                    |> Array.filter (fun (chunk : VoxelChunk) -> isTargetChunk chunk.ChunkCoord)
                let occlusionBlockCoords = replaceOcclusionBlockCoords gameplay.OcclusionBlockCoords chunksReplaced chunksToAdd
                let syncDelta =
                    { VoxelChunkSyncDelta.Empty with
                        ChunksUpdated = chunksToAdd
                        VoxelAssetsToDestroy = voxelAssetsToDestroy }
                struct ({ gameplay with VoxelChunks = voxelChunks; OcclusionBlockCoords = occlusionBlockCoords }, syncDelta)
        | None -> struct (gameplay, VoxelChunkSyncDelta.Empty)

    let private tryNearestPhysicsHit (origin : Vector3) (direction : Vector3) remainingDistance (world : World) =
        let ray = ray3 origin (direction * remainingDistance)
        let intersections = World.rayCastBodies3d ray 2UL 2UL false world
        let mutable hitOpt = None
        let mutable i = 0
        while hitOpt.IsNone && i < intersections.Length do
            let intersection = intersections[i]
            if intersection.Progress >= 0.0f && intersection.Progress <= 1.0f then
                hitOpt <- Some intersection
            i <- inc i
        hitOpt

    let private tryNearestPortalIntersection (pair : PortalPair) (origin : Vector3) (direction : Vector3) remainingDistance =
        let mutable nearestOpt = None
        let checkPortal portal =
            let side = PortalLogic.signedDistance origin portal
            let denominator = Vector3.Dot (direction, portal.Rotation.Forward)
            if side > portalRaySurfaceOffset && denominator < -0.0001f then
                let distance = -side / denominator
                if distance > portalRaySurfaceOffset && distance <= remainingDistance then
                    let point = origin + direction * distance
                    if PortalLogic.isPointWithinAperture point portal portalRayPadding then
                        match nearestOpt with
                        | Some (struct (_, nearestDistance)) when nearestDistance <= distance -> ()
                        | Some _ | None -> nearestOpt <- Some (struct (portal, distance))
        checkPortal pair.Blue
        checkPortal pair.Orange
        nearestOpt

    let private tryRayCastThroughPortals (pair : PortalPair) (origin : Vector3) (direction : Vector3) remainingDistance recursionLimit world =
        let rec loop origin direction remainingDistance depth =
            if depth <= 0 || remainingDistance <= editEpsilon then None
            else
                let physicsHitOpt = tryNearestPhysicsHit origin direction remainingDistance world
                let portalHitOpt = tryNearestPortalIntersection pair origin direction remainingDistance
                match physicsHitOpt, portalHitOpt with
                | Some physicsHit, Some (struct (_, portalDistance)) when physicsHit.Progress * remainingDistance <= portalDistance + editEpsilon ->
                    Some physicsHit
                | _, Some (struct (portal, portalDistance)) ->
                    let destination = PortalLogic.pairedPortal pair portal
                    let portalPoint = origin + direction * portalDistance
                    let direction' = PortalLogic.transferDirection portal destination direction
                    let direction' = if direction'.LengthSquared () > 0.0f then direction'.Normalized else destination.Rotation.Forward
                    let origin' = PortalLogic.transferPosition portal destination portalPoint + direction' * portalRaySurfaceOffset
                    loop origin' direction' (remainingDistance - portalDistance - portalRaySurfaceOffset) (dec depth)
                | Some physicsHit, None ->
                    Some physicsHit
                | None, None ->
                    None
        let direction = if direction.LengthSquared () > 0.0f then direction.Normalized else v3Forward
        loop origin direction remainingDistance recursionLimit

    let tryPickForward (gameplay : Gameplay) (world : World) =
        match gameplay.VoxelLevelOpt with
        | Some level ->
            let recursionLimit = Math.Clamp (gameplay.PortalPair.RecursionLimit, 1, portalRayRecursionLimitMax)
            tryRayCastThroughPortals gameplay.PortalPair world.Eye3dCenter world.Eye3dRotation.Forward editReach recursionLimit world
            |> Option.bind (fun (intersection : BodyIntersection) ->
                let normal = if intersection.Normal.LengthSquared () > 0.0f then intersection.Normal.Normalized else v3Up
                match VoxelWorld.tryWorldToBlockCoord level (intersection.Position - normal * editEpsilon) with
                | Some destroyBlockCoord ->
                    Some
                        { Position = intersection.Position
                          Normal = normal
                          DestroyBlockCoord = destroyBlockCoord
                          PlaceBlockCoordOpt = VoxelWorld.tryWorldToBlockCoord level (intersection.Position + normal * editEpsilon) }
                | None -> None)
        | None -> None

    let private updateSelectedBlockPreview (gameplay : Gameplay) (world : World) =
        let preview = Simulants.SelectedBlockPreview
        if preview.GetExists world then
            match tryGetSelectedBlock gameplay with
            | Some placeableBlock ->
                preview.SetPosition (selectedBlockPreviewPosition world) world
                preview.SetVoxelModel placeableBlock.PreviewModel world
                preview.SetVisible true world
            | None ->
                preview.SetVisible false world

    let private updateAimBlockHighlight (gameplay : Gameplay) (pickOpt : VoxelAimPick option) (world : World) =
        match gameplay.VoxelLevelOpt, pickOpt with
        | Some level, Some pick when VoxelWorld.blockContainsCell level pick.DestroyBlockCoord ->
            let bounds = VoxelWorld.blockBounds level pick.DestroyBlockCoord
            let light = Simulants.AimBlockHighlightLight
            if light.GetExists world then
                light.SetPosition bounds.Center world
                light.SetVisible true world
            for i in 0 .. dec 6 do
                let face = Simulants.AimBlockHighlightFace i
                if face.GetExists world then
                    let struct (position, rotation, scale) = getAimBlockHighlightFace bounds i
                    face.SetPosition position world
                    face.SetRotation rotation world
                    face.SetSize scale world
                    face.SetScale scale world
                    face.SetVisible true world
        | Some _, Some _ | Some _, None | None, _ ->
            let light = Simulants.AimBlockHighlightLight
            if light.GetExists world then light.SetVisible false world
            for i in 0 .. dec 6 do
                let face = Simulants.AimBlockHighlightFace i
                if face.GetExists world then face.SetVisible false world

    let updateAimVisuals (gameplay : Gameplay) (world : World) =
        updateSelectedBlockPreview gameplay world
        updateAimBlockHighlight gameplay (tryPickForward gameplay world) world

    let createVoxelLevel (world : World) =
        match VoxelBake.tryBakeSliceAtlasVolume Assets.Voxels.Minecraft sourceVoxelSize with
        | Some minecraftLevel ->
            let placeableBlocks = VoxelPalettes.createPlaceableBlocks sourceVoxelSize world
            let level = VoxelWorld.createEmptyLevel fallbackWorldSettings placeableBlocks (v3 0.0f 18.0f 0.0f) (VoxelRuntime.freshRevisionSeed ())
            let volumeSizeVoxels =
                v3i
                    (int (MathF.Round (minecraftLevel.VoxelModel.Bounds.Size.X / sourceVoxelSize.X)))
                    (int (MathF.Round (minecraftLevel.VoxelModel.Bounds.Size.Y / sourceVoxelSize.Y)))
                    (int (MathF.Round (minecraftLevel.VoxelModel.Bounds.Size.Z / sourceVoxelSize.Z)))
            let volumeOffset =
                v3i
                    (max 0 ((level.SourceSizeVoxels.X - volumeSizeVoxels.X) / 2))
                    0
                    (max 0 ((level.SourceSizeVoxels.Z - volumeSizeVoxels.Z) / 2))
            if  volumeSizeVoxels.X > level.SourceSizeVoxels.X ||
                volumeSizeVoxels.Y > level.SourceSizeVoxels.Y ||
                volumeSizeVoxels.Z > level.SourceSizeVoxels.Z then
                invalidOp "The fallback voxel atlas does not fit inside the fallback level."
            let volumeCenterX = volumeSizeVoxels.X / 2
            let volumeCenterZ = volumeSizeVoxels.Z / 2
            let spawnClearanceVoxelRadius =
                int (MathF.Ceiling (0.5f / min sourceVoxelSize.X sourceVoxelSize.Z))
            let spawnClearanceVoxelRadiusSquared =
                spawnClearanceVoxelRadius * spawnClearanceVoxelRadius
            let mutable spawnSurfaceInClearance = false
            let mutable spawnColumnDistanceSquared = Int32.MaxValue
            let mutable spawnSurfaceCoord = v3iZero
            for coord in minecraftLevel.OccupiedCoords do
                let dx = coord.X - volumeCenterX
                let dz = coord.Z - volumeCenterZ
                let distanceSquared = dx * dx + dz * dz
                let inClearance = distanceSquared <= spawnClearanceVoxelRadiusSquared
                if  inClearance && not spawnSurfaceInClearance ||
                    inClearance && coord.Y > spawnSurfaceCoord.Y ||
                    inClearance && coord.Y = spawnSurfaceCoord.Y && distanceSquared < spawnColumnDistanceSquared ||
                    not spawnSurfaceInClearance && distanceSquared < spawnColumnDistanceSquared ||
                    not spawnSurfaceInClearance && distanceSquared = spawnColumnDistanceSquared && coord.Y > spawnSurfaceCoord.Y then
                    spawnSurfaceInClearance <- inClearance
                    spawnColumnDistanceSquared <- distanceSquared
                    spawnSurfaceCoord <- coord
            let level =
                if spawnColumnDistanceSquared = Int32.MaxValue then level
                else
                    let spawnSourceCoord =
                        v3i
                            (volumeCenterX + volumeOffset.X)
                            (spawnSurfaceCoord.Y + volumeOffset.Y)
                            (volumeCenterZ + volumeOffset.Z)
                    let spawnPosition =
                        level.Bounds.Min +
                        level.LevelOffset +
                        v3
                            ((single spawnSourceCoord.X + 0.5f) * level.VoxelSize.X)
                            (single (inc spawnSourceCoord.Y) * level.VoxelSize.Y)
                            ((single spawnSourceCoord.Z + 0.5f) * level.VoxelSize.Z)
                    VoxelWorld.withSpawnPosition spawnPosition level
            for struct (coord, albedo) in minecraftLevel.OccupiedVoxels do
                VoxelWorld.setSourceCell level (coord + volumeOffset) { Albedo = albedo; Solid = true; Material = Crafted }
            let voxelChunks =
                VoxelWorld.allChunkCoords level
                |> Array.Parallel.map (fun chunkCoord -> VoxelRuntime.tryBuildChunk level chunkCoord)
                |> Array.choose id
                |> Array.map (fun chunkBuild -> VoxelRuntime.realizeChunk level chunkBuild world)
                |> VoxelRuntime.sortVoxelChunks
            let bodyShapeCount = voxelChunks |> Array.sumBy (fun (voxelChunk : VoxelChunk) -> voxelChunk.BoxCount)
            Log.infoOnce
                ("VoxelForge baked fallback atlas into " + scstring voxelChunks.Length +
                 " voxel chunks with " + scstring bodyShapeCount +
                 " merged physics boxes at spawn " + scstring level.SpawnPosition + ".")
            Some (level, voxelChunks)
        | None ->
            Log.warnOnce "VoxelForge could not bake the minecraft voxel slice atlas."
            None

    let destroyVoxelModel (voxelChunks : VoxelChunk array) (placeableBlocks : PlaceableBlock array) (levelOpt : VoxelLevel option) (world : World) =
        clearStreamBuildJobs ()
        applyVoxelChunkSyncDelta
            { VoxelChunkSyncDelta.Empty with
                ChunksRemoved = voxelChunks }
            world
        VoxelRuntime.destroyVoxelModel voxelChunks placeableBlocks levelOpt world
        destroyFarTerrainLod world

    let tryDestroyBlock (pick : VoxelAimPick) (gameplay : Gameplay) (screen : Screen) (world : World) =
        match gameplay.VoxelLevelOpt with
        | Some level when world.Advancing && VoxelWorld.blockContainsSolidCell level pick.DestroyBlockCoord ->
            VoxelWorld.removeBlock level pick.DestroyBlockCoord
            let struct (rebuilt, syncDelta) = rebuildChunks (VoxelWorld.affectedChunksForBlock level pick.DestroyBlockCoord) { gameplay with AimPickOpt = None } world
            screen.SetGameplay
                { rebuilt with
                    AimPickOpt = None }
                world
            applyVoxelChunkSyncDelta syncDelta world
            updateAimVisuals rebuilt world
        | Some _ | None -> ()

    let tryPlaceBlock (pick : VoxelAimPick) (gameplay : Gameplay) (screen : Screen) (world : World) =
        match gameplay.VoxelLevelOpt, pick.PlaceBlockCoordOpt, tryGetSelectedBlock gameplay with
        | (Some level, Some placeBlockCoord, Some placeableBlock)
            when world.Advancing &&
                 VoxelWorld.isBlockCoordInBounds level placeBlockCoord &&
                 VoxelWorld.blockIsEmpty level placeBlockCoord &&
                 not (blockIntersectsPlayer level placeBlockCoord world) ->
            VoxelWorld.placeBlock level placeableBlock placeBlockCoord
            let struct (rebuilt, syncDelta) = rebuildChunks (VoxelWorld.affectedChunksForBlock level placeBlockCoord) { gameplay with AimPickOpt = None } world
            screen.SetGameplay
                { rebuilt with
                    AimPickOpt = None }
                world
            applyVoxelChunkSyncDelta syncDelta world
            updateAimVisuals rebuilt world
        | _ -> ()

    let private copyPortalPlayerTracking (source : PortalPlayerTracking) (target : PortalPlayerTracking) =
        target.PreviousSignedDistances <- source.PreviousSignedDistances
        target.LastTeleportTime <- source.LastTeleportTime
        target.LastExitPortalOpt <- source.LastExitPortalOpt

    let resolvePortalTraversal (gameplay : Gameplay) (_screen : Screen) (world : World) =
        if world.Advancing && Simulants.GameplayPlayer.GetExists world then
            let playerEntity = Simulants.GameplayPlayer
            let player = playerEntity.GetFirstPersonPlayer world
            let result =
                PortalLogic.tryResolvePlayerTraversal
                    gameplay.PortalPair
                    gameplay.PortalPlayerTracking
                    world.UpdateTime
                    (playerEntity.GetPosition world)
                    world.Eye3dCenter
                    world.Eye3dRotation
                    (playerEntity.GetLinearVelocity world)
            copyPortalPlayerTracking result.Tracking gameplay.PortalPlayerTracking
            if result.Tracking.LastTeleportTime = world.UpdateTime then
                let struct (yaw, pitch) = PortalLogic.yawPitchFromRotation result.EyeRotation
                let player = { player with Yaw = yaw; Pitch = pitch; PreviousMousePositionOpt = None }
                playerEntity.SetPosition result.Position world
                playerEntity.SetRotation (Quaternion.CreateFromAxisAngle (v3Up, yaw)) world
                playerEntity.SetLinearVelocity result.LinearVelocity world
                playerEntity.SetFirstPersonPlayer player world
                FirstPersonPlayerLogic.syncCamera playerEntity player world

    let setInitialCamera spawnPosition (world : World) =
        let eyeCenter = spawnPosition + v3 10.0f 8.0f 12.0f
        let eyeTarget = spawnPosition + v3 0.0f 1.5f 0.0f
        let eyeRotation = Quaternion.CreateLookAt ((eyeTarget - eyeCenter).Normalized, v3Up)
        World.setEye3dCenter eyeCenter world
        World.setEye3dRotation eyeRotation world
        World.setEye3dFieldOfView 0.75f world

    let placePlayerAtSpawn spawnPosition (world : World) =
        if Simulants.GameplayPlayer.GetExists world then
            let playerEntity = Simulants.GameplayPlayer
            let player =
                { playerEntity.GetFirstPersonPlayer world with
                    PreviousMousePositionOpt = None }
            playerEntity.SetPosition spawnPosition world
            playerEntity.SetLinearVelocity v3Zero world
            playerEntity.SetFirstPersonPlayer player world
            FirstPersonPlayerLogic.syncCamera playerEntity player world

    let tryWriteProfileReadyMarker mode (world : World) =
        let filePath = Environment.GetEnvironmentVariable "VOXELFORGE_PROFILE_READY_FILE"
        if not (String.IsNullOrWhiteSpace filePath) then
            try
                let directoryPath = System.IO.Path.GetDirectoryName filePath
                if not (String.IsNullOrWhiteSpace directoryPath) then
                    System.IO.Directory.CreateDirectory directoryPath |> ignore<System.IO.DirectoryInfo>
                let payload =
                    "{\n" +
                    "  \"mode\": \"" + mode + "\",\n" +
                    "  \"updateTime\": " + scstring world.UpdateTime + ",\n" +
                    "  \"writtenUtc\": \"" + DateTime.UtcNow.ToString "o" + "\"\n" +
                    "}\n"
                System.IO.File.WriteAllText (filePath, payload)
            with exn ->
                Log.warnOnce ("VoxelForge failed to write profile ready marker due to: " + scstring exn)

type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    override this.TruncateModel gameplay =
        { gameplay with
            VoxelLevelOpt = None
            VoxelChunks = [||]
            OcclusionBlockCoords = Set.empty
            StreamCenterChunkCoordOpt = None }

    override this.UntruncateModel (current, incoming) =
        { incoming with
            VoxelLevelOpt = current.VoxelLevelOpt
            VoxelChunks = current.VoxelChunks
            OcclusionBlockCoords = current.OcclusionBlockCoords
            StreamCenterChunkCoordOpt = current.StreamCenterChunkCoordOpt }

    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.DeselectingEvent => FinishQuitting
         Screen.TimeUpdateEvent => TimeUpdate
         Game.MouseLeftDownEvent => TryDestroyBlock
         Game.MouseRightDownEvent => TryPlaceBlock]

    override this.Message (gameplay, message, _, world) =
        match message with
        | StartPlaying ->
            let gameplay = { Gameplay.initial with VoxelModelReady = true }
            match (Game.GetVoxelForge world).GeneratedWorldPackageOpt with
            | Some package -> withSignal (signal (UseGeneratedWorld package)) gameplay
            | None -> withSignal (signal EnsureVoxelModel) gameplay

        | FinishQuitting ->
            let placeableBlocks =
                match gameplay.VoxelLevelOpt with
                | Some level -> level.PlaceableBlocks
                | None -> [||]
            withSignal (signal (DestroyVoxelModel (gameplay.VoxelChunks, placeableBlocks, gameplay.VoxelLevelOpt))) Gameplay.empty

        | TimeUpdate ->
            let gameplay = GameplayLogic.updateSelectedBlockFromInput gameplay world
            if gameplay.GameplayState = Playing then
                GameplayLogic.updateAimVisuals gameplay world
            if gameplay.GameplayState = Playing && not gameplay.VoxelModelReady then
                withSignal (signal EnsureVoxelModel) { gameplay with VoxelModelReady = true }
            elif gameplay.GameplayState = Playing && GameplayLogic.shouldStreamVoxelChunks gameplay world then
                withSignal (signal StreamVoxelChunks) gameplay
            else just gameplay

        | TryDestroyBlock ->
            match GameplayLogic.tryPickForward gameplay world with
            | Some pick when world.Advancing -> withSignal (signal (DestroyBlock pick)) gameplay
            | Some _ | None -> just gameplay

        | TryPlaceBlock ->
            match GameplayLogic.tryPickForward gameplay world with
            | Some pick when world.Advancing -> withSignal (signal (PlaceBlock pick)) gameplay
            | Some _ | None -> just gameplay

    override this.Command (gameplay, command, screen, world) =
        match command with
        | EnsureVoxelModel ->
            match GameplayLogic.createVoxelLevel world with
            | Some (voxelLevel, voxelChunks) ->
                if world.Unaccompanied then GameplayLogic.setInitialCamera voxelLevel.SpawnPosition world
                let portalPair = PortalLogic.pairAtGround voxelLevel.SpawnPosition
                let gameplay =
                    { gameplay with
                        GameplayState = Playing
                        VoxelModelReady = true
                        VoxelLevelOpt = Some voxelLevel
                        VoxelChunks = voxelChunks
                        OcclusionBlockCoords = GameplayLogic.computeOcclusionBlockCoords voxelChunks
                        StreamCenterChunkCoordOpt = None
                        PortalPair = portalPair
                        AimPickOpt = None
                        SelectedBlockIndex = 0
                        SelectedBlockPreviewPositionOpt = None }
                screen.SetGameplay gameplay world
                GameplayLogic.applyVoxelChunkSyncDelta
                    { VoxelChunkSyncDelta.Empty with
                        ChunksAdded = voxelChunks }
                    world
                GameplayLogic.placePlayerAtSpawn voxelLevel.SpawnPosition world
                GameplayLogic.destroyFarTerrainLod world
                GameplayLogic.updateAimVisuals gameplay world
                GameplayLogic.tryWriteProfileReadyMarker "fallback" world
            | None ->
                let fallbackSpawn = v3 0.0f 18.0f 0.0f
                if world.Unaccompanied then GameplayLogic.setInitialCamera fallbackSpawn world
                screen.SetGameplay
                    { gameplay with
                        GameplayState = Playing
                        VoxelModelReady = true
                        VoxelLevelOpt = None
                        VoxelChunks = [||]
                        OcclusionBlockCoords = Set.empty
                        StreamCenterChunkCoordOpt = None
                        PortalPair = PortalLogic.pairAtGround fallbackSpawn
                        AimPickOpt = None
                        SelectedBlockIndex = 0
                        SelectedBlockPreviewPositionOpt = None }
                    world
                GameplayLogic.destroyFarTerrainLod world
        | UseGeneratedWorld package ->
            Log.infoOnce
                ("VoxelForge entering generated world with " +
                 scstring package.Stats.SourceVoxelCount + " source voxels, " +
                 scstring package.Chunks.Length + " chunks, and " +
                 scstring package.Stats.BodyShapeCount + " merged physics boxes.")
            if world.Unaccompanied then GameplayLogic.setInitialCamera package.SpawnPosition world
            let portalPair = PortalLogic.pairAtGround package.SpawnPosition
            let gameplay =
                GameplayLogic.streamInitialChunksAroundPosition
                    package.SpawnPosition
                    { gameplay with
                        GameplayState = Playing
                        VoxelModelReady = true
                        VoxelLevelOpt = Some package.Level
                        VoxelChunks = package.Chunks
                        OcclusionBlockCoords = GameplayLogic.computeOcclusionBlockCoords package.Chunks
                        StreamCenterChunkCoordOpt = None
                        PortalPair = portalPair
                        AimPickOpt = None
                        SelectedBlockIndex = 0
                        SelectedBlockPreviewPositionOpt = None }
                    world
            screen.SetGameplay gameplay world
            GameplayLogic.applyVoxelChunkSyncDelta
                { VoxelChunkSyncDelta.Empty with
                    ChunksAdded = gameplay.VoxelChunks }
                world
            GameplayLogic.placePlayerAtSpawn package.SpawnPosition world
            GameplayLogic.createFarTerrainLod package.Level world
            GameplayLogic.updateAimVisuals gameplay world
            GameplayLogic.tryWriteProfileReadyMarker "generated-world" world
            Game.SetVoxelForge { Game.GetVoxelForge world with GeneratedWorldPackageOpt = None } world
        | DestroyVoxelModel (voxelChunks, placeableBlocks, levelOpt) ->
            GameplayLogic.destroyVoxelModel voxelChunks placeableBlocks levelOpt world
        | DestroyBlock pick ->
            GameplayLogic.tryDestroyBlock pick gameplay screen world
        | PlaceBlock pick ->
            GameplayLogic.tryPlaceBlock pick gameplay screen world
        | StreamVoxelChunks ->
            let struct (gameplay, syncDelta, changed) = GameplayLogic.streamChunksForCurrentPositionDefault gameplay world
            if changed then
                screen.SetGameplay gameplay world
                GameplayLogic.applyVoxelChunkSyncDelta syncDelta world
                GameplayLogic.updateAimVisuals gameplay world
        | ResolvePortalTraversal ->
            GameplayLogic.resolvePortalTraversal gameplay screen world
        | StartQuitting ->
            World.publish () screen.QuitEvent screen world

    override this.PostUpdate (screen, world) =
        let gameplay = screen.GetGameplay world
        if gameplay.GameplayState = Playing then
            GameplayLogic.resolvePortalTraversal gameplay screen world

    override this.Content (gameplay, _) =

        [if gameplay.GameplayState = Playing then
            let playerSpawnPosition =
                match gameplay.VoxelLevelOpt with
                | Some level -> level.SpawnPosition
                | None -> v3 0.0f 18.0f 0.0f
            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []

                [let environmentBounds =
                    match gameplay.VoxelLevelOpt with
                    | Some level ->
                        box3
                            (level.Bounds.Min + level.LevelOffset - v3 48.0f 24.0f 48.0f)
                            (level.Bounds.Size + v3 96.0f 96.0f 96.0f)
                    | None ->
                        box3 (v3 -96.0f -16.0f -96.0f) (v3 192.0f 128.0f 192.0f)
                 let sunPosition = playerSpawnPosition + v3 -96.0f 128.0f -96.0f

                 Content.skyBox Simulants.GameplaySkyBox.Name
                    [Entity.Absolute == true
                     Entity.AmbientColor == color 0.86f 0.93f 1.0f 1.0f
                     Entity.AmbientBrightness == 0.72f
                     Entity.Color == color 0.72f 0.86f 1.0f 1.0f
                     Entity.Brightness == 1.15f
                     Entity.Presence == Omnipresent
                     Entity.Static == true]

                 Content.light3d Simulants.GameplaySunLight.Name
                    [Entity.Position := sunPosition
                     Entity.Rotation == Quaternion.CreateFromYawPitchRoll (-0.55f, -0.85f, 0.0f)
                     Entity.Presence == Omnipresent
                     Entity.AlwaysRender == true
                     Entity.Static == true
                     Entity.LightType == DirectionalLight 20.0f
                     Entity.Color == color 1.0f 0.94f 0.82f 1.0f
                     Entity.Brightness == 4.0f
                     Entity.LightCutoff == 160.0f
                     Entity.AutoAttenuate == false
                     Entity.DesireShadows == false
                     Entity.DesireFog == false]

                 Content.lightProbe3d Simulants.GameplayLightProbe.Name
                    [Entity.Position := environmentBounds.Center
                     Entity.Presence == Omnipresent
                     Entity.AlwaysRender == true
                     Entity.Static == true
                     Entity.AmbientColor == color 0.86f 0.93f 1.0f 1.0f
                     Entity.AmbientBrightness == 0.72f
                     Entity.ProbeBounds := environmentBounds]

                 Content.staticModel Simulants.GameplaySun.Name
                    [Entity.Position := sunPosition
                     Entity.Size == v3Dup 10.0f
                     Entity.Scale == v3Dup 10.0f
                     Entity.Presence == Omnipresent
                     Entity.AlwaysRender == true
                     Entity.Static == true
                     Entity.Pickable == false
                     Entity.CastShadow == false
                     Entity.StaticModel == Assets.Default.BallModel
                     Entity.MaterialProperties ==
                        { MaterialProperties.defaultProperties with
                            AlbedoOpt = ValueSome (color 1.0f 0.92f 0.65f 1.0f)
                            EmissionOpt = ValueSome 3.0f
                            RoughnessOpt = ValueSome 0.45f
                            MetallicOpt = ValueSome 0.0f }]

                 let farTerrainLodVisible =
                    match gameplay.VoxelLevelOpt with
                    | Some level -> Option.isSome level.GenerationOpt
                    | None -> false
                 Content.staticModel Simulants.FarTerrainLod.Name
                    [Entity.Visible := farTerrainLodVisible
                     Entity.Position == v3Zero
                     Entity.Size == v3One
                     Entity.Scale == v3One
                     Entity.Presence == Imposter
                     Entity.AlwaysRender == true
                     Entity.Static == true
                     Entity.Pickable == false
                     Entity.CastShadow == false
                     Entity.StaticModel == Assets.Voxels.FarTerrainLod
                     Entity.MaterialProperties == MaterialProperties.empty]

                 Content.composite<FirstPersonPlayerDispatcher> Simulants.GameplayPlayer.Name
                    [Entity.Position == playerSpawnPosition]
                    [Content.staticModel Simulants.GameplayPlayerBody.Name
                        [Entity.PositionLocal == v3 0.0f 0.85f 0.0f
                         Entity.Size == v3 0.7f 1.7f 0.7f
                         Entity.Scale == v3 0.7f 1.7f 0.7f
                         Entity.Presence == Omnipresent
                         Entity.AlwaysRender == true
                         Entity.Static == false
                         Entity.Pickable == false
                         Entity.CastShadow == false
                         Entity.StaticModel == Assets.Default.BallModel
                         Entity.MaterialProperties ==
                            { MaterialProperties.empty with
                                AlbedoOpt = ValueSome (color 0.28f 0.9f 0.65f 1.0f)
                                RoughnessOpt = ValueSome 0.8f
                                MetallicOpt = ValueSome 0.0f
                                AmbientOcclusionOpt = ValueSome 1.0f
                                EmissionOpt = ValueSome 0.08f }]]

                 for portal in PortalLogic.portals gameplay.PortalPair do
                    let destination = PortalLogic.pairedPortal gameplay.PortalPair portal
                    let aperture =
                        match portal.Id with
                        | Blue -> Simulants.BluePortalAperture
                        | Orange -> Simulants.OrangePortalAperture
                    let portalSize = v3 (portal.HalfExtents.X * 2.0f) (portal.HalfExtents.Y * 2.0f) 0.035f
                    Content.entity<PortalApertureDispatcher> aperture.Name
                        [Entity.Position := portal.Center
                         Entity.Rotation := portal.Rotation
                         Entity.Size := portalSize
                         Entity.Scale := portalSize
                         Entity.Presence == Omnipresent
                         Entity.AlwaysRender == true
                         Entity.Static == true
                         Entity.Pickable == false
                         Entity.CastShadow == false
                         Entity.PortalSourceId := PortalLogic.portalIdToInt64 portal.Id
                         Entity.PortalDestinationCenter := destination.Center
                         Entity.PortalDestinationRotation := destination.Rotation
                         Entity.PortalHalfExtents := portal.HalfExtents
                         Entity.PortalRecursionLimit := gameplay.PortalPair.RecursionLimit
                         Entity.PortalOneSided := true
                         Entity.PortalTint := PortalLogic.portalTint portal.Id]

                 let selectedBlockOpt = GameplayLogic.tryGetSelectedBlock gameplay
                 let selectedBlockPreviewModel =
                    match selectedBlockOpt with
                    | Some placeableBlock -> placeableBlock.PreviewModel
                    | None -> Assets.Default.VoxelModel

                 Content.voxel Simulants.SelectedBlockPreview.Name
                    [Entity.Size == v3One
                     Entity.Scale == v3Dup 0.35f
                     Entity.VoxelModel := selectedBlockPreviewModel
                     Entity.Visible == Option.isSome selectedBlockOpt
                     Entity.Static == true
                     Entity.MaterialProperties ==
                        { MaterialProperties.empty with
                            RoughnessOpt = ValueSome 0.88f
                            MetallicOpt = ValueSome 0.0f
                            AmbientOcclusionOpt = ValueSome 1.0f
                            EmissionOpt = ValueSome 0.0f
                            ClearCoatOpt = ValueSome 0.0f
                            ClearCoatRoughnessOpt = ValueSome 1.0f }]

                 Content.light3d Simulants.AimBlockHighlightLight.Name
                    [Entity.Visible == false
                     Entity.Presence == Omnipresent
                     Entity.AlwaysRender == true
                     Entity.Static == true
                     Entity.LightType == PointLight
                     Entity.Color == color 0.18f 0.95f 1.0f 1.0f
                     Entity.Brightness == 1.25f
                     Entity.LightCutoff == 2.5f
                     Entity.AutoAttenuate == true
                     Entity.DesireShadows == false
                     Entity.DesireFog == false]
                 for i in 0 .. dec 6 do
                    Content.staticModel (Simulants.AimBlockHighlightFace i).Name
                        [Entity.Visible == false
                         Entity.Size == v3One
                         Entity.Scale == v3One
                         Entity.Presence == Omnipresent
                         Entity.AlwaysRender == true
                         Entity.Static == true
                         Entity.Pickable == false
                         Entity.CastShadow == false
                         Entity.Clipped == true
                         Entity.DepthTest == LessThanOrEqualTest
                         Entity.RenderStyle == Deferred
                         Entity.StaticModel == Assets.Default.HighlightModel
                         Entity.MaterialProperties ==
                            { MaterialProperties.defaultProperties with
                                AlbedoOpt = ValueSome (color 0.18f 0.95f 1.0f 1.0f)
                                RoughnessOpt = ValueSome 0.25f
                                MetallicOpt = ValueSome 0.0f
                                AmbientOcclusionOpt = ValueSome 1.0f
                                EmissionOpt = ValueSome 1.65f
                                SpecularScalarOpt = ValueSome 0.0f }]

                 Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 232.0f -144.0f 0.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]]
