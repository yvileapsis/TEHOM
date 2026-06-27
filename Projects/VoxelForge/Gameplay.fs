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

type Gameplay =
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
        if bounds.Area <= 0 then false
        else
            let mutable covered = true
            let mutable y = bounds.MinY
            while covered && y <= bounds.MaxY do
                let row = y * depthWidth
                let mutable x = bounds.MinX
                while covered && x <= bounds.MaxX do
                    let depth = depths[row + x]
                    covered <- depth < Single.PositiveInfinity && bounds.NearDepth > depth + depthBias
                    x <- inc x
                y <- inc y
            covered

    let private rasterize (depths : single array) (bounds : ProjectedBounds) =
        if bounds.Area > 0 && not (Single.IsNaN bounds.FarDepth) then
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

    let private visibleChunks = HashSet<Vector3i> (HashIdentity.Structural)
    let private candidateChunks = ResizeArray<VoxelChunk> ()
    let private depthBuffer = Array.zeroCreate<single> (depthWidth * depthHeight)
    let private distanceComparer = VoxelChunkDistanceComparer ()

    let private computeVisibleChunksUncached (gameplay : Gameplay) (world : World) =
        visibleChunks.Clear ()
        candidateChunks.Clear ()
        match gameplay.VoxelLevelOpt with
        | Some _ ->
            for chunk in gameplay.VoxelChunks do
                if  (Option.isSome chunk.VoxelModelOpt || chunk.OpaqueOccluderBoxes.Length > 0) &&
                    World.boundsInView3d false Exterior (chunkBounds chunk) world then
                    candidateChunks.Add chunk
            iteratePortalEyePoses gameplay.PortalPair world.Eye3dCenter world.Eye3dRotation (fun eyeCenter eyeRotation ->
                Array.Fill (depthBuffer, Single.PositiveInfinity)
                let viewProjection = Viewport.getViewProjection3d eyeCenter eyeRotation world.Eye3dFieldOfView world.WindowViewport
                distanceComparer.EyeCenter <- eyeCenter
                candidateChunks.Sort distanceComparer
                for chunk in candidateChunks do
                    let renderable = Option.isSome chunk.VoxelModelOpt && chunk.SplatCount > 0
                    let bounds = chunkBounds chunk
                    match tryProjectBounds viewProjection bounds with
                    | ValueSome projectedBounds ->
                        let occluded = isCovered depthBuffer projectedBounds
                        if not occluded then
                            if renderable then visibleChunks.Add chunk.ChunkCoord |> ignore<bool>
                            for occluderBox in chunk.OpaqueOccluderBoxes do
                                match tryProjectBounds viewProjection occluderBox with
                                | ValueSome projectedOccluder -> rasterize depthBuffer projectedOccluder
                                | ValueNone -> ()
                    | ValueNone ->
                        if renderable then visibleChunks.Add chunk.ChunkCoord |> ignore<bool>)
        | None -> ()
        visibleChunks

    let mutable private visibleChunksCacheValid = false
    let mutable private visibleChunksCacheUpdateTime = 0L
    let mutable private visibleChunksCacheGameplayTime = 0L
    let mutable private visibleChunksCacheEyeCenter = v3Zero
    let mutable private visibleChunksCacheEyeRotation = quatIdentity
    let mutable private visibleChunksCacheChunkCount = 0

    let getVisibleChunks (gameplay : Gameplay) (world : World) =
        let updateTime = world.UpdateTime
        let gameplayTime = gameplay.GameplayTime
        let eyeCenter = world.Eye3dCenter
        let eyeRotation = world.Eye3dRotation
        let chunkCount = gameplay.VoxelChunks.Length
        if  visibleChunksCacheValid &&
            visibleChunksCacheUpdateTime = updateTime &&
            visibleChunksCacheGameplayTime = gameplayTime &&
            visibleChunksCacheEyeCenter = eyeCenter &&
            visibleChunksCacheEyeRotation = eyeRotation &&
            visibleChunksCacheChunkCount = chunkCount then
            visibleChunks
        else
            let visibleChunks = computeVisibleChunksUncached gameplay world
            visibleChunksCacheValid <- true
            visibleChunksCacheUpdateTime <- updateTime
            visibleChunksCacheGameplayTime <- gameplayTime
            visibleChunksCacheEyeCenter <- eyeCenter
            visibleChunksCacheEyeRotation <- eyeRotation
            visibleChunksCacheChunkCount <- chunkCount
            visibleChunks

    let isChunkVisible (gameplay : Gameplay) chunkCoord (world : World) =
        (getVisibleChunks gameplay world).Contains chunkCoord

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
            let mutable transform = entity.GetTransform world
            let castShadow = (World.getRenderer3dConfig world).LightShadowingEnabled && transform.CastShadow
            let visibleByGameplay =
                if renderPass.IsNormalPass && Simulants.Gameplay.GetExists world then
                    VoxelChunkVisibility.isChunkVisible (Simulants.Gameplay.GetGameplay world) (entity.GetVoxelChunkCoord world) world
                else true
            if transform.Visible && visibleByGameplay && (not renderPass.IsShadowPass || castShadow) then
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

    type private StreamBuildJob =
        { EditRevision : int
          BuildTask : Task<VoxelRuntime.VoxelChunkBuild option> }

    let private streamBuildJobs = Dictionary<Vector3i, StreamBuildJob> (HashIdentity.Structural)
    let private streamEmptyChunkCoords = HashSet<Vector3i> (HashIdentity.Structural)
    let private streamEditedChunkCoords = HashSet<Vector3i> (HashIdentity.Structural)
    let private streamBuildJobsLock = obj ()
    let private streamCoordsToRemoveBuffer = ResizeArray<Vector3i> ()
    let private streamCompletedCoordsBuffer = ResizeArray<Vector3i> ()
    let private streamCompletedBuildsBuffer = ResizeArray<VoxelRuntime.VoxelChunkBuild> ()
    let private streamLoadedChunkCoordsBuffer = HashSet<Vector3i> (HashIdentity.Structural)
    let private streamChunksToKeepBuffer = ResizeArray<VoxelChunk> ()
    let private streamChunksToDestroyBuffer = ResizeArray<VoxelChunk> ()

    let private clearStreamBuildJobs () =
        lock streamBuildJobsLock (fun () ->
            streamBuildJobs.Clear ()
            streamEmptyChunkCoords.Clear ()
            streamEditedChunkCoords.Clear ())

    let private hasStreamBuildJobs () =
        lock streamBuildJobsLock (fun () -> streamBuildJobs.Count > 0)

    let private markStreamEditedChunks (chunkCoords : Vector3i array) =
        lock streamBuildJobsLock (fun () ->
            for chunkCoord in chunkCoords do
                streamEmptyChunkCoords.Remove chunkCoord |> ignore<bool>
                streamEditedChunkCoords.Add chunkCoord |> ignore<bool>)

    let private isStreamEditedChunk (chunkCoord : Vector3i) =
        lock streamBuildJobsLock (fun () -> streamEditedChunkCoords.Contains chunkCoord)

    let private removeStreamBuildJobsOutside (desiredChunkCoordSet : HashSet<Vector3i>) =
        lock streamBuildJobsLock (fun () ->
            let coordsToRemove = streamCoordsToRemoveBuffer
            coordsToRemove.Clear ()
            for entry in streamBuildJobs do
                if not (desiredChunkCoordSet.Contains entry.Key) then
                    coordsToRemove.Add entry.Key
            for chunkCoord in coordsToRemove do
                streamBuildJobs.Remove chunkCoord |> ignore<bool>
            coordsToRemove.Clear ()
            for chunkCoord in streamEmptyChunkCoords do
                if not (desiredChunkCoordSet.Contains chunkCoord) then
                    coordsToRemove.Add chunkCoord
            for chunkCoord in coordsToRemove do
                streamEmptyChunkCoords.Remove chunkCoord |> ignore<bool>
            coordsToRemove.Clear ())

    let private tryQueueStreamBuild (level : VoxelLevel) (snapshot : VoxelEditSnapshot) useCache (chunkCoord : Vector3i) =
        lock streamBuildJobsLock (fun () ->
            if  streamBuildJobs.Count >= streamBuildJobsMax ||
                streamBuildJobs.ContainsKey chunkCoord ||
                streamEmptyChunkCoords.Contains chunkCoord then false
            else
                let buildTask =
                    Task.Run<VoxelRuntime.VoxelChunkBuild option>
                        (Func<VoxelRuntime.VoxelChunkBuild option>
                            (fun () ->
                                VoxelRuntime.tryBuildChunkWithCellLookupCached
                                    useCache
                                    level
                                    (VoxelWorld.tryGetCellWithEditSnapshotValue snapshot level)
                                    chunkCoord))
                streamBuildJobs[chunkCoord] <- { EditRevision = snapshot.Revision; BuildTask = buildTask }
                true)

    let private collectCompletedStreamBuilds (level : VoxelLevel) (desiredChunkCoordSet : HashSet<Vector3i>) (loadedChunkCoords : HashSet<Vector3i>) buildLimit =
        let currentEditRevision = VoxelWorld.getEditRevision level
        lock streamBuildJobsLock (fun () ->
            let completedCoords = streamCompletedCoordsBuffer
            let completedBuilds = streamCompletedBuildsBuffer
            completedCoords.Clear ()
            completedBuilds.Clear ()
            let mutable count = 0
            let limit = max 0 buildLimit
            for entry in streamBuildJobs do
                if count < limit && entry.Value.BuildTask.IsCompleted then
                    completedCoords.Add entry.Key
                    count <- inc count
            for chunkCoord in completedCoords do
                match streamBuildJobs.TryGetValue chunkCoord with
                | (true, job) ->
                    streamBuildJobs.Remove chunkCoord |> ignore<bool>
                    if  job.EditRevision = currentEditRevision &&
                        desiredChunkCoordSet.Contains chunkCoord &&
                        not (loadedChunkCoords.Contains chunkCoord) &&
                        job.BuildTask.Status = TaskStatus.RanToCompletion then
                        match job.BuildTask.Result with
                        | Some chunkBuild -> completedBuilds.Add chunkBuild
                        | None -> streamEmptyChunkCoords.Add chunkCoord |> ignore<bool>
                | (false, _) -> ()
            let completedBuildsArray = completedBuilds.ToArray ()
            completedCoords.Clear ()
            completedBuilds.Clear ()
            completedBuildsArray)

    let private queueMissingStreamBuilds (level : VoxelLevel) (desiredChunkCoords : Vector3i array) (loadedChunkCoords : HashSet<Vector3i>) buildLimit =
        let mutable queued = 0
        let mutable i = 0
        let mutable snapshotCaptured = false
        let mutable snapshot = Unchecked.defaultof<VoxelEditSnapshot>
        while queued < buildLimit && i < desiredChunkCoords.Length do
            let chunkCoord = desiredChunkCoords[i]
            if not (loadedChunkCoords.Contains chunkCoord) then
                if not snapshotCaptured then
                    snapshot <- VoxelWorld.snapshotEdits level
                    snapshotCaptured <- true
                let useCache = not (isStreamEditedChunk chunkCoord)
                if tryQueueStreamBuild level snapshot useCache chunkCoord then
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

    let updateSelectedBlockPreviewPosition (gameplay : Gameplay) (world : World) =
        let rotation = world.Eye3dRotation
        { gameplay with
            SelectedBlockPreviewPositionOpt =
                Some (world.Eye3dCenter + rotation.Forward * 1.25f + rotation.Right * 0.55f + rotation.Down * 0.35f) }

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

    let mutable private desiredStreamChunkCoordsCacheValid = false
    let mutable private desiredStreamChunkCoordsCacheCenter = v3iZero
    let mutable private desiredStreamChunkCoordsCacheChunkCounts = v3iZero
    let mutable private desiredStreamChunkCoordsCache = [||]
    let private desiredStreamChunkCoordSetCache = HashSet<Vector3i> (HashIdentity.Structural)

    let private desiredStreamChunkCoordsAndSet (level : VoxelLevel) (center : Vector3i) =
        if  not desiredStreamChunkCoordsCacheValid ||
            desiredStreamChunkCoordsCacheCenter <> center ||
            desiredStreamChunkCoordsCacheChunkCounts <> level.ChunkCounts then
            let coords = VoxelWorld.streamChunkCoords streamChunkRadius level center
            desiredStreamChunkCoordSetCache.Clear ()
            for chunkCoord in coords do
                desiredStreamChunkCoordSetCache.Add chunkCoord |> ignore<bool>
            desiredStreamChunkCoordsCacheValid <- true
            desiredStreamChunkCoordsCacheCenter <- center
            desiredStreamChunkCoordsCacheChunkCounts <- level.ChunkCounts
            desiredStreamChunkCoordsCache <- coords
        struct (desiredStreamChunkCoordsCache, desiredStreamChunkCoordSetCache)

    let private desiredStreamChunkCoords (level : VoxelLevel) (center : Vector3i) =
        let struct (coords, _) = desiredStreamChunkCoordsAndSet level center
        coords

    let private desiredStreamEmptyChunkCount (desiredChunkCoords : Vector3i array) =
        lock streamBuildJobsLock (fun () ->
            let mutable count = 0
            for chunkCoord in desiredChunkCoords do
                if streamEmptyChunkCoords.Contains chunkCoord then
                    count <- inc count
            count)

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
                let struct (desiredChunkCoords, desiredChunkCoordSet) = desiredStreamChunkCoordsAndSet level centerChunkCoord
                let loadedChunkCoords = streamLoadedChunkCoordsBuffer
                loadedChunkCoords.Clear ()
                for chunk in gameplay.VoxelChunks do
                    loadedChunkCoords.Add chunk.ChunkCoord |> ignore<bool>
                removeStreamBuildJobsOutside desiredChunkCoordSet
                let chunksToKeep = streamChunksToKeepBuffer
                let chunksToDestroy = streamChunksToDestroyBuffer
                chunksToKeep.Clear ()
                chunksToDestroy.Clear ()
                for chunk in gameplay.VoxelChunks do
                    if desiredChunkCoordSet.Contains chunk.ChunkCoord
                    then chunksToKeep.Add chunk
                    else chunksToDestroy.Add chunk
                let completedBuilds = collectCompletedStreamBuilds level desiredChunkCoordSet loadedChunkCoords buildLimit
                let chunksToAdd =
                    if completedBuilds.Length > 0 then
                        let chunksToAdd = Array.zeroCreate<VoxelChunk> completedBuilds.Length
                        for i in 0 .. dec completedBuilds.Length do
                            let chunk = VoxelRuntime.realizeChunk level completedBuilds[i] world
                            chunksToAdd[i] <- chunk
                            loadedChunkCoords.Add chunk.ChunkCoord |> ignore<bool>
                        chunksToAdd
                    else Array.empty
                queueMissingStreamBuilds level desiredChunkCoords loadedChunkCoords buildLimit
                let chunksToDestroyArray =
                    if chunksToDestroy.Count > 0 then chunksToDestroy.ToArray ()
                    else Array.empty
                if chunksToDestroyArray.Length > 0 then
                    VoxelRuntime.destroyVoxelChunks chunksToDestroyArray world
                let chunksChanged = chunksToDestroyArray.Length > 0 || chunksToAdd.Length > 0
                let voxelChunks =
                    if chunksChanged then
                        let voxelChunks = Array.zeroCreate<VoxelChunk> (chunksToKeep.Count + chunksToAdd.Length)
                        chunksToKeep.CopyTo (voxelChunks, 0)
                        Array.Copy (chunksToAdd, 0, voxelChunks, chunksToKeep.Count, chunksToAdd.Length)
                        VoxelRuntime.sortVoxelChunks voxelChunks
                    else gameplay.VoxelChunks
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
                             true)
                    else struct (gameplay, false)
                loadedChunkCoords.Clear ()
                chunksToKeep.Clear ()
                chunksToDestroy.Clear ()
                result
            | None -> struct (gameplay, false)
        | None -> struct (gameplay, false)

    let streamChunksForCurrentPosition buildLimit (gameplay : Gameplay) (world : World) =
        streamChunksAroundPosition buildLimit (getStreamingPosition gameplay world) gameplay world

    let streamInitialChunksAroundPosition position (gameplay : Gameplay) (world : World) =
        clearStreamBuildJobs ()
        let struct (gameplay, _) = streamChunksAroundPosition streamInitialBuildLimit position gameplay world
        gameplay

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
                else
                    let desiredChunkCoords = desiredStreamChunkCoords level centerChunkCoord
                    gameplay.VoxelChunks.Length + desiredStreamEmptyChunkCount desiredChunkCoords < desiredChunkCoords.Length
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
            markStreamEditedChunks chunkCoords
            let loadedChunkCoords = HashSet<Vector3i> (HashIdentity.Structural)
            for chunk in gameplay.VoxelChunks do
                loadedChunkCoords.Add chunk.ChunkCoord |> ignore<bool>
            let chunkCoords =
                chunkCoords
                |> Array.filter (fun chunkCoord -> loadedChunkCoords.Contains chunkCoord)
            let chunkCoordSet = Set.ofArray chunkCoords
            let struct (voxelChunks, chunksToDestroy) = VoxelRuntime.rebuildChunks chunkCoords level gameplay.VoxelChunks world
            let chunksToAdd =
                voxelChunks
                |> Array.filter (fun (chunk : VoxelChunk) -> Set.contains chunk.ChunkCoord chunkCoordSet)
            let occlusionBlockCoords = replaceOcclusionBlockCoords gameplay.OcclusionBlockCoords chunksToDestroy chunksToAdd
            struct ({ gameplay with VoxelChunks = voxelChunks; OcclusionBlockCoords = occlusionBlockCoords }, chunksToDestroy)
        | None -> struct (gameplay, [||])

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

    let createVoxelLevel (world : World) =
        match VoxelBake.tryBakeSliceAtlasVolume Assets.Voxels.Minecraft sourceVoxelSize with
        | Some minecraftLevel ->
            let placeableBlocks = VoxelPalettes.createPlaceableBlocks sourceVoxelSize world
            let level = VoxelWorld.createEmptyLevel fallbackWorldSettings placeableBlocks (v3 0.0f 18.0f 0.0f) (VoxelRuntime.freshRevisionSeed ())
            for struct (coord, albedo) in minecraftLevel.OccupiedVoxels do
                VoxelWorld.setSourceCell level coord { Albedo = albedo; Solid = true; Material = Crafted }
            let voxelChunks =
                VoxelWorld.allChunkCoords level
                |> Array.Parallel.map (fun chunkCoord -> VoxelRuntime.tryBuildChunk level chunkCoord)
                |> Array.choose id
                |> Array.map (fun chunkBuild -> VoxelRuntime.realizeChunk level chunkBuild world)
                |> VoxelRuntime.sortVoxelChunks
            let bodyShapeCount = voxelChunks |> Array.sumBy (fun (voxelChunk : VoxelChunk) -> voxelChunk.BoxCount)
            Log.infoOnce ("VoxelForge baked fallback atlas into " + scstring voxelChunks.Length + " voxel chunks with " + scstring bodyShapeCount + " merged physics boxes.")
            Some (level, voxelChunks)
        | None ->
            Log.warnOnce "VoxelForge could not bake the minecraft voxel slice atlas."
            None

    let private destroyVoxelChunks (voxelChunks : VoxelChunk array) (world : World) =
        VoxelRuntime.destroyVoxelChunks voxelChunks world

    let destroyVoxelModel (voxelChunks : VoxelChunk array) (placeableBlocks : PlaceableBlock array) (levelOpt : VoxelLevel option) (world : World) =
        clearStreamBuildJobs ()
        VoxelRuntime.destroyVoxelModel voxelChunks placeableBlocks levelOpt world

    let tryDestroyBlock (pick : VoxelAimPick) (gameplay : Gameplay) (screen : Screen) (world : World) =
        match gameplay.VoxelLevelOpt with
        | Some level when world.Advancing && VoxelWorld.blockContainsSolidCell level pick.DestroyBlockCoord ->
            VoxelWorld.removeBlock level pick.DestroyBlockCoord
            let struct (rebuilt, chunksToDestroy) = rebuildChunks (VoxelWorld.affectedChunksForBlock level pick.DestroyBlockCoord) { gameplay with AimPickOpt = None } world
            screen.SetGameplay
                { rebuilt with
                    AimPickOpt = None }
                world
            destroyVoxelChunks chunksToDestroy world
        | Some _ | None -> ()

    let tryPlaceBlock (pick : VoxelAimPick) (gameplay : Gameplay) (screen : Screen) (world : World) =
        match gameplay.VoxelLevelOpt, pick.PlaceBlockCoordOpt, tryGetSelectedBlock gameplay with
        | (Some level, Some placeBlockCoord, Some placeableBlock)
            when world.Advancing &&
                 VoxelWorld.isBlockCoordInBounds level placeBlockCoord &&
                 VoxelWorld.blockIsEmpty level placeBlockCoord &&
                 not (blockIntersectsPlayer level placeBlockCoord world) ->
            VoxelWorld.placeBlock level placeableBlock placeBlockCoord
            let struct (rebuilt, chunksToDestroy) = rebuildChunks (VoxelWorld.affectedChunksForBlock level placeBlockCoord) { gameplay with AimPickOpt = None } world
            screen.SetGameplay
                { rebuilt with
                    AimPickOpt = None }
                world
            destroyVoxelChunks chunksToDestroy world
        | _ -> ()

    let resolvePortalTraversal (gameplay : Gameplay) (screen : Screen) (world : World) =
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
            screen.SetGameplay { gameplay with PortalPlayerTracking = result.Tracking } world
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
            let gameplay =
                { gameplay with
                    GameplayTime = gameplay.GameplayTime + world.GameDelta.Updates
                    AimPickOpt = GameplayLogic.tryPickForward gameplay world }
                |> fun gameplay -> GameplayLogic.updateSelectedBlockFromInput gameplay world
                |> fun gameplay -> GameplayLogic.updateSelectedBlockPreviewPosition gameplay world
            if gameplay.GameplayState = Playing && not gameplay.VoxelModelReady then
                withSignal (signal EnsureVoxelModel) { gameplay with VoxelModelReady = true }
            elif gameplay.GameplayState = Playing && GameplayLogic.shouldStreamVoxelChunks gameplay world then
                withSignal (signal StreamVoxelChunks) gameplay
            else just gameplay

        | TryDestroyBlock ->
            match gameplay.AimPickOpt with
            | Some pick when world.Advancing -> withSignal (signal (DestroyBlock pick)) gameplay
            | Some _ | None -> just gameplay

        | TryPlaceBlock ->
            match gameplay.AimPickOpt with
            | Some pick when world.Advancing -> withSignal (signal (PlaceBlock pick)) gameplay
            | Some _ | None -> just gameplay

    override this.Command (gameplay, command, screen, world) =
        match command with
        | EnsureVoxelModel ->
            match GameplayLogic.createVoxelLevel world with
            | Some (voxelLevel, voxelChunks) ->
                if world.Unaccompanied then GameplayLogic.setInitialCamera voxelLevel.SpawnPosition world
                let portalPair = PortalLogic.pairAtGround voxelLevel.SpawnPosition
                screen.SetGameplay
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
                    world
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
        | DestroyVoxelModel (voxelChunks, placeableBlocks, levelOpt) ->
            GameplayLogic.destroyVoxelModel voxelChunks placeableBlocks levelOpt world
        | DestroyBlock pick ->
            GameplayLogic.tryDestroyBlock pick gameplay screen world
        | PlaceBlock pick ->
            GameplayLogic.tryPlaceBlock pick gameplay screen world
        | StreamVoxelChunks ->
            let struct (gameplay, changed) = GameplayLogic.streamChunksForCurrentPositionDefault gameplay world
            if changed then screen.SetGameplay gameplay world
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

                 for voxelChunk in gameplay.VoxelChunks do
                    Content.entity<VoxelChunkDispatcher> (Simulants.VoxelLevelChunk voxelChunk.ChunkCoord.X voxelChunk.ChunkCoord.Y voxelChunk.ChunkCoord.Z).Name
                        [Entity.Position := voxelChunk.ChunkCenter
                         Entity.Size := voxelChunk.ChunkSize
                         Entity.VoxelChunkCoord := voxelChunk.ChunkCoord
                         Entity.VoxelModelOpt := voxelChunk.VoxelModelOpt
                         Entity.Visible == Option.isSome voxelChunk.VoxelModelOpt
                         Entity.Presence == Exterior
                         Entity.AlwaysRender == false
                         Entity.CastShadow == false
                         Entity.BodyType == Static
                         Entity.BodyShape := voxelChunk.BodyShape
                         Entity.CollisionCategories == "10"
                         Entity.Static == true
                         Entity.MaterialProperties ==
                            { MaterialProperties.empty with
                                RoughnessOpt = ValueSome 0.92f
                                MetallicOpt = ValueSome 0.0f
                                AmbientOcclusionOpt = ValueSome 1.0f
                                EmissionOpt = ValueSome 0.0f
                                ClearCoatOpt = ValueSome 0.0f
                                ClearCoatRoughnessOpt = ValueSome 1.0f }]

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

                 match gameplay.SelectedBlockPreviewPositionOpt, GameplayLogic.tryGetSelectedBlock gameplay with
                 | Some position, Some placeableBlock ->
                    Content.voxel Simulants.SelectedBlockPreview.Name
                        [Entity.Position := position
                         Entity.Size := v3One
                         Entity.Scale := v3Dup 0.35f
                         Entity.VoxelModel := placeableBlock.PreviewModel
                         Entity.Static == true
                         Entity.MaterialProperties ==
                            { MaterialProperties.empty with
                                RoughnessOpt = ValueSome 0.88f
                                MetallicOpt = ValueSome 0.0f
                                AmbientOcclusionOpt = ValueSome 1.0f
                                EmissionOpt = ValueSome 0.0f
                                ClearCoatOpt = ValueSome 0.0f
                                ClearCoatRoughnessOpt = ValueSome 1.0f }]
                 | _, _ -> ()

                 match GameplayLogic.tryGetAimBlockBounds gameplay with
                 | Some bounds ->
                    Content.light3d Simulants.AimBlockHighlightLight.Name
                        [Entity.Position := bounds.Center
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
                        let struct (position, rotation, scale) = GameplayLogic.getAimBlockHighlightFace bounds i
                        Content.staticModel (Simulants.AimBlockHighlightFace i).Name
                            [Entity.Position := position
                             Entity.Rotation := rotation
                             Entity.Size := scale
                             Entity.Scale := scale
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
                 | None -> ()

                 Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 232.0f -144.0f 0.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]]
