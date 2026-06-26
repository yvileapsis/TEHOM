namespace VoxelForge
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu
open VoxelForge

type GameplayState =
    | Playing
    | Quit

type VoxelChunk =
    { ChunkCoord : Vector3i
      ChunkCenter : Vector3
      ChunkSize : Vector3
      BodyShape : BodyShape
      BoxCount : int
      VoxelModel : VoxelModel AssetTag }

type PlaceableBlock =
    { Name : string
      Voxels : struct (Vector3i * Color) array
      PreviewModel : VoxelModel AssetTag }

type VoxelLevel =
    { Bounds : Box3
      VoxelSize : Vector3
      OccupiedVoxels : Dictionary<Vector3i, Color>
      PlaceableBlocks : PlaceableBlock array
      NextRevision : int ref }

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
    | DestroyVoxelModel of VoxelChunk array * PlaceableBlock array
    | DestroyBlock of VoxelAimPick
    | PlaceBlock of VoxelAimPick
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
module GameplayLogic =

    let private minecraftBlockSideVoxels = 16
    let private minecraftLevelSideVoxels = 256
    let private minecraftBlockGridOffsetVoxels = v3i 8 0 8
    let private sourceVoxelSize = v3Dup (1.0f / single minecraftBlockSideVoxels)
    let private levelChunkSizeVoxels = v3i 64 64 64
    let private levelChunkCounts = v3i 4 4 4
    let private levelSize = sourceVoxelSize * single minecraftLevelSideVoxels
    let private levelOffset = v3 0.0f (levelSize.Y * 0.5f) 0.0f
    let private editReach = 6.0f
    let private editEpsilon = 0.01f
    let private placeableBlockSources : struct (string * Image AssetTag) array =
        [|struct ("Grass", Assets.Voxels.GrassBlock)
          struct ("Dirt", Assets.Voxels.DirtBlock)
          struct ("Stone", Assets.Voxels.StoneBlock)
          struct ("Cobblestone", Assets.Voxels.CobblestoneBlock)
          struct ("Sand", Assets.Voxels.SandBlock)
          struct ("Oak Log", Assets.Voxels.OakLogBlock)
          struct ("Oak Planks", Assets.Voxels.OakPlanksBlock)
          struct ("Leaves", Assets.Voxels.LeavesBlock)
          struct ("Glass", Assets.Voxels.GlassBlock)
          struct ("Water", Assets.Voxels.WaterBlock)
          struct ("Brick", Assets.Voxels.BrickBlock)|]

    let private freshRevisionSeed () =
        int (Gen.id64 % uint64 (Int32.MaxValue - 1))

    let private nextRevision (level : VoxelLevel) =
        let revision = level.NextRevision.Value
        level.NextRevision.Value <- if revision = Int32.MaxValue then 1 else inc revision
        revision

    let private divFloor dividend divisor =
        if dividend >= 0
        then dividend / divisor
        else -((-dividend + divisor - 1) / divisor)

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

    let rec private translateBodyShape translation bodyShape =
        let translateTransform transformOpt =
            match transformOpt with
            | Some (transform : Affine) -> Some { transform with Translation = transform.Translation + translation }
            | None -> Some (Affine.makeTranslation translation)
        match bodyShape with
        | BoxShape boxShape -> BoxShape { boxShape with TransformOpt = translateTransform boxShape.TransformOpt }
        | SphereShape sphereShape -> SphereShape { sphereShape with TransformOpt = translateTransform sphereShape.TransformOpt }
        | CapsuleShape capsuleShape -> CapsuleShape { capsuleShape with TransformOpt = translateTransform capsuleShape.TransformOpt }
        | BoxRoundedShape boxRoundedShape -> BoxRoundedShape { boxRoundedShape with TransformOpt = translateTransform boxRoundedShape.TransformOpt }
        | EdgeShape edgeShape -> EdgeShape { edgeShape with TransformOpt = translateTransform edgeShape.TransformOpt }
        | ContourShape contourShape -> ContourShape { contourShape with TransformOpt = translateTransform contourShape.TransformOpt }
        | PointsShape pointsShape -> PointsShape { pointsShape with TransformOpt = translateTransform pointsShape.TransformOpt }
        | GeometryShape geometryShape -> GeometryShape { geometryShape with TransformOpt = translateTransform geometryShape.TransformOpt }
        | StaticModelShape staticModelShape -> StaticModelShape { staticModelShape with TransformOpt = translateTransform staticModelShape.TransformOpt }
        | StaticModelSurfaceShape staticModelSurfaceShape -> StaticModelSurfaceShape { staticModelSurfaceShape with TransformOpt = translateTransform staticModelSurfaceShape.TransformOpt }
        | TerrainShape terrainShape -> TerrainShape { terrainShape with TransformOpt = translateTransform terrainShape.TransformOpt }
        | BodyShapes bodyShapes -> BodyShapes (bodyShapes |> List.map (translateBodyShape translation))
        | EmptyShape -> EmptyShape

    let private chunkAssetTag (chunkCoord : Vector3i) revision =
        Assets.Voxels.MinecraftLevelChunkRevision chunkCoord.X chunkCoord.Y chunkCoord.Z revision

    let private sourceCoordToChunkCoord (coord : Vector3i) =
        v3i
            (coord.X / levelChunkSizeVoxels.X)
            (coord.Y / levelChunkSizeVoxels.Y)
            (coord.Z / levelChunkSizeVoxels.Z)

    let private blockStartCoord (blockCoord : Vector3i) =
        v3i
            (minecraftBlockGridOffsetVoxels.X + blockCoord.X * minecraftBlockSideVoxels)
            (minecraftBlockGridOffsetVoxels.Y + blockCoord.Y * minecraftBlockSideVoxels)
            (minecraftBlockGridOffsetVoxels.Z + blockCoord.Z * minecraftBlockSideVoxels)

    let private sourceCoordToBlockCoord (coord : Vector3i) =
        v3i
            (divFloor (coord.X - minecraftBlockGridOffsetVoxels.X) minecraftBlockSideVoxels)
            (divFloor (coord.Y - minecraftBlockGridOffsetVoxels.Y) minecraftBlockSideVoxels)
            (divFloor (coord.Z - minecraftBlockGridOffsetVoxels.Z) minecraftBlockSideVoxels)

    let private isSourceCoordInBounds (coord : Vector3i) =
        coord.X >= 0 && coord.X < minecraftLevelSideVoxels &&
        coord.Y >= 0 && coord.Y < minecraftLevelSideVoxels &&
        coord.Z >= 0 && coord.Z < minecraftLevelSideVoxels

    let private isBlockCoordInBounds (coord : Vector3i) =
        let start = blockStartCoord coord
        start.X >= 0 && start.X + minecraftBlockSideVoxels <= minecraftLevelSideVoxels &&
        start.Y >= 0 && start.Y + minecraftBlockSideVoxels <= minecraftLevelSideVoxels &&
        start.Z >= 0 && start.Z + minecraftBlockSideVoxels <= minecraftLevelSideVoxels

    let private isChunkCoordInBounds (coord : Vector3i) =
        coord.X >= 0 && coord.X < levelChunkCounts.X &&
        coord.Y >= 0 && coord.Y < levelChunkCounts.Y &&
        coord.Z >= 0 && coord.Z < levelChunkCounts.Z

    let private tryWorldToSourceCoord (level : VoxelLevel) (position : Vector3) =
        let local = position - levelOffset
        let origin = level.Bounds.Min
        let coord =
            v3i
                (int (floor ((local.X - origin.X) / level.VoxelSize.X)))
                (int (floor ((local.Y - origin.Y) / level.VoxelSize.Y)))
                (int (floor ((local.Z - origin.Z) / level.VoxelSize.Z)))
        if isSourceCoordInBounds coord then Some coord else None

    let private tryWorldToBlockCoord (level : VoxelLevel) (position : Vector3) =
        match tryWorldToSourceCoord level position with
        | Some coord ->
            let blockCoord = sourceCoordToBlockCoord coord
            if isBlockCoordInBounds blockCoord then Some blockCoord else None
        | None -> None

    let private blockBounds (level : VoxelLevel) (blockCoord : Vector3i) =
        let start = blockStartCoord blockCoord
        let min =
            level.Bounds.Min + levelOffset +
            v3
                (single start.X * level.VoxelSize.X)
                (single start.Y * level.VoxelSize.Y)
                (single start.Z * level.VoxelSize.Z)
        box3 min v3One

    let private boxesIntersect (box : Box3) (box2 : Box3) =
        box.Min.X < box2.Max.X && box.Max.X > box2.Min.X &&
        box.Min.Y < box2.Max.Y && box.Max.Y > box2.Min.Y &&
        box.Min.Z < box2.Max.Z && box.Max.Z > box2.Min.Z

    let private blockIntersectsPlayer (level : VoxelLevel) (blockCoord : Vector3i) (world : World) =
        if Simulants.GameplayPlayer.GetExists world then
            let position = Simulants.GameplayPlayer.GetPosition world
            let playerBounds = box3 (position + v3 -0.45f 0.0f -0.45f) (v3 0.9f 1.9f 0.9f)
            boxesIntersect (blockBounds level blockCoord) playerBounds
        else false

    let private blockContainsVoxel (level : VoxelLevel) (blockCoord : Vector3i) =
        let start = blockStartCoord blockCoord
        let mutable contains = false
        let mutable y = 0
        while not contains && y < minecraftBlockSideVoxels do
            let mutable z = 0
            while not contains && z < minecraftBlockSideVoxels do
                let mutable x = 0
                while not contains && x < minecraftBlockSideVoxels do
                    contains <- level.OccupiedVoxels.ContainsKey (v3i (start.X + x) (start.Y + y) (start.Z + z))
                    x <- inc x
                z <- inc z
            y <- inc y
        contains

    let private blockIsEmpty (level : VoxelLevel) (blockCoord : Vector3i) =
        not (blockContainsVoxel level blockCoord)

    let private removeBlock (level : VoxelLevel) (blockCoord : Vector3i) =
        let start = blockStartCoord blockCoord
        for y in 0 .. dec minecraftBlockSideVoxels do
            for z in 0 .. dec minecraftBlockSideVoxels do
                for x in 0 .. dec minecraftBlockSideVoxels do
                    level.OccupiedVoxels.Remove (v3i (start.X + x) (start.Y + y) (start.Z + z)) |> ignore<bool>

    let private placeBlock (level : VoxelLevel) (placeableBlock : PlaceableBlock) (blockCoord : Vector3i) =
        let start = blockStartCoord blockCoord
        for struct (localCoord, albedo) in placeableBlock.Voxels do
            let coord = v3i (start.X + localCoord.X) (start.Y + localCoord.Y) (start.Z + localCoord.Z)
            if isSourceCoordInBounds coord then
                level.OccupiedVoxels[coord] <- albedo

    let private affectedChunksForBlock (blockCoord : Vector3i) =
        let affected = HashSet<Vector3i> (HashIdentity.Structural)
        let start = blockStartCoord blockCoord
        let finish =
            start +
            v3i
                (minecraftBlockSideVoxels - 1)
                (minecraftBlockSideVoxels - 1)
                (minecraftBlockSideVoxels - 1)
        let minCoord =
            v3i
                (max 0 (start.X - 1))
                (max 0 (start.Y - 1))
                (max 0 (start.Z - 1))
        let maxCoord =
            v3i
                (min (minecraftLevelSideVoxels - 1) (finish.X + 1))
                (min (minecraftLevelSideVoxels - 1) (finish.Y + 1))
                (min (minecraftLevelSideVoxels - 1) (finish.Z + 1))
        let minChunkCoord = sourceCoordToChunkCoord minCoord
        let maxChunkCoord = sourceCoordToChunkCoord maxCoord
        for z in minChunkCoord.Z .. maxChunkCoord.Z do
            for y in minChunkCoord.Y .. maxChunkCoord.Y do
                for x in minChunkCoord.X .. maxChunkCoord.X do
                    let coord = v3i x y z
                    if isChunkCoordInBounds coord then affected.Add coord |> ignore<bool>
        affected |> Seq.toArray

    let private sortVoxelChunks chunks =
        chunks
        |> Seq.sortBy (fun (chunk : VoxelChunk) -> struct (chunk.ChunkCoord.Z, chunk.ChunkCoord.Y, chunk.ChunkCoord.X))
        |> Seq.toArray

    let private rebuildChunk (level : VoxelLevel) (chunkCoord : Vector3i) (world : World) =
        match VoxelBake.chunkModelFromOccupied levelChunkSizeVoxels level.Bounds level.VoxelSize level.OccupiedVoxels chunkCoord with
        | Some struct (renderCenter, voxelModelDescriptor) ->
            let struct (bodyCenter, bodyShape, boxCount) =
                match VoxelBake.chunkBodyShapeFromOccupied levelChunkSizeVoxels level.Bounds level.VoxelSize level.OccupiedVoxels chunkCoord with
                | Some bodyShape -> bodyShape
                | None -> struct (renderCenter, EmptyShape, 0)
            let revision = nextRevision level
            let voxelModel = chunkAssetTag chunkCoord revision
            World.createUserDefinedVoxelModel voxelModelDescriptor voxelModel world
            Some
                { ChunkCoord = chunkCoord
                  ChunkCenter = renderCenter + levelOffset
                  ChunkSize = voxelModelDescriptor.Bounds.Size
                  BodyShape = translateBodyShape (bodyCenter - renderCenter) bodyShape
                  BoxCount = boxCount
                  VoxelModel = voxelModel }
        | None -> None

    let private rebuildChunks (chunkCoords : Vector3i seq) (gameplay : Gameplay) (world : World) =
        match gameplay.VoxelLevelOpt with
        | Some level ->
            let chunks = Dictionary<Vector3i, VoxelChunk> (HashIdentity.Structural)
            let chunksToDestroy = ResizeArray<VoxelChunk> ()
            for chunk in gameplay.VoxelChunks do
                chunks[chunk.ChunkCoord] <- chunk
            for chunkCoord in chunkCoords do
                let oldChunkOpt =
                    match chunks.TryGetValue chunkCoord with
                    | (true, chunk) -> Some chunk
                    | (false, _) -> None
                match oldChunkOpt with
                | Some oldChunk -> chunksToDestroy.Add oldChunk
                | None -> ()
                match rebuildChunk level chunkCoord world with
                | Some chunk -> chunks[chunkCoord] <- chunk
                | None -> chunks.Remove chunkCoord |> ignore<bool>
            struct ({ gameplay with VoxelChunks = sortVoxelChunks chunks.Values }, chunksToDestroy.ToArray ())
        | None -> struct (gameplay, [||])

    let private allChunkCoords =
        [|for z in 0 .. dec levelChunkCounts.Z do
            for y in 0 .. dec levelChunkCounts.Y do
                for x in 0 .. dec levelChunkCounts.X do
                    v3i x y z|]

    let private createPlaceableBlocks (world : World) =
        [|for i in 0 .. dec placeableBlockSources.Length do
            let struct (name, image) = placeableBlockSources[i]
            match VoxelBake.tryBakeSliceAtlasVolume image sourceVoxelSize with
            | Some volume ->
                let previewModel = Assets.Voxels.PlaceableBlockPreview i
                World.createUserDefinedVoxelModel volume.VoxelModel previewModel world
                yield
                    { Name = name
                      Voxels = volume.OccupiedVoxels
                      PreviewModel = previewModel }
            | None ->
                Log.warnOnce ("VoxelForge could not bake placeable block '" + name + "'.")|]

    let tryPickForward (gameplay : Gameplay) (world : World) =
        match gameplay.VoxelLevelOpt with
        | Some level ->
            let pickRay = ray3 world.Eye3dCenter (world.Eye3dRotation.Forward * editReach)
            World.rayCastBodies3d pickRay 2UL 2UL false world
            |> Array.tryHead
            |> Option.bind (fun (intersection : BodyIntersection) ->
                let normal = if intersection.Normal.LengthSquared () > 0.0f then intersection.Normal.Normalized else v3Up
                match tryWorldToBlockCoord level (intersection.Position - normal * editEpsilon) with
                | Some destroyBlockCoord ->
                    Some
                        { Position = intersection.Position
                          Normal = normal
                          DestroyBlockCoord = destroyBlockCoord
                          PlaceBlockCoordOpt = tryWorldToBlockCoord level (intersection.Position + normal * editEpsilon) }
                | None -> None)
        | None -> None

    let createVoxelLevel (world : World) =
        match VoxelBake.tryBakeSliceAtlasVolume Assets.Voxels.Minecraft sourceVoxelSize with
        | Some minecraftLevel ->
            let placeableBlocks = createPlaceableBlocks world
            let level =
                { Bounds = minecraftLevel.VoxelModel.Bounds
                  VoxelSize = minecraftLevel.VoxelModel.VoxelSize
                  OccupiedVoxels = VoxelBake.occupiedDictionary minecraftLevel
                  PlaceableBlocks = placeableBlocks
                  NextRevision = ref (freshRevisionSeed ()) }
            let voxelChunks =
                allChunkCoords
                |> Array.choose (fun chunkCoord -> rebuildChunk level chunkCoord world)
                |> sortVoxelChunks
            let bodyShapeCount = voxelChunks |> Array.sumBy (fun (voxelChunk : VoxelChunk) -> voxelChunk.BoxCount)
            Log.infoOnce ("VoxelForge generated " + scstring voxelChunks.Length + " voxel chunks with " + scstring bodyShapeCount + " merged physics boxes.")
            Some (level, voxelChunks)
        | None ->
            Log.warnOnce "VoxelForge could not bake the minecraft voxel slice atlas."
            None

    let private destroyVoxelChunks (voxelChunks : VoxelChunk array) (world : World) =
        for chunk in voxelChunks do
            World.destroyUserDefinedVoxelModel chunk.VoxelModel world

    let destroyVoxelModel (voxelChunks : VoxelChunk array) (placeableBlocks : PlaceableBlock array) (world : World) =
        destroyVoxelChunks voxelChunks world
        for placeableBlock in placeableBlocks do
            World.destroyUserDefinedVoxelModel placeableBlock.PreviewModel world
        for z in 0 .. dec levelChunkCounts.Z do
            for y in 0 .. dec levelChunkCounts.Y do
                for x in 0 .. dec levelChunkCounts.X do
                    World.destroyUserDefinedVoxelModel (Assets.Voxels.MinecraftLevelChunk x y z) world

    let tryDestroyBlock (pick : VoxelAimPick) (gameplay : Gameplay) (screen : Screen) (world : World) =
        match gameplay.VoxelLevelOpt with
        | Some level when world.Advancing && blockContainsVoxel level pick.DestroyBlockCoord ->
            removeBlock level pick.DestroyBlockCoord
            let struct (rebuilt, chunksToDestroy) = rebuildChunks (affectedChunksForBlock pick.DestroyBlockCoord) { gameplay with AimPickOpt = None } world
            screen.SetGameplay
                { gameplay with
                    VoxelChunks = rebuilt.VoxelChunks
                    AimPickOpt = None }
                world
            destroyVoxelChunks chunksToDestroy world
        | Some _ | None -> ()

    let tryPlaceBlock (pick : VoxelAimPick) (gameplay : Gameplay) (screen : Screen) (world : World) =
        match gameplay.VoxelLevelOpt, pick.PlaceBlockCoordOpt, tryGetSelectedBlock gameplay with
        | (Some level, Some placeBlockCoord, Some placeableBlock)
            when world.Advancing &&
                 isBlockCoordInBounds placeBlockCoord &&
                 blockIsEmpty level placeBlockCoord &&
                 not (blockIntersectsPlayer level placeBlockCoord world) ->
            placeBlock level placeableBlock placeBlockCoord
            let struct (rebuilt, chunksToDestroy) = rebuildChunks (affectedChunksForBlock placeBlockCoord) { gameplay with AimPickOpt = None } world
            screen.SetGameplay
                { gameplay with
                    VoxelChunks = rebuilt.VoxelChunks
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

    let setInitialCamera (world : World) =
        let eyeCenter = v3 12.0f 12.0f 14.0f
        let eyeTarget = v3 0.0f 5.0f 0.0f
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
            VoxelChunks = [||] }

    override this.UntruncateModel (current, incoming) =
        { incoming with
            VoxelLevelOpt = current.VoxelLevelOpt
            VoxelChunks = current.VoxelChunks }

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
            withSignal (signal EnsureVoxelModel) gameplay

        | FinishQuitting ->
            let placeableBlocks =
                match gameplay.VoxelLevelOpt with
                | Some level -> level.PlaceableBlocks
                | None -> [||]
            withSignal (signal (DestroyVoxelModel (gameplay.VoxelChunks, placeableBlocks))) Gameplay.empty

        | TimeUpdate ->
            let gameplay =
                { gameplay with
                    GameplayTime = gameplay.GameplayTime + world.GameDelta.Updates
                    AimPickOpt = GameplayLogic.tryPickForward gameplay world }
                |> fun gameplay -> GameplayLogic.updateSelectedBlockFromInput gameplay world
                |> fun gameplay -> GameplayLogic.updateSelectedBlockPreviewPosition gameplay world
            if gameplay.GameplayState = Playing && not gameplay.VoxelModelReady then
                withSignal (signal EnsureVoxelModel) { gameplay with VoxelModelReady = true }
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
                screen.SetGameplay
                    { gameplay with
                        GameplayState = Playing
                        VoxelModelReady = true
                        VoxelLevelOpt = Some voxelLevel
                        VoxelChunks = voxelChunks
                        AimPickOpt = None
                        SelectedBlockIndex = 0
                        SelectedBlockPreviewPositionOpt = None }
                    world
            | None ->
                screen.SetGameplay
                    { gameplay with
                        GameplayState = Playing
                        VoxelModelReady = true
                        VoxelLevelOpt = None
                        VoxelChunks = [||]
                        AimPickOpt = None
                        SelectedBlockIndex = 0
                        SelectedBlockPreviewPositionOpt = None }
                    world
            if world.Unaccompanied then GameplayLogic.setInitialCamera world
        | DestroyVoxelModel (voxelChunks, placeableBlocks) ->
            GameplayLogic.destroyVoxelModel voxelChunks placeableBlocks world
        | DestroyBlock pick ->
            GameplayLogic.tryDestroyBlock pick gameplay screen world
        | PlaceBlock pick ->
            GameplayLogic.tryPlaceBlock pick gameplay screen world
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
            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []

                [for voxelChunk in gameplay.VoxelChunks do
                    Content.voxel (Simulants.VoxelLevelChunk voxelChunk.ChunkCoord.X voxelChunk.ChunkCoord.Y voxelChunk.ChunkCoord.Z).Name
                        [Entity.FacetNames == Set.ofList [nameof VoxelFacet; nameof RigidBodyFacet]
                         Entity.Position := voxelChunk.ChunkCenter
                         Entity.Size := voxelChunk.ChunkSize
                         Entity.VoxelModel := voxelChunk.VoxelModel
                         Entity.Presence == Omnipresent
                         Entity.AlwaysRender == true
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
                    [Entity.Position == v3 0.0f 18.0f 0.0f]
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

                 match gameplay.AimPickOpt with
                 | Some pick ->
                    Content.staticModel Simulants.RayPickMarker.Name
                        [Entity.Position := pick.Position + pick.Normal * 0.08f
                         Entity.Size := v3Dup 0.08f
                         Entity.Scale := v3Dup 0.08f
                         Entity.StaticModel == Assets.Default.BallModel
                         Entity.MaterialProperties == { MaterialProperties.empty with AlbedoOpt = ValueSome Color.Cyan; EmissionOpt = ValueSome 0.35f }]
                 | None -> ()

                 Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 232.0f -144.0f 0.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]]
