namespace VoxelForge
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

type VoxelMaterialKind =
    | Grass
    | Dirt
    | Stone
    | Sand
    | Wood
    | Leaves
    | Glass
    | Water
    | Lava
    | Ore
    | Brick
    | Crafted

type VoxelCell =
    { Albedo : Color
      Solid : bool
      Material : VoxelMaterialKind }

type VoxelEdit =
    | Removed
    | Placed of VoxelCell

type VoxelChunk =
    { ChunkCoord : Vector3i
      ChunkCenter : Vector3
      ChunkSize : Vector3
      BodyShape : BodyShape
      BoxCount : int
      OcclusionBoundsOpt : Box3 option
      SolidBlockCoords : Vector3i array
      SplatCount : int
      VoxelModelOpt : VoxelModel AssetTag option
      OpaqueBlockCoords : Vector3i array
      OpaqueOccluderBoxes : Box3 array
      OpaqueFaceMask : int
      FullOpaqueChunk : bool }

type VoxelChunkBuild =
    { ChunkCoord : Vector3i
      ChunkCenter : Vector3
      ChunkSize : Vector3
      VoxelModelDescriptor : VoxelModelDescriptor
      BodyShape : BodyShape
      BoxCount : int
      OcclusionBoundsOpt : Box3 option
      SolidBlockCoords : Vector3i array
      SplatCount : int
      OpaqueBlockCoords : Vector3i array
      OpaqueOccluderBoxes : Box3 array
      OpaqueFaceMask : int
      FullOpaqueChunk : bool }

[<Struct>]
type VoxelChunkStatic =
    { ChunkCoord : Vector3i
      ChunkCenter : Vector3
      ChunkSize : Vector3
      Bounds : Box3
      HasBuild : bool
      HasRenderable : bool
      HasPhysics : bool
      HasOcclusionBounds : bool
      OcclusionBounds : Box3
      SplatCount : int
      OpaqueOccluderBoxes : Box3 array
      OpaqueFaceMask : int
      FullOpaqueChunk : bool
      EditRevision : int }

type VoxelChunkManifest =
    { ChunkCounts : Vector3i
      ChunkStatics : VoxelChunkStatic array
      DesiredStamps : int array
      VisibleStamps : int array
      LoadedStamps : int array
      EmptyFlags : bool array
      EditedFlags : bool array
      mutable DesiredStamp : int
      mutable VisibleStamp : int
      mutable LoadedStamp : int }

type VoxelBlockTemplate =
    { Name : string
      Material : VoxelMaterialKind
      Solid : bool
      Voxels : struct (Vector3i * VoxelCell) array
      Cells : Dictionary<Vector3i, VoxelCell> }

type VoxelGeneratedTemplates =
    { Grass : VoxelBlockTemplate
      Dirt : VoxelBlockTemplate
      Stone : VoxelBlockTemplate
      Sand : VoxelBlockTemplate
      Log : VoxelBlockTemplate
      Leaves : VoxelBlockTemplate
      Water : VoxelBlockTemplate
      Ore : VoxelBlockTemplate
      Lava : VoxelBlockTemplate }

type VoxelGeneration =
    { Seed : int
      SeaLevelBlocks : int
      LavaLevelBlocks : int
      TerrainScale : single
      MountainStrength : single
      CaveThreshold : single
      OreRate : single
      TreeRate : single
      Templates : VoxelGeneratedTemplates }

type VoxelEditSnapshot =
    { Revision : int
      BlockEditsOpt : Dictionary<Vector3i, VoxelBlockTemplate option> option
      EditsOpt : Dictionary<Vector3i, VoxelEdit> option }

type PlaceableBlock =
    { Name : string
      Voxels : struct (Vector3i * VoxelCell) array
      Template : VoxelBlockTemplate
      PreviewModel : VoxelModel AssetTag }

type VoxelLevel =
    { Bounds : Box3
      VoxelSize : Vector3
      WorldSizeBlocks : Vector3i
      ActiveBlockOrigin : Vector3i
      SourceSizeVoxels : Vector3i
      ChunkSizeVoxels : Vector3i
      ChunkCounts : Vector3i
      BlockSideVoxels : int
      BlockGridOffsetVoxels : Vector3i
      LevelOffset : Vector3
      GenerationOpt : VoxelGeneration option
      GeneratedBlockTemplateCache : ConcurrentDictionary<Vector3i, VoxelBlockTemplate option>
      SourceVoxels : Dictionary<Vector3i, VoxelCell>
      BlockEdits : Dictionary<Vector3i, VoxelBlockTemplate option>
      Edits : Dictionary<Vector3i, VoxelEdit>
      EditRevision : int ref
      PlaceableBlocks : PlaceableBlock array
      SpawnPosition : Vector3
      ChunkManifest : VoxelChunkManifest
      NextRevision : int ref }

type WorldGenSettings =
    { Seed : int
      WorldSizeBlocks : Vector3i
      ActiveBlockOrigin : Vector3i
      ChunkCounts : Vector3i
      ChunkSizeVoxels : Vector3i
      BlockSideVoxels : int
      BlockGridOffsetVoxels : Vector3i
      VoxelSize : Vector3
      SeaLevelBlocks : int
      LavaLevelBlocks : int
      TerrainScale : single
      MountainStrength : single
      CaveThreshold : single
      OreRate : single
      TreeRate : single
      ChunksPerUpdate : int }

type GeneratedWorldStats =
    { SourceVoxelCount : int
      SolidVoxelCount : int
      FluidVoxelCount : int
      TreeCount : int
      OreVoxelCount : int
      ChunkCount : int
      BodyShapeCount : int }

type GeneratedWorldPackage =
    { Level : VoxelLevel
      Chunks : VoxelChunk array
      SpawnPosition : Vector3
      Stats : GeneratedWorldStats }

[<RequireQualifiedAccess>]
module WorldGenSettings =

    let defaultSettings =
        { Seed = 8675309
          WorldSizeBlocks = v3i 1024 16 1024
          ActiveBlockOrigin = v3i -1 0 -1
          ChunkCounts = v3i 256 4 256
          ChunkSizeVoxels = v3i 64 64 64
          BlockSideVoxels = 16
          BlockGridOffsetVoxels = v3iZero
          VoxelSize = v3Dup (1.0f / 16.0f)
          SeaLevelBlocks = 6
          LavaLevelBlocks = 2
          TerrainScale = 0.135f
          MountainStrength = 0.85f
          CaveThreshold = 0.70f
          OreRate = 0.035f
          TreeRate = 0.025f
          ChunksPerUpdate = 32 }

[<RequireQualifiedAccess>]
module VoxelWorld =

    let chunkCount (chunkCounts : Vector3i) =
        max 0 (chunkCounts.X * chunkCounts.Y * chunkCounts.Z)

    let chunkCoordToIndexUnchecked (chunkCounts : Vector3i) (chunkCoord : Vector3i) =
        chunkCoord.X + chunkCoord.Y * chunkCounts.X + chunkCoord.Z * chunkCounts.X * chunkCounts.Y

    let chunkIndexToCoord (chunkCounts : Vector3i) chunkIndex =
        let xy = chunkCounts.X * chunkCounts.Y
        let z = chunkIndex / xy
        let y = (chunkIndex - z * xy) / chunkCounts.X
        let x = chunkIndex - z * xy - y * chunkCounts.X
        v3i x y z

    let tryChunkCoordToIndex (chunkCounts : Vector3i) (chunkCoord : Vector3i) =
        if  chunkCoord.X >= 0 && chunkCoord.X < chunkCounts.X &&
            chunkCoord.Y >= 0 && chunkCoord.Y < chunkCounts.Y &&
            chunkCoord.Z >= 0 && chunkCoord.Z < chunkCounts.Z then
            ValueSome (chunkCoordToIndexUnchecked chunkCounts chunkCoord)
        else ValueNone

    let private chunkFullBounds (bounds : Box3) (levelOffset : Vector3) (voxelSize : Vector3) (chunkSizeVoxels : Vector3i) (chunkCoord : Vector3i) =
        let min =
            bounds.Min + levelOffset +
            v3
                (single (chunkCoord.X * chunkSizeVoxels.X) * voxelSize.X)
                (single (chunkCoord.Y * chunkSizeVoxels.Y) * voxelSize.Y)
                (single (chunkCoord.Z * chunkSizeVoxels.Z) * voxelSize.Z)
        let size =
            v3
                (single chunkSizeVoxels.X * voxelSize.X)
                (single chunkSizeVoxels.Y * voxelSize.Y)
                (single chunkSizeVoxels.Z * voxelSize.Z)
        box3 min size

    let createChunkManifest (chunkCounts : Vector3i) (bounds : Box3) (levelOffset : Vector3) (voxelSize : Vector3) (chunkSizeVoxels : Vector3i) =
        let count = chunkCount chunkCounts
        let chunkStatics = Array.zeroCreate<VoxelChunkStatic> count
        let emptyOccluderBoxes = Array.empty<Box3>
        for z in 0 .. dec chunkCounts.Z do
            for y in 0 .. dec chunkCounts.Y do
                for x in 0 .. dec chunkCounts.X do
                    let chunkCoord = v3i x y z
                    let chunkIndex = chunkCoordToIndexUnchecked chunkCounts chunkCoord
                    let bounds = chunkFullBounds bounds levelOffset voxelSize chunkSizeVoxels chunkCoord
                    chunkStatics[chunkIndex] <-
                        { ChunkCoord = chunkCoord
                          ChunkCenter = bounds.Center
                          ChunkSize = bounds.Size
                          Bounds = bounds
                          HasBuild = false
                          HasRenderable = false
                          HasPhysics = false
                          HasOcclusionBounds = false
                          OcclusionBounds = Unchecked.defaultof<Box3>
                          SplatCount = 0
                          OpaqueOccluderBoxes = emptyOccluderBoxes
                          OpaqueFaceMask = 0
                          FullOpaqueChunk = false
                          EditRevision = 0 }
        { ChunkCounts = chunkCounts
          ChunkStatics = chunkStatics
          DesiredStamps = Array.zeroCreate<int> count
          VisibleStamps = Array.zeroCreate<int> count
          LoadedStamps = Array.zeroCreate<int> count
          EmptyFlags = Array.zeroCreate<bool> count
          EditedFlags = Array.zeroCreate<bool> count
          DesiredStamp = 0
          VisibleStamp = 0
          LoadedStamp = 0 }

    let markChunkManifestDesired (manifest : VoxelChunkManifest) chunkIndex =
        manifest.DesiredStamps[chunkIndex] <- manifest.DesiredStamp

    let isChunkManifestDesired (manifest : VoxelChunkManifest) chunkIndex =
        manifest.DesiredStamps[chunkIndex] = manifest.DesiredStamp

    let beginChunkManifestDesiredPass (manifest : VoxelChunkManifest) =
        manifest.DesiredStamp <- manifest.DesiredStamp + 1
        if manifest.DesiredStamp = Int32.MaxValue then
            Array.Clear manifest.DesiredStamps
            manifest.DesiredStamp <- 1
        manifest.DesiredStamp

    let markChunkManifestVisible (manifest : VoxelChunkManifest) chunkIndex =
        manifest.VisibleStamps[chunkIndex] <- manifest.VisibleStamp

    let isChunkManifestVisible (manifest : VoxelChunkManifest) chunkIndex =
        manifest.VisibleStamps[chunkIndex] = manifest.VisibleStamp

    let beginChunkManifestVisiblePass (manifest : VoxelChunkManifest) =
        manifest.VisibleStamp <- manifest.VisibleStamp + 1
        if manifest.VisibleStamp = Int32.MaxValue then
            Array.Clear manifest.VisibleStamps
            manifest.VisibleStamp <- 1
        manifest.VisibleStamp

    let markChunkManifestLoaded (manifest : VoxelChunkManifest) chunkIndex =
        manifest.LoadedStamps[chunkIndex] <- manifest.LoadedStamp

    let isChunkManifestLoaded (manifest : VoxelChunkManifest) chunkIndex =
        manifest.LoadedStamps[chunkIndex] = manifest.LoadedStamp

    let beginChunkManifestLoadedPass (manifest : VoxelChunkManifest) =
        manifest.LoadedStamp <- manifest.LoadedStamp + 1
        if manifest.LoadedStamp = Int32.MaxValue then
            Array.Clear manifest.LoadedStamps
            manifest.LoadedStamp <- 1
        manifest.LoadedStamp

    let tryGetChunkStatic (level : VoxelLevel) (chunkCoord : Vector3i) =
        match tryChunkCoordToIndex level.ChunkManifest.ChunkCounts chunkCoord with
        | ValueSome chunkIndex -> ValueSome level.ChunkManifest.ChunkStatics[chunkIndex]
        | ValueNone -> ValueNone

    let updateChunkManifestFromBuild revision (level : VoxelLevel) (chunkBuild : VoxelChunkBuild) =
        match tryChunkCoordToIndex level.ChunkManifest.ChunkCounts chunkBuild.ChunkCoord with
        | ValueSome chunkIndex ->
            let bounds = box3 (chunkBuild.ChunkCenter - chunkBuild.ChunkSize * 0.5f) chunkBuild.ChunkSize
            let hasOcclusionBounds, occlusionBounds =
                match chunkBuild.OcclusionBoundsOpt with
                | Some bounds -> true, bounds
                | None -> false, Unchecked.defaultof<Box3>
            level.ChunkManifest.ChunkStatics[chunkIndex] <-
                { ChunkCoord = chunkBuild.ChunkCoord
                  ChunkCenter = chunkBuild.ChunkCenter
                  ChunkSize = chunkBuild.ChunkSize
                  Bounds = bounds
                  HasBuild = true
                  HasRenderable = chunkBuild.SplatCount > 0
                  HasPhysics = chunkBuild.BoxCount > 0
                  HasOcclusionBounds = hasOcclusionBounds
                  OcclusionBounds = occlusionBounds
                  SplatCount = chunkBuild.SplatCount
                  OpaqueOccluderBoxes = chunkBuild.OpaqueOccluderBoxes
                  OpaqueFaceMask = chunkBuild.OpaqueFaceMask
                  FullOpaqueChunk = chunkBuild.FullOpaqueChunk
                  EditRevision = revision }
            level.ChunkManifest.EmptyFlags[chunkIndex] <- false
            level.ChunkManifest.EditedFlags[chunkIndex] <- false
        | ValueNone -> ()

    let updateChunkManifestFromChunk revision (level : VoxelLevel) (chunk : VoxelChunk) =
        match tryChunkCoordToIndex level.ChunkManifest.ChunkCounts chunk.ChunkCoord with
        | ValueSome chunkIndex ->
            let bounds = box3 (chunk.ChunkCenter - chunk.ChunkSize * 0.5f) chunk.ChunkSize
            let hasOcclusionBounds, occlusionBounds =
                match chunk.OcclusionBoundsOpt with
                | Some bounds -> true, bounds
                | None -> false, Unchecked.defaultof<Box3>
            level.ChunkManifest.ChunkStatics[chunkIndex] <-
                { ChunkCoord = chunk.ChunkCoord
                  ChunkCenter = chunk.ChunkCenter
                  ChunkSize = chunk.ChunkSize
                  Bounds = bounds
                  HasBuild = true
                  HasRenderable = chunk.SplatCount > 0 && Option.isSome chunk.VoxelModelOpt
                  HasPhysics = chunk.BoxCount > 0
                  HasOcclusionBounds = hasOcclusionBounds
                  OcclusionBounds = occlusionBounds
                  SplatCount = chunk.SplatCount
                  OpaqueOccluderBoxes = chunk.OpaqueOccluderBoxes
                  OpaqueFaceMask = chunk.OpaqueFaceMask
                  FullOpaqueChunk = chunk.FullOpaqueChunk
                  EditRevision = revision }
            level.ChunkManifest.EmptyFlags[chunkIndex] <- false
            level.ChunkManifest.EditedFlags[chunkIndex] <- false
        | ValueNone -> ()

    let markChunkManifestEmpty revision (level : VoxelLevel) (chunkCoord : Vector3i) =
        match tryChunkCoordToIndex level.ChunkManifest.ChunkCounts chunkCoord with
        | ValueSome chunkIndex ->
            let current = level.ChunkManifest.ChunkStatics[chunkIndex]
            level.ChunkManifest.ChunkStatics[chunkIndex] <-
                { current with
                    HasBuild = true
                    HasRenderable = false
                    HasPhysics = false
                    HasOcclusionBounds = false
                    OcclusionBounds = Unchecked.defaultof<Box3>
                    SplatCount = 0
                    OpaqueOccluderBoxes = Array.empty
                    OpaqueFaceMask = 0
                    FullOpaqueChunk = false
                    EditRevision = revision }
            level.ChunkManifest.EmptyFlags[chunkIndex] <- true
            level.ChunkManifest.EditedFlags[chunkIndex] <- false
        | ValueNone -> ()

    let levelSourceSizeVoxels (settings : WorldGenSettings) =
        v3i
            (settings.ChunkCounts.X * settings.ChunkSizeVoxels.X)
            (settings.ChunkCounts.Y * settings.ChunkSizeVoxels.Y)
            (settings.ChunkCounts.Z * settings.ChunkSizeVoxels.Z)

    let macroBlockCounts (settings : WorldGenSettings) =
        let sourceSize = levelSourceSizeVoxels settings
        let side = max 1 settings.BlockSideVoxels
        v3i
            (max 1 ((sourceSize.X - settings.BlockGridOffsetVoxels.X * 2) / side))
            (max 1 ((sourceSize.Y - settings.BlockGridOffsetVoxels.Y) / side))
            (max 1 ((sourceSize.Z - settings.BlockGridOffsetVoxels.Z * 2) / side))

    let normalizedWorldSizeBlocks (settings : WorldGenSettings) =
        let macroCounts = macroBlockCounts settings
        v3i
            (max macroCounts.X settings.WorldSizeBlocks.X)
            (max macroCounts.Y settings.WorldSizeBlocks.Y)
            (max macroCounts.Z settings.WorldSizeBlocks.Z)

    let activeBlockOrigin (settings : WorldGenSettings) =
        let macroCounts = macroBlockCounts settings
        let worldSizeBlocks = normalizedWorldSizeBlocks settings
        let autoOrigin =
            v3i
                (max 0 ((worldSizeBlocks.X - macroCounts.X) / 2))
                (max 0 ((worldSizeBlocks.Y - macroCounts.Y) / 2))
                (max 0 ((worldSizeBlocks.Z - macroCounts.Z) / 2))
        let requested = settings.ActiveBlockOrigin
        let origin =
            v3i
                (if requested.X >= 0 then requested.X else autoOrigin.X)
                (if requested.Y >= 0 then requested.Y else autoOrigin.Y)
                (if requested.Z >= 0 then requested.Z else autoOrigin.Z)
        v3i
            (Math.Clamp (origin.X, 0, max 0 (worldSizeBlocks.X - macroCounts.X)))
            (Math.Clamp (origin.Y, 0, max 0 (worldSizeBlocks.Y - macroCounts.Y)))
            (Math.Clamp (origin.Z, 0, max 0 (worldSizeBlocks.Z - macroCounts.Z)))

    let createEmptyLevel (settings : WorldGenSettings) placeableBlocks spawnPosition revisionSeed =
        let sourceSize = levelSourceSizeVoxels settings
        let worldSizeBlocks = normalizedWorldSizeBlocks settings
        let activeBlockOrigin = activeBlockOrigin settings
        let worldSize =
            v3
                (single sourceSize.X * settings.VoxelSize.X)
                (single sourceSize.Y * settings.VoxelSize.Y)
                (single sourceSize.Z * settings.VoxelSize.Z)
        let bounds = box3 (worldSize * -0.5f) worldSize
        let levelOffset = v3 0.0f (worldSize.Y * 0.5f) 0.0f
        { Bounds = box3 (worldSize * -0.5f) worldSize
          VoxelSize = settings.VoxelSize
          WorldSizeBlocks = worldSizeBlocks
          ActiveBlockOrigin = activeBlockOrigin
          SourceSizeVoxels = sourceSize
          ChunkSizeVoxels = settings.ChunkSizeVoxels
          ChunkCounts = settings.ChunkCounts
          BlockSideVoxels = settings.BlockSideVoxels
          BlockGridOffsetVoxels = settings.BlockGridOffsetVoxels
          LevelOffset = levelOffset
          GenerationOpt = None
          GeneratedBlockTemplateCache = ConcurrentDictionary<Vector3i, VoxelBlockTemplate option> (HashIdentity.Structural)
          SourceVoxels = Dictionary<Vector3i, VoxelCell> (HashIdentity.Structural)
          BlockEdits = Dictionary<Vector3i, VoxelBlockTemplate option> (HashIdentity.Structural)
          Edits = Dictionary<Vector3i, VoxelEdit> (HashIdentity.Structural)
          EditRevision = ref 0
          PlaceableBlocks = placeableBlocks
          SpawnPosition = spawnPosition
          ChunkManifest = createChunkManifest settings.ChunkCounts bounds levelOffset settings.VoxelSize settings.ChunkSizeVoxels
          NextRevision = ref revisionSeed }

    let blockCoordToWorldBlockCoord (level : VoxelLevel) (blockCoord : Vector3i) =
        level.ActiveBlockOrigin + blockCoord

    let sourceCoordToWorldVoxelCoord (level : VoxelLevel) (coord : Vector3i) =
        v3i
            (level.ActiveBlockOrigin.X * level.BlockSideVoxels + coord.X - level.BlockGridOffsetVoxels.X)
            (level.ActiveBlockOrigin.Y * level.BlockSideVoxels + coord.Y - level.BlockGridOffsetVoxels.Y)
            (level.ActiveBlockOrigin.Z * level.BlockSideVoxels + coord.Z - level.BlockGridOffsetVoxels.Z)

    let withSpawnPosition spawnPosition (level : VoxelLevel) =
        { level with SpawnPosition = spawnPosition }

    let withGeneration generation (level : VoxelLevel) =
        { level with GenerationOpt = Some generation }

    let allChunkCoords (level : VoxelLevel) =
        [|for z in 0 .. dec level.ChunkCounts.Z do
            for y in 0 .. dec level.ChunkCounts.Y do
                for x in 0 .. dec level.ChunkCounts.X do
                    v3i x y z|]

    let streamChunkCoords streamChunkRadius (level : VoxelLevel) (center : Vector3i) =
        [|for z in max 0 (center.Z - streamChunkRadius) .. min (dec level.ChunkCounts.Z) (center.Z + streamChunkRadius) do
            for y in 0 .. dec level.ChunkCounts.Y do
                for x in max 0 (center.X - streamChunkRadius) .. min (dec level.ChunkCounts.X) (center.X + streamChunkRadius) do
                    yield v3i x y z|]
        |> Array.sortBy (fun coord ->
            let dx = coord.X - center.X
            let dy = coord.Y - center.Y
            let dz = coord.Z - center.Z
            struct (dx * dx + dz * dz, abs dy, coord.Z, coord.Y, coord.X))

    let isSourceCoordInBounds (level : VoxelLevel) (coord : Vector3i) =
        coord.X >= 0 && coord.X < level.SourceSizeVoxels.X &&
        coord.Y >= 0 && coord.Y < level.SourceSizeVoxels.Y &&
        coord.Z >= 0 && coord.Z < level.SourceSizeVoxels.Z

    let isChunkCoordInBounds (level : VoxelLevel) (coord : Vector3i) =
        coord.X >= 0 && coord.X < level.ChunkCounts.X &&
        coord.Y >= 0 && coord.Y < level.ChunkCounts.Y &&
        coord.Z >= 0 && coord.Z < level.ChunkCounts.Z

    let sourceCoordToChunkCoord (level : VoxelLevel) (coord : Vector3i) =
        v3i
            (coord.X / level.ChunkSizeVoxels.X)
            (coord.Y / level.ChunkSizeVoxels.Y)
            (coord.Z / level.ChunkSizeVoxels.Z)

    let blockStartCoord (level : VoxelLevel) (blockCoord : Vector3i) =
        v3i
            (level.BlockGridOffsetVoxels.X + blockCoord.X * level.BlockSideVoxels)
            (level.BlockGridOffsetVoxels.Y + blockCoord.Y * level.BlockSideVoxels)
            (level.BlockGridOffsetVoxels.Z + blockCoord.Z * level.BlockSideVoxels)

    let private divFloor dividend divisor =
        if dividend >= 0 then dividend / divisor
        else -((-dividend + divisor - 1) / divisor)

    let sourceCoordToBlockCoord (level : VoxelLevel) (coord : Vector3i) =
        v3i
            (divFloor (coord.X - level.BlockGridOffsetVoxels.X) level.BlockSideVoxels)
            (divFloor (coord.Y - level.BlockGridOffsetVoxels.Y) level.BlockSideVoxels)
            (divFloor (coord.Z - level.BlockGridOffsetVoxels.Z) level.BlockSideVoxels)

    let isBlockCoordInBounds (level : VoxelLevel) (coord : Vector3i) =
        let start = blockStartCoord level coord
        start.X >= 0 && start.X + level.BlockSideVoxels <= level.SourceSizeVoxels.X &&
        start.Y >= 0 && start.Y + level.BlockSideVoxels <= level.SourceSizeVoxels.Y &&
        start.Z >= 0 && start.Z + level.BlockSideVoxels <= level.SourceSizeVoxels.Z

    let tryWorldToSourceCoord (level : VoxelLevel) (position : Vector3) =
        let local = position - level.LevelOffset
        let origin = level.Bounds.Min
        let coord =
            v3i
                (int (floor ((local.X - origin.X) / level.VoxelSize.X)))
                (int (floor ((local.Y - origin.Y) / level.VoxelSize.Y)))
                (int (floor ((local.Z - origin.Z) / level.VoxelSize.Z)))
        if isSourceCoordInBounds level coord then Some coord else None

    let tryWorldToChunkCoord (level : VoxelLevel) (position : Vector3) =
        match tryWorldToSourceCoord level position with
        | Some sourceCoord ->
            let chunkCoord = sourceCoordToChunkCoord level sourceCoord
            if isChunkCoordInBounds level chunkCoord then Some chunkCoord else None
        | None -> None

    let tryWorldToBlockCoord (level : VoxelLevel) (position : Vector3) =
        match tryWorldToSourceCoord level position with
        | Some coord ->
            let blockCoord = sourceCoordToBlockCoord level coord
            if isBlockCoordInBounds level blockCoord then Some blockCoord else None
        | None -> None

    let blockBounds (level : VoxelLevel) (blockCoord : Vector3i) =
        let start = blockStartCoord level blockCoord
        let blockSize =
            v3
                (single level.BlockSideVoxels * level.VoxelSize.X)
                (single level.BlockSideVoxels * level.VoxelSize.Y)
                (single level.BlockSideVoxels * level.VoxelSize.Z)
        let min =
            level.Bounds.Min + level.LevelOffset +
            v3
                (single start.X * level.VoxelSize.X)
                (single start.Y * level.VoxelSize.Y)
                (single start.Z * level.VoxelSize.Z)
        box3 min blockSize

    let blockTopPosition (level : VoxelLevel) (blockCoord : Vector3i) =
        let bounds = blockBounds level blockCoord
        v3 bounds.Center.X (bounds.Max.Y + 0.06f) bounds.Center.Z

    let private clamp01 value =
        Math.Clamp (value, 0.0f, 1.0f)

    let private uintOfInt value =
        uint32 (uint64 (int64 value &&& 0xFFFFFFFFL))

    let private hash3 seed x y z =
        let mutable h = uintOfInt seed
        h <- (h ^^^ (uintOfInt x * 374761393u)) * 668265263u
        h <- (h ^^^ (uintOfInt y * 2246822519u)) * 3266489917u
        h <- (h ^^^ (uintOfInt z * 3266489917u)) * 668265263u
        h <- h ^^^ (h >>> 13)
        h <- h * 1274126177u
        h ^^^ (h >>> 16)

    let private hash01 seed x y z =
        single (hash3 seed x y z &&& 0x00FFFFFFu) / single 0x01000000

    let private fade t =
        t * t * t * (t * (t * 6.0f - 15.0f) + 10.0f)

    let private lerp a b t =
        a + (b - a) * t

    let private valueNoise3 seed x y z =
        let xi = int (MathF.Floor x)
        let yi = int (MathF.Floor y)
        let zi = int (MathF.Floor z)
        let tx = fade (x - single xi)
        let ty = fade (y - single yi)
        let tz = fade (z - single zi)
        let c000 = hash01 seed xi yi zi
        let c100 = hash01 seed (xi + 1) yi zi
        let c010 = hash01 seed xi (yi + 1) zi
        let c110 = hash01 seed (xi + 1) (yi + 1) zi
        let c001 = hash01 seed xi yi (zi + 1)
        let c101 = hash01 seed (xi + 1) yi (zi + 1)
        let c011 = hash01 seed xi (yi + 1) (zi + 1)
        let c111 = hash01 seed (xi + 1) (yi + 1) (zi + 1)
        let x00 = lerp c000 c100 tx
        let x10 = lerp c010 c110 tx
        let x01 = lerp c001 c101 tx
        let x11 = lerp c011 c111 tx
        let y0 = lerp x00 x10 ty
        let y1 = lerp x01 x11 ty
        lerp y0 y1 tz

    let private fbm3 seed octaves x y z =
        let mutable amplitude = 1.0f
        let mutable frequency = 1.0f
        let mutable total = 0.0f
        let mutable value = 0.0f
        for octave in 0 .. dec octaves do
            value <- value + valueNoise3 (seed + octave * 1013) (x * frequency) (y * frequency) (z * frequency) * amplitude
            total <- total + amplitude
            amplitude <- amplitude * 0.5f
            frequency <- frequency * 2.0f
        if total > 0.0f then value / total else 0.0f

    let private fbm2 seed octaves x z =
        fbm3 seed octaves x 0.0f z

    let private ridged2 seed octaves x z =
        1.0f - abs (fbm2 seed octaves x z * 2.0f - 1.0f)

    let private ridged3 seed octaves x y z =
        1.0f - abs (fbm3 seed octaves x y z * 2.0f - 1.0f)

    let generatedBlockCounts (level : VoxelLevel) =
        let side = max 1 level.BlockSideVoxels
        v3i
            (max 1 ((level.SourceSizeVoxels.X - level.BlockGridOffsetVoxels.X * 2) / side))
            (max 1 ((level.SourceSizeVoxels.Y - level.BlockGridOffsetVoxels.Y) / side))
            (max 1 ((level.SourceSizeVoxels.Z - level.BlockGridOffsetVoxels.Z * 2) / side))

    let generatedHeightAt (level : VoxelLevel) (generation : VoxelGeneration) (x : int) (z : int) =
        let macroCounts = generatedBlockCounts level
        let worldBlockCoord = blockCoordToWorldBlockCoord level (v3i x 0 z)
        let nx = single worldBlockCoord.X * generation.TerrainScale
        let nz = single worldBlockCoord.Z * generation.TerrainScale
        let warpX = (fbm2 (generation.Seed + 17) 3 (nx * 0.45f) (nz * 0.45f) - 0.5f) * 1.8f
        let warpZ = (fbm2 (generation.Seed + 23) 3 (nx * 0.45f + 19.0f) (nz * 0.45f - 7.0f) - 0.5f) * 1.8f
        let baseNoise = fbm2 generation.Seed 5 (nx + warpX) (nz + warpZ)
        let mountainNoise = ridged2 (generation.Seed + 41) 4 (nx * 0.55f - warpZ) (nz * 0.55f + warpX)
        let centerX = single level.WorldSizeBlocks.X * 0.5f
        let centerZ = single level.WorldSizeBlocks.Z * 0.5f
        let edgeX = abs (single worldBlockCoord.X - centerX) / max 1.0f centerX
        let edgeZ = abs (single worldBlockCoord.Z - centerZ) / max 1.0f centerZ
        let edgeFalloff = 1.0f - clamp01 ((max edgeX edgeZ - 0.72f) / 0.28f)
        let height =
            single generation.SeaLevelBlocks +
            (baseNoise - 0.38f) * 5.0f +
            mountainNoise * generation.MountainStrength * 4.0f
        int (MathF.Round height) |> max 2 |> min (macroCounts.Y - 3) |> fun value -> int (single value * edgeFalloff + single generation.SeaLevelBlocks * (1.0f - edgeFalloff))

    let private chooseTerrainTemplate (generation : VoxelGeneration) (worldX : int) (worldY : int) (worldZ : int) (localY : int) (height : int) =
        let templates = generation.Templates
        if localY = height then
            if height <= generation.SeaLevelBlocks + 1 then templates.Sand else templates.Grass
        elif localY >= height - 2 then templates.Dirt
        else
            let oreNoise = hash01 (generation.Seed + 79) worldX worldY worldZ
            if localY < generation.SeaLevelBlocks + 4 && oreNoise < generation.OreRate then templates.Ore
            else templates.Stone

    let private hasTreeAt (level : VoxelLevel) (generation : VoxelGeneration) (x : int) (z : int) =
        let macroCounts = generatedBlockCounts level
        if x > 0 && z > 0 && x < dec macroCounts.X && z < dec macroCounts.Z then
            let height = generatedHeightAt level generation x z
            let worldBlockCoord = blockCoordToWorldBlockCoord level (v3i x height z)
            height > generation.SeaLevelBlocks + 1 &&
            height + 5 < macroCounts.Y &&
            hash01 (generation.Seed + 307) worldBlockCoord.X worldBlockCoord.Y worldBlockCoord.Z < generation.TreeRate &&
            worldBlockCoord.X % 3 <> 1 &&
            worldBlockCoord.Z % 3 <> 1
        else false

    let private tryTreeTemplateAt (level : VoxelLevel) (generation : VoxelGeneration) (blockCoord : Vector3i) =
        let templates = generation.Templates
        let height = generatedHeightAt level generation blockCoord.X blockCoord.Z
        if hasTreeAt level generation blockCoord.X blockCoord.Z &&
           blockCoord.Y >= height + 1 &&
           blockCoord.Y <= height + 3 then Some templates.Log
        else
            let mutable templateOpt = None
            let mutable dz = -2
            while templateOpt.IsNone && dz <= 2 do
                let mutable dx = -2
                while templateOpt.IsNone && dx <= 2 do
                    let treeX = blockCoord.X - dx
                    let treeZ = blockCoord.Z - dz
                    if hasTreeAt level generation treeX treeZ then
                        let treeHeight = generatedHeightAt level generation treeX treeZ
                        let dy = blockCoord.Y - treeHeight
                        if dy >= 3 && dy <= 5 then
                            let radius = if dy = 5 then 1 else 2
                            if abs dx + abs dz <= radius + 1 then templateOpt <- Some templates.Leaves
                    dx <- inc dx
                dz <- inc dz
            templateOpt

    let private generatedBlockRemovedByCave (level : VoxelLevel) (generation : VoxelGeneration) (blockCoord : Vector3i) (template : VoxelBlockTemplate) =
        if template.Solid && template.Material <> Wood && template.Material <> Leaves then
            let blockCounts = generatedBlockCounts level
            let sourceCenter = v3 (single blockCounts.X * 0.5f) 0.0f (single blockCounts.Z * 0.5f)
            let horizontal = v3 (single blockCoord.X) 0.0f (single blockCoord.Z) - sourceCenter
            let spawnSafe = horizontal.LengthSquared () < 9.0f && blockCoord.Y < generation.SeaLevelBlocks + 4
            let caveCeiling = generation.SeaLevelBlocks + 8
            if not spawnSafe && blockCoord.Y < caveCeiling then
                let worldBlockCoord = blockCoordToWorldBlockCoord level blockCoord
                let scale = 0.18f
                let density =
                    fbm3 (generation.Seed + 131) 4 (single worldBlockCoord.X * scale) (single worldBlockCoord.Y * scale * 1.25f) (single worldBlockCoord.Z * scale) * 0.62f +
                    ridged3 (generation.Seed + 197) 3 (single worldBlockCoord.X * scale * 1.75f) (single worldBlockCoord.Y * scale) (single worldBlockCoord.Z * scale * 1.75f) * 0.38f
                let depthGate = clamp01 (single (caveCeiling - blockCoord.Y) / 7.0f)
                density * depthGate > generation.CaveThreshold
            else false
        else false

    let private computeGeneratedBlockTemplate (level : VoxelLevel) (generation : VoxelGeneration) (blockCoord : Vector3i) =
        if not (isBlockCoordInBounds level blockCoord) then None
        else
            let height = generatedHeightAt level generation blockCoord.X blockCoord.Z
            let worldBlockCoord = blockCoordToWorldBlockCoord level blockCoord
            let templateOpt =
                if blockCoord.Y <= height then
                    Some (chooseTerrainTemplate generation worldBlockCoord.X worldBlockCoord.Y worldBlockCoord.Z blockCoord.Y height)
                elif blockCoord.Y <= generation.SeaLevelBlocks then Some generation.Templates.Water
                elif blockCoord.Y <= generation.LavaLevelBlocks &&
                     hash01 (generation.Seed + 211) worldBlockCoord.X worldBlockCoord.Y worldBlockCoord.Z < 0.35f then Some generation.Templates.Lava
                else tryTreeTemplateAt level generation blockCoord
            match templateOpt with
            | Some template when generatedBlockRemovedByCave level generation blockCoord template -> None
            | _ -> templateOpt

    let private tryGetGeneratedBlockTemplate (level : VoxelLevel) blockCoord =
        match level.GenerationOpt with
        | Some generation ->
            level.GeneratedBlockTemplateCache.GetOrAdd (blockCoord, Func<Vector3i, VoxelBlockTemplate option> (fun coord -> computeGeneratedBlockTemplate level generation coord))
        | None -> None

    let tryGetGeneratedBlockTemplateValue (level : VoxelLevel) (blockCoord : Vector3i) =
        tryGetGeneratedBlockTemplate level blockCoord

    let private generatedCellRemovedByCave (_level : VoxelLevel) (_generation : VoxelGeneration) (_coord : Vector3i) (_cell : VoxelCell) =
        false

    let tryGetGeneratedCellValueFromTemplateLocal (level : VoxelLevel) (coord : Vector3i) (localCoord : Vector3i) (template : VoxelBlockTemplate) =
        match level.GenerationOpt with
        | Some generation ->
            match template.Cells.TryGetValue localCoord with
            | (true, cell) when not (generatedCellRemovedByCave level generation coord cell) -> ValueSome cell
            | (true, _) | (false, _) -> ValueNone
        | None -> ValueNone

    let tryGetGeneratedCellValueFromTemplate (level : VoxelLevel) (coord : Vector3i) (blockCoord : Vector3i) (template : VoxelBlockTemplate) =
        tryGetGeneratedCellValueFromTemplateLocal level coord (coord - blockStartCoord level blockCoord) template

    let private tryGetGeneratedCellValue (level : VoxelLevel) (coord : Vector3i) =
        match level.GenerationOpt with
        | Some generation ->
            let blockCoord = sourceCoordToBlockCoord level coord
            match tryGetGeneratedBlockTemplate level blockCoord with
            | Some template ->
                let localCoord = coord - blockStartCoord level blockCoord
                match template.Cells.TryGetValue localCoord with
                | (true, cell) when not (generatedCellRemovedByCave level generation coord cell) -> ValueSome cell
                | (true, _) | (false, _) -> ValueNone
            | None -> ValueNone
        | None -> ValueNone

    let private tryGetGeneratedCell (level : VoxelLevel) (coord : Vector3i) =
        match tryGetGeneratedCellValue level coord with
        | ValueSome cell -> Some cell
        | ValueNone -> None

    let private spawnSearchRadiusMax = 192
    let private spawnLakeSearchRadius = 20
    let private spawnCandidatesMax = 256

    let pickGeneratedSpawn (level : VoxelLevel) =
        match level.GenerationOpt with
        | Some generation ->
            let macroCounts = generatedBlockCounts level
            let centerX = macroCounts.X / 2
            let centerZ = macroCounts.Z / 2
            let heightCache = Dictionary<int, int> ()
            let getHeight x z =
                let key = z * macroCounts.X + x
                let mutable height = 0
                if heightCache.TryGetValue (key, &height) then height
                else
                    height <- generatedHeightAt level generation x z
                    heightCache[key] <- height
                    height
            let tryNearestWaterDistance x z =
                let mutable found = false
                let mutable foundDistance = 0
                let mutable radius = 1
                let tryWater wx wz =
                    wx >= 0 && wx < macroCounts.X &&
                    wz >= 0 && wz < macroCounts.Z &&
                    getHeight wx wz < generation.SeaLevelBlocks
                while not found && radius <= spawnLakeSearchRadius do
                    let zMin = z - radius
                    let zMax = z + radius
                    let xMin = x - radius
                    let xMax = x + radius
                    let mutable dx = -radius
                    while not found && dx <= radius do
                        if tryWater (x + dx) zMin || tryWater (x + dx) zMax then
                            found <- true
                            foundDistance <- radius
                        dx <- inc dx
                    let mutable dz = -radius + 1
                    while not found && dz <= radius - 1 do
                        if tryWater xMin (z + dz) || tryWater xMax (z + dz) then
                            found <- true
                            foundDistance <- radius
                        dz <- inc dz
                    radius <- inc radius
                if found then ValueSome foundDistance else ValueNone
            let isSpawnSafe x z height =
                let feet = v3i x (height + 1) z
                let head = v3i x (height + 2) z
                let terrainBlock = v3i x height z
                height >= generation.SeaLevelBlocks &&
                isBlockCoordInBounds level feet &&
                isBlockCoordInBounds level head &&
                (match tryGetGeneratedBlockTemplate level terrainBlock with Some template -> template.Solid | None -> false) &&
                Option.isNone (tryGetGeneratedBlockTemplate level feet) &&
                Option.isNone (tryGetGeneratedBlockTemplate level head)
            let searchRadius = min spawnSearchRadiusMax (max macroCounts.X macroCounts.Z / 2)
            let candidates = ResizeArray<struct (single * int * int * int)> ()
            for z in max 0 (centerZ - searchRadius) .. min (dec macroCounts.Z) (centerZ + searchRadius) do
                for x in max 0 (centerX - searchRadius) .. min (dec macroCounts.X) (centerX + searchRadius) do
                    let height = getHeight x z
                    if height >= generation.SeaLevelBlocks then
                        let heightAboveSea = height - generation.SeaLevelBlocks
                        let waterScore =
                            match tryNearestWaterDistance x z with
                            | ValueSome distance -> 90.0f + single (spawnLakeSearchRadius - distance) * 3.0f
                            | ValueNone -> 0.0f
                        let mountainScore =
                            single heightAboveSea * 14.0f +
                            if heightAboveSea >= 4 then 36.0f else 0.0f
                        let dx = x - centerX
                        let dz = z - centerZ
                        let centerPenalty = single (dx * dx + dz * dz) * 0.00025f
                        let score = mountainScore + waterScore - centerPenalty
                        candidates.Add (struct (score, x, z, height))
            let candidates = candidates.ToArray ()
            Array.sortInPlaceWith
                (fun (struct (leftScore, leftX, leftZ, _)) (struct (rightScore, rightX, rightZ, _)) ->
                    let scoreCompare = compare rightScore leftScore
                    if scoreCompare <> 0 then scoreCompare
                    else
                        let leftDistance = (leftX - centerX) * (leftX - centerX) + (leftZ - centerZ) * (leftZ - centerZ)
                        let rightDistance = (rightX - centerX) * (rightX - centerX) + (rightZ - centerZ) * (rightZ - centerZ)
                        compare leftDistance rightDistance)
                candidates
            let mutable spawnOpt = None
            let mutable i = 0
            while spawnOpt.IsNone && i < min spawnCandidatesMax candidates.Length do
                let struct (_, x, z, height) = candidates[i]
                if isSpawnSafe x z height then
                    spawnOpt <- Some (blockTopPosition level (v3i x height z))
                i <- inc i
            match spawnOpt with
            | Some spawn -> spawn
            | None ->
                let mutable fallbackOpt = None
                let mutable radius = 0
                while fallbackOpt.IsNone && radius < max macroCounts.X macroCounts.Z do
                    for z in max 0 (centerZ - radius) .. min (dec macroCounts.Z) (centerZ + radius) do
                        for x in max 0 (centerX - radius) .. min (dec macroCounts.X) (centerX + radius) do
                            if fallbackOpt.IsNone then
                                let height = getHeight x z
                                if isSpawnSafe x z height then
                                    fallbackOpt <- Some (blockTopPosition level (v3i x height z))
                    radius <- inc radius
                match fallbackOpt with
                | Some spawn -> spawn
                | None -> blockTopPosition level (v3i centerX generation.SeaLevelBlocks centerZ)
        | None -> level.SpawnPosition

    let getEditRevision (level : VoxelLevel) =
        level.EditRevision.Value

    let private tryGetBlockEditValue (blockEdits : Dictionary<Vector3i, VoxelBlockTemplate option>) (blockCoord : Vector3i) =
        match blockEdits.TryGetValue blockCoord with
        | (true, templateOpt) -> ValueSome templateOpt
        | (false, _) -> ValueNone

    let private tryGetCellFromBlockTemplateValue (level : VoxelLevel) (coord : Vector3i) (blockCoord : Vector3i) (template : VoxelBlockTemplate) =
        let localCoord = coord - blockStartCoord level blockCoord
        match template.Cells.TryGetValue localCoord with
        | (true, cell) -> ValueSome cell
        | (false, _) -> ValueNone

    let private tryGetCellFromBlockEditValue (level : VoxelLevel) (coord : Vector3i) (blockCoord : Vector3i) (templateOpt : VoxelBlockTemplate option) =
        match templateOpt with
        | Some template -> tryGetCellFromBlockTemplateValue level coord blockCoord template
        | None -> ValueNone

    let snapshotEdits (level : VoxelLevel) =
        lock level.BlockEdits (fun () ->
            lock level.Edits (fun () ->
                { Revision = level.EditRevision.Value
                  BlockEditsOpt =
                    if level.BlockEdits.Count = 0 then None
                    else Some (Dictionary<Vector3i, VoxelBlockTemplate option> (level.BlockEdits, HashIdentity.Structural))
                  EditsOpt =
                    if level.Edits.Count = 0 then None
                    else Some (Dictionary<Vector3i, VoxelEdit> (level.Edits, HashIdentity.Structural)) }))

    let tryGetCellWithEditSnapshotValue (snapshot : VoxelEditSnapshot) (level : VoxelLevel) (coord : Vector3i) =
        let blockCoord = sourceCoordToBlockCoord level coord
        match snapshot.BlockEditsOpt with
        | Some blockEdits ->
            match tryGetBlockEditValue blockEdits blockCoord with
            | ValueSome templateOpt -> tryGetCellFromBlockEditValue level coord blockCoord templateOpt
            | ValueNone ->
                match snapshot.EditsOpt with
                | Some edits ->
                    match edits.TryGetValue coord with
                    | (true, Removed) -> ValueNone
                    | (true, Placed cell) -> ValueSome cell
                    | (false, _) ->
                        match level.SourceVoxels.TryGetValue coord with
                        | (true, cell) -> ValueSome cell
                        | (false, _) -> tryGetGeneratedCellValue level coord
                | None ->
                    match level.SourceVoxels.TryGetValue coord with
                    | (true, cell) -> ValueSome cell
                    | (false, _) -> tryGetGeneratedCellValue level coord
        | None ->
            match snapshot.EditsOpt with
            | Some edits ->
                match edits.TryGetValue coord with
                | (true, Removed) -> ValueNone
                | (true, Placed cell) -> ValueSome cell
                | (false, _) ->
                    match level.SourceVoxels.TryGetValue coord with
                    | (true, cell) -> ValueSome cell
                    | (false, _) -> tryGetGeneratedCellValue level coord
            | None ->
                match level.SourceVoxels.TryGetValue coord with
                | (true, cell) -> ValueSome cell
                | (false, _) -> tryGetGeneratedCellValue level coord

    let tryGetCellWithEditSnapshot (snapshot : VoxelEditSnapshot) (level : VoxelLevel) (coord : Vector3i) =
        match tryGetCellWithEditSnapshotValue snapshot level coord with
        | ValueSome cell -> Some cell
        | ValueNone -> None

    let tryGetCellValue (level : VoxelLevel) (coord : Vector3i) =
        let blockCoord = sourceCoordToBlockCoord level coord
        match tryGetBlockEditValue level.BlockEdits blockCoord with
        | ValueSome templateOpt -> tryGetCellFromBlockEditValue level coord blockCoord templateOpt
        | ValueNone ->
            match level.Edits.TryGetValue coord with
            | (true, Removed) -> ValueNone
            | (true, Placed cell) -> ValueSome cell
            | (false, _) ->
                match level.SourceVoxels.TryGetValue coord with
                | (true, cell) -> ValueSome cell
                | (false, _) -> tryGetGeneratedCellValue level coord

    let tryGetCell (level : VoxelLevel) (coord : Vector3i) =
        match tryGetCellValue level coord with
        | ValueSome cell -> Some cell
        | ValueNone -> None

    let containsCell level coord =
        match tryGetCellValue level coord with
        | ValueSome _ -> true
        | ValueNone -> false

    let containsSolidCell level coord =
        match tryGetCellValue level coord with
        | ValueSome cell -> cell.Solid
        | ValueNone -> false

    let setSourceCell (level : VoxelLevel) (coord : Vector3i) (cell : VoxelCell) =
        if isSourceCoordInBounds level coord then
            level.SourceVoxels[coord] <- cell

    let removeSourceCell (level : VoxelLevel) (coord : Vector3i) =
        level.SourceVoxels.Remove coord |> ignore<bool>

    let setSourceBlock (level : VoxelLevel) (blockCoord : Vector3i) (template : VoxelBlockTemplate) =
        let start = blockStartCoord level blockCoord
        for struct (localCoord, cell) in template.Voxels do
            let coord = v3i (start.X + localCoord.X) (start.Y + localCoord.Y) (start.Z + localCoord.Z)
            setSourceCell level coord cell

    let blockContainsCell (level : VoxelLevel) (blockCoord : Vector3i) =
        match tryGetBlockEditValue level.BlockEdits blockCoord with
        | ValueSome (Some template) -> template.Voxels.Length > 0
        | ValueSome None -> false
        | ValueNone when level.SourceVoxels.Count = 0 && level.Edits.Count = 0 && Option.isSome level.GenerationOpt ->
            match tryGetGeneratedBlockTemplate level blockCoord with
            | Some template -> template.Voxels.Length > 0
            | None -> false
        | ValueNone ->
            let start = blockStartCoord level blockCoord
            let mutable contains = false
            let mutable y = 0
            while not contains && y < level.BlockSideVoxels do
                let mutable z = 0
                while not contains && z < level.BlockSideVoxels do
                    let mutable x = 0
                    while not contains && x < level.BlockSideVoxels do
                        contains <- containsCell level (v3i (start.X + x) (start.Y + y) (start.Z + z))
                        x <- inc x
                    z <- inc z
                y <- inc y
            contains

    let blockContainsSolidCell (level : VoxelLevel) (blockCoord : Vector3i) =
        match tryGetBlockEditValue level.BlockEdits blockCoord with
        | ValueSome (Some template) -> template.Solid && template.Voxels.Length > 0
        | ValueSome None -> false
        | ValueNone when level.SourceVoxels.Count = 0 && level.Edits.Count = 0 && Option.isSome level.GenerationOpt ->
            match tryGetGeneratedBlockTemplate level blockCoord with
            | Some template -> template.Solid && template.Voxels.Length > 0
            | None -> false
        | ValueNone ->
            let start = blockStartCoord level blockCoord
            let mutable contains = false
            let mutable y = 0
            while not contains && y < level.BlockSideVoxels do
                let mutable z = 0
                while not contains && z < level.BlockSideVoxels do
                    let mutable x = 0
                    while not contains && x < level.BlockSideVoxels do
                        contains <- containsSolidCell level (v3i (start.X + x) (start.Y + y) (start.Z + z))
                        x <- inc x
                    z <- inc z
                y <- inc y
            contains

    let blockIsEmpty level blockCoord =
        not (blockContainsCell level blockCoord)

    let removeBlock (level : VoxelLevel) (blockCoord : Vector3i) =
        if isBlockCoordInBounds level blockCoord then
            lock level.BlockEdits (fun () ->
                level.BlockEdits[blockCoord] <- None
                level.EditRevision.Value <- inc level.EditRevision.Value)

    let placeBlock (level : VoxelLevel) (placeableBlock : PlaceableBlock) (blockCoord : Vector3i) =
        if isBlockCoordInBounds level blockCoord then
            lock level.BlockEdits (fun () ->
                level.BlockEdits[blockCoord] <- Some placeableBlock.Template
                level.EditRevision.Value <- inc level.EditRevision.Value)

    let affectedChunksForBlock (level : VoxelLevel) (blockCoord : Vector3i) =
        let start = blockStartCoord level blockCoord
        let finish =
            start +
            v3i
                (level.BlockSideVoxels - 1)
                (level.BlockSideVoxels - 1)
                (level.BlockSideVoxels - 1)
        let minCoord =
            v3i
                (max 0 (dec start.X))
                (max 0 (dec start.Y))
                (max 0 (dec start.Z))
        let maxCoord =
            v3i
                (min (dec level.SourceSizeVoxels.X) (inc finish.X))
                (min (dec level.SourceSizeVoxels.Y) (inc finish.Y))
                (min (dec level.SourceSizeVoxels.Z) (inc finish.Z))
        let minChunkCoord = sourceCoordToChunkCoord level minCoord
        let maxChunkCoord = sourceCoordToChunkCoord level maxCoord
        [|for z in minChunkCoord.Z .. maxChunkCoord.Z do
            for y in minChunkCoord.Y .. maxChunkCoord.Y do
                for x in minChunkCoord.X .. maxChunkCoord.X do
                    let coord = v3i x y z
                    if isChunkCoordInBounds level coord then yield coord|]
