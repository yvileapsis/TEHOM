namespace VoxelForge
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

type VoxelMaterialKind =
    | Concrete
    | SpalledConcrete
    | Ceramic
    | Enamel
    | CeilingPanel
    | ReinforcedGlass
    | ServiceMetal
    | StairTread
    | StairRail
    | ContainmentBrick
    | HydroponicBed
    | Vegetation
    | ProcessWater
    | Terminal
    | WoodVeneer
    | Upholstery
    | MachineCasing
    | HazardStripe
    | FluorescentFixture
    | PipeAssembly
    | InstrumentPanel
    | FacilityPlacard

type FacilityZone =
    | Administration
    | Research
    | Hydroponics
    | Utilities
    | Auditorium
    | Containment
    | Atrium
    | Pool
    | Decontamination

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
    { Concrete : VoxelBlockTemplate
      SpalledConcrete : VoxelBlockTemplate
      Ceramic : VoxelBlockTemplate
      Enamel : VoxelBlockTemplate
      CeilingPanel : VoxelBlockTemplate
      ReinforcedGlass : VoxelBlockTemplate
      ServiceMetal : VoxelBlockTemplate
      ContainmentBrick : VoxelBlockTemplate
      StairTread : VoxelBlockTemplate
      StairRail : VoxelBlockTemplate
      HydroponicBed : VoxelBlockTemplate
      Vegetation : VoxelBlockTemplate
      ProcessWater : VoxelBlockTemplate
      Terminal : VoxelBlockTemplate
      WoodVeneer : VoxelBlockTemplate
      Upholstery : VoxelBlockTemplate
      MachineCasing : VoxelBlockTemplate
      HazardStripe : VoxelBlockTemplate
      FluorescentFixture : VoxelBlockTemplate
      PipeAssembly : VoxelBlockTemplate
      InstrumentPanel : VoxelBlockTemplate
      FacilityPlacard : VoxelBlockTemplate }

type VoxelGeneration =
    { Seed : int
      FacilityFloorHeight : int
      FacilityModulePitch : int
      FacilityCorridorWidth : int
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
      FacilityFloorHeight : int
      FacilityModulePitch : int
      FacilityCorridorWidth : int
      ChunksPerUpdate : int }

type GeneratedWorldStats =
    { FacilityModuleCount : int
      FacilityFloorCount : int
      TerminalCount : int
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
          WorldSizeBlocks = v3i 1536 20 640
          ActiveBlockOrigin = v3i -1 0 -1
          ChunkCounts = v3i 384 5 160
          ChunkSizeVoxels = v3i 64 64 64
          BlockSideVoxels = 16
          BlockGridOffsetVoxels = v3iZero
          VoxelSize = v3Dup (1.0f / 16.0f)
          FacilityFloorHeight = 4
          FacilityModulePitch = 16
          FacilityCorridorWidth = 3
          ChunksPerUpdate = 16 }

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

    let private hashPercent seed x y z =
        int (hash3 seed x y z % 100u)

    let private positiveRemainder value divisor =
        let remainder = value % divisor
        if remainder < 0 then remainder + divisor else remainder

    let private floorDivide value divisor =
        let quotient = value / divisor
        if value < 0 && value % divisor <> 0 then dec quotient else quotient

    let generatedBlockCounts (level : VoxelLevel) =
        let side = max 1 level.BlockSideVoxels
        v3i
            (max 1 ((level.SourceSizeVoxels.X - level.BlockGridOffsetVoxels.X * 2) / side))
            (max 1 ((level.SourceSizeVoxels.Y - level.BlockGridOffsetVoxels.Y) / side))
            (max 1 ((level.SourceSizeVoxels.Z - level.BlockGridOffsetVoxels.Z * 2) / side))

    let private facilityModuleCoord (generation : VoxelGeneration) (worldBlockCoord : Vector3i) =
        let pitch = max 8 generation.FacilityModulePitch
        v3i
            (floorDivide worldBlockCoord.X pitch)
            (worldBlockCoord.Y / max 3 generation.FacilityFloorHeight)
            (floorDivide worldBlockCoord.Z pitch)

    let private facilitySuperCoord (generation : VoxelGeneration) (worldBlockCoord : Vector3i) =
        let superPitch = max 16 generation.FacilityModulePitch * 2
        let floorHeight = max 3 generation.FacilityFloorHeight
        v3i
            (floorDivide worldBlockCoord.X superPitch)
            ((worldBlockCoord.Y / floorHeight) / 2)
            (floorDivide worldBlockCoord.Z superPitch)
    let private dnaCourtyardCount = 3
    let private dnaSegmentCount = dnaCourtyardCount * 2
    let private dnaDistrictCount = 4

    let private facilityDnaParameters (level : VoxelLevel) (generation : VoxelGeneration) =
        let counts = generatedBlockCounts level
        let superPitch = max 16 generation.FacilityModulePitch * 2
        let marginX = max (superPitch * 7) (counts.X / 9)
        let startX = level.ActiveBlockOrigin.X + marginX
        let endX = level.ActiveBlockOrigin.X + counts.X - marginX
        let segmentPitch = max (superPitch * 4) ((endX - startX) / dnaSegmentCount)
        let centerZ = level.ActiveBlockOrigin.Z + counts.Z / 2
        let strandHalfWidth = max 8 (min superPitch (counts.Z / 16))
        let maximumOffset = max (strandHalfWidth + 2) (counts.Z / 2 - strandHalfWidth - 1)
        let strandOffset = min (superPitch * 5) maximumOffset
        let strandAmplitude =
            max 0 (min (superPitch * 2) (strandOffset - strandHalfWidth * 2))
        struct (startX, endX, segmentPitch, centerZ, strandHalfWidth, strandOffset, strandAmplitude)

    let private facilityDnaVertexZ centerZ strandOffset strandAmplitude upper vertex =
        let deviation = if vertex % 2 = 0 then -strandAmplitude else strandAmplitude
        if upper
        then centerZ + strandOffset + deviation
        else centerZ - strandOffset - deviation

    let private pointSegmentDistanceSquared px pz ax az bx bz =
        let dx = single (bx - ax)
        let dz = single (bz - az)
        let lengthSquared = dx * dx + dz * dz
        if lengthSquared <= 0.0f then
            let ox = single (px - ax)
            let oz = single (pz - az)
            ox * ox + oz * oz
        else
            let t =
                Math.Clamp
                    ((single (px - ax) * dx + single (pz - az) * dz) / lengthSquared,
                     0.0f,
                     1.0f)
            let ox = single px - (single ax + dx * t)
            let oz = single pz - (single az + dz * t)
            ox * ox + oz * oz

    let private facilityDnaContainsWorld
        (level : VoxelLevel)
        (generation : VoxelGeneration)
        (worldBlockCoord : Vector3i) =
        let struct (startX, endX, segmentPitch, centerZ, strandHalfWidth, strandOffset, strandAmplitude) =
            facilityDnaParameters level generation
        let radiusSquared = single (strandHalfWidth * strandHalfWidth)
        let mutable inside = false
        let mutable segment = 0
        while not inside && segment < dnaSegmentCount do
            let leftX = startX + segment * segmentPitch
            let rightX = if segment = dec dnaSegmentCount then endX else startX + inc segment * segmentPitch
            let upperLeftZ =
                facilityDnaVertexZ centerZ strandOffset strandAmplitude true segment
            let upperRightZ =
                facilityDnaVertexZ centerZ strandOffset strandAmplitude true (inc segment)
            let lowerLeftZ =
                facilityDnaVertexZ centerZ strandOffset strandAmplitude false segment
            let lowerRightZ =
                facilityDnaVertexZ centerZ strandOffset strandAmplitude false (inc segment)
            inside <-
                pointSegmentDistanceSquared
                    worldBlockCoord.X worldBlockCoord.Z
                    leftX upperLeftZ rightX upperRightZ <= radiusSquared ||
                pointSegmentDistanceSquared
                    worldBlockCoord.X worldBlockCoord.Z
                    leftX lowerLeftZ rightX lowerRightZ <= radiusSquared
            segment <- inc segment
        if inside then true
        else
            let connectorHalfWidth = strandHalfWidth
            let connectorRadiusSquared = single (connectorHalfWidth * connectorHalfWidth)
            let mutable atConnector = false
            let mutable courtyard = 0
            while not atConnector && courtyard <= dnaCourtyardCount do
                let vertex = courtyard * 2
                let connectorX =
                    if vertex = dnaSegmentCount then endX
                    else startX + vertex * segmentPitch
                let upperZ =
                    facilityDnaVertexZ centerZ strandOffset strandAmplitude true vertex
                let lowerZ =
                    facilityDnaVertexZ centerZ strandOffset strandAmplitude false vertex
                atConnector <-
                    pointSegmentDistanceSquared
                        worldBlockCoord.X worldBlockCoord.Z
                        connectorX lowerZ connectorX upperZ <= connectorRadiusSquared
                courtyard <- inc courtyard
            if atConnector then true
            else
                let leftEndZ =
                    facilityDnaVertexZ centerZ strandOffset strandAmplitude false 0
                let rightEndZ =
                    facilityDnaVertexZ centerZ strandOffset strandAmplitude true dnaSegmentCount
                let terminalRadiusSquared =
                    single (strandHalfWidth * 2 * (strandHalfWidth * 2))
                let leftTerminal =
                    pointSegmentDistanceSquared
                        worldBlockCoord.X worldBlockCoord.Z
                        (startX - strandHalfWidth * 4) leftEndZ
                        startX leftEndZ <= terminalRadiusSquared
                let rightTerminal =
                    pointSegmentDistanceSquared
                        worldBlockCoord.X worldBlockCoord.Z
                        endX rightEndZ
                        (endX + strandHalfWidth * 3) rightEndZ <= terminalRadiusSquared
                leftTerminal || rightTerminal

    let facilityFootprintContains (level : VoxelLevel) (generation : VoxelGeneration) (blockCoord : Vector3i) =
        facilityDnaContainsWorld level generation (blockCoordToWorldBlockCoord level blockCoord)

    let private facilityDnaSpineContainsWorld
        (level : VoxelLevel)
        (generation : VoxelGeneration)
        corridorHalfWidth
        (worldBlockCoord : Vector3i) =
        let struct (startX, endX, segmentPitch, centerZ, _, strandOffset, strandAmplitude) =
            facilityDnaParameters level generation
        let radiusSquared = single (corridorHalfWidth * corridorHalfWidth)
        let mutable inside = false
        let mutable segment = 0
        while not inside && segment < dnaSegmentCount do
            let leftX = startX + segment * segmentPitch
            let rightX = if segment = dec dnaSegmentCount then endX else startX + inc segment * segmentPitch
            let upperLeftZ =
                facilityDnaVertexZ centerZ strandOffset strandAmplitude true segment
            let upperRightZ =
                facilityDnaVertexZ centerZ strandOffset strandAmplitude true (inc segment)
            let lowerLeftZ =
                facilityDnaVertexZ centerZ strandOffset strandAmplitude false segment
            let lowerRightZ =
                facilityDnaVertexZ centerZ strandOffset strandAmplitude false (inc segment)
            inside <-
                pointSegmentDistanceSquared
                    worldBlockCoord.X worldBlockCoord.Z
                    leftX upperLeftZ rightX upperRightZ <= radiusSquared ||
                pointSegmentDistanceSquared
                    worldBlockCoord.X worldBlockCoord.Z
                    leftX lowerLeftZ rightX lowerRightZ <= radiusSquared
            segment <- inc segment
        if inside then true
        else
            let mutable atConnector = false
            let mutable courtyard = 0
            while not atConnector && courtyard <= dnaCourtyardCount do
                let vertex = courtyard * 2
                let connectorX =
                    if vertex = dnaSegmentCount then endX
                    else startX + vertex * segmentPitch
                let upperZ =
                    facilityDnaVertexZ centerZ strandOffset strandAmplitude true vertex
                let lowerZ =
                    facilityDnaVertexZ centerZ strandOffset strandAmplitude false vertex
                atConnector <-
                    pointSegmentDistanceSquared
                        worldBlockCoord.X worldBlockCoord.Z
                        connectorX lowerZ connectorX upperZ <= radiusSquared
                courtyard <- inc courtyard
            atConnector

    let private facilityDnaSpineBoundary
        (level : VoxelLevel)
        (generation : VoxelGeneration)
        corridorHalfWidth
        (worldBlockCoord : Vector3i) =
        facilityDnaSpineContainsWorld level generation corridorHalfWidth worldBlockCoord &&
        (not (facilityDnaSpineContainsWorld level generation corridorHalfWidth (worldBlockCoord + v3iLeft)) ||
         not (facilityDnaSpineContainsWorld level generation corridorHalfWidth (worldBlockCoord + v3iRight)) ||
         not (facilityDnaSpineContainsWorld level generation corridorHalfWidth (worldBlockCoord + v3iForward)) ||
         not (facilityDnaSpineContainsWorld level generation corridorHalfWidth (worldBlockCoord + v3iBack)))

    let private facilityDnaCellIndex (level : VoxelLevel) (generation : VoxelGeneration) worldX =
        let struct (startX, endX, _, _, _, _, _) = facilityDnaParameters level generation
        let span = max 1 (endX - startX)
        Math.Clamp (floorDivide ((worldX - startX) * dnaDistrictCount) span, 0, dec dnaDistrictCount)

    let private facilityDnaCourtyardSide
        (level : VoxelLevel)
        (generation : VoxelGeneration)
        (worldBlockCoord : Vector3i) =
        let struct (startX, endX, _, centerZ, _, strandOffset, _) = facilityDnaParameters level generation
        worldBlockCoord.X > startX &&
        worldBlockCoord.X < endX &&
        abs (worldBlockCoord.Z - centerZ) < strandOffset

    let private facilityDnaBoundary
        (level : VoxelLevel)
        (generation : VoxelGeneration)
        (worldBlockCoord : Vector3i) =
        let outside offset =
            not (facilityDnaContainsWorld level generation (worldBlockCoord + offset))
        outside v3iLeft || outside v3iRight || outside v3iForward || outside v3iBack

    let facilityZoneAt (level : VoxelLevel) (generation : VoxelGeneration) (blockCoord : Vector3i) =
        let worldBlockCoord = blockCoordToWorldBlockCoord level blockCoord
        let superCoord = facilitySuperCoord generation worldBlockCoord
        let counts = generatedBlockCounts level
        let centerWorld = level.ActiveBlockOrigin + v3i (counts.X / 2 + 1) 0 (counts.Z / 2 + 1)
        let centerSuper = facilitySuperCoord generation centerWorld
        if superCoord.X = centerSuper.X && superCoord.Z = centerSuper.Z && superCoord.Y = 0 then Research
        elif superCoord.X = centerSuper.X + 1 && superCoord.Z = centerSuper.Z + 1 && superCoord.Y = 1 then Containment
        else
            let cell = facilityDnaCellIndex level generation worldBlockCoord.X
            let roll = hashPercent generation.Seed superCoord.X superCoord.Y superCoord.Z
            match cell with
            | 0 ->
                if roll < 38 then Administration
                elif roll < 63 then Auditorium
                elif roll < 83 then Atrium
                else Research
            | 1 ->
                if roll < 36 then Research
                elif roll < 66 then Hydroponics
                elif roll < 86 then Atrium
                else Administration
            | 2 ->
                if roll < 31 then Research
                elif roll < 61 then Containment
                elif roll < 86 then Decontamination
                else Utilities
            | _ ->
                if roll < 36 then Utilities
                elif roll < 61 then Decontamination
                elif roll < 81 then Containment
                else Pool


    let private isLargeFacilityRoom zone =
        match zone with
        | Hydroponics | Auditorium | Atrium | Pool -> true
        | Administration | Research | Utilities | Containment | Decontamination -> false

    let private isSmallFacilityRoom zone =
        match zone with
        | Administration | Utilities | Decontamination -> true
        | Research | Hydroponics | Auditorium | Containment | Atrium | Pool -> false

    let private isTallFacilityRoom zone =
        match zone with
        | Auditorium | Atrium -> true
        | Administration | Research | Hydroponics | Utilities | Containment | Pool | Decontamination -> false

    let private isFacilityTerminalLocation
        (level : VoxelLevel)
        (generation : VoxelGeneration)
        (blockCoord : Vector3i)
        (localBaseX : int)
        (localFloorY : int)
        (localBaseZ : int) =
        let pitch = max 8 generation.FacilityModulePitch
        let moduleCoord = facilityModuleCoord generation (blockCoordToWorldBlockCoord level blockCoord)
        let counts = generatedBlockCounts level
        let centerWorld = level.ActiveBlockOrigin + v3i (counts.X / 2 + 1) 0 (counts.Z / 2 + 1)
        let centerModule = facilityModuleCoord generation centerWorld
        let forcedTerminal =
            moduleCoord.X = centerModule.X && moduleCoord.Z = centerModule.Z && moduleCoord.Y = 0 ||
            moduleCoord.X = centerModule.X + 2 && moduleCoord.Z = centerModule.Z + 2 && moduleCoord.Y = 1
        localFloorY = 1 &&
        localBaseX = pitch / 2 &&
        localBaseZ = dec pitch - 2 &&
        (forcedTerminal || hashPercent (generation.Seed + 911) moduleCoord.X moduleCoord.Y moduleCoord.Z < 8)

    let private facilityWallTemplate zone (templates : VoxelGeneratedTemplates) =
        match zone with
        | Administration -> templates.WoodVeneer
        | Research -> templates.Enamel
        | Hydroponics -> templates.ReinforcedGlass
        | Utilities -> templates.Concrete
        | Auditorium -> templates.WoodVeneer
        | Containment -> templates.ContainmentBrick
        | Atrium -> templates.Concrete
        | Pool -> templates.Ceramic
        | Decontamination -> templates.Enamel

    let private facilityFloorTemplate zone (templates : VoxelGeneratedTemplates) =
        match zone with
        | Utilities -> templates.ServiceMetal
        | Auditorium | Administration -> templates.WoodVeneer
        | Containment | Atrium -> templates.Concrete
        | Research | Hydroponics | Pool | Decontamination -> templates.Ceramic

    let private tryFacilityRoomFeature
        zone
        (templates : VoxelGeneratedTemplates)
        localBaseX
        localSuperX
        localY
        localBaseZ
        localSuperZ =
        let between minimum maximum value = value >= minimum && value <= maximum
        let debrisLocation =
            (localSuperX = 5 && between 9 11 localSuperZ) ||
            (localSuperX = 6 && localSuperZ = 10) ||
            (localSuperX = 8 && between 10 11 localSuperZ) ||
            (localSuperX = 25 && between 20 22 localSuperZ)
        let debris = localY = 1 && debrisLocation
        match zone with
        | Research ->
            if debris then Some templates.SpalledConcrete
            elif localY = 1 && localBaseZ = 13 && (localBaseX = 5 || localBaseX = 11)
            then Some templates.InstrumentPanel
            elif localY = 2 && localBaseX = 13 && between 6 12 localBaseZ
            then Some templates.PipeAssembly
            elif localY = 1 &&
                 ((between 6 13 localBaseX && (localBaseZ = 5 || localBaseZ = 13)) ||
                  (between 6 13 localBaseZ && localBaseX = 13))
            then Some templates.Enamel
            elif (localY = 1 || localY = 2) &&
                 localBaseX = 11 &&
                 between 6 12 localBaseZ &&
                 not (between 8 10 localBaseZ)
            then Some templates.ReinforcedGlass
            else None
        | Hydroponics ->
            if debris then Some templates.SpalledConcrete
            elif localY = 1 &&
               (localSuperX = 7 || localSuperX = 13 || localSuperX = 19 || localSuperX = 25) &&
               between 6 26 localSuperZ
            then Some templates.HydroponicBed
            elif localY = 2 &&
                 not debrisLocation &&
                 (localSuperX = 7 || localSuperX = 13 || localSuperX = 19 || localSuperX = 25) &&
                 between 6 26 localSuperZ
            then Some templates.Vegetation
            elif localY = 1 && (localSuperX = 10 || localSuperX = 22) && between 6 26 localSuperZ
            then Some templates.ProcessWater
            else None
        | Utilities ->
            if debris then Some templates.SpalledConcrete
            elif localY = 2 && localBaseX = 13 && between 5 13 localBaseZ
            then Some templates.PipeAssembly
            elif localY = 1 && localBaseX = 13 && localBaseZ = 9
            then Some templates.InstrumentPanel
            elif (localY = 1 || localY = 2) &&
                 ((localBaseX = 5 || localBaseX = 13) && (localBaseZ = 5 || localBaseZ = 13))
            then Some templates.MachineCasing
            elif localY = 1 &&
                 ((localBaseX = 4 && between 5 13 localBaseZ) ||
                  (localBaseZ = 4 && between 5 13 localBaseX))
            then Some templates.HazardStripe
            else None
        | Auditorium ->
            let seatRow =
                localSuperZ = 7 || localSuperZ = 10 || localSuperZ = 13 ||
                localSuperZ = 16 || localSuperZ = 19 || localSuperZ = 22
            let seatColumn =
                between 5 27 localSuperX &&
                localSuperX <> 10 && localSuperX <> 21
            let seatHeight =
                if localSuperZ >= 19 then 3
                elif localSuperZ >= 13 then 2
                else 1
            if debris then Some templates.SpalledConcrete
            elif seatRow && seatColumn && localY = seatHeight
            then Some templates.Upholstery
            elif seatRow && seatColumn && localY >= 1 && localY < seatHeight
            then Some templates.WoodVeneer
            elif localY = 1 && between 25 28 localSuperZ && between 5 27 localSuperX
            then Some templates.WoodVeneer
            elif localY = 4 &&
                 (localSuperX = 4 || localSuperX = 28) &&
                 between 5 27 localSuperZ
            then Some templates.WoodVeneer
            elif localY = 5 &&
                 (localSuperX = 4 || localSuperX = 28) &&
                 between 5 27 localSuperZ
            then Some templates.ServiceMetal
            else None
        | Containment ->
            if debris then Some templates.SpalledConcrete
            elif (localY = 1 || localY = 2) &&
               localBaseX = 10 &&
               between 5 13 localBaseZ &&
               not (between 8 10 localBaseZ)
            then Some templates.ReinforcedGlass
            elif localY = 1 && localBaseX = 12 && localBaseZ = 9
            then Some templates.InstrumentPanel
            elif localY = 1 && localBaseZ = 4 && between 5 13 localBaseX
            then Some templates.HazardStripe
            else None
        | Administration ->
            if debris then Some templates.SpalledConcrete
            elif localY = 1 &&
               ((localBaseX = 13 && between 5 13 localBaseZ) ||
                (localBaseZ = 13 && between 5 13 localBaseX))
            then Some templates.WoodVeneer
            elif localY = 1 && localBaseX = 6 && localBaseZ = 6
            then Some templates.Upholstery
            else None
        | Atrium ->
            let centralPlanter = between 13 18 localSuperX && between 13 18 localSuperZ
            if debris then Some templates.SpalledConcrete
            elif localY = 1 && centralPlanter then Some templates.HydroponicBed
            elif localY = 2 && not debrisLocation && centralPlanter then Some templates.Vegetation
            elif localY = 1 &&
                 ((localSuperX = 10 || localSuperX = 21) && between 11 20 localSuperZ ||
                  (localSuperZ = 10 || localSuperZ = 21) && between 11 20 localSuperX)
            then Some templates.WoodVeneer
            elif localY = 4 &&
                 (((localSuperX = 4 || localSuperX = 28) && between 5 27 localSuperZ) ||
                  (between 4 28 localSuperX && between 15 16 localSuperZ))
            then Some templates.ServiceMetal
            elif localY = 5 &&
                 (localSuperZ = 14 || localSuperZ = 17) &&
                 between 4 28 localSuperX
            then Some templates.PipeAssembly
            else None
        | Pool ->
            if debris then Some templates.SpalledConcrete
            elif localY = 1 && between 8 24 localSuperX && between 8 24 localSuperZ
            then Some templates.ProcessWater
            elif localY = 1 &&
                 ((localSuperX = 6 || localSuperX = 26) && between 7 25 localSuperZ)
            then Some templates.HazardStripe
            elif localY = 1 && localSuperX = 27 && between 10 22 localSuperZ
            then Some templates.MachineCasing
            else None
        | Decontamination ->
            if debris then Some templates.SpalledConcrete
            elif localY = 1 && localBaseX = 12 && localBaseZ = 12
            then Some templates.InstrumentPanel
            elif localY = 2 && localBaseX = 12 && between 6 12 localBaseZ
            then Some templates.PipeAssembly
            elif (localY = 1 || localY = 2) &&
                 (localBaseX = 6 || localBaseX = 12) &&
                 (localBaseZ = 6 || localBaseZ = 12)
            then Some templates.MachineCasing
            elif localY = 1 &&
                 ((localBaseX = 4 && between 5 13 localBaseZ) ||
                  (localBaseZ = 4 && between 5 13 localBaseX))
            then Some templates.HazardStripe
            else None

    let private computeGeneratedBlockTemplate (level : VoxelLevel) (generation : VoxelGeneration) (blockCoord : Vector3i) =
        if not (isBlockCoordInBounds level blockCoord) then None
        elif not (facilityFootprintContains level generation blockCoord) then None
        else
            let counts = generatedBlockCounts level
            let templates = generation.Templates
            let pitch = max 8 generation.FacilityModulePitch
            let superPitch = pitch * 2
            let floorHeight = max 3 generation.FacilityFloorHeight
            let corridorWidth = Math.Clamp (generation.FacilityCorridorWidth, 2, pitch - 5)
            let worldBlockCoord = blockCoordToWorldBlockCoord level blockCoord
            let localBaseX = positiveRemainder worldBlockCoord.X pitch
            let localBaseZ = positiveRemainder worldBlockCoord.Z pitch
            let localSuperX = positiveRemainder worldBlockCoord.X superPitch
            let localSuperZ = positiveRemainder worldBlockCoord.Z superPitch
            let localFloorY = positiveRemainder worldBlockCoord.Y floorHeight
            let zone = facilityZoneAt level generation blockCoord
            let roomHeight = if isTallFacilityRoom zone then floorHeight * 2 else floorHeight
            let localY = positiveRemainder worldBlockCoord.Y roomHeight
            let superCoord = facilitySuperCoord generation worldBlockCoord
            let layoutRoll =
                hashPercent
                    (generation.Seed + 2381)
                    superCoord.X
                    superCoord.Y
                    superCoord.Z
            let stairRoom =
                zone = Atrium ||
                zone = Auditorium && layoutRoll < 65
            let largeRoom =
                isLargeFacilityRoom zone ||
                layoutRoll < 18
            let smallRoom =
                not largeRoom &&
                (isSmallFacilityRoom zone || layoutRoll >= 78)
            let angularRoom =
                not largeRoom &&
                not smallRoom &&
                layoutRoll >= 42 &&
                layoutRoll < 70
            let footprintBoundary = facilityDnaBoundary level generation worldBlockCoord
            let courtyardSide = facilityDnaCourtyardSide level generation worldBlockCoord
            let circulationSpine =
                facilityDnaSpineContainsWorld level generation corridorWidth worldBlockCoord
            let circulationBoundary =
                facilityDnaSpineBoundary level generation corridorWidth worldBlockCoord
            let corridorDoor =
                abs (localBaseX - (pitch / 2 + 1)) <= 1 ||
                abs (localBaseZ - (pitch / 2 + 1)) <= 1
            let roof = blockCoord.Y = dec counts.Y
            if roof then Some templates.Concrete
            elif footprintBoundary then
                if courtyardSide then Some templates.ReinforcedGlass
                else Some templates.Concrete
            elif circulationSpine && localFloorY = 0 then
                Some templates.Ceramic
            elif circulationSpine then
                if circulationBoundary && not (corridorDoor && localFloorY <= 2) then
                    Some (facilityWallTemplate zone templates)
                elif localFloorY = dec floorHeight &&
                     hashPercent
                        (generation.Seed + 1777)
                        worldBlockCoord.X
                        (worldBlockCoord.Y / floorHeight)
                        worldBlockCoord.Z < 6
                then Some templates.FluorescentFixture
                elif localFloorY = dec floorHeight then
                    Some templates.CeilingPanel
                else None
            elif localY = 0 then
                Some (facilityFloorTemplate zone templates)
            else
                let wallX, wallZ, door =
                    if largeRoom then
                        let wallX =
                            (localSuperX = corridorWidth || localSuperX = dec superPitch) &&
                            localSuperZ >= corridorWidth
                        let wallZ =
                            (localSuperZ = corridorWidth || localSuperZ = dec superPitch) &&
                            localSuperX >= corridorWidth
                        let transverseDoor value =
                            abs (value - (pitch / 2 + 1)) <= 1 ||
                            abs (value - (pitch + pitch / 2 + 1)) <= 1
                        wallX, wallZ,
                        (wallX && transverseDoor localSuperZ ||
                         wallZ && transverseDoor localSuperX)
                    else
                        let wallX =
                            (localBaseX = corridorWidth || localBaseX = dec pitch) &&
                            localBaseZ >= corridorWidth
                        let wallZ =
                            (localBaseZ = corridorWidth || localBaseZ = dec pitch) &&
                            localBaseX >= corridorWidth
                        let doorCenter = pitch / 2 + 1
                        wallX, wallZ,
                        (wallX && abs (localBaseZ - doorCenter) <= 1 ||
                         wallZ && abs (localBaseX - doorCenter) <= 1)
                let subdivisionWallX =
                    smallRoom && localBaseX = pitch / 2 + 1 && localBaseZ > corridorWidth
                let subdivisionWallZ =
                    smallRoom && localBaseZ = pitch / 2 + 1 && localBaseX > corridorWidth
                let subdivisionDoor =
                    subdivisionWallX && (localBaseZ = 6 || localBaseZ = 12) ||
                    subdivisionWallZ && (localBaseX = 6 || localBaseX = 12)
                let angularWallX =
                    angularRoom &&
                    localBaseX = pitch / 2 + 2 &&
                    localBaseZ >= pitch / 2
                let angularWallZ =
                    angularRoom &&
                    localBaseZ = pitch / 2 + 2 &&
                    localBaseX >= pitch / 2
                let angularDoor =
                    angularWallX && abs (localBaseZ - (pitch - 3)) <= 1 ||
                    angularWallZ && abs (localBaseX - (pitch - 3)) <= 1
                let placardAgainstWall =
                    if largeRoom
                    then localSuperZ = dec (dec superPitch)
                    else localBaseZ = dec (dec pitch)
                let placardSupportWorldCoord = worldBlockCoord + v3i 0 0 1
                let placard =
                    localY = 2 &&
                    localBaseX = corridorWidth + 2 &&
                    placardAgainstWall &&
                    facilityDnaContainsWorld level generation placardSupportWorldCoord &&
                    (facilityDnaBoundary level generation placardSupportWorldCoord ||
                     not (facilityDnaSpineContainsWorld level generation corridorWidth placardSupportWorldCoord))
                let structuralWall =
                    ((wallX || wallZ) && not (door && localY <= 2)) ||
                    ((subdivisionWallX || subdivisionWallZ) &&
                     not (subdivisionDoor && localY <= 2)) ||
                    ((angularWallX || angularWallZ) &&
                     not (angularDoor && localY <= 2))
                if structuralWall then
                    Some (facilityWallTemplate zone templates)
                elif isFacilityTerminalLocation level generation blockCoord localBaseX localFloorY localBaseZ then
                    Some templates.Terminal
                elif placard then
                    Some templates.FacilityPlacard
                elif localY = 2 &&
                     (zone = Utilities || zone = Decontamination) &&
                     localBaseX = 2 && localBaseZ >= 4 && localBaseZ <= 14 then
                    Some templates.PipeAssembly
                else
                    let interior =
                        if largeRoom then
                            localSuperX > corridorWidth && localSuperX < dec superPitch &&
                            localSuperZ > corridorWidth && localSuperZ < dec superPitch
                        else
                            localBaseX > corridorWidth && localBaseX < dec pitch &&
                            localBaseZ > corridorWidth && localBaseZ < dec pitch
                    let fixture =
                        localY = dec roomHeight &&
                        ((interior &&
                          if largeRoom
                          then localSuperX % 8 = 0 && localSuperZ % 8 = 0
                          else localBaseX = pitch / 2 && (localBaseZ = 6 || localBaseZ = 12)) ||
                         (not interior &&
                          ((localBaseX = 1 && localBaseZ = pitch / 2) ||
                           (localBaseZ = 1 && localBaseX = pitch / 2))))
                    let stairRise = localSuperZ - 8
                    let stairSupport =
                        stairRoom &&
                        localSuperX >= 7 && localSuperX <= 10 &&
                        localSuperZ >= 9 && localSuperZ <= 11 &&
                        localY >= 1 &&
                        localY < stairRise
                    let stairStep =
                        stairRoom &&
                        localSuperX >= 7 && localSuperX <= 10 &&
                        localSuperZ >= 9 && localSuperZ <= 11 &&
                        localY = stairRise
                    let stairLanding =
                        stairRoom &&
                        localY = 4 &&
                        localSuperX >= 7 && localSuperX <= 12 &&
                        localSuperZ >= 11 && localSuperZ <= 15
                    let stairRail =
                        stairRoom &&
                        localSuperX = 11 &&
                        localSuperZ >= 9 && localSuperZ <= 15 &&
                        localY = min 4 (localSuperZ - 8)
                    if fixture then Some templates.FluorescentFixture
                    elif localY = dec roomHeight then Some templates.CeilingPanel
                    elif stairStep then Some templates.StairTread
                    elif stairSupport then Some templates.Concrete
                    elif stairLanding then Some templates.ServiceMetal
                    elif stairRail then Some templates.StairRail
                    elif interior then
                        tryFacilityRoomFeature
                            zone
                            templates
                            localBaseX
                            localSuperX
                            localY
                            localBaseZ
                            localSuperZ
                    else None

    let private tryGetGeneratedBlockTemplate (level : VoxelLevel) blockCoord =
        match level.GenerationOpt with
        | Some generation ->
            level.GeneratedBlockTemplateCache.GetOrAdd
                (blockCoord,
                 Func<Vector3i, VoxelBlockTemplate option>
                    (fun coord -> computeGeneratedBlockTemplate level generation coord))
        | None -> None

    let tryGetGeneratedBlockTemplateValue (level : VoxelLevel) (blockCoord : Vector3i) =
        tryGetGeneratedBlockTemplate level blockCoord

    let tryGetGeneratedCellValueFromTemplateLocal
        (level : VoxelLevel)
        (_coord : Vector3i)
        (localCoord : Vector3i)
        (template : VoxelBlockTemplate) =
        match level.GenerationOpt with
        | Some _ ->
            match template.Cells.TryGetValue localCoord with
            | (true, cell) -> ValueSome cell
            | (false, _) -> ValueNone
        | None -> ValueNone

    let tryGetGeneratedCellValueFromTemplate
        (level : VoxelLevel)
        (coord : Vector3i)
        (blockCoord : Vector3i)
        (template : VoxelBlockTemplate) =
        tryGetGeneratedCellValueFromTemplateLocal level coord (coord - blockStartCoord level blockCoord) template

    let private tryGetGeneratedCellValue (level : VoxelLevel) (coord : Vector3i) =
        match level.GenerationOpt with
        | Some _ ->
            let blockCoord = sourceCoordToBlockCoord level coord
            match tryGetGeneratedBlockTemplate level blockCoord with
            | Some template ->
                let localCoord = coord - blockStartCoord level blockCoord
                match template.Cells.TryGetValue localCoord with
                | (true, cell) -> ValueSome cell
                | (false, _) -> ValueNone
            | None -> ValueNone
        | None -> ValueNone

    let private tryGetGeneratedCell (level : VoxelLevel) (coord : Vector3i) =
        match tryGetGeneratedCellValue level coord with
        | ValueSome cell -> Some cell
        | ValueNone -> None

    let pickGeneratedZoneSpawn desiredZone (level : VoxelLevel) =
        match level.GenerationOpt with
        | Some generation ->
            let counts = generatedBlockCounts level
            let pitch = max 8 generation.FacilityModulePitch
            let floorHeight = max 3 generation.FacilityFloorHeight
            let roomCenter = pitch / 2
            let firstWorldX =
                level.ActiveBlockOrigin.X +
                positiveRemainder
                    (roomCenter - positiveRemainder level.ActiveBlockOrigin.X pitch)
                    pitch
            let firstWorldZ =
                level.ActiveBlockOrigin.Z +
                positiveRemainder
                    (roomCenter - positiveRemainder level.ActiveBlockOrigin.Z pitch)
                    pitch
            let lastWorldX = level.ActiveBlockOrigin.X + counts.X - 1
            let lastWorldZ = level.ActiveBlockOrigin.Z + counts.Z - 1
            let centerWorldX = level.ActiveBlockOrigin.X + counts.X / 2
            let centerWorldZ = level.ActiveBlockOrigin.Z + counts.Z / 2
            let expectedFloor = facilityFloorTemplate desiredZone generation.Templates
            let mutable bestDistance = Int64.MaxValue
            let mutable bestBlockCoordOpt = None
            for worldZ in firstWorldZ .. pitch .. lastWorldZ do
                for worldX in firstWorldX .. pitch .. lastWorldX do
                    let blockCoord =
                        v3i
                            (worldX - level.ActiveBlockOrigin.X)
                            0
                            (worldZ - level.ActiveBlockOrigin.Z)
                    let hasRequiredGeometry =
                        if desiredZone <> Atrium then true
                        else
                            match
                                tryGetGeneratedBlockTemplate
                                    level
                                    (blockCoord + v3i 4 (floorHeight) 4),
                                tryGetGeneratedBlockTemplate
                                    level
                                    (blockCoord + v3i 0 (floorHeight) 7)
                            with
                            | Some landing, Some bridge ->
                                landing.Name = generation.Templates.ServiceMetal.Name &&
                                bridge.Name = generation.Templates.ServiceMetal.Name
                            | _ -> false
                    if hasRequiredGeometry &&
                       facilityFootprintContains level generation blockCoord &&
                       facilityZoneAt level generation blockCoord = desiredZone then
                        match tryGetGeneratedBlockTemplate level blockCoord with
                        | Some floor when floor.Name = expectedFloor.Name &&
                                          Option.isNone (tryGetGeneratedBlockTemplate level (blockCoord + v3iUp)) &&
                                          Option.isNone (tryGetGeneratedBlockTemplate level (blockCoord + v3iUp * 2)) ->
                            let dx = int64 (worldX - centerWorldX)
                            let dz = int64 (worldZ - centerWorldZ)
                            let distance = dx * dx + dz * dz
                            if distance < bestDistance then
                                bestDistance <- distance
                                bestBlockCoordOpt <- Some blockCoord
                        | Some _ | None -> ()
            match bestBlockCoordOpt with
            | Some spawnBlockCoord -> blockTopPosition level spawnBlockCoord
            | None -> failwithf "VoxelForge facility generated no clear %A room." desiredZone
        | None -> level.SpawnPosition

    let pickGeneratedSpawn (level : VoxelLevel) =
        pickGeneratedZoneSpawn Research level

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
