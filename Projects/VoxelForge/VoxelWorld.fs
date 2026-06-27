namespace VoxelForge
open System
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
      VoxelModel : VoxelModel AssetTag }

type VoxelBlockTemplate =
    { Name : string
      Material : VoxelMaterialKind
      Solid : bool
      Voxels : struct (Vector3i * VoxelCell) array }

type PlaceableBlock =
    { Name : string
      Voxels : struct (Vector3i * VoxelCell) array
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
      SourceVoxels : Dictionary<Vector3i, VoxelCell>
      Edits : Dictionary<Vector3i, VoxelEdit>
      PlaceableBlocks : PlaceableBlock array
      SpawnPosition : Vector3
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
          WorldSizeBlocks = v3i 1024 64 1024
          ActiveBlockOrigin = v3i -1 0 -1
          ChunkCounts = v3i 8 4 8
          ChunkSizeVoxels = v3i 64 64 64
          BlockSideVoxels = 16
          BlockGridOffsetVoxels = v3i 8 0 8
          VoxelSize = v3Dup (1.0f / 16.0f)
          SeaLevelBlocks = 6
          LavaLevelBlocks = 2
          TerrainScale = 0.135f
          MountainStrength = 0.85f
          CaveThreshold = 0.70f
          OreRate = 0.035f
          TreeRate = 0.10f
          ChunksPerUpdate = 2 }

[<RequireQualifiedAccess>]
module VoxelWorld =

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
        { Bounds = box3 (worldSize * -0.5f) worldSize
          VoxelSize = settings.VoxelSize
          WorldSizeBlocks = worldSizeBlocks
          ActiveBlockOrigin = activeBlockOrigin
          SourceSizeVoxels = sourceSize
          ChunkSizeVoxels = settings.ChunkSizeVoxels
          ChunkCounts = settings.ChunkCounts
          BlockSideVoxels = settings.BlockSideVoxels
          BlockGridOffsetVoxels = settings.BlockGridOffsetVoxels
          LevelOffset = v3 0.0f (worldSize.Y * 0.5f) 0.0f
          SourceVoxels = Dictionary<Vector3i, VoxelCell> (HashIdentity.Structural)
          Edits = Dictionary<Vector3i, VoxelEdit> (HashIdentity.Structural)
          PlaceableBlocks = placeableBlocks
          SpawnPosition = spawnPosition
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

    let allChunkCoords (level : VoxelLevel) =
        [|for z in 0 .. dec level.ChunkCounts.Z do
            for y in 0 .. dec level.ChunkCounts.Y do
                for x in 0 .. dec level.ChunkCounts.X do
                    v3i x y z|]

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

    let tryGetCell (level : VoxelLevel) (coord : Vector3i) =
        match level.Edits.TryGetValue coord with
        | (true, Removed) -> None
        | (true, Placed cell) -> Some cell
        | (false, _) ->
            match level.SourceVoxels.TryGetValue coord with
            | (true, cell) -> Some cell
            | (false, _) -> None

    let containsCell level coord =
        match tryGetCell level coord with
        | Some _ -> true
        | None -> false

    let containsSolidCell level coord =
        match tryGetCell level coord with
        | Some cell -> cell.Solid
        | None -> false

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
        let start = blockStartCoord level blockCoord
        for y in 0 .. dec level.BlockSideVoxels do
            for z in 0 .. dec level.BlockSideVoxels do
                for x in 0 .. dec level.BlockSideVoxels do
                    let coord = v3i (start.X + x) (start.Y + y) (start.Z + z)
                    if isSourceCoordInBounds level coord then
                        level.Edits[coord] <- Removed

    let placeBlock (level : VoxelLevel) (placeableBlock : PlaceableBlock) (blockCoord : Vector3i) =
        removeBlock level blockCoord
        let start = blockStartCoord level blockCoord
        for struct (localCoord, cell) in placeableBlock.Voxels do
            let coord = v3i (start.X + localCoord.X) (start.Y + localCoord.Y) (start.Z + localCoord.Z)
            if isSourceCoordInBounds level coord then
                level.Edits[coord] <- Placed cell

    let affectedChunksForBlock (level : VoxelLevel) (blockCoord : Vector3i) =
        let affected = HashSet<Vector3i> (HashIdentity.Structural)
        let start = blockStartCoord level blockCoord
        let finish =
            start +
            v3i
                (level.BlockSideVoxels - 1)
                (level.BlockSideVoxels - 1)
                (level.BlockSideVoxels - 1)
        let minCoord =
            v3i
                (max 0 (start.X - 1))
                (max 0 (start.Y - 1))
                (max 0 (start.Z - 1))
        let maxCoord =
            v3i
                (min (level.SourceSizeVoxels.X - 1) (finish.X + 1))
                (min (level.SourceSizeVoxels.Y - 1) (finish.Y + 1))
                (min (level.SourceSizeVoxels.Z - 1) (finish.Z + 1))
        let minChunkCoord = sourceCoordToChunkCoord level minCoord
        let maxChunkCoord = sourceCoordToChunkCoord level maxCoord
        for z in minChunkCoord.Z .. maxChunkCoord.Z do
            for y in minChunkCoord.Y .. maxChunkCoord.Y do
                for x in minChunkCoord.X .. maxChunkCoord.X do
                    let coord = v3i x y z
                    if isChunkCoordInBounds level coord then affected.Add coord |> ignore<bool>
        affected |> Seq.toArray
