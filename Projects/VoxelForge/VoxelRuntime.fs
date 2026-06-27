namespace VoxelForge
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module VoxelRuntime =

    let freshRevisionSeed () =
        int (Gen.id64 % uint64 (Int32.MaxValue - 1))

    let nextRevision (level : VoxelLevel) =
        let revision = level.NextRevision.Value
        level.NextRevision.Value <- if revision = Int32.MaxValue then 1 else inc revision
        revision

    let rec translateBodyShape translation bodyShape =
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

    let chunkAssetTag (chunkCoord : Vector3i) revision =
        Assets.Voxels.MinecraftLevelChunkRevision chunkCoord.X chunkCoord.Y chunkCoord.Z revision

    let sortVoxelChunks chunks =
        chunks
        |> Seq.sortBy (fun (chunk : VoxelChunk) -> struct (chunk.ChunkCoord.Z, chunk.ChunkCoord.Y, chunk.ChunkCoord.X))
        |> Seq.toArray

    let private collisionSolidThreshold = 0.20f

    let private levelBlockCounts (level : VoxelLevel) =
        let side = max 1 level.BlockSideVoxels
        v3i
            (max 1 ((level.SourceSizeVoxels.X - level.BlockGridOffsetVoxels.X * 2) / side))
            (max 1 ((level.SourceSizeVoxels.Y - level.BlockGridOffsetVoxels.Y) / side))
            (max 1 ((level.SourceSizeVoxels.Z - level.BlockGridOffsetVoxels.Z * 2) / side))

    let private divCeilNonNegative dividend divisor =
        if dividend <= 0 then 0 else (dividend + divisor - 1) / divisor

    let private tryChunkBlockRange (level : VoxelLevel) (chunkCoord : Vector3i) =
        let side = max 1 level.BlockSideVoxels
        let blockCounts = levelBlockCounts level
        let chunkMin =
            v3i
                (chunkCoord.X * level.ChunkSizeVoxels.X)
                (chunkCoord.Y * level.ChunkSizeVoxels.Y)
                (chunkCoord.Z * level.ChunkSizeVoxels.Z)
        let chunkMaxExclusive = chunkMin + level.ChunkSizeVoxels
        let firstBlock sourceMin offset count =
            Math.Clamp (divCeilNonNegative (sourceMin - offset) side, 0, count)
        let lastBlock sourceMaxExclusive offset count =
            Math.Clamp ((sourceMaxExclusive - 1 - offset) / side, -1, count - 1)
        let minBlock =
            v3i
                (firstBlock chunkMin.X level.BlockGridOffsetVoxels.X blockCounts.X)
                (firstBlock chunkMin.Y level.BlockGridOffsetVoxels.Y blockCounts.Y)
                (firstBlock chunkMin.Z level.BlockGridOffsetVoxels.Z blockCounts.Z)
        let maxBlock =
            v3i
                (lastBlock chunkMaxExclusive.X level.BlockGridOffsetVoxels.X blockCounts.X)
                (lastBlock chunkMaxExclusive.Y level.BlockGridOffsetVoxels.Y blockCounts.Y)
                (lastBlock chunkMaxExclusive.Z level.BlockGridOffsetVoxels.Z blockCounts.Z)
        if minBlock.X <= maxBlock.X && minBlock.Y <= maxBlock.Y && minBlock.Z <= maxBlock.Z
        then Some struct (minBlock, maxBlock)
        else None

    let private blockHasCollision (level : VoxelLevel) (blockCoord : Vector3i) =
        let side = max 1 level.BlockSideVoxels
        let solidTarget =
            max 1 (int (MathF.Ceiling (single (side * side * side) * collisionSolidThreshold)))
        let start = VoxelWorld.blockStartCoord level blockCoord
        let mutable solidCount = 0
        let mutable y = 0
        while solidCount < solidTarget && y < side do
            let mutable z = 0
            while solidCount < solidTarget && z < side do
                let mutable x = 0
                while solidCount < solidTarget && x < side do
                    match VoxelWorld.tryGetCell level (v3i (start.X + x) (start.Y + y) (start.Z + z)) with
                    | Some cell when cell.Solid -> solidCount <- inc solidCount
                    | Some _ | None -> ()
                    x <- inc x
                z <- inc z
            y <- inc y
        solidCount >= solidTarget

    let private chunkBodyShapeFromBlocks (level : VoxelLevel) (chunkCoord : Vector3i) (renderCenter : Vector3) =
        match tryChunkBlockRange level chunkCoord with
        | Some (struct (minBlock, maxBlock)) ->
            let blockCounts = maxBlock - minBlock + v3iOne
            let filled = Array3D.zeroCreate<bool> blockCounts.X blockCounts.Y blockCounts.Z
            let visited = Array3D.zeroCreate<bool> blockCounts.X blockCounts.Y blockCounts.Z
            let solidBlockCoords = ResizeArray<Vector3i> ()
            let mutable occupiedAny = false
            for y in 0 .. dec blockCounts.Y do
                for z in 0 .. dec blockCounts.Z do
                    for x in 0 .. dec blockCounts.X do
                        let blockCoord = minBlock + v3i x y z
                        if blockHasCollision level blockCoord then
                            filled[x, y, z] <- true
                            solidBlockCoords.Add blockCoord
                            occupiedAny <- true
            if occupiedAny then
                let canUse x y z = filled[x, y, z] && not visited[x, y, z]
                let canGrowZ x y z sizeX sizeZ =
                    let z = z + sizeZ
                    let mutable canGrow = z < blockCounts.Z
                    let mutable ix = 0
                    while canGrow && ix < sizeX do
                        canGrow <- canUse (x + ix) y z
                        ix <- inc ix
                    canGrow
                let canGrowY x y z sizeX sizeY sizeZ =
                    let y = y + sizeY
                    let mutable canGrow = y < blockCounts.Y
                    let mutable iz = 0
                    while canGrow && iz < sizeZ do
                        let mutable ix = 0
                        while canGrow && ix < sizeX do
                            canGrow <- canUse (x + ix) y (z + iz)
                            ix <- inc ix
                        iz <- inc iz
                    canGrow
                let blockWorldSize =
                    v3
                        (single level.BlockSideVoxels * level.VoxelSize.X)
                        (single level.BlockSideVoxels * level.VoxelSize.Y)
                        (single level.BlockSideVoxels * level.VoxelSize.Z)
                let bodyShapes = List<BodyShape> ()
                let mutable occlusionMin = v3Dup Single.MaxValue
                let mutable occlusionMax = v3Dup Single.MinValue
                for y in 0 .. dec blockCounts.Y do
                    for z in 0 .. dec blockCounts.Z do
                        for x in 0 .. dec blockCounts.X do
                            if canUse x y z then
                                let mutable sizeX = 1
                                while x + sizeX < blockCounts.X && canUse (x + sizeX) y z do
                                    sizeX <- inc sizeX
                                let mutable sizeZ = 1
                                while canGrowZ x y z sizeX sizeZ do
                                    sizeZ <- inc sizeZ
                                let mutable sizeY = 1
                                while canGrowY x y z sizeX sizeY sizeZ do
                                    sizeY <- inc sizeY
                                for iy in 0 .. dec sizeY do
                                    for iz in 0 .. dec sizeZ do
                                        for ix in 0 .. dec sizeX do
                                            visited[x + ix, y + iy, z + iz] <- true
                                let blockCoord = minBlock + v3i x y z
                                let start = VoxelWorld.blockStartCoord level blockCoord
                                let boxSize =
                                    v3
                                        (single sizeX * blockWorldSize.X)
                                        (single sizeY * blockWorldSize.Y)
                                        (single sizeZ * blockWorldSize.Z)
                                let boxMin =
                                    level.Bounds.Min +
                                    v3
                                        (single start.X * level.VoxelSize.X)
                                        (single start.Y * level.VoxelSize.Y)
                                        (single start.Z * level.VoxelSize.Z)
                                let boxCenter = boxMin + boxSize * 0.5f
                                occlusionMin <- Vector3.Min (occlusionMin, boxMin)
                                occlusionMax <- Vector3.Max (occlusionMax, boxMin + boxSize)
                                bodyShapes.Add (BoxShape { Size = boxSize; TransformOpt = Some (Affine.makeTranslation (boxCenter - renderCenter)); PropertiesOpt = None })
                let occlusionBoundsOpt =
                    if bodyShapes.Count > 0
                    then Some (box3 occlusionMin (occlusionMax - occlusionMin))
                    else None
                struct (BodyShapes (bodyShapes |> Seq.toList), bodyShapes.Count, occlusionBoundsOpt, solidBlockCoords.ToArray ())
            else struct (EmptyShape, 0, None, [||])
        | None -> struct (EmptyShape, 0, None, [||])

    let rebuildChunk (level : VoxelLevel) (chunkCoord : Vector3i) (world : World) =
        let tryGetCell coord = VoxelWorld.tryGetCell level coord
        match VoxelBake.chunkModelFromCells level.ChunkSizeVoxels level.Bounds level.VoxelSize tryGetCell chunkCoord with
        | Some struct (renderCenter, voxelModelDescriptor) ->
            let struct (bodyShape, boxCount, occlusionBoundsOpt, solidBlockCoords) = chunkBodyShapeFromBlocks level chunkCoord renderCenter
            let revision = nextRevision level
            let voxelModel = chunkAssetTag chunkCoord revision
            World.createUserDefinedVoxelModel voxelModelDescriptor voxelModel world
            Some
                { ChunkCoord = chunkCoord
                  ChunkCenter = renderCenter + level.LevelOffset
                  ChunkSize = voxelModelDescriptor.Bounds.Size
                  BodyShape = bodyShape
                  BoxCount = boxCount
                  OcclusionBoundsOpt = occlusionBoundsOpt |> Option.map (fun bounds -> box3 (bounds.Min + level.LevelOffset) bounds.Size)
                  SolidBlockCoords = solidBlockCoords
                  VoxelModel = voxelModel }
        | None -> None

    let rebuildChunks (chunkCoords : Vector3i seq) (level : VoxelLevel) (currentChunks : VoxelChunk array) (world : World) =
        let chunks = Dictionary<Vector3i, VoxelChunk> (HashIdentity.Structural)
        let chunksToDestroy = ResizeArray<VoxelChunk> ()
        for chunk in currentChunks do
            chunks[chunk.ChunkCoord] <- chunk
        for chunkCoord in chunkCoords do
            match chunks.TryGetValue chunkCoord with
            | (true, oldChunk) -> chunksToDestroy.Add oldChunk
            | (false, _) -> ()
            match rebuildChunk level chunkCoord world with
            | Some chunk -> chunks[chunkCoord] <- chunk
            | None -> chunks.Remove chunkCoord |> ignore<bool>
        struct (sortVoxelChunks chunks.Values, chunksToDestroy.ToArray ())

    let destroyVoxelChunks (voxelChunks : VoxelChunk array) (world : World) =
        for chunk in voxelChunks do
            World.destroyUserDefinedVoxelModel chunk.VoxelModel world

    let destroyVoxelModel (voxelChunks : VoxelChunk array) (placeableBlocks : PlaceableBlock array) (levelOpt : VoxelLevel option) (world : World) =
        destroyVoxelChunks voxelChunks world
        for placeableBlock in placeableBlocks do
            World.destroyUserDefinedVoxelModel placeableBlock.PreviewModel world
        match levelOpt with
        | Some level ->
            for z in 0 .. dec level.ChunkCounts.Z do
                for y in 0 .. dec level.ChunkCounts.Y do
                    for x in 0 .. dec level.ChunkCounts.X do
                        World.destroyUserDefinedVoxelModel (Assets.Voxels.MinecraftLevelChunk x y z) world
        | None -> ()
