namespace VoxelForge
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module VoxelRuntime =

    type VoxelChunkBuild = VoxelForge.VoxelChunkBuild

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

    [<Struct>]
    type private TemplateSurfaceVoxel =
        { LocalCoord : Vector3i
          Cell : VoxelCell }

    [<Struct>]
    type private TemplateBlockInfo =
        { SurfaceVoxels : TemplateSurfaceVoxel array
          HasCollision : bool
          OpaqueOccluder : bool }

    let private voxelDirections =
        [|struct (v3iRight, v3Right, VoxelFaces.RightFace)
          struct (v3iLeft, v3Left, VoxelFaces.LeftFace)
          struct (v3iUp, v3Up, VoxelFaces.UpFace)
          struct (v3iDown, v3Down, VoxelFaces.DownFace)
          struct (v3iForward, v3Forward, VoxelFaces.ForwardFace)
          struct (v3iBack, v3Back, VoxelFaces.BackFace)|]

    let private templateBlockInfoCache = ConcurrentDictionary<string, TemplateBlockInfo> ()

    let private templateBlockInfoKey (template : VoxelBlockTemplate) =
        template.Name + "|" + string template.Material + "|" + string template.Solid + "|" + string template.Voxels.Length

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

    let private tryGetCachedGeneratedBlockTemplate (blockTemplateCache : Dictionary<Vector3i, VoxelBlockTemplate option>) (level : VoxelLevel) (blockCoord : Vector3i) =
        match blockTemplateCache.TryGetValue blockCoord with
        | (true, templateOpt) -> templateOpt
        | (false, _) ->
            let templateOpt = VoxelWorld.tryGetGeneratedBlockTemplateValue level blockCoord
            blockTemplateCache[blockCoord] <- templateOpt
            templateOpt

    let private chunkSourceMinCoord (level : VoxelLevel) (chunkCoord : Vector3i) =
        v3i
            (chunkCoord.X * level.ChunkSizeVoxels.X)
            (chunkCoord.Y * level.ChunkSizeVoxels.Y)
            (chunkCoord.Z * level.ChunkSizeVoxels.Z)

    let private chunkHasRelevantEdits (level : VoxelLevel) (chunkCoord : Vector3i) =
        if level.Edits.Count = 0 && level.BlockEdits.Count = 0 then false
        else
            let chunkMin = chunkSourceMinCoord level chunkCoord
            let chunkMax = chunkMin + level.ChunkSizeVoxels - v3iOne
            let minCoord =
                v3i
                    (max 0 (dec chunkMin.X))
                    (max 0 (dec chunkMin.Y))
                    (max 0 (dec chunkMin.Z))
            let maxCoord =
                v3i
                    (min (dec level.SourceSizeVoxels.X) (inc chunkMax.X))
                    (min (dec level.SourceSizeVoxels.Y) (inc chunkMax.Y))
                    (min (dec level.SourceSizeVoxels.Z) (inc chunkMax.Z))
            let mutable relevant = false
            let side = max 1 level.BlockSideVoxels
            lock level.BlockEdits (fun () ->
                for entry in level.BlockEdits do
                    if not relevant then
                        let blockStart = VoxelWorld.blockStartCoord level entry.Key
                        let blockMax = blockStart + v3i (dec side) (dec side) (dec side)
                        relevant <-
                            blockMax.X >= minCoord.X && blockStart.X <= maxCoord.X &&
                            blockMax.Y >= minCoord.Y && blockStart.Y <= maxCoord.Y &&
                            blockMax.Z >= minCoord.Z && blockStart.Z <= maxCoord.Z)
            if relevant then true
            else
                lock level.Edits (fun () ->
                    for entry in level.Edits do
                        if not relevant then
                            let coord = entry.Key
                            relevant <-
                                coord.X >= minCoord.X && coord.X <= maxCoord.X &&
                                coord.Y >= minCoord.Y && coord.Y <= maxCoord.Y &&
                                coord.Z >= minCoord.Z && coord.Z <= maxCoord.Z
                    relevant)

    let private canUseGeneratedChunkLookup (level : VoxelLevel) (chunkCoord : Vector3i) =
        Option.isSome level.GenerationOpt && level.SourceVoxels.Count = 0 && not (chunkHasRelevantEdits level chunkCoord)

    let private generatedChunkDefinitelyEmpty (level : VoxelLevel) (chunkCoord : Vector3i) =
        canUseGeneratedChunkLookup level chunkCoord &&
        match tryChunkBlockRange level chunkCoord with
        | Some (struct (minBlock, maxBlock)) ->
            let blockTemplateCache = Dictionary<Vector3i, VoxelBlockTemplate option> (HashIdentity.Structural)
            let mutable anyTemplate = false
            let mutable y = minBlock.Y
            while not anyTemplate && y <= maxBlock.Y do
                let mutable z = minBlock.Z
                while not anyTemplate && z <= maxBlock.Z do
                    let mutable x = minBlock.X
                    while not anyTemplate && x <= maxBlock.X do
                        anyTemplate <- Option.isSome (tryGetCachedGeneratedBlockTemplate blockTemplateCache level (v3i x y z))
                        x <- inc x
                    z <- inc z
                y <- inc y
            not anyTemplate
        | None -> true

    let private tryMakeGeneratedChunkCellLookup (level : VoxelLevel) (chunkCoord : Vector3i) =
        if canUseGeneratedChunkLookup level chunkCoord then
            let blockTemplateCache = Dictionary<Vector3i, VoxelBlockTemplate option> (HashIdentity.Structural)
            let mutable lastBlockCoord = v3i Int32.MinValue Int32.MinValue Int32.MinValue
            let mutable lastBlockStart = v3iZero
            let mutable lastTemplateOpt : VoxelBlockTemplate option = None
            Some
                (fun (coord : Vector3i) ->
                    if VoxelWorld.isSourceCoordInBounds level coord then
                        let blockCoord = VoxelWorld.sourceCoordToBlockCoord level coord
                        let templateOpt =
                            if blockCoord = lastBlockCoord then lastTemplateOpt
                            else
                                let templateOpt = tryGetCachedGeneratedBlockTemplate blockTemplateCache level blockCoord
                                lastBlockCoord <- blockCoord
                                lastBlockStart <- VoxelWorld.blockStartCoord level blockCoord
                                lastTemplateOpt <- templateOpt
                                templateOpt
                        match templateOpt with
                        | Some template -> VoxelWorld.tryGetGeneratedCellValueFromTemplateLocal level coord (coord - lastBlockStart) template
                        | None -> ValueNone
                    else ValueNone)
        else None

    let private blockHasCollision (tryGetCell : Vector3i -> VoxelCell voption) (level : VoxelLevel) (blockCoord : Vector3i) =
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
                    match tryGetCell (v3i (start.X + x) (start.Y + y) (start.Z + z)) with
                    | ValueSome cell when cell.Solid -> solidCount <- inc solidCount
                    | ValueSome _ | ValueNone -> ()
                    x <- inc x
                z <- inc z
            y <- inc y
        solidCount >= solidTarget

    let private isOpaqueCell (cell : VoxelCell) =
        cell.Solid &&
        match cell.Material with
        | Grass | Dirt | Stone | Sand | Wood | Ore | Brick | Crafted -> true
        | Leaves | Glass | Water | Lava -> false

    let private isLocalBlockCoord (side : int) (coord : Vector3i) =
        coord.X >= 0 && coord.X < side &&
        coord.Y >= 0 && coord.Y < side &&
        coord.Z >= 0 && coord.Z < side

    let private blockFaceOpaqueInTemplate (side : int) (template : VoxelBlockTemplate) faceIndex =
        let isOpaque x y z =
            match template.Cells.TryGetValue (v3i x y z) with
            | (true, cell) when isOpaqueCell cell -> true
            | (true, _) | (false, _) -> false
        let mutable opaque = true
        match faceIndex with
        | 0 ->
            let mutable y = 0
            while opaque && y < side do
                let mutable z = 0
                while opaque && z < side do
                    opaque <- isOpaque 0 y z
                    z <- inc z
                y <- inc y
        | 1 ->
            let mutable y = 0
            while opaque && y < side do
                let mutable z = 0
                while opaque && z < side do
                    opaque <- isOpaque (dec side) y z
                    z <- inc z
                y <- inc y
        | 2 ->
            let mutable z = 0
            while opaque && z < side do
                let mutable x = 0
                while opaque && x < side do
                    opaque <- isOpaque x 0 z
                    x <- inc x
                z <- inc z
        | 3 ->
            let mutable z = 0
            while opaque && z < side do
                let mutable x = 0
                while opaque && x < side do
                    opaque <- isOpaque x (dec side) z
                    x <- inc x
                z <- inc z
        | 4 ->
            let mutable y = 0
            while opaque && y < side do
                let mutable x = 0
                while opaque && x < side do
                    opaque <- isOpaque x y 0
                    x <- inc x
                y <- inc y
        | 5 ->
            let mutable y = 0
            while opaque && y < side do
                let mutable x = 0
                while opaque && x < side do
                    opaque <- isOpaque x y (dec side)
                    x <- inc x
                y <- inc y
        | _ -> opaque <- false
        opaque

    let private getTemplateBlockInfo (side : int) (template : VoxelBlockTemplate) =
        let key = templateBlockInfoKey template
        templateBlockInfoCache.GetOrAdd
            (key,
             Func<string, TemplateBlockInfo> (fun _ ->
                let surfaceVoxels = ResizeArray<TemplateSurfaceVoxel> ()
                for struct (localCoord, cell) in template.Voxels do
                    let mutable surface = false
                    let mutable i = 0
                    while not surface && i < voxelDirections.Length do
                        let struct (offset, _, _) = voxelDirections[i]
                        let neighbor = localCoord + offset
                        surface <-
                            not (isLocalBlockCoord side neighbor) ||
                            not (template.Cells.ContainsKey neighbor)
                        i <- inc i
                    if surface then
                        surfaceVoxels.Add { LocalCoord = localCoord; Cell = cell }
                let solidTarget =
                    max 1 (int (MathF.Ceiling (single (side * side * side) * collisionSolidThreshold)))
                let opaqueOccluder =
                    blockFaceOpaqueInTemplate side template 0 &&
                    blockFaceOpaqueInTemplate side template 1 &&
                    blockFaceOpaqueInTemplate side template 2 &&
                    blockFaceOpaqueInTemplate side template 3 &&
                    blockFaceOpaqueInTemplate side template 4 &&
                    blockFaceOpaqueInTemplate side template 5
                { SurfaceVoxels = surfaceVoxels.ToArray ()
                  HasCollision = template.Solid && template.Voxels.Length >= solidTarget
                  OpaqueOccluder = opaqueOccluder }))

    let private blockFaceOpaque (tryGetCell : Vector3i -> VoxelCell voption) (level : VoxelLevel) (blockCoord : Vector3i) faceIndex =
        let side = max 1 level.BlockSideVoxels
        let start = VoxelWorld.blockStartCoord level blockCoord
        let isOpaque x y z =
            match tryGetCell (v3i (start.X + x) (start.Y + y) (start.Z + z)) with
            | ValueSome cell when isOpaqueCell cell -> true
            | ValueSome _ | ValueNone -> false
        let mutable opaque = true
        match faceIndex with
        | 0 ->
            let mutable y = 0
            while opaque && y < side do
                let mutable z = 0
                while opaque && z < side do
                    opaque <- isOpaque 0 y z
                    z <- inc z
                y <- inc y
        | 1 ->
            let mutable y = 0
            while opaque && y < side do
                let mutable z = 0
                while opaque && z < side do
                    opaque <- isOpaque (dec side) y z
                    z <- inc z
                y <- inc y
        | 2 ->
            let mutable z = 0
            while opaque && z < side do
                let mutable x = 0
                while opaque && x < side do
                    opaque <- isOpaque x 0 z
                    x <- inc x
                z <- inc z
        | 3 ->
            let mutable z = 0
            while opaque && z < side do
                let mutable x = 0
                while opaque && x < side do
                    opaque <- isOpaque x (dec side) z
                    x <- inc x
                z <- inc z
        | 4 ->
            let mutable y = 0
            while opaque && y < side do
                let mutable x = 0
                while opaque && x < side do
                    opaque <- isOpaque x y 0
                    x <- inc x
                y <- inc y
        | 5 ->
            let mutable y = 0
            while opaque && y < side do
                let mutable x = 0
                while opaque && x < side do
                    opaque <- isOpaque x y (dec side)
                    x <- inc x
                y <- inc y
        | _ -> opaque <- false
        opaque

    let private blockIsOpaqueOccluder tryGetCell level blockCoord =
        blockFaceOpaque tryGetCell level blockCoord 0 &&
        blockFaceOpaque tryGetCell level blockCoord 1 &&
        blockFaceOpaque tryGetCell level blockCoord 2 &&
        blockFaceOpaque tryGetCell level blockCoord 3 &&
        blockFaceOpaque tryGetCell level blockCoord 4 &&
        blockFaceOpaque tryGetCell level blockCoord 5

    let private computeFaceMask (filled : bool[,,]) (blockCounts : Vector3i) =
        let mutable mask = 0
        let mutable faceCovered = blockCounts.X > 0 && blockCounts.Y > 0 && blockCounts.Z > 0
        let mutable y = 0
        while faceCovered && y < blockCounts.Y do
            let mutable z = 0
            while faceCovered && z < blockCounts.Z do
                faceCovered <- filled[0, y, z]
                z <- inc z
            y <- inc y
        if faceCovered then mask <- mask ||| 1
        faceCovered <- blockCounts.X > 0 && blockCounts.Y > 0 && blockCounts.Z > 0
        y <- 0
        while faceCovered && y < blockCounts.Y do
            let mutable z = 0
            while faceCovered && z < blockCounts.Z do
                faceCovered <- filled[dec blockCounts.X, y, z]
                z <- inc z
            y <- inc y
        if faceCovered then mask <- mask ||| 2
        faceCovered <- blockCounts.X > 0 && blockCounts.Y > 0 && blockCounts.Z > 0
        let mutable z = 0
        while faceCovered && z < blockCounts.Z do
            let mutable x = 0
            while faceCovered && x < blockCounts.X do
                faceCovered <- filled[x, 0, z]
                x <- inc x
            z <- inc z
        if faceCovered then mask <- mask ||| 4
        faceCovered <- blockCounts.X > 0 && blockCounts.Y > 0 && blockCounts.Z > 0
        z <- 0
        while faceCovered && z < blockCounts.Z do
            let mutable x = 0
            while faceCovered && x < blockCounts.X do
                faceCovered <- filled[x, dec blockCounts.Y, z]
                x <- inc x
            z <- inc z
        if faceCovered then mask <- mask ||| 8
        faceCovered <- blockCounts.X > 0 && blockCounts.Y > 0 && blockCounts.Z > 0
        y <- 0
        while faceCovered && y < blockCounts.Y do
            let mutable x = 0
            while faceCovered && x < blockCounts.X do
                faceCovered <- filled[x, y, 0]
                x <- inc x
            y <- inc y
        if faceCovered then mask <- mask ||| 16
        faceCovered <- blockCounts.X > 0 && blockCounts.Y > 0 && blockCounts.Z > 0
        y <- 0
        while faceCovered && y < blockCounts.Y do
            let mutable x = 0
            while faceCovered && x < blockCounts.X do
                faceCovered <- filled[x, y, dec blockCounts.Z]
                x <- inc x
            y <- inc y
        if faceCovered then mask <- mask ||| 32
        mask

    let private chunkBodyShapeFromBlocks (tryGetCell : Vector3i -> VoxelCell voption) (level : VoxelLevel) (chunkCoord : Vector3i) (renderCenter : Vector3) =
        match tryChunkBlockRange level chunkCoord with
        | Some (struct (minBlock, maxBlock)) ->
            let blockCounts = maxBlock - minBlock + v3iOne
            let filled = Array3D.zeroCreate<bool> blockCounts.X blockCounts.Y blockCounts.Z
            let opaque = Array3D.zeroCreate<bool> blockCounts.X blockCounts.Y blockCounts.Z
            let visited = Array3D.zeroCreate<bool> blockCounts.X blockCounts.Y blockCounts.Z
            let solidBlockCoords = ResizeArray<Vector3i> ()
            let opaqueBlockCoords = ResizeArray<Vector3i> ()
            let mutable occupiedAny = false
            let mutable opaqueAny = false
            for y in 0 .. dec blockCounts.Y do
                for z in 0 .. dec blockCounts.Z do
                    for x in 0 .. dec blockCounts.X do
                        let blockCoord = minBlock + v3i x y z
                        if blockHasCollision tryGetCell level blockCoord then
                            filled[x, y, z] <- true
                            solidBlockCoords.Add blockCoord
                            occupiedAny <- true
                        if blockIsOpaqueOccluder tryGetCell level blockCoord then
                            opaque[x, y, z] <- true
                            opaqueBlockCoords.Add blockCoord
                            opaqueAny <- true
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
                let opaqueOccluderBoxes =
                    if opaqueAny then
                        let visitedOpaque = Array3D.zeroCreate<bool> blockCounts.X blockCounts.Y blockCounts.Z
                        let canUseOpaque x y z = opaque[x, y, z] && not visitedOpaque[x, y, z]
                        let canGrowOpaqueZ x y z sizeX sizeZ =
                            let z = z + sizeZ
                            let mutable canGrow = z < blockCounts.Z
                            let mutable ix = 0
                            while canGrow && ix < sizeX do
                                canGrow <- canUseOpaque (x + ix) y z
                                ix <- inc ix
                            canGrow
                        let canGrowOpaqueY x y z sizeX sizeY sizeZ =
                            let y = y + sizeY
                            let mutable canGrow = y < blockCounts.Y
                            let mutable iz = 0
                            while canGrow && iz < sizeZ do
                                let mutable ix = 0
                                while canGrow && ix < sizeX do
                                    canGrow <- canUseOpaque (x + ix) y (z + iz)
                                    ix <- inc ix
                                iz <- inc iz
                            canGrow
                        [|for y in 0 .. dec blockCounts.Y do
                            for z in 0 .. dec blockCounts.Z do
                                for x in 0 .. dec blockCounts.X do
                                    if canUseOpaque x y z then
                                        let mutable sizeX = 1
                                        while x + sizeX < blockCounts.X && canUseOpaque (x + sizeX) y z do
                                            sizeX <- inc sizeX
                                        let mutable sizeZ = 1
                                        while canGrowOpaqueZ x y z sizeX sizeZ do
                                            sizeZ <- inc sizeZ
                                        let mutable sizeY = 1
                                        while canGrowOpaqueY x y z sizeX sizeY sizeZ do
                                            sizeY <- inc sizeY
                                        for iy in 0 .. dec sizeY do
                                            for iz in 0 .. dec sizeZ do
                                                for ix in 0 .. dec sizeX do
                                                    visitedOpaque[x + ix, y + iy, z + iz] <- true
                                        let blockCoord = minBlock + v3i x y z
                                        let start = VoxelWorld.blockStartCoord level blockCoord
                                        let boxSize =
                                            v3
                                                (single sizeX * blockWorldSize.X)
                                                (single sizeY * blockWorldSize.Y)
                                                (single sizeZ * blockWorldSize.Z)
                                        let boxMin =
                                            level.Bounds.Min + level.LevelOffset +
                                            v3
                                                (single start.X * level.VoxelSize.X)
                                                (single start.Y * level.VoxelSize.Y)
                                                (single start.Z * level.VoxelSize.Z)
                                        yield box3 boxMin boxSize|]
                    else [||]
                let opaqueFaceMask = computeFaceMask opaque blockCounts
                let fullOpaqueChunk = opaqueBlockCoords.Count = blockCounts.X * blockCounts.Y * blockCounts.Z
                struct (BodyShapes (bodyShapes |> Seq.toList), bodyShapes.Count, occlusionBoundsOpt, solidBlockCoords.ToArray (), opaqueBlockCoords.ToArray (), opaqueOccluderBoxes, opaqueFaceMask, fullOpaqueChunk)
            else struct (EmptyShape, 0, None, [||], [||], [||], 0, false)
        | None -> struct (EmptyShape, 0, None, [||], [||], [||], 0, false)

    let private chunkBodyShapeFromBlockMasks (level : VoxelLevel) (minBlock : Vector3i) (blockCounts : Vector3i) (filled : bool[,,]) (opaque : bool[,,]) (solidBlockCoords : ResizeArray<Vector3i>) (opaqueBlockCoords : ResizeArray<Vector3i>) (renderCenter : Vector3) =
        if solidBlockCoords.Count > 0 then
            let visited = Array3D.zeroCreate<bool> blockCounts.X blockCounts.Y blockCounts.Z
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
            let opaqueOccluderBoxes =
                if opaqueBlockCoords.Count > 0 then
                    let visitedOpaque = Array3D.zeroCreate<bool> blockCounts.X blockCounts.Y blockCounts.Z
                    let canUseOpaque x y z = opaque[x, y, z] && not visitedOpaque[x, y, z]
                    let canGrowOpaqueZ x y z sizeX sizeZ =
                        let z = z + sizeZ
                        let mutable canGrow = z < blockCounts.Z
                        let mutable ix = 0
                        while canGrow && ix < sizeX do
                            canGrow <- canUseOpaque (x + ix) y z
                            ix <- inc ix
                        canGrow
                    let canGrowOpaqueY x y z sizeX sizeY sizeZ =
                        let y = y + sizeY
                        let mutable canGrow = y < blockCounts.Y
                        let mutable iz = 0
                        while canGrow && iz < sizeZ do
                            let mutable ix = 0
                            while canGrow && ix < sizeX do
                                canGrow <- canUseOpaque (x + ix) y (z + iz)
                                ix <- inc ix
                            iz <- inc iz
                        canGrow
                    [|for y in 0 .. dec blockCounts.Y do
                        for z in 0 .. dec blockCounts.Z do
                            for x in 0 .. dec blockCounts.X do
                                if canUseOpaque x y z then
                                    let mutable sizeX = 1
                                    while x + sizeX < blockCounts.X && canUseOpaque (x + sizeX) y z do
                                        sizeX <- inc sizeX
                                    let mutable sizeZ = 1
                                    while canGrowOpaqueZ x y z sizeX sizeZ do
                                        sizeZ <- inc sizeZ
                                    let mutable sizeY = 1
                                    while canGrowOpaqueY x y z sizeX sizeY sizeZ do
                                        sizeY <- inc sizeY
                                    for iy in 0 .. dec sizeY do
                                        for iz in 0 .. dec sizeZ do
                                            for ix in 0 .. dec sizeX do
                                                visitedOpaque[x + ix, y + iy, z + iz] <- true
                                    let blockCoord = minBlock + v3i x y z
                                    let start = VoxelWorld.blockStartCoord level blockCoord
                                    let boxSize =
                                        v3
                                            (single sizeX * blockWorldSize.X)
                                            (single sizeY * blockWorldSize.Y)
                                            (single sizeZ * blockWorldSize.Z)
                                    let boxMin =
                                        level.Bounds.Min + level.LevelOffset +
                                        v3
                                            (single start.X * level.VoxelSize.X)
                                            (single start.Y * level.VoxelSize.Y)
                                            (single start.Z * level.VoxelSize.Z)
                                    yield box3 boxMin boxSize|]
                else [||]
            let opaqueFaceMask = computeFaceMask opaque blockCounts
            let fullOpaqueChunk = opaqueBlockCoords.Count = blockCounts.X * blockCounts.Y * blockCounts.Z
            struct (BodyShapes (bodyShapes |> Seq.toList), bodyShapes.Count, occlusionBoundsOpt, solidBlockCoords.ToArray (), opaqueBlockCoords.ToArray (), opaqueOccluderBoxes, opaqueFaceMask, fullOpaqueChunk)
        else struct (EmptyShape, 0, None, [||], [||], [||], 0, false)

    let private tryBuildChunkFromBlockLookup (level : VoxelLevel) (chunkCoord : Vector3i) (tryGetBlockTemplate : Vector3i -> VoxelBlockTemplate option) =
        match tryChunkBlockRange level chunkCoord with
            | Some (struct (minBlock, maxBlock)) ->
                let side = max 1 level.BlockSideVoxels
                let blockCounts = maxBlock - minBlock + v3iOne
                let filled = Array3D.zeroCreate<bool> blockCounts.X blockCounts.Y blockCounts.Z
                let opaque = Array3D.zeroCreate<bool> blockCounts.X blockCounts.Y blockCounts.Z
                let solidBlockCoords = ResizeArray<Vector3i> ()
                let opaqueBlockCoords = ResizeArray<Vector3i> ()
                let splats = List<VoxelSplat> ()
                let occupiedVoxels = ResizeArray<struct (Vector3i * Color)> ()
                let globalMinCoord = chunkSourceMinCoord level chunkCoord
                let mutable occupiedAny = false
                let origin = level.Bounds.Min
                for y in 0 .. dec blockCounts.Y do
                    for z in 0 .. dec blockCounts.Z do
                        for x in 0 .. dec blockCounts.X do
                            let blockCoord = minBlock + v3i x y z
                            match tryGetBlockTemplate blockCoord with
                            | Some template ->
                                occupiedAny <- true
                                let info = getTemplateBlockInfo side template
                                if info.HasCollision then
                                    filled[x, y, z] <- true
                                    solidBlockCoords.Add blockCoord
                                if info.OpaqueOccluder then
                                    opaque[x, y, z] <- true
                                    opaqueBlockCoords.Add blockCoord
                                let blockStart = VoxelWorld.blockStartCoord level blockCoord
                                for struct (localCoord, cell) in template.Voxels do
                                    occupiedVoxels.Add (struct (blockStart + localCoord - globalMinCoord, cell.Albedo))
                                for surfaceVoxel in info.SurfaceVoxels do
                                    let mutable exposed = false
                                    let mutable normal = v3Zero
                                    let mutable faces = VoxelFaces.NoFaces
                                    for struct (offset, direction, face) in voxelDirections do
                                        let localNeighbor = surfaceVoxel.LocalCoord + offset
                                        let neighborOccupied =
                                            if isLocalBlockCoord side localNeighbor then
                                                template.Cells.ContainsKey localNeighbor
                                            else
                                                let blockOffset =
                                                    v3i
                                                        (if localNeighbor.X < 0 then -1 elif localNeighbor.X >= side then 1 else 0)
                                                        (if localNeighbor.Y < 0 then -1 elif localNeighbor.Y >= side then 1 else 0)
                                                        (if localNeighbor.Z < 0 then -1 elif localNeighbor.Z >= side then 1 else 0)
                                                let wrappedLocal =
                                                    v3i
                                                        (if localNeighbor.X < 0 then dec side elif localNeighbor.X >= side then 0 else localNeighbor.X)
                                                        (if localNeighbor.Y < 0 then dec side elif localNeighbor.Y >= side then 0 else localNeighbor.Y)
                                                        (if localNeighbor.Z < 0 then dec side elif localNeighbor.Z >= side then 0 else localNeighbor.Z)
                                                match tryGetBlockTemplate (blockCoord + blockOffset) with
                                                | Some neighborTemplate -> neighborTemplate.Cells.ContainsKey wrappedLocal
                                                | None -> false
                                        if not neighborOccupied then
                                            exposed <- true
                                            normal <- normal + direction
                                            faces <- faces ||| face
                                    if exposed then
                                        let sourceCoord = blockStart + surfaceVoxel.LocalCoord
                                        splats.Add
                                            { Position =
                                                origin +
                                                v3
                                                    ((single sourceCoord.X + 0.5f) * level.VoxelSize.X)
                                                    ((single sourceCoord.Y + 0.5f) * level.VoxelSize.Y)
                                                    ((single sourceCoord.Z + 0.5f) * level.VoxelSize.Z)
                                              Albedo = surfaceVoxel.Cell.Albedo
                                              Normal = if normal.LengthSquared () > 0.0f then normal.Normalized else v3Up
                                              Faces = faces }
                            | None -> ()
                if occupiedAny then
                    let chunkWorldSize =
                        v3
                            (single level.ChunkSizeVoxels.X * level.VoxelSize.X)
                            (single level.ChunkSizeVoxels.Y * level.VoxelSize.Y)
                            (single level.ChunkSizeVoxels.Z * level.VoxelSize.Z)
                    let chunkMin =
                        origin +
                        v3
                            (single globalMinCoord.X * level.VoxelSize.X)
                            (single globalMinCoord.Y * level.VoxelSize.Y)
                            (single globalMinCoord.Z * level.VoxelSize.Z)
                    let chunkCenter = chunkMin + chunkWorldSize * 0.5f
                    let renderCenter = chunkCenter
                    let descriptorBounds = box3 (chunkWorldSize * -0.5f) chunkWorldSize
                    let splatsArray = Array.zeroCreate<VoxelSplat> splats.Count
                    for i in 0 .. dec splats.Count do
                        let splat = splats[i]
                        splatsArray[i] <- { splat with Position = splat.Position - renderCenter }
                    let gridOrigin = descriptorBounds.Min + level.VoxelSize * 0.5f
                    let grid = VoxelBake.makeGridDescriptor level.ChunkSizeVoxels gridOrigin level.VoxelSize occupiedVoxels
                    let voxelModelDescriptor =
                        { Splats = splatsArray
                          Grid = Some grid
                          Bounds = descriptorBounds
                          VoxelSize = level.VoxelSize }
                    let struct (bodyShape, boxCount, occlusionBoundsOpt, solidBlockCoords, opaqueBlockCoords, opaqueOccluderBoxes, opaqueFaceMask, fullOpaqueChunk) =
                        chunkBodyShapeFromBlockMasks level minBlock blockCounts filled opaque solidBlockCoords opaqueBlockCoords renderCenter
                    Some
                        { ChunkCoord = chunkCoord
                          ChunkCenter = renderCenter + level.LevelOffset
                          ChunkSize = voxelModelDescriptor.Bounds.Size
                          VoxelModelDescriptor = voxelModelDescriptor
                          BodyShape = bodyShape
                          BoxCount = boxCount
                          OcclusionBoundsOpt = occlusionBoundsOpt |> Option.map (fun bounds -> box3 (bounds.Min + level.LevelOffset) bounds.Size)
                          SolidBlockCoords = solidBlockCoords
                          SplatCount = splatsArray.Length
                          OpaqueBlockCoords = opaqueBlockCoords
                          OpaqueOccluderBoxes = opaqueOccluderBoxes
                          OpaqueFaceMask = opaqueFaceMask
                          FullOpaqueChunk = fullOpaqueChunk }
                else None
            | None -> None

    let private tryBuildGeneratedChunkFromBlocks (level : VoxelLevel) (chunkCoord : Vector3i) =
        if not (canUseGeneratedChunkLookup level chunkCoord) then None
        else
            let blockTemplateCache = Dictionary<Vector3i, VoxelBlockTemplate option> (HashIdentity.Structural)
            let tryGetBlockTemplate blockCoord = tryGetCachedGeneratedBlockTemplate blockTemplateCache level blockCoord
            tryBuildChunkFromBlockLookup level chunkCoord tryGetBlockTemplate

    let private tryBuildChunkWithEditSnapshotFromBlocks (level : VoxelLevel) (snapshot : VoxelEditSnapshot) (chunkCoord : Vector3i) =
        if Option.isNone level.GenerationOpt || level.SourceVoxels.Count <> 0 then ValueNone
        else
            let side = max 1 level.BlockSideVoxels
            let blockTemplateCache = Dictionary<Vector3i, VoxelBlockTemplate option> (HashIdentity.Structural)
            let editedBlockTemplateCache = Dictionary<Vector3i, VoxelBlockTemplate option> (HashIdentity.Structural)
            let applyCellEdits (blockCoord : Vector3i) (generatedTemplateOpt : VoxelBlockTemplate option) (edits : Dictionary<Vector3i, VoxelEdit>) =
                let blockStart = VoxelWorld.blockStartCoord level blockCoord
                let mutable edited = false
                let cells =
                    match generatedTemplateOpt with
                    | Some template -> Dictionary<Vector3i, VoxelCell> (template.Cells, HashIdentity.Structural)
                    | None -> Dictionary<Vector3i, VoxelCell> (HashIdentity.Structural)
                for entry in edits do
                    let coord = entry.Key
                    if  coord.X >= blockStart.X && coord.X < blockStart.X + side &&
                        coord.Y >= blockStart.Y && coord.Y < blockStart.Y + side &&
                        coord.Z >= blockStart.Z && coord.Z < blockStart.Z + side then
                        edited <- true
                        let localCoord = coord - blockStart
                        match entry.Value with
                        | Removed -> cells.Remove localCoord |> ignore<bool>
                        | Placed cell -> cells[localCoord] <- cell
                if edited then
                    if cells.Count = 0 then None
                    else
                        let voxels = Array.zeroCreate<struct (Vector3i * VoxelCell)> cells.Count
                        let mutable i = 0
                        let mutable solid = false
                        let mutable material = Crafted
                        let mutable first = true
                        for entry in cells do
                            let cell = entry.Value
                            if first then
                                material <- cell.Material
                                first <- false
                            solid <- solid || cell.Solid
                            voxels[i] <- struct (entry.Key, cell)
                            i <- inc i
                        Some
                            { Name = "Edited " + string snapshot.Revision + " " + string blockCoord.X + "," + string blockCoord.Y + "," + string blockCoord.Z
                              Material = material
                              Solid = solid
                              Voxels = voxels
                              Cells = cells }
                else generatedTemplateOpt
            let tryGetEditedBlockTemplate blockCoord =
                match editedBlockTemplateCache.TryGetValue blockCoord with
                | (true, templateOpt) -> templateOpt
                | (false, _) ->
                    let generatedTemplateOpt = tryGetCachedGeneratedBlockTemplate blockTemplateCache level blockCoord
                    let templateOpt =
                        match snapshot.BlockEditsOpt with
                        | Some blockEdits ->
                            match blockEdits.TryGetValue blockCoord with
                            | (true, templateOpt) -> templateOpt
                            | (false, _) ->
                                match snapshot.EditsOpt with
                                | Some edits -> applyCellEdits blockCoord generatedTemplateOpt edits
                                | None -> generatedTemplateOpt
                        | None ->
                            match snapshot.EditsOpt with
                            | Some edits -> applyCellEdits blockCoord generatedTemplateOpt edits
                            | None -> generatedTemplateOpt
                    editedBlockTemplateCache[blockCoord] <- templateOpt
                    templateOpt
            ValueSome (tryBuildChunkFromBlockLookup level chunkCoord tryGetEditedBlockTemplate)

    let tryBuildChunkWithCellLookup (level : VoxelLevel) (tryGetCell : Vector3i -> VoxelCell voption) (chunkCoord : Vector3i) =
        if generatedChunkDefinitelyEmpty level chunkCoord then None
        elif canUseGeneratedChunkLookup level chunkCoord then tryBuildGeneratedChunkFromBlocks level chunkCoord
        else
            let tryGetCell =
                match tryMakeGeneratedChunkCellLookup level chunkCoord with
                | Some tryGetCell -> tryGetCell
                | None -> tryGetCell
            match VoxelBake.chunkModelFromCells level.ChunkSizeVoxels level.Bounds level.VoxelSize tryGetCell chunkCoord with
            | Some struct (renderCenter, voxelModelDescriptor) ->
                let struct (bodyShape, boxCount, occlusionBoundsOpt, solidBlockCoords, opaqueBlockCoords, opaqueOccluderBoxes, opaqueFaceMask, fullOpaqueChunk) = chunkBodyShapeFromBlocks tryGetCell level chunkCoord renderCenter
                let splatCount = voxelModelDescriptor.Splats.Length
                Some
                    { ChunkCoord = chunkCoord
                      ChunkCenter = renderCenter + level.LevelOffset
                      ChunkSize = voxelModelDescriptor.Bounds.Size
                      VoxelModelDescriptor = voxelModelDescriptor
                      BodyShape = bodyShape
                      BoxCount = boxCount
                      OcclusionBoundsOpt = occlusionBoundsOpt |> Option.map (fun bounds -> box3 (bounds.Min + level.LevelOffset) bounds.Size)
                      SolidBlockCoords = solidBlockCoords
                      SplatCount = splatCount
                      OpaqueBlockCoords = opaqueBlockCoords
                      OpaqueOccluderBoxes = opaqueOccluderBoxes
                      OpaqueFaceMask = opaqueFaceMask
                      FullOpaqueChunk = fullOpaqueChunk }
            | None -> None

    let tryBuildChunk (level : VoxelLevel) (chunkCoord : Vector3i) =
        let tryGetCell coord = VoxelWorld.tryGetCellValue level coord
        tryBuildChunkWithCellLookup level tryGetCell chunkCoord

    let private chunkBuildCacheMagic = "VFCB"
    let private chunkBuildCacheVersion = 9
    let private chunkBuildCacheMaxBytes = 8L * 1024L * 1024L * 1024L
    let private chunkBuildCacheTrimEvery = 64
    let private chunkBuildCacheTrimLock = obj ()
    let mutable private chunkBuildCacheSaveCount = 0

    let private writeVector3i (writer : BinaryWriter) (value : Vector3i) =
        writer.Write value.X
        writer.Write value.Y
        writer.Write value.Z

    let private readVector3i (reader : BinaryReader) =
        v3i (reader.ReadInt32 ()) (reader.ReadInt32 ()) (reader.ReadInt32 ())

    let private writeVector3 (writer : BinaryWriter) (value : Vector3) =
        writer.Write value.X
        writer.Write value.Y
        writer.Write value.Z

    let private readVector3 (reader : BinaryReader) =
        v3 (reader.ReadSingle ()) (reader.ReadSingle ()) (reader.ReadSingle ())

    let private writeColor (writer : BinaryWriter) (value : Color) =
        writer.Write value.R
        writer.Write value.G
        writer.Write value.B
        writer.Write value.A

    let private readColor (reader : BinaryReader) =
        color (reader.ReadSingle ()) (reader.ReadSingle ()) (reader.ReadSingle ()) (reader.ReadSingle ())

    let private writeBox3 (writer : BinaryWriter) (value : Box3) =
        writeVector3 writer value.Min
        writeVector3 writer value.Size

    let private readBox3 (reader : BinaryReader) =
        let min = readVector3 reader
        let size = readVector3 reader
        box3 min size

    let private chunkBuildCacheSignature (level : VoxelLevel) =
        match level.GenerationOpt with
        | Some generation ->
            String.Join
                ("|",
                 [|string level.WorldSizeBlocks.X; string level.WorldSizeBlocks.Y; string level.WorldSizeBlocks.Z
                   string level.ActiveBlockOrigin.X; string level.ActiveBlockOrigin.Y; string level.ActiveBlockOrigin.Z
                   string level.SourceSizeVoxels.X; string level.SourceSizeVoxels.Y; string level.SourceSizeVoxels.Z
                   string level.ChunkCounts.X; string level.ChunkCounts.Y; string level.ChunkCounts.Z
                   string level.ChunkSizeVoxels.X; string level.ChunkSizeVoxels.Y; string level.ChunkSizeVoxels.Z
                   string level.BlockSideVoxels
                   string level.BlockGridOffsetVoxels.X; string level.BlockGridOffsetVoxels.Y; string level.BlockGridOffsetVoxels.Z
                   string level.VoxelSize.X; string level.VoxelSize.Y; string level.VoxelSize.Z
                   string generation.Seed
                   string generation.SeaLevelBlocks
                   string generation.LavaLevelBlocks
                   string generation.TerrainScale
                   string generation.MountainStrength
                   string generation.CaveThreshold
                   string generation.OreRate
                   string generation.TreeRate|])
        | None -> String.Empty

    let private safePathPart (value : string) =
        let invalidChars = Path.GetInvalidFileNameChars ()
        let chars = value.ToCharArray ()
        for i in 0 .. dec chars.Length do
            if Array.contains chars[i] invalidChars then chars[i] <- '_'
        String (chars)

    let private canUseChunkBuildCache (level : VoxelLevel) =
        Option.isSome level.GenerationOpt

    let shouldPrebuildChunkBuildCache (_level : VoxelLevel) =
        false

    let private chunkBuildCacheDirectoryPath (level : VoxelLevel) =
        let signature = chunkBuildCacheSignature level
        let safeSignature =
            signature.Replace("|", "_").Replace("-", "m").Replace(".", "p").Replace(",", "p")
            |> safePathPart
        Path.Combine
            (Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData,
             "VoxelForge",
             "ChunkBuilds",
             "v" + string chunkBuildCacheVersion,
             safeSignature)

    let private chunkBuildCacheFilePath (level : VoxelLevel) (chunkCoord : Vector3i) =
        Path.Combine (chunkBuildCacheDirectoryPath level, "chunk_" + string chunkCoord.X + "_" + string chunkCoord.Y + "_" + string chunkCoord.Z + ".vfcb")

    let private chunkBuildCacheCompleteFilePath (level : VoxelLevel) =
        Path.Combine (chunkBuildCacheDirectoryPath level, "complete.vfcbc")

    let private trimChunkBuildCache (level : VoxelLevel) =
        try
            let directoryPath = chunkBuildCacheDirectoryPath level
            if Directory.Exists directoryPath then
                let files =
                    Directory.GetFiles (directoryPath, "*.vfcb", SearchOption.TopDirectoryOnly)
                    |> Array.map FileInfo
                let mutable totalBytes = 0L
                for file in files do
                    totalBytes <- totalBytes + file.Length
                if totalBytes > chunkBuildCacheMaxBytes then
                    let files = files |> Array.sortBy (fun file -> file.LastAccessTimeUtc.Ticks, file.LastWriteTimeUtc.Ticks)
                    let mutable i = 0
                    while totalBytes > chunkBuildCacheMaxBytes && i < files.Length do
                        let file = files[i]
                        try
                            let length = file.Length
                            file.Delete ()
                            totalBytes <- totalBytes - length
                        with _ -> ()
                        i <- inc i
        with exn ->
            Log.warnOnce ("VoxelForge failed to trim chunk build cache due to: " + scstring exn)

    let private noteChunkBuildCacheSave (level : VoxelLevel) =
        let shouldTrim =
            lock chunkBuildCacheTrimLock (fun () ->
                chunkBuildCacheSaveCount <- inc chunkBuildCacheSaveCount
                chunkBuildCacheSaveCount % chunkBuildCacheTrimEvery = 0)
        if shouldTrim then trimChunkBuildCache level

    let isChunkBuildCacheComplete (level : VoxelLevel) =
        if canUseChunkBuildCache level then
            let filePath = chunkBuildCacheCompleteFilePath level
            if File.Exists filePath then
                try
                    use stream = File.Open (filePath, FileMode.Open, FileAccess.Read, FileShare.Read)
                    use reader = new BinaryReader (stream)
                    let magic = reader.ReadString ()
                    let version = reader.ReadInt32 ()
                    let signature = reader.ReadString ()
                    magic = chunkBuildCacheMagic && version = chunkBuildCacheVersion && signature = chunkBuildCacheSignature level
                with _ -> false
            else false
        else false

    let markChunkBuildCacheComplete (level : VoxelLevel) =
        if canUseChunkBuildCache level then
            try
                let filePath = chunkBuildCacheCompleteFilePath level
                Directory.CreateDirectory (Path.GetDirectoryName filePath) |> ignore<DirectoryInfo>
                use stream = File.Open (filePath, FileMode.Create, FileAccess.Write, FileShare.None)
                use writer = new BinaryWriter (stream)
                writer.Write chunkBuildCacheMagic
                writer.Write chunkBuildCacheVersion
                writer.Write (chunkBuildCacheSignature level)
            with exn ->
                Log.warnOnce ("VoxelForge failed to mark chunk build cache complete due to: " + scstring exn)

    let private tryGetBodyShapeBoxes (bodyShape : BodyShape) =
        let boxes = ResizeArray<struct (Vector3 * Vector3)> ()
        let rec appendBodyShape bodyShape =
            match bodyShape with
            | EmptyShape -> true
            | BoxShape boxShape ->
                match boxShape.TransformOpt, boxShape.PropertiesOpt with
                | Some transform, None ->
                    boxes.Add (struct (boxShape.Size, transform.Translation))
                    true
                | None, None ->
                    boxes.Add (struct (boxShape.Size, v3Zero))
                    true
                | _ -> false
            | BodyShapes bodyShapes ->
                let mutable valid = true
                for bodyShape in bodyShapes do
                    if valid then valid <- appendBodyShape bodyShape
                valid
            | SphereShape _
            | CapsuleShape _
            | BoxRoundedShape _
            | EdgeShape _
            | ContourShape _
            | PointsShape _
            | GeometryShape _
            | StaticModelShape _
            | StaticModelSurfaceShape _
            | TerrainShape _ -> false
        if appendBodyShape bodyShape then Some (boxes.ToArray ())
        else None

    let private writeVector3iArray (writer : BinaryWriter) (values : Vector3i array) =
        writer.Write values.Length
        for value in values do
            writeVector3i writer value

    let private readVector3iArray (reader : BinaryReader) =
        let count = reader.ReadInt32 ()
        let values = Array.zeroCreate<Vector3i> count
        for i in 0 .. dec count do
            values[i] <- readVector3i reader
        values

    let private writeBox3Array (writer : BinaryWriter) (values : Box3 array) =
        writer.Write values.Length
        for value in values do
            writeBox3 writer value

    let private readBox3Array (reader : BinaryReader) =
        let count = reader.ReadInt32 ()
        let values = Array.zeroCreate<Box3> count
        for i in 0 .. dec count do
            values[i] <- readBox3 reader
        values

    let private packVoxelSplatPosition (bounds : Box3) (voxelSize : Vector3) (position : Vector3) (paletteIndex : int) =
        let voxelOrigin = bounds.Min + voxelSize * 0.5f
        let mutable packingWarning = false
        let quantize (position : single) (origin : single) (voxelSize : single) =
            let coord = int (MathF.Round ((position - origin) / voxelSize))
            let reconstructed = origin + single coord * voxelSize
            let tolerance = max 0.0001f (MathF.Abs voxelSize * 0.01f)
            if coord < 0 || coord > 63 || MathF.Abs (reconstructed - position) > tolerance then
                packingWarning <- true
                Math.Clamp (coord, 0, 63)
            else coord
        let x = quantize position.X voxelOrigin.X voxelSize.X
        let y = quantize position.Y voxelOrigin.Y voxelSize.Y
        let z = quantize position.Z voxelOrigin.Z voxelSize.Z
        let paletteIndex =
            if paletteIndex > 0x3FFF then
                packingWarning <- true
                0x3FFFu
            else uint paletteIndex
        struct (uint x ||| (uint y <<< 6) ||| (uint z <<< 12) ||| (paletteIndex <<< 18), packingWarning)

    let private unpackVoxelSplatPosition (bounds : Box3) (voxelSize : Vector3) (packed : uint) =
        let voxelOrigin = bounds.Min + voxelSize * 0.5f
        v3
            (voxelOrigin.X + single (packed &&& 0x3Fu) * voxelSize.X)
            (voxelOrigin.Y + single ((packed >>> 6) &&& 0x3Fu) * voxelSize.Y)
            (voxelOrigin.Z + single ((packed >>> 12) &&& 0x3Fu) * voxelSize.Z)

    let private writeVoxelModelDescriptor (writer : BinaryWriter) (descriptor : VoxelModelDescriptor) =
        writeBox3 writer descriptor.Bounds
        writeVector3 writer descriptor.VoxelSize
        let paletteIndices = Dictionary<Color, int> ()
        let paletteColors = ResizeArray<Color> ()
        let colorIndices = Array.zeroCreate<int> descriptor.Splats.Length
        for i in 0 .. dec descriptor.Splats.Length do
            let color = descriptor.Splats[i].Albedo
            let mutable colorIndex = 0
            if not (paletteIndices.TryGetValue (color, &colorIndex)) then
                colorIndex <- paletteColors.Count
                paletteIndices[color] <- colorIndex
                paletteColors.Add color
            colorIndices[i] <- colorIndex
        writer.Write paletteColors.Count
        for color in paletteColors do
            writeColor writer color
        writer.Write descriptor.Splats.Length
        let mutable packingWarning = false
        for i in 0 .. dec descriptor.Splats.Length do
            let splat = descriptor.Splats[i]
            let struct (packed, splatPackingWarning) = packVoxelSplatPosition descriptor.Bounds descriptor.VoxelSize splat.Position colorIndices[i]
            writer.Write packed
            writer.Write (byte splat.Faces)
            packingWarning <- packingWarning || splatPackingWarning
        if packingWarning then
            Log.warnOnce "A voxel chunk build cache entry exceeded the packed 64x64x64 / 16384-color splat format; cached splat keys were clamped."
        match descriptor.Grid with
        | Some grid ->
            writer.Write true
            writeVector3i writer grid.Size
            writeVector3 writer grid.Origin
            writer.Write grid.IndexBits
            writer.Write grid.Palette.Length
            for color in grid.Palette do writeColor writer color
            writer.Write grid.Indices.Length
            let bytes = Array.zeroCreate<byte> (grid.Indices.Length * sizeof<uint>)
            Buffer.BlockCopy (grid.Indices, 0, bytes, 0, bytes.Length)
            writer.Write bytes
        | None ->
            writer.Write false

    let private readVoxelModelDescriptor (reader : BinaryReader) =
        let bounds = readBox3 reader
        let voxelSize = readVector3 reader
        let paletteCount = reader.ReadInt32 ()
        let palette = Array.zeroCreate<Color> paletteCount
        for i in 0 .. dec paletteCount do
            palette[i] <- readColor reader
        let splatCount = reader.ReadInt32 ()
        let splats = Array.zeroCreate<VoxelSplat> splatCount
        for i in 0 .. dec splatCount do
            let packed = reader.ReadUInt32 ()
            let faces = enum<VoxelFaces> (int (reader.ReadByte ()))
            let colorIndex = int (packed >>> 18)
            splats[i] <-
                { Position = unpackVoxelSplatPosition bounds voxelSize packed
                  Albedo = if colorIndex < palette.Length then palette[colorIndex] else Color.White
                  Normal = v3Up
                  Faces = faces }
        let gridOpt =
            if reader.ReadBoolean () then
                let size = readVector3i reader
                let origin = readVector3 reader
                let indexBits = reader.ReadInt32 ()
                let gridPaletteCount = reader.ReadInt32 ()
                let gridPalette = Array.zeroCreate<Color> gridPaletteCount
                for i in 0 .. dec gridPaletteCount do
                    gridPalette[i] <- readColor reader
                let indicesLength = reader.ReadInt32 ()
                let bytes = reader.ReadBytes (indicesLength * sizeof<uint>)
                if bytes.Length <> indicesLength * sizeof<uint> then
                    raise (EndOfStreamException "Voxel grid cache data ended unexpectedly.")
                let indices = Array.zeroCreate<uint> indicesLength
                Buffer.BlockCopy (bytes, 0, indices, 0, bytes.Length)
                Some
                    { Size = size
                      Origin = origin
                      IndexBits = indexBits
                      Indices = indices
                      Palette = gridPalette }
            else None
        { Splats = splats
          Grid = gridOpt
          Bounds = bounds
          VoxelSize = voxelSize }

    let private writeBodyShapeBoxes (writer : BinaryWriter) (bodyBoxes : struct (Vector3 * Vector3) array) =
        writer.Write bodyBoxes.Length
        for struct (size, translation) in bodyBoxes do
            writeVector3 writer size
            writeVector3 writer translation

    let private readBodyShapeBoxes (reader : BinaryReader) =
        let count = reader.ReadInt32 ()
        let bodyShapes = Array.zeroCreate<BodyShape> count
        for i in 0 .. dec count do
            let size = readVector3 reader
            let translation = readVector3 reader
            bodyShapes[i] <- BoxShape { Size = size; TransformOpt = Some (Affine.makeTranslation translation); PropertiesOpt = None }
        if count = 0 then EmptyShape
        else BodyShapes (bodyShapes |> Array.toList)

    let private writeChunkBuild (writer : BinaryWriter) (chunkBuild : VoxelChunkBuild) =
        match tryGetBodyShapeBoxes chunkBuild.BodyShape with
        | Some bodyBoxes ->
            writeVector3i writer chunkBuild.ChunkCoord
            writeVector3 writer chunkBuild.ChunkCenter
            writeVector3 writer chunkBuild.ChunkSize
            writeVoxelModelDescriptor writer chunkBuild.VoxelModelDescriptor
            writeBodyShapeBoxes writer bodyBoxes
            match chunkBuild.OcclusionBoundsOpt with
            | Some bounds ->
                writer.Write true
                writeBox3 writer bounds
            | None -> writer.Write false
            writeVector3iArray writer chunkBuild.SolidBlockCoords
            writer.Write chunkBuild.SplatCount
            writeVector3iArray writer chunkBuild.OpaqueBlockCoords
            writeBox3Array writer chunkBuild.OpaqueOccluderBoxes
            writer.Write chunkBuild.OpaqueFaceMask
            writer.Write chunkBuild.FullOpaqueChunk
            true
        | None -> false

    let private readChunkBuild (reader : BinaryReader) =
        let chunkCoord = readVector3i reader
        let chunkCenter = readVector3 reader
        let chunkSize = readVector3 reader
        let voxelModelDescriptor = readVoxelModelDescriptor reader
        let bodyShape = readBodyShapeBoxes reader
        let boxCount =
            match bodyShape with
            | BodyShapes bodyShapes -> bodyShapes.Length
            | EmptyShape -> 0
            | BoxShape _ -> 1
            | SphereShape _
            | CapsuleShape _
            | BoxRoundedShape _
            | EdgeShape _
            | ContourShape _
            | PointsShape _
            | GeometryShape _
            | StaticModelShape _
            | StaticModelSurfaceShape _
            | TerrainShape _ -> 0
        let occlusionBoundsOpt =
            if reader.ReadBoolean () then Some (readBox3 reader)
            else None
        let solidBlockCoords = readVector3iArray reader
        let splatCount = reader.ReadInt32 ()
        let opaqueBlockCoords = readVector3iArray reader
        let opaqueOccluderBoxes = readBox3Array reader
        let opaqueFaceMask = reader.ReadInt32 ()
        let fullOpaqueChunk = reader.ReadBoolean ()
        { ChunkCoord = chunkCoord
          ChunkCenter = chunkCenter
          ChunkSize = chunkSize
          VoxelModelDescriptor = voxelModelDescriptor
          BodyShape = bodyShape
          BoxCount = boxCount
          OcclusionBoundsOpt = occlusionBoundsOpt
          SolidBlockCoords = solidBlockCoords
          SplatCount = splatCount
          OpaqueBlockCoords = opaqueBlockCoords
          OpaqueOccluderBoxes = opaqueOccluderBoxes
          OpaqueFaceMask = opaqueFaceMask
          FullOpaqueChunk = fullOpaqueChunk }

    let private tryLoadChunkBuild (level : VoxelLevel) (chunkCoord : Vector3i) =
        if canUseChunkBuildCache level then
            let filePath = chunkBuildCacheFilePath level chunkCoord
            if File.Exists filePath then
                try
                    use stream = File.Open (filePath, FileMode.Open, FileAccess.Read, FileShare.Read)
                    use reader = new BinaryReader (stream)
                    let magic = reader.ReadString ()
                    let version = reader.ReadInt32 ()
                    let signature = reader.ReadString ()
                    let cachedChunkCoord = readVector3i reader
                    if  magic = chunkBuildCacheMagic &&
                        version = chunkBuildCacheVersion &&
                        signature = chunkBuildCacheSignature level &&
                        cachedChunkCoord = chunkCoord then
                        let hasChunkBuild = reader.ReadBoolean ()
                        let chunkBuildOpt =
                            if hasChunkBuild then
                                use compressedStream = new DeflateStream (stream, CompressionMode.Decompress, true)
                                use payloadReader = new BinaryReader (compressedStream)
                                Some (readChunkBuild payloadReader)
                            else None
                        try File.SetLastAccessTimeUtc (filePath, DateTime.UtcNow)
                        with _ -> ()
                        ValueSome chunkBuildOpt
                    else ValueNone
                with exn ->
                    Log.warnOnce ("VoxelForge failed to load chunk build cache due to: " + scstring exn)
                    ValueNone
            else ValueNone
        else ValueNone

    let private saveChunkBuild (level : VoxelLevel) (chunkCoord : Vector3i) (chunkBuildOpt : VoxelChunkBuild option) =
        if canUseChunkBuildCache level then
            try
                match chunkBuildOpt with
                | Some chunkBuild when Option.isNone (tryGetBodyShapeBoxes chunkBuild.BodyShape) -> ()
                | Some _ | None ->
                    let filePath = chunkBuildCacheFilePath level chunkCoord
                    Directory.CreateDirectory (Path.GetDirectoryName filePath) |> ignore<DirectoryInfo>
                    use stream = File.Open (filePath, FileMode.Create, FileAccess.Write, FileShare.None)
                    use writer = new BinaryWriter (stream)
                    writer.Write chunkBuildCacheMagic
                    writer.Write chunkBuildCacheVersion
                    writer.Write (chunkBuildCacheSignature level)
                    writeVector3i writer chunkCoord
                    match chunkBuildOpt with
                    | Some chunkBuild ->
                        writer.Write true
                        writer.Flush ()
                        use compressedStream = new DeflateStream (stream, CompressionLevel.Fastest, true)
                        use payloadWriter = new BinaryWriter (compressedStream)
                        writeChunkBuild payloadWriter chunkBuild |> ignore<bool>
                        payloadWriter.Flush ()
                    | None -> writer.Write false
                    noteChunkBuildCacheSave level
            with exn ->
                Log.warnOnce ("VoxelForge failed to save chunk build cache due to: " + scstring exn)

    let tryBuildChunkWithCellLookupCached useCache (level : VoxelLevel) (tryGetCell : Vector3i -> VoxelCell voption) (chunkCoord : Vector3i) =
        if useCache then
            match tryLoadChunkBuild level chunkCoord with
            | ValueSome chunkBuildOpt -> chunkBuildOpt
            | ValueNone ->
                let chunkBuildOpt = tryBuildChunkWithCellLookup level tryGetCell chunkCoord
                saveChunkBuild level chunkCoord chunkBuildOpt
                chunkBuildOpt
        else tryBuildChunkWithCellLookup level tryGetCell chunkCoord

    let tryBuildChunkWithEditSnapshotCached useCache (level : VoxelLevel) (snapshot : VoxelEditSnapshot) (chunkCoord : Vector3i) =
        let tryGetCell coord = VoxelWorld.tryGetCellWithEditSnapshotValue snapshot level coord
        if useCache then tryBuildChunkWithCellLookupCached true level tryGetCell chunkCoord
        else
            let hasBlockEdits =
                match snapshot.BlockEditsOpt with
                | Some blockEdits -> blockEdits.Count > 0
                | None -> false
            if hasBlockEdits then tryBuildChunkWithCellLookupCached false level tryGetCell chunkCoord
            else
                match tryBuildChunkWithEditSnapshotFromBlocks level snapshot chunkCoord with
                | ValueSome chunkBuildOpt -> chunkBuildOpt
                | ValueNone -> tryBuildChunkWithCellLookupCached false level tryGetCell chunkCoord

    let tryBuildChunkCached (level : VoxelLevel) (chunkCoord : Vector3i) =
        let tryGetCell coord = VoxelWorld.tryGetCellValue level coord
        tryBuildChunkWithCellLookupCached true level tryGetCell chunkCoord

    let private makeRealizedChunk voxelModelOpt (level : VoxelLevel) (chunkBuild : VoxelChunkBuild) =
        VoxelWorld.updateChunkManifestFromBuild (VoxelWorld.getEditRevision level) level chunkBuild
        { ChunkCoord = chunkBuild.ChunkCoord
          ChunkCenter = chunkBuild.ChunkCenter
          ChunkSize = chunkBuild.ChunkSize
          BodyShape = chunkBuild.BodyShape
          BoxCount = chunkBuild.BoxCount
          OcclusionBoundsOpt = chunkBuild.OcclusionBoundsOpt
          SolidBlockCoords = chunkBuild.SolidBlockCoords
          SplatCount = chunkBuild.SplatCount
          VoxelModelOpt = voxelModelOpt
          OpaqueBlockCoords = chunkBuild.OpaqueBlockCoords
          OpaqueOccluderBoxes = chunkBuild.OpaqueOccluderBoxes
          OpaqueFaceMask = chunkBuild.OpaqueFaceMask
          FullOpaqueChunk = chunkBuild.FullOpaqueChunk }

    let realizeChunk (level : VoxelLevel) (chunkBuild : VoxelChunkBuild) (world : World) =
        let voxelModelOpt =
            if chunkBuild.SplatCount > 0 then
                let revision = nextRevision level
                let voxelModel = chunkAssetTag chunkBuild.ChunkCoord revision
                World.createUserDefinedVoxelModel chunkBuild.VoxelModelDescriptor voxelModel world
                Some voxelModel
            else None
        makeRealizedChunk voxelModelOpt level chunkBuild

    let private updateRealizedChunk (previous : VoxelChunk) (level : VoxelLevel) (chunkBuild : VoxelChunkBuild) (world : World) =
        let voxelModelOpt, destroyPreviousAsset =
            if chunkBuild.SplatCount > 0 then
                match previous.VoxelModelOpt with
                | Some voxelModel ->
                    World.updateUserDefinedVoxelModel chunkBuild.VoxelModelDescriptor voxelModel world
                    Some voxelModel, false
                | None ->
                    let revision = nextRevision level
                    let voxelModel = chunkAssetTag chunkBuild.ChunkCoord revision
                    World.createUserDefinedVoxelModel chunkBuild.VoxelModelDescriptor voxelModel world
                    Some voxelModel, false
            else None, previous.VoxelModelOpt.IsSome
        makeRealizedChunk voxelModelOpt level chunkBuild, destroyPreviousAsset

    let rebuildChunk (level : VoxelLevel) (chunkCoord : Vector3i) (world : World) =
        match tryBuildChunk level chunkCoord with
        | Some chunkBuild ->
            Some (realizeChunk level chunkBuild world)
        | None ->
            VoxelWorld.markChunkManifestEmpty (VoxelWorld.getEditRevision level) level chunkCoord
            None

    let rebuildChunks (chunkCoords : Vector3i seq) (level : VoxelLevel) (currentChunks : VoxelChunk array) (world : World) =
        let targetCoords = chunkCoords |> Seq.toArray
        if targetCoords.Length = 0 then struct (currentChunks, Array.empty, Array.empty)
        else
            let snapshot = VoxelWorld.snapshotEdits level
            let processedTargets = Array.zeroCreate<bool> targetCoords.Length
            let chunks = ResizeArray<VoxelChunk> (currentChunks.Length + targetCoords.Length)
            let chunksReplaced = ResizeArray<VoxelChunk> (targetCoords.Length)
            let voxelAssetsToDestroy = ResizeArray<VoxelChunk> ()
            let indexOfTarget coord =
                let mutable index = -1
                let mutable i = 0
                while index < 0 && i < targetCoords.Length do
                    if targetCoords[i] = coord then index <- i
                    i <- inc i
                index
            let tryBuildTarget chunkCoord =
                match tryBuildChunkWithEditSnapshotCached false level snapshot chunkCoord with
                | Some chunkBuild -> Some chunkBuild
                | None ->
                    VoxelWorld.markChunkManifestEmpty snapshot.Revision level chunkCoord
                    None
            for chunk in currentChunks do
                let targetIndex = indexOfTarget chunk.ChunkCoord
                if targetIndex >= 0 then
                    processedTargets[targetIndex] <- true
                    chunksReplaced.Add chunk
                    match tryBuildTarget chunk.ChunkCoord with
                    | Some chunkBuild ->
                        let rebuiltChunk, destroyPreviousAsset = updateRealizedChunk chunk level chunkBuild world
                        chunks.Add rebuiltChunk
                        if destroyPreviousAsset then voxelAssetsToDestroy.Add chunk
                    | None ->
                        if chunk.VoxelModelOpt.IsSome then voxelAssetsToDestroy.Add chunk
                else chunks.Add chunk
            let mutable appended = false
            for i in 0 .. dec targetCoords.Length do
                if not processedTargets[i] then
                    match tryBuildTarget targetCoords[i] with
                    | Some chunkBuild ->
                        chunks.Add (realizeChunk level chunkBuild world)
                        appended <- true
                    | None -> ()
            let chunksArray = chunks.ToArray ()
            let chunksArray = if appended then sortVoxelChunks chunksArray else chunksArray
            struct (chunksArray, chunksReplaced.ToArray (), voxelAssetsToDestroy.ToArray ())

    let destroyVoxelChunks (voxelChunks : VoxelChunk array) (world : World) =
        for chunk in voxelChunks do
            match chunk.VoxelModelOpt with
            | Some voxelModel -> World.destroyUserDefinedVoxelModel voxelModel world
            | None -> ()

    let destroyVoxelModel (voxelChunks : VoxelChunk array) (placeableBlocks : PlaceableBlock array) (_levelOpt : VoxelLevel option) (world : World) =
        destroyVoxelChunks voxelChunks world
        for placeableBlock in placeableBlocks do
            World.destroyUserDefinedVoxelModel placeableBlock.PreviewModel world
