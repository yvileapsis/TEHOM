namespace VoxelForge
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module VoxelRuntime =

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

    let tryBuildChunkWithCellLookup (level : VoxelLevel) (tryGetCell : Vector3i -> VoxelCell voption) (chunkCoord : Vector3i) =
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
    let private chunkBuildCacheVersion = 1

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

    let private chunkBuildCacheFilePath (level : VoxelLevel) (chunkCoord : Vector3i) =
        let signature = chunkBuildCacheSignature level
        let safeSignature =
            signature.Replace("|", "_").Replace("-", "m").Replace(".", "p").Replace(",", "p")
            |> safePathPart
        let directoryPath =
            Path.Combine
                (Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData,
                 "VoxelForge",
                 "ChunkBuilds",
                 "v" + string chunkBuildCacheVersion,
                 safeSignature)
        Path.Combine (directoryPath, "chunk_" + string chunkCoord.X + "_" + string chunkCoord.Y + "_" + string chunkCoord.Z + ".vfcb")

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

    let private writeVoxelModelDescriptor (writer : BinaryWriter) (descriptor : VoxelModelDescriptor) =
        writeBox3 writer descriptor.Bounds
        writeVector3 writer descriptor.VoxelSize
        writer.Write descriptor.Splats.Length
        for splat in descriptor.Splats do
            writeVector3 writer splat.Position
            writeColor writer splat.Albedo
            writeVector3 writer splat.Normal

    let private readVoxelModelDescriptor (reader : BinaryReader) =
        let bounds = readBox3 reader
        let voxelSize = readVector3 reader
        let splatCount = reader.ReadInt32 ()
        let splats = Array.zeroCreate<VoxelSplat> splatCount
        for i in 0 .. dec splatCount do
            splats[i] <-
                { Position = readVector3 reader
                  Albedo = readColor reader
                  Normal = readVector3 reader }
        { Splats = splats
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
                        if hasChunkBuild then ValueSome (Some (readChunkBuild reader))
                        else ValueSome None
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
                        writeChunkBuild writer chunkBuild |> ignore<bool>
                    | None -> writer.Write false
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

    let tryBuildChunkCached (level : VoxelLevel) (chunkCoord : Vector3i) =
        let tryGetCell coord = VoxelWorld.tryGetCellValue level coord
        tryBuildChunkWithCellLookupCached true level tryGetCell chunkCoord

    let realizeChunk (level : VoxelLevel) (chunkBuild : VoxelChunkBuild) (world : World) =
        let splatCount = chunkBuild.SplatCount
        let voxelModelOpt =
            if splatCount > 0 then
                let revision = nextRevision level
                let voxelModel = chunkAssetTag chunkBuild.ChunkCoord revision
                World.createUserDefinedVoxelModel chunkBuild.VoxelModelDescriptor voxelModel world
                Some voxelModel
            else None
        { ChunkCoord = chunkBuild.ChunkCoord
          ChunkCenter = chunkBuild.ChunkCenter
          ChunkSize = chunkBuild.ChunkSize
          BodyShape = chunkBuild.BodyShape
          BoxCount = chunkBuild.BoxCount
          OcclusionBoundsOpt = chunkBuild.OcclusionBoundsOpt
          SolidBlockCoords = chunkBuild.SolidBlockCoords
          SplatCount = splatCount
          VoxelModelOpt = voxelModelOpt
          OpaqueBlockCoords = chunkBuild.OpaqueBlockCoords
          OpaqueOccluderBoxes = chunkBuild.OpaqueOccluderBoxes
          OpaqueFaceMask = chunkBuild.OpaqueFaceMask
          FullOpaqueChunk = chunkBuild.FullOpaqueChunk }

    let rebuildChunk (level : VoxelLevel) (chunkCoord : Vector3i) (world : World) =
        match tryBuildChunk level chunkCoord with
        | Some chunkBuild ->
            Some (realizeChunk level chunkBuild world)
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
            match tryBuildChunk level chunkCoord with
            | Some chunkBuild -> chunks[chunkCoord] <- realizeChunk level chunkBuild world
            | None -> chunks.Remove chunkCoord |> ignore<bool>
        struct (sortVoxelChunks chunks.Values, chunksToDestroy.ToArray ())

    let destroyVoxelChunks (voxelChunks : VoxelChunk array) (world : World) =
        for chunk in voxelChunks do
            match chunk.VoxelModelOpt with
            | Some voxelModel -> World.destroyUserDefinedVoxelModel voxelModel world
            | None -> ()

    let destroyVoxelModel (voxelChunks : VoxelChunk array) (placeableBlocks : PlaceableBlock array) (_levelOpt : VoxelLevel option) (world : World) =
        destroyVoxelChunks voxelChunks world
        for placeableBlock in placeableBlocks do
            World.destroyUserDefinedVoxelModel placeableBlock.PreviewModel world
