namespace VoxelForge
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module VoxelLighting =

    [<Literal>]
    let MaxLight = 15uy

    let SunRotation = Quaternion.CreateFromYawPitchRoll (-0.55f, -0.85f, 0.0f)
    let SunlightDirection = SunRotation.Down.Normalized

    let private chunkWorkPerTick = 4096
    let private blockReservoirWorkPerTick = 128
    let private blockWorkPerTick = 128
    let private microReservoirWorkPerTick = 512
    let private microWorkPerTick = 512
    let private microChunkRadius = 0
    let private chunkBlockBudget = 32
    let private directionEpsilon = 0.15f

    let private directions =
        [|struct (v3iRight, v3Right)
          struct (v3iLeft, v3Left)
          struct (v3iUp, v3Up)
          struct (v3iDown, v3Down)
          struct (v3iForward, v3Forward)
          struct (v3iBack, v3Back)|]

    let private tryGetLevel (fluid : VoxelLightFluidState) coord =
        let mutable value = 0uy
        if fluid.Levels.TryGetValue (coord, &value) then value else 0uy

    let private enqueue (fluid : VoxelLightFluidState) coord =
        if fluid.Queued.Add coord then fluid.Frontier.Enqueue coord

    let private enqueueNeighborhood fluid coord =
        enqueue fluid coord
        for struct (offset, _) in directions do enqueue fluid (coord + offset)

    let private setLevel onChanged (fluid : VoxelLightFluidState) coord value =
        let previous = tryGetLevel fluid coord
        if previous <> value then
            if value = 0uy then fluid.Levels.Remove coord |> ignore<bool>
            else fluid.Levels[coord] <- value
            onChanged coord

    let private addLevel onChanged fluid coord amount =
        if amount > 0uy then
            let current = tryGetLevel fluid coord
            let next = byte (min (int MaxLight) (int current + int amount))
            if next <> current then
                setLevel onChanged fluid coord next
                enqueueNeighborhood fluid coord

    let private inBounds (size : Vector3i) (coord : Vector3i) =
        coord.X >= 0 && coord.X < size.X &&
        coord.Y >= 0 && coord.Y < size.Y &&
        coord.Z >= 0 && coord.Z < size.Z

    let private chunkFaceMask (level : VoxelLevel) coord =
        match VoxelWorld.tryGetChunkStatic level coord with
        | ValueSome chunkStatic when chunkStatic.HasBuild -> chunkStatic.OpaqueFaceMask
        | _ -> 0

    let private chunkTransitionOpen (level : VoxelLevel) source destination =
        let step = destination - source
        let sourceMask = chunkFaceMask level source
        let destinationMask = chunkFaceMask level destination
        if step = v3iRight then sourceMask &&& 2 = 0 && destinationMask &&& 1 = 0
        elif step = v3iLeft then sourceMask &&& 1 = 0 && destinationMask &&& 2 = 0
        elif step = v3iUp then sourceMask &&& 8 = 0 && destinationMask &&& 4 = 0
        elif step = v3iDown then sourceMask &&& 4 = 0 && destinationMask &&& 8 = 0
        elif step = v3iForward then sourceMask &&& 16 = 0 && destinationMask &&& 32 = 0
        elif step = v3iBack then sourceMask &&& 32 = 0 && destinationMask &&& 16 = 0
        else false

    let private alwaysTraverse _ _ = true

    let private processFluid
        workMax
        cohesive
        (priorityOpt : struct (PriorityQueue<Vector3i, struct (int * int * int)> * Vector3i) voption)
        inBounds
        isOpen
        canTraverse
        onChanged
        onTransfer
        (fluid : VoxelLightFluidState) =
        let priorityFrontier =
            match priorityOpt with
            | ValueSome struct (frontier, _) -> frontier
            | ValueNone -> null
        let movePendingToPriority () =
            match priorityOpt with
            | ValueSome struct (_, centerChunk) ->
                while fluid.Frontier.Count > 0 do
                    let coord = fluid.Frontier.Dequeue ()
                    let dx = coord.X - centerChunk.X
                    let dz = coord.Z - centerChunk.Z
                    priorityFrontier.Enqueue (coord, struct (dx * dx + dz * dz, coord.Z, coord.X))
            | ValueNone -> ()
        let mutable work = 0
        let mutable running = true
        while work < workMax && running do
            let mutable coord = v3iZero
            if isNull priorityFrontier then
                if fluid.Frontier.Count > 0 then coord <- fluid.Frontier.Dequeue ()
                else running <- false
            else
                movePendingToPriority ()
                if priorityFrontier.Count > 0 then coord <- priorityFrontier.Dequeue ()
                else running <- false
            if running then
                fluid.Queued.Remove coord |> ignore<bool>
                let sourceLevel = tryGetLevel fluid coord
                if sourceLevel > 0uy then
                    if not (inBounds coord) || not (isOpen coord) then
                        setLevel onChanged fluid coord 0uy
                        enqueueNeighborhood fluid coord
                    elif sourceLevel > 1uy then
                        let mutable bestCoord = v3iZero
                        let mutable bestScore = Int32.MinValue
                        let mutable found = false
                        for struct (offset, direction) in directions do
                            let neighbor = coord + offset
                            if inBounds neighbor && isOpen neighbor && canTraverse coord neighbor then
                                let neighborLevel = tryGetLevel fluid neighbor
                                let difference = int sourceLevel - int neighborLevel
                                if difference > 0 then
                                    let mutable cohesiveNeighbors = 0
                                    for struct (cohesiveOffset, _) in directions do
                                        if tryGetLevel fluid (neighbor + cohesiveOffset) > 0uy then
                                            cohesiveNeighbors <- inc cohesiveNeighbors
                                    let forward = Vector3.Dot (direction, SunlightDirection)
                                    let canTransfer =
                                        if cohesive then
                                            if neighborLevel = 0uy then
                                                sourceLevel >= MaxLight - 1uy &&
                                                (forward > 0.25f || cohesiveNeighbors >= 2)
                                            else difference > 1
                                        else
                                            let requiredDifference = if forward > 0.25f || cohesiveNeighbors >= 2 then 1 else 2
                                            difference > requiredDifference
                                    if canTransfer then
                                        let score = difference * 64 + int (forward * 12.0f) + cohesiveNeighbors * 5
                                        if score > bestScore then
                                            bestCoord <- neighbor
                                            bestScore <- score
                                            found <- true
                        if found then
                            let destinationLevel = tryGetLevel fluid bestCoord
                            setLevel onChanged fluid coord (sourceLevel - 1uy)
                            setLevel onChanged fluid bestCoord (destinationLevel + 1uy)
                            onTransfer coord bestCoord
                            enqueueNeighborhood fluid coord
                            enqueueNeighborhood fluid bestCoord
                work <- inc work
        if not (isNull priorityFrontier) then movePendingToPriority ()
        work

    let private prepareChunkPriorityFrontier (lighting : VoxelLightingState) (centerChunk : Vector3i) =
        let priorityFrontier = lighting.ChunkPriorityFrontier
        if
            not lighting.ChunkPriorityCenterInitialized ||
            lighting.ChunkPriorityCenter.X <> centerChunk.X ||
            lighting.ChunkPriorityCenter.Z <> centerChunk.Z
        then
            while priorityFrontier.Count > 0 do
                lighting.ChunkFluid.Frontier.Enqueue (priorityFrontier.Dequeue ())
            lighting.ChunkPriorityCenter <- centerChunk
            lighting.ChunkPriorityCenterInitialized <- true
        while lighting.ChunkFluid.Frontier.Count > 0 do
            let coord = lighting.ChunkFluid.Frontier.Dequeue ()
            let dx = coord.X - centerChunk.X
            let dz = coord.Z - centerChunk.Z
            priorityFrontier.Enqueue (coord, struct (dx * dx + dz * dz, coord.Z, coord.X))

    let private cellTransmitsLight (cell : VoxelCell) =
        not cell.Solid ||
        match cell.Material with
        | Leaves | Glass | Water -> true
        | Grass | Dirt | Stone | Sand | Wood | Lava | Ore | Brick | Crafted -> false

    let private microCellOpen (level : VoxelLevel) coord =
        if not (VoxelWorld.isSourceCoordInBounds level coord) then false
        else
            match VoxelWorld.tryGetCellValue level coord with
            | ValueSome cell -> cellTransmitsLight cell
            | ValueNone -> true

    let private blockCellOpen (level : VoxelLevel) blockCoord =
        let lighting = level.Lighting
        let mutable cached = false
        if lighting.BlockOpenCache.TryGetValue (blockCoord, &cached) then cached
        else
            let openCell =
                if not (VoxelWorld.isBlockCoordInBounds level blockCoord) then false
                else
                    let start = VoxelWorld.blockStartCoord level blockCoord
                    let side = level.BlockSideVoxels
                    let mutable found = false
                    let mutable y = 0
                    while not found && y < side do
                        let mutable z = 0
                        while not found && z < side do
                            let mutable x = 0
                            while not found && x < side do
                                found <- microCellOpen level (v3i (start.X + x) (start.Y + y) (start.Z + z))
                                x <- inc x
                            z <- inc z
                        y <- inc y
                    found
            lighting.BlockOpenCache[blockCoord] <- openCell
            openCell

    let private chunkCellOpen (level : VoxelLevel) (chunkCoord : Vector3i) =
        match VoxelWorld.tryChunkCoordToIndex level.ChunkCounts chunkCoord with
        | ValueSome chunkIndex ->
            let chunkStatic = level.ChunkManifest.ChunkStatics[chunkIndex]
            not (chunkStatic.HasBuild && chunkStatic.FullOpaqueChunk)
        | ValueNone -> false

    let private sunIngressMask =
        (if SunlightDirection.X > directionEpsilon then 2uy elif SunlightDirection.X < -directionEpsilon then 1uy else 0uy) |||
        (if SunlightDirection.Y > directionEpsilon then 8uy elif SunlightDirection.Y < -directionEpsilon then 4uy else 0uy) |||
        (if SunlightDirection.Z > directionEpsilon then 16uy elif SunlightDirection.Z < -directionEpsilon then 32uy else 0uy)

    let private ingressMaskFromStep (step : Vector3i) =
        if step = v3iRight then 2uy
        elif step = v3iLeft then 1uy
        elif step = v3iUp then 8uy
        elif step = v3iDown then 4uy
        elif step = v3iForward then 32uy
        else 16uy

    let private entryMaskInRange (minCoord : Vector3i) (maxCoord : Vector3i) (coord : Vector3i) =
        (if SunlightDirection.X > directionEpsilon && coord.X = minCoord.X then 2uy elif SunlightDirection.X < -directionEpsilon && coord.X = maxCoord.X then 1uy else 0uy) |||
        (if SunlightDirection.Y > directionEpsilon && coord.Y = minCoord.Y then 8uy elif SunlightDirection.Y < -directionEpsilon && coord.Y = maxCoord.Y then 4uy else 0uy) |||
        (if SunlightDirection.Z > directionEpsilon && coord.Z = minCoord.Z then 16uy elif SunlightDirection.Z < -directionEpsilon && coord.Z = maxCoord.Z then 32uy else 0uy)

    let private isOnIngressFace mask side (local : Vector3i) =
        (mask &&& 1uy <> 0uy && local.X = dec side) ||
        (mask &&& 2uy <> 0uy && local.X = 0) ||
        (mask &&& 4uy <> 0uy && local.Y = dec side) ||
        (mask &&& 8uy <> 0uy && local.Y = 0) ||
        (mask &&& 16uy <> 0uy && local.Z = 0) ||
        (mask &&& 32uy <> 0uy && local.Z = dec side)

    let private initializeChunkSources (level : VoxelLevel) =
        let lighting = level.Lighting
        if not lighting.Initialized then
            let worldMin = v3iZero
            let worldMax = level.ChunkCounts - v3iOne
            let onChunkChanged coord =
                lighting.ChunkTouched.Add coord |> ignore<bool>
                lighting.DirtyChunks.Add coord |> ignore<bool>
            let upstreamOpen coord step exitFace enterFace =
                let mutable previous = coord
                let mutable cursor = coord + step
                let mutable openPath = true
                while openPath && inBounds level.ChunkCounts cursor do
                    openPath <-
                        chunkCellOpen level cursor &&
                        chunkFaceMask level previous &&& exitFace = 0 &&
                        chunkFaceMask level cursor &&& enterFace = 0
                    previous <- cursor
                    cursor <- cursor + step
                openPath && chunkFaceMask level previous &&& exitFace = 0
            let trySeed coord =
                if lighting.ChunkSourceEvaluated.Add coord then
                    let ingress = entryMaskInRange worldMin worldMax coord
                    let skyVisible =
                        (ingress &&& 1uy <> 0uy && upstreamOpen coord v3iRight 2 1) ||
                        (ingress &&& 2uy <> 0uy && upstreamOpen coord v3iLeft 1 2) ||
                        (ingress &&& 4uy <> 0uy && upstreamOpen coord v3iUp 8 4) ||
                        (ingress &&& 8uy <> 0uy && upstreamOpen coord v3iDown 4 8) ||
                        (ingress &&& 16uy <> 0uy && upstreamOpen coord v3iForward 16 32) ||
                        (ingress &&& 32uy <> 0uy && upstreamOpen coord v3iBack 32 16)
                    if skyVisible && chunkCellOpen level coord && lighting.ChunkSources.Add coord then
                        addLevel onChunkChanged lighting.ChunkFluid coord MaxLight
            if abs SunlightDirection.X > directionEpsilon then
                let x = if SunlightDirection.X > 0.0f then worldMin.X else worldMax.X
                for z in worldMin.Z .. worldMax.Z do
                    for y in worldMin.Y .. worldMax.Y do
                        trySeed (v3i x y z)
            if abs SunlightDirection.Y > directionEpsilon then
                let y = if SunlightDirection.Y > 0.0f then worldMin.Y else worldMax.Y
                for z in worldMin.Z .. worldMax.Z do
                    for x in worldMin.X .. worldMax.X do
                        trySeed (v3i x y z)
            if abs SunlightDirection.Z > directionEpsilon then
                let z = if SunlightDirection.Z > 0.0f then worldMin.Z else worldMax.Z
                for y in worldMin.Y .. worldMax.Y do
                    for x in worldMin.X .. worldMax.X do
                        trySeed (v3i x y z)
            lighting.Initialized <- true

    let private chunkBlockRange (level : VoxelLevel) (chunkCoord : Vector3i) =
        let minSource =
            v3i
                (chunkCoord.X * level.ChunkSizeVoxels.X)
                (chunkCoord.Y * level.ChunkSizeVoxels.Y)
                (chunkCoord.Z * level.ChunkSizeVoxels.Z)
        let maxSource = minSource + level.ChunkSizeVoxels - v3iOne
        struct (VoxelWorld.sourceCoordToBlockCoord level minSource, VoxelWorld.sourceCoordToBlockCoord level maxSource)

    let private markBlockChange (level : VoxelLevel) blockCoord =
        let blockStart = VoxelWorld.blockStartCoord level blockCoord
        level.Lighting.DirtyChunks.Add (VoxelWorld.sourceCoordToChunkCoord level blockStart) |> ignore<bool>

    let private enqueueBlockReservoir (lighting : VoxelLightingState) chunkCoord =
        if lighting.BlockReservoirQueued.Add chunkCoord then lighting.BlockReservoirFrontier.Enqueue chunkCoord

    let private enqueueMicroReservoir (lighting : VoxelLightingState) blockCoord =
        if lighting.MicroReservoirQueued.Add blockCoord then lighting.MicroReservoirFrontier.Enqueue blockCoord

    let private addReservoir (reservoirs : Dictionary<Vector3i, int>) enqueueReservoir coord amount =
        if amount > 0 then
            let mutable current = 0
            reservoirs.TryGetValue (coord, &current) |> ignore<bool>
            reservoirs[coord] <- current + amount
            enqueueReservoir coord

    let private adjustFineAmount (lighting : VoxelLightingState) blockCoord delta =
        if delta <> 0 then
            let mutable current = 0
            lighting.FineAmounts.TryGetValue (blockCoord, &current) |> ignore<bool>
            let next = current + delta
            if next > 0 then lighting.FineAmounts[blockCoord] <- next
            else lighting.FineAmounts.Remove blockCoord |> ignore<bool>

    let private addFineReservoir (level : VoxelLevel) blockCoord amount =
        if amount > 0 then
            addReservoir
                level.Lighting.MicroReservoirs
                (enqueueMicroReservoir level.Lighting)
                blockCoord
                amount
            adjustFineAmount level.Lighting blockCoord amount

    let private levelFromReservoir amount childCount =
        if amount <= 0 || childCount <= 0 then 0uy
        else byte (min (int MaxLight) ((amount + dec childCount) / childCount))

    let private sampleBlockHierarchyLight (level : VoxelLevel) blockCoord =
        let lighting = level.Lighting
        let mutable residual = 0
        lighting.BlockFineResiduals.TryGetValue (blockCoord, &residual) |> ignore<bool>
        let microVolume = level.BlockSideVoxels * level.BlockSideVoxels * level.BlockSideVoxels
        let blockLevel =
            max
                (tryGetLevel lighting.BlockFluid blockCoord)
                (levelFromReservoir residual microVolume)
        if blockLevel > 0uy then blockLevel
        else
            let blockStart = VoxelWorld.blockStartCoord level blockCoord
            let chunkCoord = VoxelWorld.sourceCoordToChunkCoord level blockStart
            if not (lighting.ChunkTouched.Contains chunkCoord) then 0uy
            else
                let chunkLevel = tryGetLevel lighting.ChunkFluid chunkCoord
                let mutable reservoir = 0
                if lighting.BlockReservoirs.TryGetValue (chunkCoord, &reservoir) then
                    let struct (minBlock, maxBlock) = chunkBlockRange level chunkCoord
                    let childCount =
                        (inc maxBlock.X - minBlock.X) *
                        (inc maxBlock.Y - minBlock.Y) *
                        (inc maxBlock.Z - minBlock.Z)
                    max chunkLevel (levelFromReservoir reservoir childCount)
                else chunkLevel

    let private refineLoadedChunks (level : VoxelLevel) (chunks : VoxelChunk array) (centerChunk : Vector3i) =
        let lighting = level.Lighting
        let noChange _ = ()
        for chunk in chunks do
            let nearCenter =
                abs (chunk.ChunkCoord.X - centerChunk.X) <= 1 &&
                abs (chunk.ChunkCoord.Y - centerChunk.Y) <= 1 &&
                abs (chunk.ChunkCoord.Z - centerChunk.Z) <= 1
            let chunkLevel = tryGetLevel lighting.ChunkFluid chunk.ChunkCoord
            if nearCenter && chunkLevel > 0uy then
                let struct (minBlock, maxBlock) = chunkBlockRange level chunk.ChunkCoord
                let blockCount =
                    (inc maxBlock.X - minBlock.X) *
                    (inc maxBlock.Y - minBlock.Y) *
                    (inc maxBlock.Z - minBlock.Z)
                lighting.ChunkTouched.Add chunk.ChunkCoord |> ignore<bool>
                lighting.DirtyChunks.Add chunk.ChunkCoord |> ignore<bool>
                setLevel noChange lighting.ChunkFluid chunk.ChunkCoord 0uy
                enqueueNeighborhood lighting.ChunkFluid chunk.ChunkCoord
                addReservoir lighting.BlockReservoirs (enqueueBlockReservoir lighting) chunk.ChunkCoord (int chunkLevel * min blockCount chunkBlockBudget)

    let private cacheLoadedBlockOpenness (level : VoxelLevel) (chunks : VoxelChunk array) (centerChunk : Vector3i) =
        let lighting = level.Lighting
        for chunk in chunks do
            let coord = chunk.ChunkCoord
            if abs (coord.X - centerChunk.X) <= 1 &&
               abs (coord.Y - centerChunk.Y) <= 1 &&
               abs (coord.Z - centerChunk.Z) <= 1 &&
               lighting.BlockOpenCachedChunks.Add coord then
                for blockCoord in chunk.OpaqueBlockCoords do
                    lighting.BlockOpenCache[blockCoord] <- false

    let private setBlockIngress (lighting : VoxelLightingState) blockCoord mask =
        let mutable current = 0uy
        lighting.BlockIngress.TryGetValue (blockCoord, &current) |> ignore<bool>
        lighting.BlockIngress[blockCoord] <- current ||| mask

    let private ensureBlockReservoirEntrances (level : VoxelLevel) chunkCoord =
        let lighting = level.Lighting
        match lighting.BlockReservoirEntrances.TryGetValue chunkCoord with
        | true, entrances -> entrances
        | false, _ ->
            let struct (minBlock, maxBlock) = chunkBlockRange level chunkCoord
            let center = (minBlock + maxBlock) / 2
            let entrances =
                [|for z in minBlock.Z .. maxBlock.Z do
                      for y in minBlock.Y .. maxBlock.Y do
                          for x in minBlock.X .. maxBlock.X do
                              let coord = v3i x y z
                              let mask = entryMaskInRange minBlock maxBlock coord
                              if mask <> 0uy && blockCellOpen level coord then
                                  yield struct (coord, mask)|]
                |> Array.sortBy
                    (fun struct (coord, _) ->
                        let offset = coord - center
                        struct
                            (int (Vector3.Dot (offset.V3, SunlightDirection)) * -1,
                             abs offset.X + abs offset.Y + abs offset.Z,
                             coord.Z,
                             coord.Y,
                             coord.X))
            lighting.BlockReservoirEntrances[chunkCoord] <- entrances
            entrances

    let private processBlockReservoirs (level : VoxelLevel) =
        let lighting = level.Lighting
        let microVolume = level.BlockSideVoxels * level.BlockSideVoxels * level.BlockSideVoxels
        let maxFineAmount = int MaxLight * microVolume
        let mutable work = 0
        while work < blockReservoirWorkPerTick && lighting.BlockReservoirFrontier.Count > 0 do
            let chunkCoord = lighting.BlockReservoirFrontier.Dequeue ()
            lighting.BlockReservoirQueued.Remove chunkCoord |> ignore<bool>
            let mutable reservoir = 0
            if lighting.BlockReservoirs.TryGetValue (chunkCoord, &reservoir) && reservoir > 0 then
                let entrances = ensureBlockReservoirEntrances level chunkCoord
                if entrances.Length > 0 then
                    let mutable cursor = 0
                    lighting.BlockReservoirCursors.TryGetValue (chunkCoord, &cursor) |> ignore<bool>
                    let mutable probes = 0
                    let mutable found = false
                    let mutable target = v3iZero
                    let mutable targetMask = 0uy
                    let mutable targetFilled = false
                    while not found && probes < entrances.Length && work < blockReservoirWorkPerTick do
                        let struct (candidate, mask) = entrances[cursor]
                        probes <- inc probes
                        work <- inc work
                        if lighting.RefinedBlocks.Contains candidate then
                            let mutable fineAmount = 0
                            lighting.FineAmounts.TryGetValue (candidate, &fineAmount) |> ignore<bool>
                            if fineAmount + microVolume <= maxFineAmount then
                                target <- candidate
                                targetMask <- mask
                                targetFilled <- fineAmount + microVolume = maxFineAmount
                                found <- true
                            else cursor <- (cursor + 1) % entrances.Length
                        else
                            let current = tryGetLevel lighting.BlockFluid candidate
                            if current < MaxLight then
                                target <- candidate
                                targetMask <- mask
                                targetFilled <- current + 1uy = MaxLight
                                found <- true
                            else cursor <- (cursor + 1) % entrances.Length
                    if found && targetFilled then cursor <- (cursor + 1) % entrances.Length
                    lighting.BlockReservoirCursors[chunkCoord] <- cursor
                    if found then
                        setBlockIngress lighting target targetMask
                        if lighting.RefinedBlocks.Contains target then
                            lighting.MicroEntrances.Remove target |> ignore<bool>
                            addFineReservoir level target microVolume
                        else addLevel (markBlockChange level) lighting.BlockFluid target 1uy
                        reservoir <- dec reservoir
                        if reservoir = 0 then
                            lighting.BlockReservoirs.Remove chunkCoord |> ignore<bool>
                            lighting.BlockReservoirCursors.Remove chunkCoord |> ignore<bool>
                        else lighting.BlockReservoirs[chunkCoord] <- reservoir
                    if reservoir > 0 && (found || probes < entrances.Length) then
                        enqueueBlockReservoir lighting chunkCoord
                else
                    lighting.BlockReservoirCursors.Remove chunkCoord |> ignore<bool>
            else
                lighting.BlockReservoirCursors.Remove chunkCoord |> ignore<bool>
                work <- inc work
        work

    let private chunkNear (center : Vector3i) (radius : int) (coord : Vector3i) =
        abs (coord.X - center.X) <= radius &&
        abs (coord.Y - center.Y) <= 1 &&
        abs (coord.Z - center.Z) <= radius

    let private ensureMicroEntrances (level : VoxelLevel) blockCoord =
        let lighting = level.Lighting
        let mutable entrances = Unchecked.defaultof<Vector3i array>
        if lighting.MicroEntrances.TryGetValue (blockCoord, &entrances) then entrances
        else
            let mutable ingress = sunIngressMask
            lighting.BlockIngress.TryGetValue (blockCoord, &ingress) |> ignore<bool>
            if ingress = 0uy then ingress <- sunIngressMask
            let start = VoxelWorld.blockStartCoord level blockCoord
            let side = level.BlockSideVoxels
            let cells = ResizeArray<Vector3i> ()
            for z in 0 .. dec side do
                for y in 0 .. dec side do
                    for x in 0 .. dec side do
                        let local = v3i x y z
                        if isOnIngressFace ingress side local then
                            let coord = start + local
                            if microCellOpen level coord then cells.Add coord
            entrances <- cells.ToArray ()
            lighting.MicroEntrances[blockCoord] <- entrances
            entrances

    let private activeFineBlocks (level : VoxelLevel) (chunks : VoxelChunk array) centerChunk =
        let active = HashSet<Vector3i> (HashIdentity.Structural)
        for chunk in chunks do
            if chunkNear centerChunk microChunkRadius chunk.ChunkCoord then
                let struct (minBlock, maxBlock) = chunkBlockRange level chunk.ChunkCoord
                for z in minBlock.Z .. maxBlock.Z do
                    for y in minBlock.Y .. maxBlock.Y do
                        for x in minBlock.X .. maxBlock.X do
                            active.Add (v3i x y z) |> ignore<bool>
        active

    let private derefineBlock (level : VoxelLevel) blockCoord =
        let lighting = level.Lighting
        if lighting.RefinedBlocks.Contains blockCoord then
            let noChange _ = ()
            let side = level.BlockSideVoxels
            let microVolume = side * side * side
            let mutable physicalAmount = 0
            lighting.FineAmounts.TryGetValue (blockCoord, &physicalAmount) |> ignore<bool>
            let start = VoxelWorld.blockStartCoord level blockCoord
            for z in 0 .. dec side do
                for y in 0 .. dec side do
                    for x in 0 .. dec side do
                        let coord = start + v3i x y z
                        setLevel noChange lighting.MicroFluid coord 0uy
                        lighting.MicroFluid.Queued.Remove coord |> ignore<bool>
            let blockLevel = byte (min (int MaxLight) (physicalAmount / microVolume))
            let residual = physicalAmount - int blockLevel * microVolume
            setLevel noChange lighting.BlockFluid blockCoord blockLevel
            markBlockChange level blockCoord
            enqueueNeighborhood lighting.BlockFluid blockCoord
            if residual > 0 then lighting.BlockFineResiduals[blockCoord] <- residual
            else lighting.BlockFineResiduals.Remove blockCoord |> ignore<bool>
            lighting.MicroReservoirs.Remove blockCoord |> ignore<bool>
            lighting.FineAmounts.Remove blockCoord |> ignore<bool>
            lighting.MicroReservoirQueued.Remove blockCoord |> ignore<bool>
            lighting.MicroReservoirCursors.Remove blockCoord |> ignore<bool>
            lighting.MicroEntrances.Remove blockCoord |> ignore<bool>
            lighting.MicroSeededCounts.Remove blockCoord |> ignore<bool>
            lighting.FineFallbackLevels.Remove blockCoord |> ignore<bool>
            lighting.FineVisualReady.Remove blockCoord |> ignore<bool>
            lighting.BlockIngress.Remove blockCoord |> ignore<bool>
            lighting.RefinedBlocks.Remove blockCoord |> ignore<bool>
            let chunkCoord = VoxelWorld.sourceCoordToChunkCoord level start
            lighting.DirtyChunks.Add chunkCoord |> ignore<bool>

    let private derefineFarBlocks (level : VoxelLevel) (active : HashSet<Vector3i>) =
        let toDerefine =
            level.Lighting.RefinedBlocks
            |> Seq.filter (active.Contains >> not)
            |> Seq.sortBy (fun coord -> struct (coord.Z, coord.Y, coord.X))
            |> Seq.toArray
        for blockCoord in toDerefine do derefineBlock level blockCoord

    let private refineActiveBlocks (level : VoxelLevel) (active : HashSet<Vector3i>) =
        let lighting = level.Lighting
        let noChange _ = ()
        let microVolume = level.BlockSideVoxels * level.BlockSideVoxels * level.BlockSideVoxels
        for blockCoord in active |> Seq.sortBy (fun coord -> struct (coord.Z, coord.Y, coord.X)) do
            if blockCellOpen level blockCoord then
                let blockLevel = tryGetLevel lighting.BlockFluid blockCoord
                let mutable residual = 0
                lighting.BlockFineResiduals.TryGetValue (blockCoord, &residual) |> ignore<bool>
                let fallback = sampleBlockHierarchyLight level blockCoord
                if blockLevel > 0uy || residual > 0 then
                    setLevel noChange lighting.BlockFluid blockCoord 0uy
                    enqueueNeighborhood lighting.BlockFluid blockCoord
                    lighting.BlockFineResiduals.Remove blockCoord |> ignore<bool>
                    addFineReservoir level blockCoord (int blockLevel * microVolume + residual)
                if lighting.RefinedBlocks.Add blockCoord then
                    lighting.FineFallbackLevels[blockCoord] <- fallback
                    let blockStart = VoxelWorld.blockStartCoord level blockCoord
                    lighting.DirtyChunks.Add (VoxelWorld.sourceCoordToChunkCoord level blockStart) |> ignore<bool>
                ensureMicroEntrances level blockCoord |> ignore<Vector3i array>

    let private markMicroChange (level : VoxelLevel) (coord : Vector3i) =
        let lighting = level.Lighting
        if VoxelWorld.isSourceCoordInBounds level coord then
            lighting.DirtyChunks.Add (VoxelWorld.sourceCoordToChunkCoord level coord) |> ignore<bool>
        for struct (offset, _) in directions do
            let neighbor = coord + offset
            if VoxelWorld.isSourceCoordInBounds level neighbor then
                lighting.DirtyChunks.Add (VoxelWorld.sourceCoordToChunkCoord level neighbor) |> ignore<bool>

    let private markFineVisualReady (level : VoxelLevel) blockCoord =
        let lighting = level.Lighting
        if lighting.FineVisualReady.Add blockCoord then
            let start = VoxelWorld.blockStartCoord level blockCoord
            lighting.DirtyChunks.Add (VoxelWorld.sourceCoordToChunkCoord level start) |> ignore<bool>

    let private updateFineVisualReadiness (level : VoxelLevel) (active : HashSet<Vector3i>) =
        let lighting = level.Lighting
        for blockCoord in active do
            if not (lighting.FineVisualReady.Contains blockCoord) &&
               not (lighting.FineAmounts.ContainsKey blockCoord) then
                let start = VoxelWorld.blockStartCoord level blockCoord
                let chunkCoord = VoxelWorld.sourceCoordToChunkCoord level start
                let blockFlowSettled =
                    not (lighting.BlockReservoirs.ContainsKey chunkCoord) &&
                    lighting.BlockFluid.Frontier.Count = 0
                if blockFlowSettled then markFineVisualReady level blockCoord

    let private processMicroReservoirs (level : VoxelLevel) =
        let lighting = level.Lighting
        let mutable work = 0
        while work < microReservoirWorkPerTick && lighting.MicroReservoirFrontier.Count > 0 do
            let blockCoord = lighting.MicroReservoirFrontier.Dequeue ()
            lighting.MicroReservoirQueued.Remove blockCoord |> ignore<bool>
            let mutable reservoir = 0
            if lighting.MicroReservoirs.TryGetValue (blockCoord, &reservoir) && reservoir > 0 then
                let entrances = ensureMicroEntrances level blockCoord
                let mutable cursor = 0
                lighting.MicroReservoirCursors.TryGetValue (blockCoord, &cursor) |> ignore<bool>
                let mutable found = false
                let mutable searched = 0
                while not found && searched < entrances.Length do
                    let index = (cursor + searched) % entrances.Length
                    let coord = entrances[index]
                    let previous = tryGetLevel lighting.MicroFluid coord
                    if microCellOpen level coord && previous < MaxLight then
                        addLevel (markMicroChange level) lighting.MicroFluid coord 1uy
                        if previous = 0uy then
                            let mutable seededCount = 0
                            lighting.MicroSeededCounts.TryGetValue (blockCoord, &seededCount) |> ignore<bool>
                            seededCount <- inc seededCount
                            lighting.MicroSeededCounts[blockCoord] <- seededCount
                            if seededCount >= entrances.Length then markFineVisualReady level blockCoord
                        cursor <- if previous + 1uy = MaxLight then (inc index) % entrances.Length else index
                        reservoir <- dec reservoir
                        found <- true
                    searched <- inc searched
                lighting.MicroReservoirCursors[blockCoord] <- cursor
                if reservoir = 0 then lighting.MicroReservoirs.Remove blockCoord |> ignore<bool>
                else lighting.MicroReservoirs[blockCoord] <- reservoir
                if reservoir = 0 || entrances.Length = 0 then markFineVisualReady level blockCoord
                if reservoir > 0 && found then enqueueMicroReservoir lighting blockCoord
            work <- inc work
        work

    let advance (level : VoxelLevel) (chunks : VoxelChunk array) (centerChunk : Vector3i) =
        lock level.Lighting.SyncRoot (fun () ->
            level.Lighting.Tick <- inc level.Lighting.Tick
            initializeChunkSources level
            cacheLoadedBlockOpenness level chunks centerChunk
            for chunk in chunks do
                if
                    chunk.SplatCount > 0 &&
                    not (level.Lighting.LightDescriptors.ContainsKey chunk.ChunkCoord)
                then
                    level.Lighting.DirtyChunks.Add chunk.ChunkCoord |> ignore<bool>
            let noChange _ = ()
            let noTransfer _ _ = ()
            let markChunkChange coord =
                level.Lighting.DirtyChunks.Add coord |> ignore<bool>
                level.Lighting.ChunkTouched.Add coord |> ignore<bool>
            prepareChunkPriorityFrontier level.Lighting centerChunk
            let chunkWork =
                processFluid
                    chunkWorkPerTick
                    false
                    (ValueSome (struct (level.Lighting.ChunkPriorityFrontier, centerChunk)))
                    (inBounds level.ChunkCounts)
                    (chunkCellOpen level)
                    (chunkTransitionOpen level)
                    markChunkChange
                    noTransfer
                    level.Lighting.ChunkFluid
            refineLoadedChunks level chunks centerChunk
            let blockReservoirWork = processBlockReservoirs level
            let microVolume = level.BlockSideVoxels * level.BlockSideVoxels * level.BlockSideVoxels
            let blockTransfer source destination =
                let lighting = level.Lighting
                setBlockIngress lighting destination (ingressMaskFromStep (destination - source))
                if lighting.RefinedBlocks.Contains destination then
                    let transferred = tryGetLevel lighting.BlockFluid destination
                    if transferred > 0uy then
                        setLevel noChange lighting.BlockFluid destination 0uy
                        addFineReservoir level destination (int transferred * microVolume)
            let blockLodOpen coord =
                if not (blockCellOpen level coord) then false
                elif not (level.Lighting.RefinedBlocks.Contains coord) then true
                else
                    let mutable amount = 0
                    level.Lighting.FineAmounts.TryGetValue (coord, &amount) |> ignore<bool>
                    amount + microVolume <= int MaxLight * microVolume
            let blockWork =
                processFluid
                    blockWorkPerTick
                    true
                    ValueNone
                    (VoxelWorld.isBlockCoordInBounds level)
                    blockLodOpen
                    alwaysTraverse
                    (markBlockChange level)
                    blockTransfer
                    level.Lighting.BlockFluid
            let active = activeFineBlocks level chunks centerChunk
            derefineFarBlocks level active
            refineActiveBlocks level active
            let microReservoirWork = processMicroReservoirs level
            let microTransfer source destination =
                let lighting = level.Lighting
                let sourceBlock = VoxelWorld.sourceCoordToBlockCoord level source
                let destinationBlock = VoxelWorld.sourceCoordToBlockCoord level destination
                if sourceBlock <> destinationBlock then
                    adjustFineAmount lighting sourceBlock -1
                    setBlockIngress lighting destinationBlock (ingressMaskFromStep (destinationBlock - sourceBlock))
                    if lighting.RefinedBlocks.Contains destinationBlock then
                        adjustFineAmount lighting destinationBlock 1
                        markFineVisualReady level destinationBlock
                    else
                        setLevel (markMicroChange level) lighting.MicroFluid destination 0uy
                        let mutable residual = 0
                        lighting.BlockFineResiduals.TryGetValue (destinationBlock, &residual) |> ignore<bool>
                        residual <- inc residual
                        if residual >= microVolume then
                            residual <- residual - microVolume
                            addLevel (markBlockChange level) lighting.BlockFluid destinationBlock 1uy
                        if residual > 0 then lighting.BlockFineResiduals[destinationBlock] <- residual
                        else lighting.BlockFineResiduals.Remove destinationBlock |> ignore<bool>

            let microLodOpen coord =
                if not (microCellOpen level coord) then false
                else
                    let lighting = level.Lighting
                    let blockCoord = VoxelWorld.sourceCoordToBlockCoord level coord
                    if lighting.RefinedBlocks.Contains blockCoord then true
                    else
                        let mutable residual = 0
                        lighting.BlockFineResiduals.TryGetValue (blockCoord, &residual) |> ignore<bool>
                        (int (tryGetLevel lighting.BlockFluid blockCoord) * microVolume + residual) <
                            int MaxLight * microVolume
            let microWork =
                processFluid
                    microWorkPerTick
                    true
                    ValueNone
                    (VoxelWorld.isSourceCoordInBounds level)
                    microLodOpen
                    alwaysTraverse
                    (markMicroChange level)
                    microTransfer
                    level.Lighting.MicroFluid
            updateFineVisualReadiness level active
            struct (chunkWork, blockReservoirWork, blockWork, microReservoirWork, microWork))

    let shouldRelight (level : VoxelLevel) =
        lock level.Lighting.SyncRoot (fun () ->
            level.Lighting.DirtyChunks.Count > 0)

    let takeDirtyChunks (maxColumnCount : int) (level : VoxelLevel) (loadedChunks : VoxelChunk array) (centerChunk : Vector3i) =
        lock level.Lighting.SyncRoot (fun () ->
            let dirtyColumns = HashSet<struct (int * int)> (HashIdentity.Structural)
            for chunk in loadedChunks do
                let coord = chunk.ChunkCoord
                if level.Lighting.DirtyChunks.Contains coord then
                    dirtyColumns.Add (struct (coord.X, coord.Z)) |> ignore<bool>
            let candidates = dirtyColumns |> Seq.toArray
            Array.sortInPlaceBy
                (fun struct (x, z) ->
                    let dx = x - centerChunk.X
                    let dz = z - centerChunk.Z
                    struct (dx * dx + dz * dz, z, x))
                candidates
            let selectedColumns = HashSet<struct (int * int)> (HashIdentity.Structural)
            for i in 0 .. min (dec maxColumnCount) (dec candidates.Length) do
                selectedColumns.Add candidates[i] |> ignore<bool>
            let selected = ResizeArray<Vector3i> ()
            for chunk in loadedChunks do
                let coord = chunk.ChunkCoord
                if selectedColumns.Contains (struct (coord.X, coord.Z)) then
                    selected.Add coord
            for coord in selected do level.Lighting.DirtyChunks.Remove coord |> ignore<bool>
            selected.ToArray ())


    let private sampleVisualLight (level : VoxelLevel) coord =
        if not (VoxelWorld.isSourceCoordInBounds level coord) then 0uy
        else
            let lighting = level.Lighting
            if not lighting.Initialized then 0uy
            else
                let blockCoord = VoxelWorld.sourceCoordToBlockCoord level coord
                if lighting.RefinedBlocks.Contains blockCoord then
                    if lighting.FineVisualReady.Contains blockCoord then
                        tryGetLevel lighting.MicroFluid coord
                    else
                        let mutable fallback = MaxLight
                        lighting.FineFallbackLevels.TryGetValue (blockCoord, &fallback) |> ignore<bool>
                        fallback
                else
                    sampleBlockHierarchyLight level blockCoord

    let lightDescriptor (level : VoxelLevel) (chunkCoord : Vector3i) (descriptor : VoxelModelDescriptor) =
        lock level.Lighting.SyncRoot (fun () ->
            let chunkMin =
                v3i
                    (chunkCoord.X * level.ChunkSizeVoxels.X)
                    (chunkCoord.Y * level.ChunkSizeVoxels.Y)
                    (chunkCoord.Z * level.ChunkSizeVoxels.Z)
            let localOrigin = descriptor.Bounds.Min + descriptor.VoxelSize * 0.5f
            let quantize position origin voxelSize = int (MathF.Round ((position - origin) / voxelSize))
            let splats = Array.zeroCreate<VoxelSplat> descriptor.Splats.Length
            for i in 0 .. dec descriptor.Splats.Length do
                let splat = descriptor.Splats[i]
                let localCoord =
                    v3i
                        (quantize splat.Position.X localOrigin.X descriptor.VoxelSize.X)
                        (quantize splat.Position.Y localOrigin.Y descriptor.VoxelSize.Y)
                        (quantize splat.Position.Z localOrigin.Z descriptor.VoxelSize.Z)
                let sourceCoord = chunkMin + localCoord
                let mutable light = 0uy
                for struct (offset, _) in directions do
                    light <- max light (sampleVisualLight level (sourceCoord + offset))
                splats[i] <- { splat with Light = light }
            { descriptor with Splats = splats })

    let invalidateBlock (level : VoxelLevel) (blockCoord : Vector3i) =
        lock level.Lighting.SyncRoot (fun () ->
            let lighting = level.Lighting
            lighting.BlockOpenCache.Remove blockCoord |> ignore<bool>
            derefineBlock level blockCoord
            lighting.MicroEntrances.Remove blockCoord |> ignore<bool>
            enqueueNeighborhood lighting.BlockFluid blockCoord
            if lighting.MicroReservoirs.ContainsKey blockCoord then enqueueMicroReservoir lighting blockCoord
            let start = VoxelWorld.blockStartCoord level blockCoord
            let finish = start + v3iDup (dec level.BlockSideVoxels)
            for z in dec start.Z .. inc finish.Z do
                for y in dec start.Y .. inc finish.Y do
                    for x in dec start.X .. inc finish.X do
                        let coord = v3i x y z
                        if tryGetLevel lighting.MicroFluid coord > 0uy then enqueueNeighborhood lighting.MicroFluid coord
            let minChunk = VoxelWorld.sourceCoordToChunkCoord level (Vector3i.Max (v3iZero, start - v3iOne))
            let maxChunk = VoxelWorld.sourceCoordToChunkCoord level (Vector3i.Min (level.SourceSizeVoxels - v3iOne, finish + v3iOne))
            for z in minChunk.Z .. maxChunk.Z do
                for y in minChunk.Y .. maxChunk.Y do
                    for x in minChunk.X .. maxChunk.X do
                        let chunkCoord = v3i x y z
                        enqueueNeighborhood lighting.ChunkFluid chunkCoord
                        lighting.BlockReservoirEntrances.Remove chunkCoord |> ignore<bool>
                        lighting.BlockReservoirCursors.Remove chunkCoord |> ignore<bool>
                        if lighting.BlockReservoirs.ContainsKey chunkCoord then enqueueBlockReservoir lighting chunkCoord
                        lighting.LightDescriptors.Remove chunkCoord |> ignore<bool>
                        lighting.DirtyChunks.Add chunkCoord |> ignore<bool>)
