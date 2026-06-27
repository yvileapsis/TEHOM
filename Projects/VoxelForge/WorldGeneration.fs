namespace VoxelForge
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open Prime
open Nu

type WorldGenerationPhase =
    | Waiting
    | Preparing
    | BuildingChunks
    | Completed
    | Failed of string

type WorldGenerationModel =
    { Settings : WorldGenSettings
      Phase : WorldGenerationPhase
      LevelOpt : VoxelLevel option
      PendingChunks : Vector3i array
      BuiltChunks : VoxelChunk array
      StatsOpt : GeneratedWorldStats option
      Progress : single
      Status : string }

    static member val initial =
        { Settings = WorldGenSettings.defaultSettings
          Phase = Waiting
          LevelOpt = None
          PendingChunks = [||]
          BuiltChunks = [||]
          StatsOpt = None
          Progress = 0.0f
          Status = "Waiting" }

type WorldGenerationMessage =
    | StartGeneration
    | TimeUpdate
    interface Message

type WorldGenerationCommand =
    | PrepareWorld
    | BuildNextChunks
    interface Command

[<AutoOpen>]
module WorldGenerationExtensions =
    type Screen with
        member this.GetWorldGeneration world = this.GetModelGeneric<WorldGenerationModel> world
        member this.SetWorldGeneration value world = this.SetModelGeneric<WorldGenerationModel> value world
        member this.WorldGeneration = this.ModelGeneric<WorldGenerationModel> ()
        member this.WorldGeneratedEvent = Events.WorldGeneratedEvent --> this

[<RequireQualifiedAccess>]
module WorldGenerationLogic =

    let private cacheMagic = "VFWG"
    let private cacheVersion = 1

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

    let private materialToInt material =
        match material with
        | Grass -> 0
        | Dirt -> 1
        | Stone -> 2
        | Sand -> 3
        | Wood -> 4
        | Leaves -> 5
        | Glass -> 6
        | Water -> 7
        | Lava -> 8
        | Ore -> 9
        | Brick -> 10
        | Crafted -> 11

    let private materialOfInt value =
        match value with
        | 0 -> Grass
        | 1 -> Dirt
        | 2 -> Stone
        | 3 -> Sand
        | 4 -> Wood
        | 5 -> Leaves
        | 6 -> Glass
        | 7 -> Water
        | 8 -> Lava
        | 9 -> Ore
        | 10 -> Brick
        | 11 -> Crafted
        | _ -> Crafted

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

    let private settingsSignature (settings : WorldGenSettings) =
        let macroCounts = VoxelWorld.macroBlockCounts settings
        let worldSizeBlocks = VoxelWorld.normalizedWorldSizeBlocks settings
        let activeBlockOrigin = VoxelWorld.activeBlockOrigin settings
        String.Join
            ("|",
             [|string settings.Seed
               string worldSizeBlocks.X; string worldSizeBlocks.Y; string worldSizeBlocks.Z
               string activeBlockOrigin.X; string activeBlockOrigin.Y; string activeBlockOrigin.Z
               string macroCounts.X; string macroCounts.Y; string macroCounts.Z
               string settings.ChunkCounts.X; string settings.ChunkCounts.Y; string settings.ChunkCounts.Z
               string settings.ChunkSizeVoxels.X; string settings.ChunkSizeVoxels.Y; string settings.ChunkSizeVoxels.Z
               string settings.BlockSideVoxels
               string settings.BlockGridOffsetVoxels.X; string settings.BlockGridOffsetVoxels.Y; string settings.BlockGridOffsetVoxels.Z
               string settings.VoxelSize.X; string settings.VoxelSize.Y; string settings.VoxelSize.Z
               string settings.SeaLevelBlocks
               string settings.LavaLevelBlocks
               string settings.TerrainScale
               string settings.MountainStrength
               string settings.CaveThreshold
               string settings.OreRate
               string settings.TreeRate|])

    let private cacheFilePath (settings : WorldGenSettings) =
        let signature = settingsSignature settings
        let safeSignature = signature.Replace ("|", "_")
        let safeSignature = safeSignature.Replace ("-", "m")
        let safeSignature = safeSignature.Replace (".", "p")
        let safeSignature = safeSignature.Replace (",", "p")
        let directoryPath =
            Path.Combine
                (Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData,
                 "VoxelForge",
                 "GeneratedWorlds")
        Path.Combine (directoryPath, "world_" + safeSignature + ".vfw")

    let private countStats (level : VoxelLevel) (treeCount : int) (oreMaterial : VoxelMaterialKind) =
        let mutable solidCount = 0
        let mutable fluidCount = 0
        let mutable oreCount = 0
        for entry in level.SourceVoxels do
            let cell = entry.Value
            if cell.Solid then solidCount <- inc solidCount
            if cell.Material = Water || cell.Material = Lava then fluidCount <- inc fluidCount
            if cell.Material = oreMaterial then oreCount <- inc oreCount
        { SourceVoxelCount = level.SourceVoxels.Count
          SolidVoxelCount = solidCount
          FluidVoxelCount = fluidCount
          TreeCount = treeCount
          OreVoxelCount = oreCount
          ChunkCount = 0
          BodyShapeCount = 0 }

    let private saveWorldCache (settings : WorldGenSettings) (level : VoxelLevel) (stats : GeneratedWorldStats) =
        try
            let filePath = cacheFilePath settings
            Directory.CreateDirectory (Path.GetDirectoryName filePath) |> ignore<DirectoryInfo>
            use stream = File.Open (filePath, FileMode.Create, FileAccess.Write, FileShare.None)
            use writer = new BinaryWriter (stream)
            writer.Write cacheMagic
            writer.Write cacheVersion
            writer.Write (settingsSignature settings)
            writeVector3i writer level.WorldSizeBlocks
            writeVector3i writer level.ActiveBlockOrigin
            writeVector3 writer level.SpawnPosition
            writer.Write stats.TreeCount
            writer.Write level.SourceVoxels.Count
            for entry in level.SourceVoxels do
                let coord = entry.Key
                let cell = entry.Value
                writeVector3i writer coord
                writeColor writer cell.Albedo
                writer.Write cell.Solid
                writer.Write (materialToInt cell.Material)
            Log.info ("VoxelForge saved generated world cache to '" + filePath + "'.")
        with exn ->
            Log.warn ("VoxelForge failed to save generated world cache due to: " + scstring exn)

    let private tryLoadWorldCache (settings : WorldGenSettings) (placeableBlocks : PlaceableBlock array) =
        let filePath = cacheFilePath settings
        if File.Exists filePath then
            try
                use stream = File.Open (filePath, FileMode.Open, FileAccess.Read, FileShare.Read)
                use reader = new BinaryReader (stream)
                let magic = reader.ReadString ()
                let version = reader.ReadInt32 ()
                let signature = reader.ReadString ()
                if magic = cacheMagic && version = cacheVersion && signature = settingsSignature settings then
                    let level = VoxelWorld.createEmptyLevel settings placeableBlocks v3Zero (VoxelRuntime.freshRevisionSeed ())
                    let worldSizeBlocks = readVector3i reader
                    let activeBlockOrigin = readVector3i reader
                    let spawnPosition = readVector3 reader
                    let treeCount = reader.ReadInt32 ()
                    let sourceVoxelCount = reader.ReadInt32 ()
                    if worldSizeBlocks = level.WorldSizeBlocks && activeBlockOrigin = level.ActiveBlockOrigin then
                        for _ in 0 .. dec sourceVoxelCount do
                            let coord = readVector3i reader
                            let albedo = readColor reader
                            let solid = reader.ReadBoolean ()
                            let material = materialOfInt (reader.ReadInt32 ())
                            VoxelWorld.setSourceCell level coord { Albedo = albedo; Solid = solid; Material = material }
                        let level = VoxelWorld.withSpawnPosition spawnPosition level
                        let stats = countStats level treeCount Ore
                        Log.info ("VoxelForge loaded generated world cache from '" + filePath + "'.")
                        Some struct (level, stats)
                    else None
                else None
            with exn ->
                Log.warn ("VoxelForge failed to load generated world cache due to: " + scstring exn)
                None
        else None

    let private heightAt (settings : WorldGenSettings) (level : VoxelLevel) (macroCounts : Vector3i) x z =
        let worldBlockCoord = VoxelWorld.blockCoordToWorldBlockCoord level (v3i x 0 z)
        let nx = single worldBlockCoord.X * settings.TerrainScale
        let nz = single worldBlockCoord.Z * settings.TerrainScale
        let warpX = (fbm2 (settings.Seed + 17) 3 (nx * 0.45f) (nz * 0.45f) - 0.5f) * 1.8f
        let warpZ = (fbm2 (settings.Seed + 23) 3 (nx * 0.45f + 19.0f) (nz * 0.45f - 7.0f) - 0.5f) * 1.8f
        let baseNoise = fbm2 settings.Seed 5 (nx + warpX) (nz + warpZ)
        let mountainNoise = ridged2 (settings.Seed + 41) 4 (nx * 0.55f - warpZ) (nz * 0.55f + warpX)
        let centerX = single level.WorldSizeBlocks.X * 0.5f
        let centerZ = single level.WorldSizeBlocks.Z * 0.5f
        let edgeX = abs (single worldBlockCoord.X - centerX) / max 1.0f centerX
        let edgeZ = abs (single worldBlockCoord.Z - centerZ) / max 1.0f centerZ
        let edgeFalloff = 1.0f - clamp01 ((max edgeX edgeZ - 0.72f) / 0.28f)
        let height =
            single settings.SeaLevelBlocks +
            (baseNoise - 0.38f) * 5.0f +
            mountainNoise * settings.MountainStrength * 4.0f
        int (MathF.Round height) |> max 2 |> min (macroCounts.Y - 3) |> fun value -> int (single value * edgeFalloff + single settings.SeaLevelBlocks * (1.0f - edgeFalloff))

    let private chooseTerrainTemplate settings worldX worldY worldZ localY height (grass : VoxelBlockTemplate) (dirt : VoxelBlockTemplate) (stone : VoxelBlockTemplate) (sand : VoxelBlockTemplate) (ore : VoxelBlockTemplate) =
        if localY = height then
            if height <= settings.SeaLevelBlocks + 1 then sand else grass
        elif localY >= height - 2 then dirt
        else
            let oreNoise = hash01 (settings.Seed + 79) worldX worldY worldZ
            if localY < settings.SeaLevelBlocks + 4 && oreNoise < settings.OreRate then ore
            else stone

    let private addCaves (settings : WorldGenSettings) (level : VoxelLevel) =
        let toRemove = ResizeArray<Vector3i> ()
        let sourceCenter = v3 (single level.SourceSizeVoxels.X * 0.5f) 0.0f (single level.SourceSizeVoxels.Z * 0.5f)
        let safeRadius = single (level.BlockSideVoxels * 3)
        let safeRadiusSquared = safeRadius * safeRadius
        for entry in level.SourceVoxels do
            let coord = entry.Key
            let cell = entry.Value
            if cell.Solid && cell.Material <> Wood && cell.Material <> Leaves then
                let horizontal = v3 (single coord.X) 0.0f (single coord.Z) - sourceCenter
                let spawnSafe = horizontal.LengthSquared () < safeRadiusSquared && coord.Y < (settings.SeaLevelBlocks + 4) * level.BlockSideVoxels
                let caveCeiling = (settings.SeaLevelBlocks + 8) * level.BlockSideVoxels
                if not spawnSafe && coord.Y < caveCeiling then
                    let worldCoord = VoxelWorld.sourceCoordToWorldVoxelCoord level coord
                    let scale = 0.045f
                    let density =
                        fbm3 (settings.Seed + 131) 4 (single worldCoord.X * scale) (single worldCoord.Y * scale * 1.35f) (single worldCoord.Z * scale) * 0.62f +
                        ridged3 (settings.Seed + 197) 3 (single worldCoord.X * scale * 1.75f) (single worldCoord.Y * scale) (single worldCoord.Z * scale * 1.75f) * 0.38f
                    let depthGate =
                        clamp01 (single (caveCeiling - coord.Y) / single (level.BlockSideVoxels * 7))
                    if density * depthGate > settings.CaveThreshold then
                        toRemove.Add coord
        for coord in toRemove do
            VoxelWorld.removeSourceCell level coord

    let private addStillWater (settings : WorldGenSettings) (level : VoxelLevel) (heightMap : int[,]) (water : VoxelBlockTemplate) =
        let macroCounts = v3i (heightMap.GetLength 0) level.ChunkCounts.Y (heightMap.GetLength 1)
        for z in 0 .. dec macroCounts.Z do
            for x in 0 .. dec macroCounts.X do
                let height = heightMap[x, z]
                if height < settings.SeaLevelBlocks then
                    for y in height + 1 .. settings.SeaLevelBlocks do
                        let blockCoord = v3i x y z
                        if VoxelWorld.isBlockCoordInBounds level blockCoord && VoxelWorld.blockIsEmpty level blockCoord then
                            VoxelWorld.setSourceBlock level blockCoord water

    let private addStillLava (settings : WorldGenSettings) (level : VoxelLevel) (macroCounts : Vector3i) (lava : VoxelBlockTemplate) =
        for z in 0 .. dec macroCounts.Z do
            for y in 0 .. min settings.LavaLevelBlocks (dec macroCounts.Y) do
                for x in 0 .. dec macroCounts.X do
                    let blockCoord = v3i x y z
                    let worldBlockCoord = VoxelWorld.blockCoordToWorldBlockCoord level blockCoord
                    if VoxelWorld.isBlockCoordInBounds level blockCoord &&
                       VoxelWorld.blockIsEmpty level blockCoord &&
                       hash01 (settings.Seed + 211) worldBlockCoord.X worldBlockCoord.Y worldBlockCoord.Z < 0.35f then
                        VoxelWorld.setSourceBlock level blockCoord lava

    let private addTrees (settings : WorldGenSettings) (level : VoxelLevel) (heightMap : int[,]) (macroCounts : Vector3i) (log : VoxelBlockTemplate) (leaves : VoxelBlockTemplate) =
        let mutable treeCount = 0
        for z in 1 .. macroCounts.Z - 2 do
            for x in 1 .. macroCounts.X - 2 do
                let height = heightMap[x, z]
                let worldBlockCoord = VoxelWorld.blockCoordToWorldBlockCoord level (v3i x height z)
                let treeNoise = hash01 (settings.Seed + 307) worldBlockCoord.X worldBlockCoord.Y worldBlockCoord.Z
                if height > settings.SeaLevelBlocks + 1 &&
                   height + 5 < macroCounts.Y &&
                   treeNoise < settings.TreeRate &&
                   worldBlockCoord.X % 3 <> 1 &&
                   worldBlockCoord.Z % 3 <> 1 then
                    let mutable canPlace = true
                    for y in height + 1 .. height + 5 do
                        let blockCoord = v3i x y z
                        canPlace <- canPlace && VoxelWorld.blockIsEmpty level blockCoord
                    if canPlace then
                        treeCount <- inc treeCount
                        for y in height + 1 .. height + 3 do
                            VoxelWorld.setSourceBlock level (v3i x y z) log
                        for y in height + 3 .. height + 5 do
                            let radius = if y = height + 5 then 1 else 2
                            for dz in -radius .. radius do
                                for dx in -radius .. radius do
                                    if abs dx + abs dz <= radius + 1 then
                                        let blockCoord = v3i (x + dx) y (z + dz)
                                        if VoxelWorld.isBlockCoordInBounds level blockCoord && VoxelWorld.blockIsEmpty level blockCoord then
                                            VoxelWorld.setSourceBlock level blockCoord leaves
        treeCount

    let private pickSpawn (settings : WorldGenSettings) (level : VoxelLevel) (heightMap : int[,]) (macroCounts : Vector3i) =
        let centerX = macroCounts.X / 2
        let centerZ = macroCounts.Z / 2
        let mutable spawnOpt = None
        let mutable radius = 0
        while spawnOpt.IsNone && radius < max macroCounts.X macroCounts.Z do
            for z in max 0 (centerZ - radius) .. min (dec macroCounts.Z) (centerZ + radius) do
                for x in max 0 (centerX - radius) .. min (dec macroCounts.X) (centerX + radius) do
                    if spawnOpt.IsNone then
                        let height = heightMap[x, z]
                        let feet = v3i x (height + 1) z
                        let head = v3i x (height + 2) z
                        if height >= settings.SeaLevelBlocks &&
                           VoxelWorld.isBlockCoordInBounds level feet &&
                           VoxelWorld.isBlockCoordInBounds level head &&
                           VoxelWorld.blockContainsSolidCell level (v3i x height z) &&
                           VoxelWorld.blockIsEmpty level feet &&
                           VoxelWorld.blockIsEmpty level head then
                            spawnOpt <- Some (VoxelWorld.blockTopPosition level (v3i x height z))
            radius <- inc radius
        match spawnOpt with
        | Some spawn -> spawn
        | None -> VoxelWorld.blockTopPosition level (v3i centerX settings.SeaLevelBlocks centerZ)

    let prepareWorld (settings : WorldGenSettings) (world : World) =
        let placeableBlocks = VoxelPalettes.createPlaceableBlocks settings.VoxelSize world
        match tryLoadWorldCache settings placeableBlocks with
        | Some (struct (level, stats)) ->
            struct (level, VoxelWorld.allChunkCoords level, stats)
        | None ->
            let templates = VoxelPalettes.createBlockTemplates settings.VoxelSize
            let grass = VoxelPalettes.requireTemplate "Grass" templates
            let dirt = VoxelPalettes.requireTemplate "Dirt" templates
            let stone = VoxelPalettes.requireTemplate "Stone" templates
            let sand = VoxelPalettes.requireTemplate "Sand" templates
            let log = VoxelPalettes.requireTemplate "Oak Log" templates
            let leaves = VoxelPalettes.requireTemplate "Leaves" templates
            let water = VoxelPalettes.requireTemplate "Water" templates
            let ore = VoxelPalettes.deriveTintedTemplate "Copper Ore" Ore true (color 0.95f 0.56f 0.22f 1.0f) 0.55f stone
            let lava = VoxelPalettes.deriveTintedTemplate "Lava" Lava false (color 1.0f 0.24f 0.02f 1.0f) 0.86f water
            let level = VoxelWorld.createEmptyLevel settings placeableBlocks v3Zero (VoxelRuntime.freshRevisionSeed ())
            let macroCounts = VoxelWorld.macroBlockCounts settings
            let heightMap = Array2D.zeroCreate<int> macroCounts.X macroCounts.Z
            for z in 0 .. dec macroCounts.Z do
                for x in 0 .. dec macroCounts.X do
                    let height = heightAt settings level macroCounts x z
                    heightMap[x, z] <- height
                    for y in 0 .. height do
                        let worldBlockCoord = VoxelWorld.blockCoordToWorldBlockCoord level (v3i x y z)
                        let template =
                            chooseTerrainTemplate
                                settings
                                worldBlockCoord.X
                                worldBlockCoord.Y
                                worldBlockCoord.Z
                                y
                                height
                                grass
                                dirt
                                stone
                                sand
                                ore
                        VoxelWorld.setSourceBlock level (v3i x y z) template
            addCaves settings level
            addStillWater settings level heightMap water
            addStillLava settings level macroCounts lava
            let treeCount = addTrees settings level heightMap macroCounts log leaves
            let spawn = pickSpawn settings level heightMap macroCounts
            let level = VoxelWorld.withSpawnPosition spawn level
            let stats = countStats level treeCount Ore
            saveWorldCache settings level stats
            struct (level, VoxelWorld.allChunkCoords level, stats)

type WorldGenerationDispatcher () =
    inherit ScreenDispatcher<WorldGenerationModel, WorldGenerationMessage, WorldGenerationCommand> (WorldGenerationModel.initial)

    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world then WorldGenerationModel.initial else WorldGenerationModel.initial

    override this.TruncateModel generation =
        { generation with
            Phase = Waiting
            LevelOpt = None
            PendingChunks = [||]
            BuiltChunks = [||]
            StatsOpt = None
            Progress = 0.0f
            Status = "Waiting" }

    override this.UntruncateModel (_, incoming) =
        { incoming with
            Phase = Waiting
            LevelOpt = None
            PendingChunks = [||]
            BuiltChunks = [||]
            StatsOpt = None
            Progress = 0.0f
            Status = "Waiting" }

    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartGeneration
         Screen.TimeUpdateEvent => TimeUpdate]

    override this.Message (generation, message, _, _) =
        match message with
        | StartGeneration ->
            withSignal (signal PrepareWorld) { generation with Phase = Preparing; Progress = 0.0f; Status = "Preparing world" }
        | TimeUpdate ->
            match generation.Phase with
            | BuildingChunks -> withSignal (signal BuildNextChunks) generation
            | Waiting | Preparing | Completed | Failed _ -> just generation

    override this.Command (generation, command, screen, world) =
        match command with
        | PrepareWorld ->
            try
                let struct (level, pendingChunks, stats) = WorldGenerationLogic.prepareWorld generation.Settings world
                screen.SetWorldGeneration
                    { generation with
                        Phase = BuildingChunks
                        LevelOpt = Some level
                        PendingChunks = pendingChunks
                        BuiltChunks = [||]
                        StatsOpt = Some stats
                        Progress = 0.08f
                        Status = "Building voxel chunks" }
                    world
            with exn ->
                screen.SetWorldGeneration
                    { generation with
                        Phase = Failed exn.Message
                        LevelOpt = None
                        PendingChunks = [||]
                        BuiltChunks = [||]
                        Progress = 1.0f
                        Status = "World generation failed: " + exn.Message }
                    world
        | BuildNextChunks ->
            match generation.LevelOpt, generation.StatsOpt with
            | Some level, Some stats ->
                let chunksPerUpdate = max 1 generation.Settings.ChunksPerUpdate
                let buildCount = min chunksPerUpdate generation.PendingChunks.Length
                let chunkBuilds =
                    generation.PendingChunks
                    |> Array.take buildCount
                    |> Array.Parallel.map (fun chunkCoord -> VoxelRuntime.tryBuildChunk level chunkCoord)
                    |> Array.choose id
                let chunksBuiltNow =
                    chunkBuilds
                    |> Array.map (fun chunkBuild -> VoxelRuntime.realizeChunk level chunkBuild world)
                let builtChunks = Array.append generation.BuiltChunks chunksBuiltNow
                let pendingChunks = generation.PendingChunks |> Array.skip buildCount
                let totalChunks = max 1 (builtChunks.Length + pendingChunks.Length)
                let progress = 0.08f + 0.92f * (single builtChunks.Length / single totalChunks)
                if pendingChunks.Length = 0 then
                    let builtChunks = VoxelRuntime.sortVoxelChunks builtChunks
                    let stats =
                        { stats with
                            ChunkCount = builtChunks.Length
                            BodyShapeCount = builtChunks |> Array.sumBy (fun chunk -> chunk.BoxCount) }
                    let package =
                        { Level = level
                          Chunks = builtChunks
                          SpawnPosition = level.SpawnPosition
                          Stats = stats }
                    screen.SetWorldGeneration
                        { generation with
                            Phase = Completed
                            PendingChunks = [||]
                            BuiltChunks = builtChunks
                            StatsOpt = Some stats
                            Progress = 1.0f
                            Status = "Entering world" }
                        world
                    World.publish package screen.WorldGeneratedEvent screen world
                else
                    screen.SetWorldGeneration
                        { generation with
                            PendingChunks = pendingChunks
                            BuiltChunks = builtChunks
                            Progress = progress
                            Status = "Building voxel chunks " + scstring builtChunks.Length + " / " + scstring totalChunks }
                        world
            | None, _ | _, None -> ()

    override this.Content (generation, _) =
        [Content.group Simulants.WorldGenerationEnvironment.Name []
            [Content.skyBox Simulants.WorldGenerationSkyBox.Name
                [Entity.Absolute == true
                 Entity.AmbientColor == color 0.86f 0.93f 1.0f 1.0f
                 Entity.AmbientBrightness == 0.72f
                 Entity.Color == color 0.72f 0.86f 1.0f 1.0f
                 Entity.Brightness == 1.15f
                 Entity.Presence == Omnipresent
                 Entity.Static == true]

             Content.light3d Simulants.WorldGenerationSunLight.Name
                [Entity.Position == v3 -48.0f 96.0f -64.0f
                 Entity.Rotation == Quaternion.CreateFromYawPitchRoll (-0.55f, -0.85f, 0.0f)
                 Entity.Presence == Omnipresent
                 Entity.Static == true
                 Entity.LightType == DirectionalLight 20.0f
                 Entity.Color == color 1.0f 0.94f 0.82f 1.0f
                 Entity.Brightness == 4.0f
                 Entity.LightCutoff == 128.0f
                 Entity.AutoAttenuate == false
                 Entity.DesireShadows == false
                 Entity.DesireFog == false]

             Content.lightProbe3d Simulants.WorldGenerationLightProbe.Name
                [Entity.Position == v3 0.0f 24.0f 0.0f
                 Entity.Presence == Omnipresent
                 Entity.Static == true
                 Entity.AmbientColor == color 0.86f 0.93f 1.0f 1.0f
                 Entity.AmbientBrightness == 0.72f
                 Entity.ProbeBounds == box3 (v3 -96.0f -16.0f -96.0f) (v3 192.0f 128.0f 192.0f)]

             Content.staticModel Simulants.WorldGenerationSun.Name
                [Entity.Position == v3 -48.0f 96.0f -64.0f
                 Entity.Size == v3Dup 8.0f
                 Entity.Scale == v3Dup 8.0f
                 Entity.Presence == Omnipresent
                 Entity.Static == true
                 Entity.Pickable == false
                 Entity.CastShadow == false
                 Entity.StaticModel == Assets.Default.BallModel
                 Entity.MaterialProperties ==
                    { MaterialProperties.defaultProperties with
                        AlbedoOpt = ValueSome (color 1.0f 0.92f 0.65f 1.0f)
                        EmissionOpt = ValueSome 3.0f
                        RoughnessOpt = ValueSome 0.45f
                        MetallicOpt = ValueSome 0.0f }]]

         Content.group Simulants.WorldGenerationGui.Name []
            [Content.text Simulants.WorldGenerationTitle.Name
                [Entity.Position == v3 0.0f 64.0f 0.0f
                 Entity.Size == v3 420.0f 48.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.Text == "Generating World"]

             Content.text Simulants.WorldGenerationStatus.Name
                [Entity.Position == v3 0.0f 20.0f 0.0f
                 Entity.Size == v3 520.0f 32.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.Text := generation.Status]

             Content.fillBar Simulants.WorldGenerationProgress.Name
                [Entity.Position == v3 0.0f -28.0f 0.0f
                 Entity.Size == v3 420.0f 24.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Fill := generation.Progress
                 Entity.FillInset == 0.12f
                 Entity.FillColor == color 0.18f 0.78f 0.52f 1.0f
                 Entity.BorderColor == color 0.08f 0.18f 0.22f 1.0f]]]
