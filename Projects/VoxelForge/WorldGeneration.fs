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
      CompletedChunkBuildCount : int
      TotalChunkBuildCount : int
      PrebuildingChunkCache : bool
      StatsOpt : GeneratedWorldStats option
      Progress : single
      Status : string }

    static member val initial =
        { Settings = WorldGenSettings.defaultSettings
          Phase = Waiting
          LevelOpt = None
          PendingChunks = [||]
          BuiltChunks = [||]
          CompletedChunkBuildCount = 0
          TotalChunkBuildCount = 0
          PrebuildingChunkCache = false
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

    let private initialStreamChunkRadius = 2

    let private countFacilityTerminals (level : VoxelLevel) (generation : VoxelGeneration) =
        let counts = VoxelWorld.generatedBlockCounts level
        let pitch = max 8 generation.FacilityModulePitch
        let floorHeight = max 3 generation.FacilityFloorHeight
        let firstModuleX = level.ActiveBlockOrigin.X / pitch
        let firstModuleZ = level.ActiveBlockOrigin.Z / pitch
        let lastModuleX = (level.ActiveBlockOrigin.X + dec counts.X) / pitch
        let lastModuleZ = (level.ActiveBlockOrigin.Z + dec counts.Z) / pitch
        let floorCount = (counts.Y + dec floorHeight) / floorHeight
        let mutable terminalCount = 0
        for story in 0 .. dec floorCount do
            for moduleZ in firstModuleZ .. lastModuleZ do
                for moduleX in firstModuleX .. lastModuleX do
                    let blockCoord =
                        v3i
                            (moduleX * pitch + pitch / 2 - level.ActiveBlockOrigin.X)
                            (story * floorHeight + 1 - level.ActiveBlockOrigin.Y)
                            (moduleZ * pitch + dec pitch - 2 - level.ActiveBlockOrigin.Z)
                    match VoxelWorld.tryGetGeneratedBlockTemplateValue level blockCoord with
                    | Some template when template.Material = Terminal -> terminalCount <- inc terminalCount
                    | Some _ | None -> ()
        terminalCount

    let private makeFacilityStats (level : VoxelLevel) (generation : VoxelGeneration) =
        let counts = VoxelWorld.generatedBlockCounts level
        let pitch = max 8 generation.FacilityModulePitch
        let floorHeight = max 3 generation.FacilityFloorHeight
        let moduleCountX = (counts.X + dec pitch) / pitch
        let moduleCountZ = (counts.Z + dec pitch) / pitch
        { FacilityModuleCount = moduleCountX * moduleCountZ
          FacilityFloorCount = (counts.Y + dec floorHeight) / floorHeight
          TerminalCount = countFacilityTerminals level generation
          ChunkCount = 0
          BodyShapeCount = 0 }

    let initialChunkCoords (level : VoxelLevel) =
        match VoxelWorld.tryWorldToChunkCoord level level.SpawnPosition with
        | Some centerChunkCoord -> VoxelWorld.streamChunkCoords initialStreamChunkRadius level centerChunkCoord
        | None -> [||]

    let prepareWorld (settings : WorldGenSettings) (world : World) =
        let placeableBlocks = VoxelPalettes.createPlaceableBlocks settings.VoxelSize world
        let templates = VoxelPalettes.createBlockTemplates settings.VoxelSize
        let generation =
            { Seed = settings.Seed
              FacilityFloorHeight = settings.FacilityFloorHeight
              FacilityModulePitch = settings.FacilityModulePitch
              FacilityCorridorWidth = settings.FacilityCorridorWidth
              Templates =
                { Concrete = VoxelPalettes.requireTemplate "Aggregate Structural Concrete" templates
                  SpalledConcrete = VoxelPalettes.requireTemplate "Spalled Aggregate Debris" templates
                  Ceramic = VoxelPalettes.requireTemplate "Ochre Ceramic Tile" templates
                  Enamel = VoxelPalettes.requireTemplate "Sage Enamel Panel" templates
                  CeilingPanel = VoxelPalettes.requireTemplate "Ivory Acoustic Ceiling Panel" templates
                  ReinforcedGlass = VoxelPalettes.requireTemplate "Wire-Reinforced Laboratory Glass" templates
                  ServiceMetal = VoxelPalettes.requireTemplate "Open Service Grating" templates
                  StairTread = VoxelPalettes.requireTemplate "Cast Terrazzo Stair Tread" templates
                  StairRail = VoxelPalettes.requireTemplate "Signal Red Stair Handrail" templates
                  ContainmentBrick = VoxelPalettes.requireTemplate "Glazed Containment Brick" templates
                  HydroponicBed = VoxelPalettes.requireTemplate "Hydroponic Planter" templates
                  Vegetation = VoxelPalettes.requireTemplate "Arboretum Vegetation" templates
                  ProcessWater = VoxelPalettes.requireTemplate "Chlorinated Process Water" templates
                  Terminal = VoxelPalettes.requireTemplate "Relay Control Terminal" templates
                  WoodVeneer = VoxelPalettes.requireTemplate "Walnut Veneer Panel" templates
                  Upholstery = VoxelPalettes.requireTemplate "Moss Auditorium Upholstery" templates
                  MachineCasing = VoxelPalettes.requireTemplate "Stainless Process Machinery" templates
                  HazardStripe = VoxelPalettes.requireTemplate "Black-Ochre Hazard Marking" templates
                  FluorescentFixture = VoxelPalettes.requireTemplate "Ceramic Fluorescent Luminaire" templates
                  PipeAssembly = VoxelPalettes.requireTemplate "Painted Process Pipe Assembly" templates
                  InstrumentPanel = VoxelPalettes.requireTemplate "Analog Instrument Bank" templates
                  FacilityPlacard = VoxelPalettes.requireTemplate "Complex 17 Sector Placard" templates } }
        let level =
            VoxelWorld.createEmptyLevel settings placeableBlocks v3Zero (VoxelRuntime.freshRevisionSeed ())
            |> VoxelWorld.withGeneration generation
        let profileView : string = Environment.GetEnvironmentVariable "VOXELFORGE_PROFILE_VIEW"
        let spawnPosition =
            if not (isNull profileView) &&
               String.Equals (profileView.Trim (), "architecture", StringComparison.OrdinalIgnoreCase)
            then VoxelWorld.pickGeneratedZoneSpawn Atrium level
            else VoxelWorld.pickGeneratedSpawn level
        let level = VoxelWorld.withSpawnPosition spawnPosition level
        let stats = makeFacilityStats level generation
        let initialChunks = initialChunkCoords level
        struct (level, initialChunks, stats, false)

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
            CompletedChunkBuildCount = 0
            TotalChunkBuildCount = 0
            PrebuildingChunkCache = false
            StatsOpt = None
            Progress = 0.0f
            Status = "Waiting" }

    override this.UntruncateModel (_, incoming) =
        { incoming with
            Phase = Waiting
            LevelOpt = None
            PendingChunks = [||]
            BuiltChunks = [||]
            CompletedChunkBuildCount = 0
            TotalChunkBuildCount = 0
            PrebuildingChunkCache = false
            StatsOpt = None
            Progress = 0.0f
            Status = "Waiting" }

    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartGeneration
         Screen.TimeUpdateEvent => TimeUpdate]

    override this.Message (generation, message, _, _) =
        match message with
        | StartGeneration ->
            withSignal (signal PrepareWorld) { generation with Phase = Preparing; Progress = 0.0f; Status = "Reading facility survey archive" }
        | TimeUpdate ->
            match generation.Phase with
            | BuildingChunks -> withSignal (signal BuildNextChunks) generation
            | Waiting | Preparing | Completed | Failed _ -> just generation

    override this.Command (generation, command, screen, world) =
        match command with
        | PrepareWorld ->
            try
                let struct (level, pendingChunks, stats, prebuildingChunkCache) = WorldGenerationLogic.prepareWorld generation.Settings world
                screen.SetWorldGeneration
                    { generation with
                        Phase = BuildingChunks
                        LevelOpt = Some level
                        PendingChunks = pendingChunks
                        BuiltChunks = [||]
                        CompletedChunkBuildCount = 0
                        TotalChunkBuildCount = pendingChunks.Length
                        PrebuildingChunkCache = prebuildingChunkCache
                        StatsOpt = Some stats
                        Progress = 0.08f
                        Status = "Mapping sealed sectors" }
                    world
            with exn ->
                screen.SetWorldGeneration
                    { generation with
                        Phase = Failed exn.Message
                        LevelOpt = None
                        PendingChunks = [||]
                        BuiltChunks = [||]
                        CompletedChunkBuildCount = 0
                        TotalChunkBuildCount = 0
                        PrebuildingChunkCache = false
                        Progress = 1.0f
                        Status = "Facility mapping failed: " + exn.Message }
                    world
        | BuildNextChunks ->
            match generation.LevelOpt, generation.StatsOpt with
            | Some level, Some stats ->
                let chunksPerUpdate = max 1 generation.Settings.ChunksPerUpdate
                let buildCount = min chunksPerUpdate generation.PendingChunks.Length
                let initialChunkSet = HashSet<Vector3i> (HashIdentity.Structural)
                for chunkCoord in WorldGenerationLogic.initialChunkCoords level do
                    initialChunkSet.Add chunkCoord |> ignore<bool>
                let chunkBuildResults =
                    generation.PendingChunks
                    |> Array.take buildCount
                    |> Array.Parallel.map (fun chunkCoord -> struct (chunkCoord, VoxelRuntime.tryBuildChunkCached level chunkCoord))
                let chunksBuiltNow = ResizeArray<VoxelChunk> ()
                let editRevision = VoxelWorld.getEditRevision level
                for struct (chunkCoord, chunkBuildOpt) in chunkBuildResults do
                    match chunkBuildOpt with
                    | Some chunkBuild when initialChunkSet.Contains chunkCoord ->
                        chunksBuiltNow.Add (VoxelRuntime.realizeChunk level chunkBuild world)
                    | Some chunkBuild ->
                        VoxelWorld.updateChunkManifestFromBuild editRevision level chunkBuild
                    | None ->
                        VoxelWorld.markChunkManifestEmpty editRevision level chunkCoord
                if generation.PrebuildingChunkCache then
                    level.GeneratedBlockTemplateCache.Clear ()
                let builtChunks = Array.append generation.BuiltChunks (chunksBuiltNow.ToArray ())
                let pendingChunks = generation.PendingChunks |> Array.skip buildCount
                let completedChunkBuildCount = generation.CompletedChunkBuildCount + buildCount
                let totalChunkBuildCount = max 1 generation.TotalChunkBuildCount
                let progress = 0.08f + 0.92f * (single completedChunkBuildCount / single totalChunkBuildCount)
                if pendingChunks.Length = 0 then
                    let builtChunks = VoxelRuntime.sortVoxelChunks builtChunks
                    if generation.PrebuildingChunkCache then
                        VoxelRuntime.markChunkBuildCacheComplete level
                        level.GeneratedBlockTemplateCache.Clear ()
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
                            CompletedChunkBuildCount = completedChunkBuildCount
                            TotalChunkBuildCount = generation.TotalChunkBuildCount
                            PrebuildingChunkCache = false
                            StatsOpt = Some stats
                            Progress = 1.0f
                            Status = "Opening personnel bulkhead" }
                        world
                    World.publish package screen.WorldGeneratedEvent screen world
                else
                    screen.SetWorldGeneration
                        { generation with
                            PendingChunks = pendingChunks
                            BuiltChunks = builtChunks
                            CompletedChunkBuildCount = completedChunkBuildCount
                            Progress = progress
                            Status =
                                "Compiling sector geometry " +
                                scstring completedChunkBuildCount + " / " +
                                scstring totalChunkBuildCount }
                        world
            | None, _ | _, None -> ()

    override this.Content (generation, _) =
        [Content.group Simulants.WorldGenerationEnvironment.Name []
            [Content.skyBox Simulants.WorldGenerationSkyBox.Name
                [Entity.Absolute == true
                 Entity.AmbientColor == color 0.18f 0.22f 0.19f 1.0f
                 Entity.AmbientBrightness == 0.46f
                 Entity.Color == color 0.035f 0.045f 0.042f 1.0f
                 Entity.Brightness == 0.22f
                 Entity.Presence == Omnipresent
                 Entity.Static == true]

             Content.light3d Simulants.WorldGenerationSunLight.Name
                [Entity.Position == v3 0.0f 16.0f 0.0f
                 Entity.Rotation == Quaternion.CreateFromYawPitchRoll (-0.15f, -0.92f, 0.0f)
                 Entity.Presence == Omnipresent
                 Entity.Static == true
                 Entity.LightType == DirectionalLight 20.0f
                 Entity.Color == color 0.74f 0.82f 0.76f 1.0f
                 Entity.Brightness == 1.35f
                 Entity.LightCutoff == 64.0f
                 Entity.AutoAttenuate == false
                 Entity.DesireShadows == false
                 Entity.DesireFog == false]

             Content.lightProbe3d Simulants.WorldGenerationLightProbe.Name
                [Entity.Position == v3Zero
                 Entity.Presence == Omnipresent
                 Entity.Static == true
                 Entity.AmbientColor == color 0.34f 0.39f 0.35f 1.0f
                 Entity.AmbientBrightness == 0.58f
                 Entity.ProbeBounds == box3 (v3 -96.0f -16.0f -96.0f) (v3 192.0f 128.0f 192.0f)]]

         Content.group Simulants.WorldGenerationGui.Name []
            [Content.panel "Backdrop"
                [Entity.Position == v3Zero
                 Entity.Size == v3 640.0f 360.0f 0.0f
                 Entity.Elevation == 1.0f
                 Entity.Absolute == true
                 Entity.BackdropImageOpt == Some Assets.Default.White
                 Entity.Color == color 0.055f 0.07f 0.06f 0.94f]
                []

             Content.text Simulants.WorldGenerationTitle.Name
                [Entity.Position == v3 0.0f 76.0f 0.0f
                 Entity.Size == v3 520.0f 48.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.FontSizing == Some 27.0f
                 Entity.TextColor == color 0.78f 0.82f 0.66f 1.0f
                 Entity.Text == "INITIALIZING COMPLEX 17"]

             Content.text "Directive"
                [Entity.Position == v3 0.0f 45.0f 0.0f
                 Entity.Size == v3 520.0f 24.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.FontSizing == Some 14.0f
                 Entity.TextColor == color 0.46f 0.62f 0.55f 1.0f
                 Entity.Text == "PERSONNEL SURVEY // ARCHIVE 4-B"]

             Content.text Simulants.WorldGenerationStatus.Name
                [Entity.Position == v3 0.0f 6.0f 0.0f
                 Entity.Size == v3 540.0f 32.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor == color 0.82f 0.78f 0.61f 1.0f
                 Entity.Text := generation.Status]

             Content.fillBar Simulants.WorldGenerationProgress.Name
                [Entity.Position == v3 0.0f -38.0f 0.0f
                 Entity.Size == v3 420.0f 18.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Fill := generation.Progress
                 Entity.FillInset == 0.16f
                 Entity.FillColor == color 0.18f 0.55f 0.43f 1.0f
                 Entity.BorderColor == color 0.31f 0.22f 0.12f 1.0f]]]
