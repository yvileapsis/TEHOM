namespace VoxelForge
open System
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
      ChunkSize : Vector3 }

type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      VoxelModelReady : bool
      VoxelChunks : VoxelChunk array
      RayPickPositionOpt : Vector3 option }

    static member val empty =
        { GameplayTime = 0L
          GameplayState = Quit
          VoxelModelReady = false
          VoxelChunks = [||]
          RayPickPositionOpt = None }

    static member val initial =
        { Gameplay.empty with
            GameplayState = Playing }

type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | TimeUpdate
    interface Message

type GameplayCommand =
    | EnsureVoxelModel
    | DestroyVoxelModel
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
    let private sourceVoxelSize = v3Dup (1.0f / single minecraftBlockSideVoxels)
    let private levelChunkSizeVoxels = v3i 64 64 64
    let private levelChunkCounts = v3i 4 4 4
    let private levelSize = sourceVoxelSize * single minecraftLevelSideVoxels
    let private levelOffset = v3 0.0f (levelSize.Y * 0.5f) 0.0f

    let tryPickGround world =
        let ray = World.getMouseRay3dWorld world
        if abs ray.Direction.Y > 0.0001f then
            let t = -ray.Origin.Y / ray.Direction.Y
            if t > 0.0f then Some (ray.Origin + ray.Direction * t)
            else None
        else None

    let createVoxelModel world =
        match VoxelBake.tryBakeSliceAtlas Assets.Voxels.Minecraft sourceVoxelSize with
        | Some minecraftLevel ->
            let minecraftChunks = VoxelBake.chunk levelChunkSizeVoxels minecraftLevel
            [|for struct (chunkCoord, chunkCenter, minecraftChunk) in minecraftChunks do
                World.createUserDefinedVoxelModel minecraftChunk (Assets.Voxels.MinecraftLevelChunk chunkCoord.X chunkCoord.Y chunkCoord.Z) world
                { ChunkCoord = chunkCoord
                  ChunkCenter = chunkCenter + levelOffset
                  ChunkSize = minecraftChunk.Bounds.Size }|]
        | None ->
            Log.warnOnce "VoxelForge could not bake the minecraft voxel slice atlas."
            [||]

    let destroyVoxelModel world =
        for z in 0 .. dec levelChunkCounts.Z do
            for y in 0 .. dec levelChunkCounts.Y do
                for x in 0 .. dec levelChunkCounts.X do
                    World.destroyUserDefinedVoxelModel (Assets.Voxels.MinecraftLevelChunk x y z) world

    let setInitialCamera world =
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

    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.DeselectingEvent => FinishQuitting
         Screen.TimeUpdateEvent => TimeUpdate]

    override this.Message (gameplay, message, _, world) =
        match message with
        | StartPlaying ->
            let gameplay = { Gameplay.initial with VoxelModelReady = true }
            withSignal (signal EnsureVoxelModel) gameplay

        | FinishQuitting ->
            withSignal (signal DestroyVoxelModel) Gameplay.empty

        | TimeUpdate ->
            let gameplay =
                { gameplay with
                    GameplayTime = gameplay.GameplayTime + world.GameDelta.Updates
                    RayPickPositionOpt = GameplayLogic.tryPickGround world }
            if gameplay.GameplayState = Playing && not gameplay.VoxelModelReady then
                withSignal (signal EnsureVoxelModel) { gameplay with VoxelModelReady = true }
            else just gameplay

    override this.Command (gameplay, command, screen, world) =
        match command with
        | EnsureVoxelModel ->
            let voxelChunks = GameplayLogic.createVoxelModel world
            screen.SetGameplay { gameplay with GameplayState = Playing; VoxelModelReady = true; VoxelChunks = voxelChunks } world
            if world.Unaccompanied then GameplayLogic.setInitialCamera world
        | DestroyVoxelModel ->
            GameplayLogic.destroyVoxelModel world
        | StartQuitting ->
            World.publish () screen.QuitEvent screen world

    override this.Content (gameplay, _) =

        [if gameplay.GameplayState = Playing then
            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []

                [for voxelChunk in gameplay.VoxelChunks do
                    Content.voxel (Simulants.VoxelLevelChunk voxelChunk.ChunkCoord.X voxelChunk.ChunkCoord.Y voxelChunk.ChunkCoord.Z).Name
                        [Entity.Position == voxelChunk.ChunkCenter
                         Entity.Size == voxelChunk.ChunkSize
                         Entity.VoxelModel == Assets.Voxels.MinecraftLevelChunk voxelChunk.ChunkCoord.X voxelChunk.ChunkCoord.Y voxelChunk.ChunkCoord.Z
                         Entity.MaterialProperties ==
                            { MaterialProperties.empty with
                                RoughnessOpt = ValueSome 0.92f
                                MetallicOpt = ValueSome 0.0f
                                AmbientOcclusionOpt = ValueSome 1.0f
                                EmissionOpt = ValueSome 0.0f
                                ClearCoatOpt = ValueSome 0.0f
                                ClearCoatRoughnessOpt = ValueSome 1.0f }]

                 match gameplay.RayPickPositionOpt with
                 | Some position ->
                    Content.staticModel Simulants.RayPickMarker.Name
                        [Entity.Position := position + v3Up * 0.35f
                         Entity.Size == v3Dup 0.35f
                         Entity.StaticModel == Assets.Default.BallModel
                         Entity.MaterialProperties == { MaterialProperties.empty with AlbedoOpt = ValueSome Color.Cyan; EmissionOpt = ValueSome 0.35f }]
                 | None -> ()

                 Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 232.0f -144.0f 0.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]]
