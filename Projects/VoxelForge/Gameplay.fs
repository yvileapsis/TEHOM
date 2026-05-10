namespace VoxelForge
open System
open System.Numerics
open Prime
open Nu
open VoxelForge

type GameplayState =
    | Playing
    | Quit

type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      VoxelModelReady : bool
      RayPickPositionOpt : Vector3 option }

    static member val empty =
        { GameplayTime = 0L
          GameplayState = Quit
          VoxelModelReady = false
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

    let private fieldTiles = v2i 16 16
    let private sourceVoxelSize = v3Dup 0.2f
    let private fieldChunkSizeVoxels = v3i 64 16 64
    let fieldChunkCounts = v2i 4 4
    let fieldChunkSize = v3 (single fieldChunkSizeVoxels.X * sourceVoxelSize.X) (single fieldChunkSizeVoxels.Y * sourceVoxelSize.Y) (single fieldChunkSizeVoxels.Z * sourceVoxelSize.Z)
    let fieldSize = v3 (3.2f * single fieldTiles.X) 3.2f (3.2f * single fieldTiles.Y)

    let fieldChunkCenter x z =
        let fieldMin = fieldSize * -0.5f
        v3
            (fieldMin.X + (single x + 0.5f) * fieldChunkSize.X)
            0.0f
            (fieldMin.Z + (single z + 0.5f) * fieldChunkSize.Z)

    let tryPickGround world =
        let ray = World.getMouseRay3dWorld world
        if abs ray.Direction.Y > 0.0001f then
            let t = -ray.Origin.Y / ray.Direction.Y
            if t > 0.0f then Some (ray.Origin + ray.Direction * t)
            else None
        else None

    let createVoxelModel world =
        match VoxelBake.tryBakeSliceAtlas Assets.Voxels.GrassBlock sourceVoxelSize with
        | Some grassBlock ->
            let grassField = VoxelBake.tile fieldTiles.X fieldTiles.Y grassBlock
            let grassChunks = VoxelBake.chunk fieldChunkSizeVoxels grassField
            for struct (chunkCoord, _, grassChunk) in grassChunks do
                World.createUserDefinedVoxelModel grassChunk (Assets.Voxels.GrassFieldChunk chunkCoord.X chunkCoord.Z) world
        | None ->
            Log.warnOnce "VoxelForge could not bake the grass_block voxel slice atlas."

    let destroyVoxelModel world =
        for z in 0 .. dec fieldChunkCounts.Y do
            for x in 0 .. dec fieldChunkCounts.X do
                World.destroyUserDefinedVoxelModel (Assets.Voxels.GrassFieldChunk x z) world

    let setInitialCamera world =
        let eyeCenter = v3 28.0f 24.0f 34.0f
        let eyeRotation = Quaternion.CreateLookAt ((v3Zero - eyeCenter).Normalized, v3Up)
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

    override this.Command (_, command, screen, world) =
        match command with
        | EnsureVoxelModel ->
            GameplayLogic.createVoxelModel world
            if world.Unaccompanied then GameplayLogic.setInitialCamera world
        | DestroyVoxelModel ->
            GameplayLogic.destroyVoxelModel world
        | StartQuitting ->
            World.publish () screen.QuitEvent screen world

    override this.Content (gameplay, _) =

        [if gameplay.GameplayState = Playing then
            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []

                [for z in 0 .. dec GameplayLogic.fieldChunkCounts.Y do
                    for x in 0 .. dec GameplayLogic.fieldChunkCounts.X do
                        Content.voxel (Simulants.VoxelFieldChunk x z).Name
                            [Entity.Position == GameplayLogic.fieldChunkCenter x z
                             Entity.Size == GameplayLogic.fieldChunkSize
                             Entity.VoxelModel == Assets.Voxels.GrassFieldChunk x z
                             Entity.MaterialProperties == { MaterialProperties.empty with RoughnessOpt = ValueSome 0.84f }]

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
