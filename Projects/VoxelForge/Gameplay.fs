namespace VoxelForge
open System
open System.Collections.Generic
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
      ChunkSize : Vector3
      BodyShape : BodyShape
      BoxCount : int }

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

    let rec private translateBodyShape translation bodyShape =
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

    let tryPickForward (world : World) =
        let pickRay = ray3 world.Eye3dCenter (world.Eye3dRotation.Forward * 1000.0f)
        World.rayCastBodies3d pickRay 2UL 2UL false world
        |> Array.tryHead
        |> Option.map (fun (intersection : BodyIntersection) -> intersection.Position)

    let createVoxelLevel world =
        match VoxelBake.tryBakeSliceAtlasVolume Assets.Voxels.Minecraft sourceVoxelSize with
        | Some minecraftLevel ->
            let minecraftChunks = VoxelBake.chunk levelChunkSizeVoxels minecraftLevel.VoxelModel
            let voxelPhysicsChunks = Dictionary<Vector3i, struct (Vector3 * BodyShape * int)> (HashIdentity.Structural)
            for struct (chunkCoord, chunkCenter, bodyShape, boxCount) in VoxelBake.chunkBodyShapes levelChunkSizeVoxels minecraftLevel do
                voxelPhysicsChunks.Add (chunkCoord, struct (chunkCenter, bodyShape, boxCount))
            let voxelChunks =
                [|for struct (chunkCoord, chunkCenter, minecraftChunk) in minecraftChunks do
                    let struct (bodyCenter, bodyShape, boxCount) =
                        match voxelPhysicsChunks.TryGetValue chunkCoord with
                        | (true, physicsChunk) -> physicsChunk
                        | (false, _) -> struct (chunkCenter, EmptyShape, 0)
                    let bodyShape = translateBodyShape (bodyCenter - chunkCenter) bodyShape
                    World.createUserDefinedVoxelModel minecraftChunk (Assets.Voxels.MinecraftLevelChunk chunkCoord.X chunkCoord.Y chunkCoord.Z) world
                    { ChunkCoord = chunkCoord
                      ChunkCenter = chunkCenter + levelOffset
                      ChunkSize = minecraftChunk.Bounds.Size
                      BodyShape = bodyShape
                      BoxCount = boxCount }|]
            let bodyShapeCount = voxelChunks |> Array.sumBy (fun (voxelChunk : VoxelChunk) -> voxelChunk.BoxCount)
            Log.infoOnce ("VoxelForge generated " + scstring voxelChunks.Length + " voxel chunks with " + scstring bodyShapeCount + " merged physics boxes.")
            voxelChunks
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
                    RayPickPositionOpt = GameplayLogic.tryPickForward world }
            if gameplay.GameplayState = Playing && not gameplay.VoxelModelReady then
                withSignal (signal EnsureVoxelModel) { gameplay with VoxelModelReady = true }
            else just gameplay

    override this.Command (gameplay, command, screen, world) =
        match command with
        | EnsureVoxelModel ->
            let voxelChunks = GameplayLogic.createVoxelLevel world
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
                        [Entity.FacetNames == Set.ofList [nameof VoxelFacet; nameof RigidBodyFacet]
                         Entity.Position == voxelChunk.ChunkCenter
                         Entity.Size == voxelChunk.ChunkSize
                         Entity.VoxelModel == Assets.Voxels.MinecraftLevelChunk voxelChunk.ChunkCoord.X voxelChunk.ChunkCoord.Y voxelChunk.ChunkCoord.Z
                         Entity.BodyType == Static
                         Entity.BodyShape == voxelChunk.BodyShape
                         Entity.CollisionCategories == "10"
                         Entity.Static == true
                         Entity.MaterialProperties ==
                            { MaterialProperties.empty with
                                RoughnessOpt = ValueSome 0.92f
                                MetallicOpt = ValueSome 0.0f
                                AmbientOcclusionOpt = ValueSome 1.0f
                                EmissionOpt = ValueSome 0.0f
                                ClearCoatOpt = ValueSome 0.0f
                                ClearCoatRoughnessOpt = ValueSome 1.0f }]

                 Content.entity<FirstPersonPlayerDispatcher> Simulants.GameplayPlayer.Name
                    [Entity.Position == v3 0.0f 18.0f 0.0f]

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
