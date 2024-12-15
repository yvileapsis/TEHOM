namespace MyGame
open System
open System.Numerics
open Prime
open Nu

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type Gameplay = {
    GameplayTime : int64
    GameplayState : GameplayState
    Selected : Entity option
    SelectedPosition : Vector3 option
    Entities : Entity list
    SelectedEntities : Entity list
}
with
    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit
        Selected = None
        SelectedPosition = None
        Entities = []
        SelectedEntities = []
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Gameplay.empty with
            GameplayState = Playing
    }

// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | TimeUpdate
    | SelectUnits of Entity list
    | Die of Entity
    interface Message

// this is our MMCC command type.
type GameplayCommand =
    | SetupScene
    | LeftClick
    | RightClick
    | StartQuitting
    | AttackCharacter of Entity
    | DestroyEnemy of Entity
    | TrackPlayer
    | PublishSelectionEvent
    interface Command


// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.BeginSelectingEvent = Events.BeginSelectingEvent --> this
        member this.QuitEvent = Events.QuitEvent --> this

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (gameplay, screen) = [
        Screen.SelectEvent => StartPlaying
        Screen.DeselectingEvent => FinishQuitting
        Screen.TimeUpdateEvent => TimeUpdate
        Game.MouseLeftDownEvent => LeftClick
        Game.MouseRightDownEvent => RightClick
        // Game.MouseLeftUpEvent => SetEntities gameplay.Entities
        // Events.AttackEvent --> Simulants.GameplayScene --> Address.Wildcard =|> fun evt -> AttackCharacter evt.Data
        Events.SelectionEvent --> Simulants.GameplayManagers --> Address.Wildcard =|> fun evt -> SelectUnits evt.Data
    ]

    // here we handle the above messages
    override this.Message (gameplay, message, screen, world) =

        match message with
        | StartPlaying ->
            let gameplay = Gameplay.initial
            let entities =
                World.getEntities Simulants.GameplayScene world
                |> Seq.filter (fun entity -> entity.Is<CharacterDispatcher> world)
                |> List.ofSeq

            let gameplay = { gameplay with Entities = entities }

            withSignals [SetupScene; TrackPlayer] gameplay
        | SelectUnits entities ->

            let gameplay = { gameplay with SelectedEntities = entities }
            just gameplay
        | FinishQuitting ->
            let gameplay = Gameplay.empty
            just gameplay

        | Die deadCharacter ->
            let character = deadCharacter.GetCharacter world
            match character.CharacterType with
            | Player -> withSignal StartQuitting gameplay
            | Enemy ->
                withSignal (DestroyEnemy deadCharacter) gameplay

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
            just gameplay


    // here we handle the above commands
    override this.Command (gameplay, command, screen, world) =

        match command with
        | SetupScene ->
//            let world = Simulants.GameplayPlayer.SetPosition (v3 0.0f 1.65f 0.0f) world
            let world = World.synchronizeNav3d screen world
            let world = World.setEye2dCenter (v2 0f 0f) world
            just world

        | StartQuitting ->
            let world = World.publish () screen.QuitEvent screen world
            just world

        | AttackCharacter entity ->
            let character = entity.GetCharacter world
            let character = { character with HitPoints = max (dec character.HitPoints) 0 }
            let (signals, character) =
                if character.HitPoints > 0 then
                    match character.ActionState with
                    | InjuryState _ -> just character
                    | _ ->
                        let character = { character with ActionState = InjuryState { InjuryTime = world.UpdateTime }}
                        World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world
                        just character
                else
                    match character.ActionState with
                    | WoundState _ -> just character
                    | _ ->
                        let character = { character with ActionState = WoundState { WoundTime = world.UpdateTime }}
                        World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world
                        just character
            let world = entity.SetCharacter character world
            withSignals signals world

        | DestroyEnemy entity ->
            let world = World.destroyEntity entity world
            just world

        | TrackPlayer ->

            // update eye to look at player
            (*let player = Simulants.GameplayPlayer.GetCharacter world
            let position = Simulants.GameplayPlayer.GetPosition world
            let rotation = Simulants.GameplayPlayer.GetRotation world
            let positionInterp = player.PositionInterp position
            let rotationInterp = player.RotationInterp rotation * Quaternion.CreateFromAxisAngle (v3Right, -0.1f)
            let world = World.setEye3dCenter (positionInterp + v3Up * 1.75f - rotationInterp.Forward * 3.0f) world
            let world = World.setEye3dRotation rotationInterp world
            *)
            just world

        | LeftClick ->

            [PublishSelectionEvent], world

        | RightClick ->
            let firstIntersection =
                let ray = World.getMouseRay3dWorld world
                let origin = ray.Origin
                let finale = ray.Origin + 100f * ray.Direction
                let array = World.rayCast3dBodies origin finale 0xFFFFFFFF 0xFFFFFFFF false world
                Array.tryHead array

            match firstIntersection with
            | Some intersectionData ->

                let position =
//                    World.position3dToPosition2d intersectionData.Position world
//                    / (single Constants.Render.VirtualScalar)
//                    |> Some
                    Some intersectionData.Position

                let gameplay = { gameplay with SelectedPosition = position }

                let world = screen.SetGameplay gameplay world

                just world

            | None ->
                just world

        | PublishSelectionEvent ->

            let entities = gameplay.Entities
            let world = World.publish entities screen.BeginSelectingEvent screen world
            just world
(*
            let selectedPositionOpt =
                match selectedOpt with
                | Some entity ->
                    let v3 = entity.GetPosition world
                    World.position3dToPosition2d (v3) world
                    / (single Constants.Render.VirtualScalar)
                    |> Some
                | None ->
                    None
*)



    // here we describe the content of the game including the hud, the scene, and the player
    override this.Content (gameplay, screen) = [
        // the scene group while playing


        if gameplay.GameplayState = Playing then
            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" [] [
                Content.staticModel "StaticModel" [
                    Entity.Position == v3 0.0f 3.0f -2.0f
                    Entity.Rotation :=
                        Quaternion.CreateFromAxisAngle ((v3 1.0f 0.75f 0.5f).Normalized, gameplay.GameplayTime % 360L |> single |> Math.DegreesToRadians)
                ]

                Content.entity<PlayerDispatcher> Simulants.GameplayPlayer.Name [
                    Entity.Persistent == false
                    Entity.DieEvent => Die Simulants.GameplayPlayer
                ]

                for i in List.init 1 id do
                    ContentEx.voxel $"VoxelTest{i}" [
                        Entity.VoxelChunk == (SlicesVoxel Assets.Voxels.Cars)
                        Entity.Position == v3 (single (i / 25) * 8f) 4f (single (i % 25) * 8f)
                        Entity.Size == v3 8f 8f 8f
                    ]
            ]

         // the gui group
        Content.group Simulants.GameplayGui.Name [] [
            // quit
            Content.button Simulants.GameplayQuit.Name [
                Entity.Position == v3 232.0f -144.0f 0.0f
                Entity.Text == "Quit"
                Entity.ClickEvent => StartQuitting
            ]

            Content.staticModel Simulants.GameplayMarker.Name [
                Entity.Position := match gameplay.SelectedPosition with | Some pos -> pos | None -> v3 0f 10f 0f
                Entity.Scale == v3 0.5f 0.5f 0.5f
                Entity.MaterialProperties == { MaterialProperties.empty with AlbedoOpt = ValueSome Color.Red }
                Entity.BodyShape == (SphereShape { Radius = 0.5f; TransformOpt = None; PropertiesOpt = None })
                Entity.StaticModel == Assets.Default.BallModel
            ]

        ]

        Content.group Simulants.GameplayManagers.Name [] [

            Content.entity<CameraManagerDispatcher> Simulants.GameplayCameraManager.Name []
            Content.entity<SelectionManagerDispatcher> Simulants.GameplaySelectionManager.Name []
            Content.entity<CursorManagerDispatcher> Simulants.GameplayCursorManager.Name  []
        ]

    ]