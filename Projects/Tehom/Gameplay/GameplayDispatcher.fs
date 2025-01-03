namespace Tehom

open System
open System.Numerics
open Prime
open Nu

type GameplayState =
    | Playing
    | Quit

type [<SymbolicExpansion>] Gameplay = {
    GameplayTime : int64
    GameplayState : GameplayState
    CoordinatesAndRotations : (Vector3 * Quaternion) list
}
with
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit
        CoordinatesAndRotations = []
    }

    static member initial = {
        Gameplay.empty with
            GameplayState = Playing
            CoordinatesAndRotations =
                [
                    v3 0f 1f 2.75f,  Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 0f, 0f, 0f)
                    v3 0f 1f -2.75f, Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 180f, 0f, 0f)
                    v3 2.75f 1f 0f,  Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 90f, 0f, 0f)
                    v3 -2.75f 1f 0f, Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians -90f, 0f, 0f)
                ]
//                |> List.randomShuffle
    }

type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | Update
    | Shuffle
    | TimeUpdate
    interface Message

type GameplayCommand =
    | SetupScene
    | StartQuitting
    interface Command

[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.QuitEvent = Events.QuitEvent --> this

type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.initial)

    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    override this.Definitions (_, _) = [
        Screen.SelectEvent => StartPlaying
        Screen.DeselectingEvent => FinishQuitting
        Screen.UpdateEvent => Update
        Screen.TimeUpdateEvent => TimeUpdate
    ]

    override this.Message (model, message, entity, world) =
        match message with
        | StartPlaying ->
            [SetupScene], model

        | FinishQuitting ->
            let model = Gameplay.empty
            just model

        | Update ->
            just model

        | Shuffle ->
            let model = Gameplay.initial
            just model

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let model = { model with GameplayTime = model.GameplayTime + gameDelta.Updates }
            just model

    override this.Command (model, command, entity, world) =

        match command with
        | SetupScene ->
//            let world = World.setEye3dCenter (v3 0f 2f 3f) world
//            let world = World.setEye3dRotation (Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -20f, 0f)) world
            let world = World.setEye2dCenter (v2 0f 0f) world
            let world = World.setEye2dCenter (v2 640f 360f) world
            just world

        | StartQuitting ->
            let world = World.publish () entity.QuitEvent entity world
            just world


    override this.Content (gameplay, entity) = [

        Content.group Simulants.GameplayGui.Name [] [
            Content.entity<CombatDispatcher> Simulants.GameplayCombat.Name [
                //Entity.PositionLocal == v3 -160f -90f 0f
            ]

            Content.button "Shuffle" [
                Entity.Position == v3 -192f 160f 0f
                Entity.Size == v3 64f 16f 0f
                Entity.FontSizing == Some 8
                Entity.Text == "Shuffle!"
                Entity.ClickEvent => Shuffle
            ]
        ]

        match gameplay.GameplayState with
        | Playing ->

            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Room/Scene.nugroup" [] [
                let coordinates, rotations = gameplay.CoordinatesAndRotations |> List.unzip

                Content.composite<Ball3dDispatcher> "Chisel" [
                    Entity.Position := coordinates[0]
                    Entity.Rotation := rotations[0]
                ] [
                    ContentEx.camera "Camera" [
                        Entity.PositionLocal == v3 0f 0.5f 0.25f
                        Entity.RotationLocal == Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -20f, 0f)
                    ]
                ]

                Content.ball3d "Maldoror" [
                    Entity.Position := coordinates[1]
                    Entity.Rotation := rotations[1]
                ]

                Content.ball3d "Sigetical" [
                    Entity.Position := coordinates[2]
                    Entity.Rotation := rotations[2]
                ]

                Content.ball3d "Nanghait" [
                    Entity.Position := coordinates[3]
                    Entity.Rotation := rotations[3]
                ]

            ]

            Content.group Simulants.GameplayCharacters.Name [] [
                character CharacterContent.player
                character CharacterContent.rat
            ]
        | Quit -> ()
    ]