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
}
with
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit
    }

    static member initial = {
        Gameplay.empty with
            GameplayState = Playing
    }

type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | Update
    | TimeUpdate
    interface Message

type GameplayCommand =
    | SetupScene
    | StartQuitting
    interface Command

[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Combat> world
        member this.SetGameplay value world = this.SetModelGeneric<Combat> value world
        member this.Gameplay = this.ModelGeneric<Combat> ()
        member this.QuitEvent = Events.QuitEvent --> this

type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

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

    override this.Message (model, message, screen, world) =
        match message with
        | StartPlaying ->
            [SetupScene], model

        | FinishQuitting ->
            let model = Gameplay.empty
            just model

        | Update ->
            just model

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let model = { model with GameplayTime = model.GameplayTime + gameDelta.Updates }
            just model

    override this.Command (model, command, screen, world) =

        match command with
        | SetupScene ->
            let world = World.setEye3dCenter (v3 0f 2f 3f) world
            let world = World.setEye3dRotation (Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -20f, 0f)) world
            let world = World.setEye2dCenter (v2 0f 0f) world
            let world = World.setEye2dCenter (v2 640f 360f) world
            just world

        | StartQuitting ->
            let world = World.publish () screen.QuitEvent screen world
            just world


    override this.Content (gameplay, screen) = [

        Content.group Simulants.GameplayGui.Name [] [
            Content.entity<CombatDispatcher> Simulants.GameplayCombat.Name [
                Entity.PositionLocal == v3 -160f -90f 0f
            ]
        ]

        Content.groupFromFile Simulants.GameplayScene.Name "Assets/Room/Scene.nugroup" [] []

        match gameplay.GameplayState with
        | Playing ->
            Content.group Simulants.GameplayCharacters.Name [] [
                character CharacterContent.player
                character CharacterContent.rat
            ]
        | Quit -> ()
    ]