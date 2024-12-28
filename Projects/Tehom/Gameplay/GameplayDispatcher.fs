namespace Tehom

open System
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

    // this represents the gameplay model in its initial state, such as when gameplay starts.
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

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) = [
        Screen.SelectEvent => StartPlaying
        Screen.DeselectingEvent => FinishQuitting
        Screen.UpdateEvent => Update
        Screen.TimeUpdateEvent => TimeUpdate
    ]

    // here we handle the above messages
    override this.Message (model, message, screen, world) =
        match message with
        | StartPlaying ->
            just model

        | FinishQuitting ->
            let model = Gameplay.empty
            just model

        | Update ->
            just model

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let model = { model with GameplayTime = model.GameplayTime + gameDelta.Updates }
            just model

    // here we handle the above commands
    override this.Command (model, command, screen, world) =

        match command with
        | SetupScene ->
            let world = World.setEye3dCenter (v3 0f 2f 3f) world
            let world = World.setEye3dRotation (Numerics.Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -20f, 0f)) world
            just world

        | StartQuitting ->
            let world = World.publish () screen.QuitEvent screen world
            just world


    // here we describe the content of the game including the hud, the scene, and the player
    override this.Content (gameplay, screen) = [
        // the gui group

        Content.group Simulants.GameplayGui.Name [] [
            Content.entity<CombatDispatcher> Simulants.GameplayCombat.Name []
        ]

        Content.groupFromFile Simulants.GameplayScene.Name "Assets/Room/Scene.nugroup" [] []

        // the scene group while playing
        match gameplay.GameplayState with
        | Playing ->
            Content.group Simulants.GameplayCharacters.Name [] [
                character CharacterContent.player
                character CharacterContent.rat
            ]
        // no scene group otherwise
        | Quit -> ()
    ]