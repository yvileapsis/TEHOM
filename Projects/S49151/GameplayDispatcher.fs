namespace S49151
open System
open System.Numerics
open Prime
open Nu
open S49151

// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | TimeUpdate
    interface Message

// this is our gameplay MMCC command type.
type GameplayCommand =
    | StartQuitting
    interface Command

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
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
        Screen.TimeUpdateEvent => TimeUpdate
    ]

    // here we handle the above messages
    override this.Message (model, message, _, world) =

        match message with
        | StartPlaying ->
            let gameplay = Gameplay.initial
            just gameplay

        | FinishQuitting ->
            let gameplay = Gameplay.empty
            just gameplay

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { model with GameplayTime = model.GameplayTime + gameDelta.Updates }
            just gameplay

    // here we handle the above commands
    override this.Command (_, command, screen, world) =
        match command with
        | StartQuitting ->
            World.publish () screen.QuitEvent screen world

    // here we describe the content of the game including the scene and the hud
    override this.Content (model, _) = [// the scene group while playing
        if model.GameplayState = Playing then

            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" [] [

                // decor
                Content.staticModel "StaticModel" [
                    Entity.Position == v3 0.0f 0.0f -2.0f
                    Entity.Rotation := Quaternion.CreateFromAxisAngle (
                        (v3 1.0f 0.75f 0.5f).Normalized,
                        model.GameplayTime % 360L |> single |> Math.DegreesToRadians
                    )
                ]

                // quit
                Content.button Simulants.GameplayQuit.Name [
                    Entity.Position == v3 232.0f -144.0f 0.0f
                    Entity.Text == "Quit"
                    Entity.ClickEvent => StartQuitting
                ]
            ]
    ]