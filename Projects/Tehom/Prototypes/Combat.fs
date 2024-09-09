namespace Tehom

open System
open System.Numerics
open Prime
open Nu


type Stat =
    | Crippled
    | Feeble
    | Poor
    | Average
    | Gifted
    | Superior
    | Exceptional
    | Transcendent
    | Divine
    | Impossible
// C F P A S G E T D I
with
    static member asInt = function
        | Crippled -> 0
        | Feeble -> 1
        | Poor -> 2
        | Average -> 3
        | Gifted -> 4
        | Superior -> 5
        | Exceptional -> 6
        | Transcendent -> 7
        | Divine -> 8
        | Impossible -> 9



type Stats = {
    Plasma : Stat
    Oil : Stat
    Lymph : Stat
    Gall : Stat
}
with
    static member player = {
        Plasma = Average
        Oil = Average
        Lymph = Average
        Gall = Average
    }

module Dice =
    let rollDie () = Gen.random2 1 7
    let rollDice count =
        Seq.init count (fun _ -> rollDie ())
    let rollDiceTreshold count treshold () =
        rollDice count
        |> Seq.fold (fun state x -> if x > treshold then state + 1 else state) 0

type CombatState =
    | Playing
    | Quit



// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Combat =
    { GameplayTime : int64
      GameplayState : CombatState
      Dice : int
       }

    // this represents the gameplay model in a vacant state, such as when the gameplay screen is not selected.
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit
        Dice = 0
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Combat.empty
        with
            GameplayState = Playing
    }

    // this updates the gameplay model every frame that gameplay is active.
    static member update gameplay world =
        match gameplay.GameplayState with
        | Playing ->
            let dice = (Dice.rollDiceTreshold 10 3 ())
            { gameplay with Dice = dice }
        | Playing | Quit -> gameplay

// this is our gameplay MMCC message type.
type CombatMessage =
    | StartPlaying
    | FinishQuitting
    | Update
    | TimeUpdate
    interface Message

// this is our gameplay MMCC command type.
type CombatCommand =
    | StartQuitting
    interface Command

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module CombatExtensions =
    type Screen with
        member this.GetCombat world = this.GetModelGeneric<Combat> world
        member this.SetCombat value world = this.SetModelGeneric<Combat> value world
        member this.Combat = this.ModelGeneric<Combat> ()
        member this.QuitEvent = Events.QuitEvent --> this

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type CombatDispatcher () =
    inherit ScreenDispatcher<Combat, CombatMessage, CombatCommand> (Combat.empty)

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Combat.initial
        else Combat.empty

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.DeselectingEvent => FinishQuitting
         Screen.UpdateEvent => Update
         Screen.TimeUpdateEvent => TimeUpdate]

    // here we handle the above messages
    override this.Message (gameplay, message, _, world) =

        match message with
        | StartPlaying ->
            let gameplay = Combat.initial
            just gameplay

        | FinishQuitting ->
            let gameplay = Combat.empty
            just gameplay

        | Update ->
            let gameplay = Combat.update gameplay world
            just gameplay

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
            just gameplay

    // here we handle the above commands
    override this.Command (_, command, screen, world) =

        match command with
        | StartQuitting ->
            let world = World.publish () screen.QuitEvent screen world
            just world

    // here we describe the content of the game including the hud, the scene, and the player
    override this.Content (gameplay, _) = [
        // the gui group
        Content.group Simulants.GameplayGui.Name [] [
            richText "CombatWhatever" [
                Entity.Position == v3 0.0f 0.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                Entity.Text := string gameplay.Dice
                Entity.TextColor == Color.FloralWhite
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 10
            ]
        ]

        // the scene group while playing
        match gameplay.GameplayState with
        | Playing -> ()
        // no scene group otherwise
        | Quit -> ()
    ]