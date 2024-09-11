namespace Tehom

open System
open System.Numerics
open Prime
open Nu

type Element =
    | Plasma
    | Oil
    | Gall
    | Lymph

type MajorWounds =
    | Healthy = 0
    | Hurt = 1
    | Bruised = 2
    | Wounded = 3
    | Injured = 4
    | Critical = 5
    | Down = 6
    | Dying = 7
    | Dead = 8

type Stat =
    | Crippled = 0
    | Feeble = 1
    | Poor = 2
    | Average = 3
    | Gifted = 4
    | Superior = 5
    | Exceptional = 6
    | Transcendent = 7
    | Divine = 8
    | Impossible = 9
// C F P A S G E T D I

type StatChange =
    | NoFocus = 0
    | Focus = 1
    | Focus2 = 2
    | Focus3 = 3
    | Focus4 = 4
    | Focus5 = 5

type Character = {
    Plasma : Stat
    Oil : Stat
    Gall : Stat
    Lymph : Stat

    PlasmaChange : StatChange
    OilChange : StatChange
    GallChange : StatChange
    LymphChange : StatChange

    Edges : Stat list

    Damage : int

    Stances : int
    StancesLeft : int

    MajorWounds : MajorWounds
    MinorWounds : int

    Initiative : int
}
with
    static member empty = {
        Plasma = Stat.Crippled
        Oil = Stat.Crippled
        Gall = Stat.Crippled
        Lymph = Stat.Crippled

        PlasmaChange = StatChange.NoFocus
        OilChange = StatChange.NoFocus
        GallChange = StatChange.NoFocus
        LymphChange = StatChange.NoFocus

        Damage = 0

        Edges = []

        Stances = 0
        StancesLeft = 0

        MajorWounds = MajorWounds.Healthy
        MinorWounds = 0

        Initiative = 0
    }

    static member player = {
        Character.empty with
            Plasma = Stat.Gifted
            Oil = Stat.Gifted
            Lymph = Stat.Gifted
            Gall = Stat.Gifted

            Damage = 5
            Stances = 3
    }

    static member enemy = {
        Character.empty with
            Plasma = Stat.Average
            Oil = Stat.Average
            Lymph = Stat.Average
            Gall = Stat.Average

            Damage = 5
            Stances = 2
    }

    static member getElement element this =
        match element with
        | Plasma -> this.Plasma
        | Oil -> this.Oil
        | Lymph -> this.Lymph
        | Gall -> this.Gall

    static member changeStance =
        ""

module Dice =
    let rollDie () = Gen.random2 1 7
    let rollDice count =
        Seq.init count (fun _ -> rollDie ())
    let rollDiceThreshold count threshold () =
        rollDice count
        |> Seq.fold (fun state x -> if x > threshold then state + 1 else state) 0

type CombatState =
    | Playing
    | Quit



// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Combat = {
    GameplayTime : int64
    GameplayState : CombatState
    Player : Character
    Enemy : Character
    ResultPlayer : int
    ResultEnemy : int
}
with
    // this represents the gameplay model in a vacant state, such as when the gameplay screen is not selected.
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit
        Player = Character.empty
        Enemy = Character.empty
        ResultPlayer = 0
        ResultEnemy = 0
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Combat.empty with
            GameplayState = Playing
            Player = Character.player
            Enemy = Character.enemy
    }

    // this updates the gameplay model every frame that gameplay is active.
    static member update gameplay world =
        match gameplay.GameplayState with
        | Playing ->

            let resolveChallenge attacker defender () =
                let getSuccessesForElement element character =
                    let stat = character |> Character.getElement element |> int
                    Dice.rollDiceThreshold stat 3 ()

                getSuccessesForElement Gall attacker,
                getSuccessesForElement Lymph defender

            let resultLeft, resultRight = resolveChallenge gameplay.Player gameplay.Enemy ()

            { gameplay with ResultPlayer = resultLeft; ResultEnemy = resultRight }

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
    override this.Definitions (_, _) = [
        Screen.SelectEvent => StartPlaying
        Screen.DeselectingEvent => FinishQuitting
        Screen.UpdateEvent => Update
        Screen.TimeUpdateEvent => TimeUpdate
    ]

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
        let winner = if gameplay.ResultPlayer > gameplay.ResultEnemy then "Player" else "Enemy"

        Content.group Simulants.GameplayGui.Name [] [
            richText "CombatWhatever" [
                Entity.Position == v3 0.0f 0.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                Entity.Text := $"{gameplay.ResultPlayer} - {gameplay.ResultEnemy} - {winner} won!"
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