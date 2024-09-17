namespace Tehom

open System
open System.Numerics
open Prime
open Nu

type Element =
    | Gall
    | Lymph
    | Oil
    | Plasma

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

type Stats = Stat * Stat * Stat * Stat

type Stance = int * int * int * int

type Move =
    | Block
    | Burst
    | Cast
    | Climb
    | Crawl
    | Crouch
    | Dash
    | Delay
    | Dodge
    | Fire
    | Grab
    | Jump
    | Knockout
    | Power
    | Press
    | Ready
    | Retarget
    | Roll
    | Sidestep
    | Slam
    | Spin
    | Stride
    | Strike
    | Sweep
    | Swim
    | Toss
    | Throw
with
    static member positioning = [
        Climb; Crawl; Crouch; Dash; Jump; Roll; Sidestep; Stride; Swim;
    ]
    static member attacks = [
        Fire; Grab; Knockout; Power; Press; Retarget; Slam; Strike; Throw; Toss;
    ]
    static member defence = [
        Block; Crouch; Dodge; Jump; Roll; Spin;
    ]
    static member special = [
        Burst; Ready; Sweep;
    ]

type Character = {
    Name : String
    Stats : Stats
    Stance : Stance

    Edges : Stat list

    Damage : int

    Stances : int
    StancesLeft : int

    MajorWounds : MajorWounds
    MinorWounds : int

    Initiative : int
}
with
    static member getElement element stats =
        let (gall, lymph, oil, plasma) = stats
        match element with
        | Gall -> gall
        | Lymph -> lymph
        | Oil -> oil
        | Plasma -> plasma

    static member statsEmpty : Stats = Stat.Crippled, Stat.Crippled, Stat.Crippled, Stat.Crippled
    static member stanceEmpty : Stance = 0, 0, 0, 0
    static member stanceMove toStat fromStat (stance : Stance) =

        let moveOne toStat fromStat stance =
            let (gall, lymph, oil, plasma) = stance

            let stance : Stance =
                match fromStat with
                | Gall -> gall - 1, lymph, oil, plasma
                | Lymph -> gall, lymph - 1, oil, plasma
                | Oil -> gall, lymph, oil - 1, plasma
                | Plasma-> gall, lymph, oil, plasma - 1

            let (gall, lymph, oil, plasma) = stance

            let stance : Stance =
                match toStat with
                | Gall -> gall + 1, lymph, oil, plasma
                | Lymph -> gall, lymph + 1, oil, plasma
                | Oil -> gall, lymph, oil + 1, plasma
                | Plasma-> gall, lymph, oil, plasma + 1

            stance

        List.zip toStat fromStat
        |> List.fold (fun stance (toStat, fromStat) -> moveOne toStat fromStat stance) stance

    static member empty = {
        Name = String.empty
        Stats = Character.statsEmpty
        Stance = Character.stanceEmpty

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
            Name = "Player"
            Stats = Stat.Gifted, Stat.Gifted, Stat.Gifted, Stat.Gifted

            Damage = 5
            Stances = 2
    }

    static member enemy = {
        Character.empty with
            Name = "Enemy"
            Stats = Stat.Average, Stat.Average, Stat.Average, Stat.Average

            Damage = 5
            Stances = 1
    }

    static member stanceVerify (stance : Stance) (stats : Stats) =
        let (gall, lymph, oil, plasma) = stats
        let (gallChange, lymphChange, oilChange, plasmaChange) = stance
        gallChange + lymphChange + oilChange + plasmaChange |> int = 0
        && - int gallChange < int gall
        && - int lymphChange < int lymph
        && - int oilChange < int oil
        && - int plasmaChange < int plasma

    static member stanceChange (stance : Stance) character =
        {
            character with
                Stance = stance
                StancesLeft = character.StancesLeft - 1
        }

    static member stanceReset (character : Character) =
        {
            character with
                Stance = Character.stanceEmpty
                StancesLeft = character.Stances
        }

    static member getStats character : Stats =
        let (gall, lymph, oil, plasma) = character.Stats
        let (gallChange, lymphChange, oilChange, plasmaChange) = character.Stance
        gall + enum gallChange, lymph + enum lymphChange, oil + enum oilChange, plasma + enum plasmaChange

    static member getDamage character =
        character.Damage

    static member doDamage damage character =
        let (_, lymph, oil, _) = character.Stats

        let lymph = int lymph
        let oil = int oil

        let maxDamage newValue =
             MajorWounds.Dead
             |> int
             |> min newValue
             |> enum

        if (damage < lymph) then character
        else if (damage < 2 * lymph) then
            if (character.MinorWounds < oil) then
                { character with MinorWounds = character.MinorWounds + 1 }
            else
                { character with MajorWounds = maxDamage (1 + int character.MajorWounds) }
        else if (damage < 3 * (int lymph)) then
            { character with MajorWounds = maxDamage (1 + int character.MajorWounds) }
        else if (damage < 4 * lymph) then
            { character with MajorWounds = maxDamage (2 + int character.MajorWounds) }
        else if (damage < 5 * lymph) then
            { character with MajorWounds = maxDamage (3 + int character.MajorWounds) }
        else if (damage < 6 * lymph) then
            { character with MajorWounds = maxDamage (4 + int character.MajorWounds) }
        else if (damage < 7 * lymph) then
            { character with MajorWounds = maxDamage (5 + int character.MajorWounds) }
        else
            { character with MajorWounds = maxDamage (6 + int character.MajorWounds) }






module Dice =
    let rollDie () = Gen.random2 1 7
    let rollDice count =
        Seq.init count (fun _ -> rollDie ())
    let rollDiceThreshold count threshold =
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

    Turn : int
    SuccessesPlayer : int
    SuccessesEnemy : int
    MovesPlayer : Move list
    MovesEnemy : Move list
}
with
    // this represents the gameplay model in a vacant state, such as when the gameplay screen is not selected.
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit
        Player = Character.empty
        Enemy = Character.empty
        SuccessesPlayer = 0
        SuccessesEnemy = 0

        Turn = 0

        MovesPlayer = []
        MovesEnemy = []
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
        | Playing
        | Playing | Quit -> gameplay

    static member turn gameplay world =

        // beginning of the turn, reset
        let gameplay = {
            gameplay with
                Player = Character.stanceReset gameplay.Player
                Enemy = Character.stanceReset gameplay.Enemy
        }

        // change stances
        let gameplay = {
            gameplay with
                Player = Character.stanceChange (
                    Character.stanceMove
                        [Gall; Gall; Gall]
                        [Lymph; Oil; Plasma]
                        Character.stanceEmpty
                    ) gameplay.Player
                Enemy = Character.stanceChange (
                    Character.stanceMove
                        [Lymph; Lymph; Lymph]
                        [Gall; Oil; Plasma]
                        Character.stanceEmpty
                    ) gameplay.Enemy
        }

        let getBaseStat element character =
            character.Stats
            |> Character.getElement element
            |> int

        let getStat element character =
            character
            |> Character.getStats
            |> Character.getElement element
            |> int

        let baseStatAttacker = getBaseStat Gall gameplay.Player
        let baseStatDefender = getBaseStat Lymph gameplay.Enemy

        let statAttacker = getStat Gall gameplay.Player
        let statDefender = getStat Lymph gameplay.Enemy

        let successesAttacker = Dice.rollDiceThreshold statAttacker 3
        let successesDefender = Dice.rollDiceThreshold statDefender 3

        let gameplay = {
            gameplay with
                SuccessesPlayer = successesAttacker
                SuccessesEnemy = successesDefender
        }

        let getRandomMoveSet length list =
            let gen () = Gen.randomItem list
            List.init length (fun _ -> gen ())

        let attackMoves =
            if (gameplay.Player.MajorWounds < MajorWounds.Down) then
                getRandomMoveSet baseStatAttacker Move.attacks
            else
                []

        let defenceMoves =
            if (gameplay.Enemy.MajorWounds < MajorWounds.Down) then

                let attackMovesLength =
                    List.fold (fun number move ->
                        if List.contains move Move.attacks then number + 1 else number
                    ) 0 attackMoves

                getRandomMoveSet (min baseStatDefender attackMovesLength) Move.defence
                @
                if (baseStatDefender > attackMovesLength) then
                    [ Move.Ready ]
                    @
                    getRandomMoveSet (baseStatDefender - attackMovesLength) Move.attacks
                else
                    []
            else
                []

        let gameplay = {
            gameplay with
                MovesPlayer = attackMoves
                MovesEnemy = defenceMoves
        }

        let attacker = gameplay.Player
        let defender = gameplay.Enemy

        let (attacker, defender) =
            let handleMoves successes moves attacker defender =
                moves
                |> List.indexed
                |> List.foldWhile (fun (attacker, defender) (i, move) ->
                    if (i < successes) then
                        // really simple handler, just checks if it's an attack
                        // should be turned into a separate function that accumulates bonuses, etc
                        if List.contains move Move.attacks then
                            let damage = Character.getDamage attacker
                            let defender = Character.doDamage damage defender
                            Some (attacker, defender)
                        else
                            Some (attacker, defender)
                    else
                        None
                ) (attacker, defender)

            if (successesAttacker > successesDefender) then
                let (attacker, defender) = handleMoves successesAttacker attackMoves attacker defender
                attacker, defender
            else if (successesAttacker < successesDefender) then
                let (defender, attacker) = handleMoves successesDefender defenceMoves defender attacker
                attacker, defender
            else
                attacker, defender

        {
            gameplay with
                Player = attacker
                Enemy = defender
                Turn = gameplay.Turn + 1
        }


// this is our gameplay MMCC message type.
type CombatMessage =
    | StartPlaying
    | FinishQuitting
    | Update
    | Turn
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

        | Turn ->
            let gameplay = Combat.turn gameplay world
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
        let turnWinner =
            if gameplay.SuccessesPlayer = gameplay.SuccessesEnemy then
                "Draw!"
            else if gameplay.SuccessesPlayer > gameplay.SuccessesEnemy then
                $"{gameplay.Player.Name} advances!"
            else
                $"{gameplay.Enemy.Name} advances!"

        let combatWinner =
            if gameplay.Player.MajorWounds = MajorWounds.Dead && gameplay.Enemy.MajorWounds = MajorWounds.Dead then
                "Everyone died!"
            else if gameplay.Player.MajorWounds = MajorWounds.Dead then
                $"{gameplay.Enemy.Name} won!"
            else if gameplay.Enemy.MajorWounds = MajorWounds.Dead then
                $"{gameplay.Player.Name} won!"
            else
                ""

        Content.group Simulants.GameplayGui.Name [] [
            richText "CombatSummary" [
                Entity.Position == v3 0.0f 80.0f 0.0f
                Entity.Size == v3 240.0f 40.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text := $"{gameplay.SuccessesPlayer} - {gameplay.SuccessesEnemy} - {turnWinner}

                Turn: {gameplay.Turn} {combatWinner}"
                Entity.TextColor == Color.FloralWhite
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 10
            ]

            Content.button "AdvanceTurn" [
                Entity.Position == v3 40.0f 150.0f 0.0f
                Entity.Size == v3 80.0f 20.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text == "Advance Turn"
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 10
                Entity.ClickEvent => Turn
            ]

            Content.button "ResetGame" [
                Entity.Position == v3 -40.0f 150.0f 0.0f
                Entity.Size == v3 80.0f 20.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text == "Reset Game"
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 10
                Entity.ClickEvent => StartPlaying
            ]


            let statsBox character = [
                let (gall, lymph, oil, plasma) = character.Stats
                let (gallStance, lymphStance, oilStance, plasmaStance) = character.Stance
                let minorWounds = character.MinorWounds
                let majorWounds = character.MajorWounds

                Content.text "Minor Wounds" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"{minorWounds}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]

                Content.text "Major Wounds" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"{majorWounds}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]

                Content.text "Gall" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"{gall} {gallStance}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]
                Content.text "Lymph" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"{lymph} {lymphStance}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]
                Content.text "Oil" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"{oil} {oilStance}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]
                Content.text "Plasma" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"{plasma} {plasmaStance}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]

            ]

            Content.association "StatsBoxPlayer" [
                Entity.Position == v3 -160.0f 0.0f 0.0f
                Entity.Size == v3 80.0f 80.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Layout == Flow (FlowDownward, FlowUnlimited)
            ] (statsBox gameplay.Player)

            Content.association "StatsBoxEnemy" [
                Entity.Position == v3 240.0f 0.0f 0.0f
                Entity.Size == v3 80.0f 80.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Layout == Flow (FlowDownward, FlowUnlimited)
            ] (statsBox gameplay.Enemy)


            Content.association "MovesPlayer" [
                Entity.Position == v3 -80.0f 0.0f 0.0f
                Entity.Size == v3 80.0f 80.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Layout == Flow (FlowDownward, FlowUnlimited)
            ] [
                for i, move in List.indexed gameplay.MovesPlayer ->
                    Content.text $"MovesPlayer{i}" [
                        Entity.Size == v3 80.0f 10.0f 0.0f
                        Entity.Text := $"{move}"
                        Entity.Font == Assets.Gui.ClearSansFont
                        Entity.FontSizing == Some 10
                        Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                        Entity.TextColor :=
                            if i < gameplay.SuccessesPlayer then
                                Color.FloralWhite
                            else
                                Color.Gray
                    ]
            ]

            Content.association "MovesEnemy" [
                Entity.Position == v3 80.0f 0.0f 0.0f
                Entity.Size == v3 80.0f 80.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Layout == Flow (FlowDownward, FlowUnlimited)
            ] [
                for i, move in List.indexed gameplay.MovesEnemy ->
                    Content.text $"MovesEnemy{i}" [
                        Entity.Size == v3 80.0f 10.0f 0.0f
                        Entity.Text := $"{move}"
                        Entity.Font == Assets.Gui.ClearSansFont
                        Entity.FontSizing == Some 10
                        Entity.Justification == Justified (JustifyRight, JustifyMiddle)
                        Entity.TextColor :=
                            if i < gameplay.SuccessesEnemy then
                                Color.FloralWhite
                            else
                                Color.Gray
                    ]
            ]
        ]

        // the scene group while playing
        match gameplay.GameplayState with
        | Playing -> ()
        // no scene group otherwise
        | Quit -> ()
    ]