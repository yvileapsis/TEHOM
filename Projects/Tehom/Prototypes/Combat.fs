namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open FGL

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

type PhysicalAction =
    | NoPhysicalAction
    | Moves of Move list

type MentalAction =
    | NoMentalAction

type Character = {
    // Static stats
    Name : String
    Stats : Stats
    Edges : Stat list
    Stances : int

    // Dynamic stats
    Damage : int

    MajorWounds : MajorWounds
    MinorWounds : int

    Initiative : int

    Stance : Stance
    StancesLeft : int
    Check : Element option
    CheckSuccesses : int

    PhysicalAction : PhysicalAction
    MentalAction : MentalAction
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

        Check = None
        CheckSuccesses = 0

        PhysicalAction = NoPhysicalAction
        MentalAction = NoMentalAction
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
        if (character.StancesLeft > 0) then
            {
                character with
                    Stance = stance
                    StancesLeft = character.StancesLeft - 1
            }
        else
            character

    static member stanceReset (character : Character) =
        {
            character with
                Stance = Character.stanceEmpty
                StancesLeft = character.Stances
        }

    static member getStats character : Stats =
        character.Stats

    static member getStancedStats character : Stats =
        let (gall, lymph, oil, plasma) = character.Stats
        let (gallChange, lymphChange, oilChange, plasmaChange) = character.Stance
        gall + enum gallChange, lymph + enum lymphChange, oil + enum oilChange, plasma + enum plasmaChange

    static member getStat element character =
        character.Stats
        |> Character.getElement element
        |> int

    static member getStancedStat element character =
        character
        |> Character.getStancedStats
        |> Character.getElement element
        |> int

    static member getDamage character =
        character.Damage

    static member doDamage damage character =
        let (_, lymph, oil, _) = Character.getStats character

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

    static member getMaxInitiative character =
        let (gall, _, _, plasma) = Character.getStats character
        int gall, int plasma


module Random =
    let rollDie () = Gen.random2 1 7
    let rollDice count =
        Seq.init count (fun _ -> rollDie ())
    let rollDiceThreshold count threshold =
        rollDice count
        |> Seq.fold (fun state x -> if x > threshold then state + 1 else state) 0

    let getItemsFromList length list =
        let gen () = Gen.randomItem list
        List.init length (fun _ -> gen ())

    let rollInitiative max =
        Gen.random2 0 max


type Area = {
    Name : string
    Actors : Set<String>
}
with
    static member room1 = {
        Name = "Dark Room"
        Actors = Set.ofList ["Player"; "Table"; "Cat"; "Key"]
    }

    static member room2 = {
        Name = "Mediumly Lit Room"
        Actors = Set.empty
    }

    static member room3 = {
        Name = "Bright Room"
        Actors = Set.ofList [ "Enemy" ]
    }

type Pathway = {
    Open : bool
    Length : int
}
with
    static member empty = { Open = true; Length = 10 }


type Level = {
    Name : string
    Areas : Graph<String, Area, Pathway>
}
with
    static member empty = {
        Name = String.empty
        Areas = Graph.empty
    }

    static member level1 = {
        Name = "Sheol"
        Areas =
            Graph.empty
            |> Vertices.add ("Room 1", Area.room1)
            |> Vertices.add ("Room 2", Area.room2)
            |> Undirected.Edges.add ("Room 1", "Room 2", Pathway.empty)
            |> Vertices.add ("Room 3", Area.room3)
            |> Undirected.Edges.add  ("Room 2", "Room 3", Pathway.empty)
    }

type CombatState =
    | Playing
    | Quit


// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Combat = {
    GameplayTime : int64
    GameplayState : CombatState
    Combatants : Character list

    DisplayLeft : String
    DisplayRight : String

    OldCombatant : int
    CurrentCombatant : int
    Turn : int

    Level : Level
}
with
    // this represents the gameplay model in a vacant state, such as when the gameplay screen is not selected.
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit

        Combatants = []

        DisplayLeft = String.empty
        DisplayRight = String.empty

        OldCombatant = 0
        CurrentCombatant = 0
        Turn = 0

        Level = Level.empty
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Combat.empty with
            GameplayState = Playing
            Combatants =
                [ Character.player; Character.enemy ]
                |> List.map Character.stanceReset
                |> List.map (fun c ->
                    let gall, plasma = Character.getMaxInitiative c
                    { c with Initiative = Random.rollInitiative (gall + plasma) }
                )
                |> List.sortBy (_.Initiative)
                |> List.rev
            DisplayLeft = "Player"
            DisplayRight = "Enemy"
            Level = Level.level1
    }

    // this updates the gameplay model every frame that gameplay is active.
    static member update gameplay world =
        match gameplay.GameplayState with
        | Playing
        | Playing | Quit -> gameplay

    // TODO: separate combat into initiative phase, action phase and end phase.
    // TODO: separate turn into beginning, middle and end too
    // To handle different moves in sequences I need working positioning and limb systems


    // attack, prototype of attacker AI
    static member turnCombatantPlan combatantID combatant gameplay world =
        // pick target with the most wounds, excluding combatant himself
        let target =
            gameplay.Combatants
            |> List.removeAt combatantID
            |> List.sortBy (_.MajorWounds)
            |> List.last

        let targetID =
            gameplay.Combatants
            |> List.findIndex ((=) target)

        let statCombatant = Character.getStat Gall combatant

        let movesCombatant =
            if (combatant.MajorWounds < MajorWounds.Down) then

                let combatantName = combatant.Name
                let targetName = target.Name

                let combatantVertex =
                    gameplay.Level.Areas
                    |> Vertices.toVertexList
                    |> List.find (fun (string, vertex) -> Set.contains combatantName vertex.Actors)
                    |> fst

                let targetVertex =
                    gameplay.Level.Areas
                    |> Vertices.toVertexList
                    |> List.find (fun (string, vertex) -> Set.contains targetName vertex.Actors)
                    |> fst

                let path =
                    gameplay.Level.Areas
                    |> Undirected.Edges.map (fun v1 v2 edge -> 1u)
                    |> Query.sp combatantVertex targetVertex

                let movementMoves = List.length path - 1

                if (movementMoves > 0) then
                    if (statCombatant > movementMoves) then
                        Random.getItemsFromList movementMoves Move.positioning
                        @ Random.getItemsFromList (statCombatant - movementMoves) Move.attacks
                    else
                        Random.getItemsFromList statCombatant Move.positioning
                else
                    Random.getItemsFromList statCombatant Move.attacks
            else
                []

        let combatant = { combatant with PhysicalAction = Moves movesCombatant }

        // btw thinking with high enough air you should be able to tell enemy's stance
        // TODO: implement correct stance selection once skills are implemented
        let combatant = { combatant with Check = Some Gall }

        let combatant =
            Character.stanceChange (Character.stanceMove
                [Gall; Gall; Gall]
                [Lymph; Oil; Plasma]
                Character.stanceEmpty
            ) combatant

        combatantID, combatant, targetID, target

    // response, prototype of defender AI
    static member turnTargetPlan combatantID combatant targetID target gameplay world =
        let movesCombatant =
            match combatant.PhysicalAction with
            | Moves moves -> moves
            | _ -> []

        let statTarget = Character.getStat Lymph target

        let movesTarget =
            if (target.MajorWounds < MajorWounds.Down) then

                let combatantBlockableMoves =
                    List.fold (fun number move ->
                        if List.contains move Move.attacks then number + 1 else number
                    ) 0 movesCombatant

                Random.getItemsFromList (min statTarget combatantBlockableMoves) Move.defence
                @
                if (statTarget > combatantBlockableMoves) then
                    [ Move.Ready ]
                    @
                    Random.getItemsFromList (statTarget - combatantBlockableMoves) Move.attacks
                else
                    []
            else
                []

        let target = { target with PhysicalAction = Moves movesTarget }

        let target = { target with Check = Some Lymph }

        let target =
            Character.stanceChange (Character.stanceMove
                [Lymph; Lymph; Lymph]
                [Gall; Oil; Plasma]
                Character.stanceEmpty
            ) target

        combatantID, combatant, targetID, target

    // resolution
    static member turnResolution combatantID combatant targetID target gameplay world =

        let movesCombatant =
            match combatant.PhysicalAction with
            | Moves moves -> moves
            | _ -> []

        let movesTarget =
            match target.PhysicalAction with
            | Moves moves -> moves
            | _ -> []

        let statCombatant = Character.getStancedStat Gall combatant
        let statTarget = Character.getStancedStat Lymph target

        let successesCombatant = Random.rollDiceThreshold statCombatant 3
        let successesTarget = Random.rollDiceThreshold statTarget 3

        let combatant = { combatant with CheckSuccesses = successesCombatant }
        let target = { target with CheckSuccesses = successesTarget }


        let (combatant, target) =
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

            if (successesCombatant > successesTarget) then
                let (combatant, target) = handleMoves successesCombatant movesCombatant combatant target
                combatant, target
            else if (successesCombatant < successesTarget) then
                let (target, combatant) = handleMoves successesTarget movesTarget target combatant
                combatant, target
            else
                combatant, target


        combatantID, combatant, targetID, target


    static member turn gameplay world =

        let combatantID = gameplay.CurrentCombatant

        let combatant = gameplay.Combatants[combatantID]

        let combatant = Character.stanceReset combatant

        let combatantID, combatant, targetID, target =
            Combat.turnCombatantPlan combatantID combatant gameplay world

        let combatantID, combatant, targetID, target =
            Combat.turnTargetPlan combatantID combatant targetID target gameplay world

        let combatantID, combatant, targetID, target =
            Combat.turnResolution combatantID combatant targetID target gameplay world

        let combatants =
            gameplay.Combatants
            |> List.mapi (fun i x ->
                if i = combatantID then combatant
                else if i = targetID then target
                else x
            )

        let nextCombatantID =
            if combatantID + 1 < List.length combatants then
                combatantID + 1
            else
                0

        {
            gameplay with
                Combatants = combatants
                CurrentCombatant = nextCombatantID
                OldCombatant = combatantID
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
        let player =
            gameplay.Combatants
            |> List.tryFind (fun combatant -> combatant.Name = gameplay.DisplayLeft)
        let enemy =
            gameplay.Combatants
            |> List.tryFind (fun combatant -> combatant.Name = gameplay.DisplayRight)

        match player, enemy with
            | Some player, Some enemy ->

                let currentCombatant = gameplay.Combatants[gameplay.OldCombatant].Name

                let playerMoves =
                    match player.PhysicalAction with
                    | NoPhysicalAction -> []
                    | Moves x -> x

                let enemyMoves =
                    match enemy.PhysicalAction with
                    | NoPhysicalAction -> []
                    | Moves x -> x


                let turnWinner =
                    if player.CheckSuccesses = enemy.CheckSuccesses then
                        "Draw!"
                    else if player.CheckSuccesses > enemy.CheckSuccesses then
                        $"{player.Name} advances!"
                    else
                        $"{enemy.Name} advances!"

                let combatWinner =
                    if player.MajorWounds = MajorWounds.Dead && enemy.MajorWounds = MajorWounds.Dead then
                        "Everyone died!"
                    else if player.MajorWounds = MajorWounds.Dead then
                        $"{enemy.Name} won!"
                    else if enemy.MajorWounds = MajorWounds.Dead then
                        $"{player.Name} won!"
                    else
                        ""

                Content.group Simulants.GameplayGui.Name [] [
                    richText "CombatSummary" [
                        Entity.Position == v3 0.0f 80.0f 0.0f
                        Entity.Size == v3 240.0f 40.0f 0.0f
                        Entity.Elevation == 10.0f
                        Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                        Entity.Text := $"{currentCombatant} - {player.CheckSuccesses} - {enemy.CheckSuccesses} - {turnWinner}

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
                            Entity.Text := $"Wounds {minorWounds}"
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
                        Content.text "Stances" [
                            Entity.Size == v3 80.0f 10.0f 0.0f
                            Entity.Text := $"Stances {character.StancesLeft}"
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
                    ] (statsBox player)

                    Content.association "StatsBoxEnemy" [
                        Entity.Position == v3 200.0f 0.0f 0.0f
                        Entity.Size == v3 80.0f 80.0f 0.0f
                        Entity.Elevation == 10.0f
                        Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                        Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                    ] (statsBox enemy)


                    Content.association "MovesPlayer" [
                        Entity.Position == v3 -80.0f 0.0f 0.0f
                        Entity.Size == v3 80.0f 80.0f 0.0f
                        Entity.Elevation == 10.0f
                        Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                        Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                    ] [
                        for i, move in List.indexed playerMoves ->
                            Content.text $"MovesPlayer{i}" [
                                Entity.Size == v3 80.0f 10.0f 0.0f
                                Entity.Text := $"{move}"
                                Entity.Font == Assets.Gui.ClearSansFont
                                Entity.FontSizing == Some 10
                                Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                                Entity.TextColor :=
                                    if i < player.CheckSuccesses then
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
                        for i, move in List.indexed enemyMoves ->
                            Content.text $"MovesEnemy{i}" [
                                Entity.Size == v3 80.0f 10.0f 0.0f
                                Entity.Text := $"{move}"
                                Entity.Font == Assets.Gui.ClearSansFont
                                Entity.FontSizing == Some 10
                                Entity.Justification == Justified (JustifyRight, JustifyMiddle)
                                Entity.TextColor :=
                                    if i < enemy.CheckSuccesses then
                                        Color.FloralWhite
                                    else
                                        Color.Gray
                            ]
                    ]
                ]
            | _ -> ()

        // the scene group while playing
        match gameplay.GameplayState with
        | Playing -> ()
        // no scene group otherwise
        | Quit -> ()
    ]