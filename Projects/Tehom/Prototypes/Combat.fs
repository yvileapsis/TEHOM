namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open FGL
open Character
open Area

(*
TODO: Positioning system
TODO: Limb system
limbs should have min max angles which should impact reach
TODO: Moves and their effects
TODO: Skills and stances
TODO: Player-controllable combat
TODO: Combat start and combat end
TODO: Non-combat exploration system with player controls
TODO: Containers and notes
TODO: Visibility system
TODO: Enemy AI
TODO: Tiniest vertical slice:
* 5 Rooms:
* Waitroom, barricaded windows, rolling hospital bed, locked exit door, you wake up here
* Registration room, safe with useful stuff like a pistol maybe
* Main hall
* Electrical room to fix the lights, rat attacks
* Surgery room, surgery table, dog on the table, note (dog ate the key), opening it lets spider chandalier escape
* Goal is to go to surgery room, take the key, return to waitroom, open the door and exit.
* Optionally you can fix lights and fight spider chandalier.
*)

type GameEffect =
    | Damage of String * int
    | Move of String * String * (String * uint32) option

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

    static member bonusDamage moveID moves damage =
        let before, after = List.splitAt moveID moves
        let damage =
            match before with
            | [] -> damage
            | _ ->
                before
                |> List.rev
                |> List.foldWhile (fun bonus move ->
                    match move with
                    | Power -> Some (bonus + 5)
                    | _ -> None
                ) damage
        let damage =
            match after with
            | _::after ->
                after
                |> List.foldWhile (fun bonus move ->
                    match move with
                    | Press -> Some (bonus + 2)
                    | _ -> None
                ) damage
            | _ -> damage
        damage

    // TODO: fix bug where entity ends up disconnected from the floor due to spare distance fix
    static member handle moveID moves attacker defender area =
        let move = List.item moveID moves
        match move with
        | Stride
        | Climb
        | Crawl
        | Jump
        | Roll
        | Sidestep
        | Dash
        | Swim ->
                let reach = Character.getReach attacker
                let speed = Character.getSpeed attacker

                match Area.moveWithinReach attacker.ID defender.ID reach speed area with
                | Some (moveTo, fix) ->
                    [Move (attacker.ID, moveTo, fix)]
                | None ->
                    []
        | Block
        | Crouch
        | Delay
        | Dodge
        | Spin ->
            []
        | Cast
        | Power
        | Press
        | Ready
        | Retarget ->
            []
        | Burst
        | Fire ->
            []
        | Grab
        | Knockout
        | Slam
        | Strike
        | Sweep
        | Toss
        | Throw ->
            let damage = Character.getDamage attacker
            let damage = Move.bonusDamage moveID moves damage
            [Damage (defender.ID, damage)]

type PhysicalAction =
    | NoPhysicalAction
    | FullPhysicalAction
    | Sequence of Move list


type MentalAction =
    | NoMentalAction
    | FullMentalAction


module Random =
    let rollDie () = Gen.random2 1 7
    let rollDice count =
        Seq.init count (fun _ -> rollDie ())
    let rollDiceThreshold count threshold =
        rollDice count
        |> Seq.fold (fun state x -> if x > threshold then state + 1 else state) 0

    let getItemsFromList (length: uint32) list =
        let gen () = Gen.randomItem list
        List.init (length |> int) (fun _ -> gen ())

    let rollInitiative max =
        Gen.random2 0 max

type CharacterAction = {
    Turn : int
    Target : String
    PhysicalAction : PhysicalAction
    MentalAction : MentalAction
    Element : Element option
    Successes : int option
    Blocks : int option
}
with
    static member empty = {
        Turn = 0
        Target = String.empty
        PhysicalAction = NoPhysicalAction
        MentalAction = NoMentalAction
        Element = None
        Successes = None
        Blocks = None
    }

type CombatState =
    | Playing
    | Quit

type CharacterExtended = Character * CharacterAction list

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Combat = {
    GameplayTime : int64
    GameplayState : CombatState
    Combatants : CharacterExtended list
    CombatantID : String

    DisplayLeft : String
    DisplayRight : String

    Turn : int

    Area : Area
//    Characters : Character list
}
with
    // this represents the gameplay model in a vacant state, such as when the gameplay screen is not selected.
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit

        Combatants = List.empty
        CombatantID = String.empty

        DisplayLeft = String.empty
        DisplayRight = String.empty

        Turn = 0

        Area = Area.empty
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Combat.empty with
            GameplayState = Playing
            Combatants =
                [ Character.player; Character.rat ]
                |> List.map Character.stanceReset
                |> List.map (fun c ->
                    let gall, plasma = Character.getMaxInitiative c
                    { c with Initiative = Random.rollInitiative (gall + plasma) }
                )
                |> List.sortBy (_.Initiative)
                |> List.rev
                |> List.map (fun c -> c, [])

            DisplayLeft = Character.player.ID
            DisplayRight = Character.rat.ID
            Area = Area.level1
    }

    // this updates the gameplay model every frame that gameplay is active.
    static member update gameplay world =
        match gameplay.GameplayState with
        | Playing
        | Playing | Quit -> gameplay

    // attack, prototype of attacker AI
    static member turnAttackerPlan attacker gameplay =

        // pick target with the most wounds, excluding combatant himself
        let target =
            gameplay.Combatants
            |> List.map fst
            |> List.remove ((=) attacker)
            |> List.sortBy (_.MajorWounds)
            |> List.last

        let statCombatant = Character.getStat Gall attacker

        let movesCombatant =
            if (attacker.MajorWounds < MajorWounds.Down) then

                let combatantName = attacker.ID
                let targetName = target.ID

                let area = gameplay.Area

                match Area.findPath combatantName targetName area with
                // no path
                | [] -> []
                | path ->
                    let reach = Character.getReach attacker
                    let distance = List.last path |> snd
                    if (reach >= distance) then
                        // in reach
                        Random.getItemsFromList statCombatant Move.attacks
                    else
                        // not in reach

                        let speed = Character.getSpeed attacker

                        if (speed * statCombatant >= distance) then
                            // can run to
                            let movementMoves = round (float distance / float speed + 0.5) |> uint32
                            List.init (movementMoves |> int) (fun i -> Stride)
                            @ Random.getItemsFromList (statCombatant - movementMoves) Move.attacks
                        else
                            // can't run to
                            List.init (statCombatant |> int) (fun i -> Stride)
            else
                []

        let action = {
            CharacterAction.empty with
                Turn = gameplay.Turn
                Target = target.ID
                PhysicalAction = Sequence movesCombatant
                MentalAction = NoMentalAction
                Element = Some Gall
        }

        target, action

    // response, prototype of defender AI
    static member turnDefenderPlan attacker attackerAction defender gameplay =

        let movesCombatant =
            match attackerAction.PhysicalAction with
            | Sequence moves -> moves
            | _ -> []

        let statDefender = Character.getStat Lymph defender

        let movesTarget =
            if (defender.MajorWounds < MajorWounds.Down) then

                let combatantName = attacker.ID
                let targetName = defender.ID

                let area = gameplay.Area

                match Area.findPath combatantName targetName area with
                // no path
                | [] -> []
                | path ->
                    let reach = Character.getReach attacker
                    let distance = List.last path |> snd
                    if (reach >= distance) then
                        // in reach

                        let combatantBlockableMoves =
                            List.fold (fun number move ->
                                if List.contains move Move.attacks then number + 1u else number
                            ) 0u movesCombatant

                        Random.getItemsFromList (min statDefender combatantBlockableMoves) Move.defence
                        @
                        if (statDefender > combatantBlockableMoves) then
                            [ Move.Ready ]
                            @
                            Random.getItemsFromList (statDefender - combatantBlockableMoves) Move.attacks
                        else
                            []
                    else
                        []
            else
                []

        let action = {
            CharacterAction.empty with
                Turn = gameplay.Turn
                Target = attacker.ID
                PhysicalAction = Sequence movesTarget
                MentalAction = NoMentalAction
                Element = Some Lymph
        }

        action


    // btw thinking with high enough air you should be able to tell enemy's stance
    // TODO: implement correct stance selection once skills are implemented
    // Thoughts: you can change stance before action, after action, before mental action, after mental action, before enemy attack, after eney attack?
    static member turnAttackerStanceChange combatant =

        let combatant =
            Character.stanceChange (Character.stanceMove
                [Gall; Gall; Gall]
                [Lymph; Oil; Plasma]
                Character.stanceEmpty
            ) combatant

        combatant


    static member turnDefenderStanceChange combatant =

        let combatant =
            Character.stanceChange (Character.stanceMove
                [Lymph; Lymph; Lymph]
                [Gall; Oil; Plasma]
                Character.stanceEmpty
            ) combatant

        combatant


    static member applyStanceToAction combatant combatantAction =

        match combatantAction.Element with
        | Some element ->
            let stat = Character.getStancedStat element combatant |> int

            let successes = Random.rollDiceThreshold stat 3

            let combatantAction = { combatantAction with Successes = Some successes }

            combatantAction

        | None ->

            combatantAction

    static member opposedCheck attacker attackerAction defender defenderAction level =

        // add karma betting

        let attackerAction : CharacterAction = { attackerAction with Blocks = defenderAction.Successes }
        let defenderAction : CharacterAction = { defenderAction with Blocks = attackerAction.Successes }

        let signals = [attacker, attackerAction] @ [defender, defenderAction]

        signals


    static member turn gameplay world =

        let attackerID =
            if gameplay.CombatantID <> String.empty then
                gameplay.CombatantID
            else
                gameplay.Combatants
                |> List.head
                |> fst
                |> _.ID


        let attacker =
            gameplay.Combatants
            |> List.map fst
            |> List.find (fun c -> c.ID = attackerID)

        let defender, attackerAction =
            Combat.turnAttackerPlan attacker gameplay

        let defenderAction =
            Combat.turnDefenderPlan attacker attackerAction defender gameplay

        let attackerIndex =
            gameplay.Combatants
            |> List.map fst
            |> List.findIndex (fun c -> c.ID = attacker.ID)

        let defenderIndex =
            gameplay.Combatants
            |> List.map fst
            |> List.findIndex (fun c -> c.ID = defender.ID)

        let attacker = Character.stanceReset attacker

        let attacker = Combat.turnAttackerStanceChange attacker
        let attackerAction = Combat.applyStanceToAction attacker attackerAction

        let defender = Combat.turnDefenderStanceChange defender
        let defenderAction = Combat.applyStanceToAction defender defenderAction

        let level = gameplay.Area

        let signals = Combat.opposedCheck attacker attackerAction defender defenderAction level

        let combatants =
            gameplay.Combatants
            |> List.mapi (fun i (x, history) ->
                if i = attackerIndex then attacker, attackerAction::history
                else if i = defenderIndex then defender, defenderAction::history
                else x, history
            )

        let nextCombatantID =
            let index =
                if attackerIndex + 1 < List.length combatants then
                    attackerIndex + 1
                else
                    0

            combatants
            |> List.item index
            |> fst
            |> _.ID

        let gameplay = {
            gameplay with
                Combatants = combatants
                CombatantID = nextCombatantID
                Turn = gameplay.Turn + 1
        }

        signals, gameplay


    static member doEffect effect gameplay =
        match effect with
        | Damage (characterID, damage) ->

            let character =
                gameplay.Combatants
                |> List.find (fun (c, _) -> c.ID = characterID)
                |> fst

            let character = Character.doDamage damage character

            let combatants =
                gameplay.Combatants
                |> List.map (fun (c, history) -> (if c.ID = character.ID then character else c), history)

            let gameplay = { gameplay with Combatants = combatants }

            gameplay

        | Move (character, location, spareDistanceFix) ->

            let level = gameplay.Area

            let level =
                Area.moveSite character location level

            let level =
                match spareDistanceFix with
                | Some (location, distance) ->
                    Area.spareDistance distance character location level
                | None ->
                    level

            let gameplay = { gameplay with Area = level }

            gameplay


// this is our gameplay MMCC message type.
type CombatMessage =
    | StartPlaying
    | FinishQuitting
    | Update
    | Turn
    | ActorAction of Character * CharacterAction
    | ActorMove of String * String * int * Move list
    | GameEffect of GameEffect
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
            let effects, gameplay = Combat.turn gameplay world

            let signals : Signal list =
                List.map (fun tuple -> ActorAction tuple) effects

            withSignals signals gameplay

        | ActorAction (actor, action) ->

            let attack = action.PhysicalAction

            match attack with
            | NoPhysicalAction ->
                just gameplay
            | FullPhysicalAction ->
                just gameplay
            | Sequence moves ->

                let successes = match action.Successes with Some x -> x | None -> 0
                let blocks = match action.Blocks with Some x -> x | None -> 0

                let signals : Signal list =
                    let indexes, moves =
                        moves
                        |> List.indexed
                        |> List.takeWhile (fun (i, move) ->
                            // positioning is always successful for now, but should check if within reach of enemy later
                            ((List.contains move Move.positioning) || (successes > blocks)) && (i < successes)
                        )
                        |> List.unzip

                    List.map (fun i -> ActorMove (actor.ID, action.Target, i, moves)) indexes

                withSignals signals gameplay

        | ActorMove (actorID, targetID, moveID, moves) ->

            let actor =
                gameplay.Combatants
                |> List.find (fun (c, _) -> c.ID = actorID)
                |> fst

            let target =
                gameplay.Combatants
                |> List.find (fun (c, _) -> c.ID = targetID)
                |> fst

            let signals : Signal list =
                Move.handle moveID moves actor target gameplay.Area
                |> List.map (fun signal -> GameEffect signal)

            withSignals signals gameplay

        | GameEffect effect ->

            let gameplay = Combat.doEffect effect gameplay

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

            let player =
                gameplay.Combatants
                |> List.tryFind (fun (combatant, _) -> combatant.ID = gameplay.DisplayLeft)
            let enemy =
                gameplay.Combatants
                |> List.tryFind (fun (combatant, _) -> combatant.ID = gameplay.DisplayRight)


            match player, enemy with
            | Some (player, _), Some (enemy, _) ->
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
            | _ ->
                ()

            match player, enemy with
            | Some (player, playerLastTurn::_), Some (enemy, enemyLastTurn::_) ->

                let playerMoves =
                    match playerLastTurn.PhysicalAction with
                    | NoPhysicalAction -> []
                    | Sequence x -> x
                    | FullPhysicalAction -> []

                let enemyMoves =
                    match enemyLastTurn.PhysicalAction with
                    | NoPhysicalAction -> []
                    | Sequence x -> x
                    | FullPhysicalAction -> []

                let playerSuccesses =
                    match playerLastTurn.Successes with
                    | Some x -> x
                    | None -> -1

                let enemySuccesses =
                    match enemyLastTurn.Successes with
                    | Some x -> x
                    | None -> -1

                let turnWinner =
                    if playerSuccesses = enemySuccesses then
                        "Draw!"
                    else if playerSuccesses > enemySuccesses then
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


                richText "CombatSummary" [
                    Entity.Position == v3 0.0f 80.0f 0.0f
                    Entity.Size == v3 240.0f 40.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Text := $" {playerSuccesses} - {enemySuccesses} - {turnWinner}

                    Turn: {gameplay.Turn} {combatWinner}"
                    Entity.TextColor == Color.FloralWhite
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                ]

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
                                if i < playerSuccesses then
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
                                if i < enemySuccesses then
                                    Color.FloralWhite
                                else
                                    Color.Gray
                        ]
                ]
            | _ -> ()

        ]

        // the scene group while playing
        match gameplay.GameplayState with
        | Playing -> ()
        // no scene group otherwise
        | Quit -> ()
    ]