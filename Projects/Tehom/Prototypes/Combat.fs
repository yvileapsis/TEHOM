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
    | CharacterFunction of Entity * (Character -> Character)
    | Damage of Entity * int
    | MoveInter of String * String
    | MoveIntra of String * String * uint32

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
    static member handle moveID moves (attacker : Entity) (defender : Entity) area world =
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
                let attackerCharacter = attacker.GetCharacter world
                let reach = Character.getReach attackerCharacter
                let speed = Character.getSpeed attackerCharacter

                match Area.moveWithinReach attacker.Name defender.Name reach speed area with
                | Some moveInter, Some moveIntra ->
                    [MoveInter moveInter; MoveIntra moveIntra]
                | Some moveInter, None ->
                    [MoveInter moveInter]
                | None, Some moveIntra ->
                    [MoveIntra moveIntra]
                | None, None ->
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
            let attackerCharacter = attacker.GetCharacter world
            let damage = Character.getDamage attackerCharacter
            let damage = Move.bonusDamage moveID moves damage
            [Damage (defender, damage)]

// TODO: actions should be together, have some sort of cost, stance should be an action
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
    Target : Entity option
    PhysicalAction : PhysicalAction
    MentalAction : MentalAction
    Element : Element option
    Successes : int option
    Blocks : int option
}
with
    static member empty = {
        Turn = 0
        Target = None
        PhysicalAction = NoPhysicalAction
        MentalAction = NoMentalAction
        Element = None
        Successes = None
        Blocks = None
    }

type CombatState =
    | Playing
    | Quit

type CharacterExtended = Entity * CharacterAction list

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Combat = {
    GameplayTime : int64
    GameplayState : CombatState
    Combatants : CharacterExtended list
    CombatantID : Entity option

    DisplayLeft : Character
    DisplayRight : Character

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
        CombatantID = None

        DisplayLeft = Character.empty
        DisplayRight = Character.empty

        Turn = 0

        Area = Area.empty
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Combat.empty with
            GameplayState = Playing
            DisplayLeft = Character.player
            DisplayRight = Character.rat
            Area = Area.level1
    }

    // this updates the gameplay model every frame that gameplay is active.
    static member update gameplay world =
        match gameplay.GameplayState with
        | Playing ->

            let left = gameplay.DisplayLeft.ID
            let right = gameplay.DisplayRight.ID

            let actors = World.getEntities Simulants.CombatCharacters world

            let leftCharacter =
                actors
                |> Seq.map (fun entity -> entity.GetCharacter world)
                |> Seq.find (fun character-> character.ID = left)

            let rightCharacter =
                actors
                |> Seq.map (fun entity -> entity.GetCharacter world)
                |> Seq.find (fun character-> character.ID = right)

            { gameplay with DisplayLeft = leftCharacter; DisplayRight = rightCharacter }
        | Quit -> gameplay


    static member getCharacterMostWounds entities world =
        entities
        |> List.sortBy (fun (entity: Entity) ->
            let character = entity.GetCharacter world
            character.MajorWounds
        )
        |> List.last

    // attack, prototype of attacker AI
    static member turnAttackerPlan (attacker: Entity) gameplay world =

        // pick target with the most wounds, excluding combatant himself
        let target =
            let otherCombatants =
                gameplay.Combatants
                |> List.map fst
                |> List.remove (fun entity -> entity = attacker)
            Combat.getCharacterMostWounds otherCombatants world

        // check if actor can do moves
        let canAct =
            let character = attacker.GetCharacter world
            character.MajorWounds < MajorWounds.Down

        if canAct then

            let combatantName = attacker.Name
            let targetName = target.Name

            let area = gameplay.Area

            let physicalAction =
                match Area.findPath combatantName targetName area with
                // no path
                | [] ->
                    NoPhysicalAction
                | path ->

                    let character = attacker.GetCharacter world
                    let statCombatant = Character.getStat Gall character
                    let reach = Character.getReach character
                    let speed = Character.getSpeed character

                    let distance = List.last path |> snd

                    if (reach >= distance) then
                        // in reach
                        Random.getItemsFromList statCombatant Move.attacks
                    else
                        // not in reach
                        if (speed * statCombatant >= distance) then
                            // can run to
                            let movementMoves = round (float distance / float speed + 0.5) |> uint32
                            List.init (movementMoves |> int) (fun i -> Stride)
                            @ Random.getItemsFromList (statCombatant - movementMoves) Move.attacks
                        else
                            // can't run to
                            List.init (statCombatant |> int) (fun i -> Stride)
                    |> Sequence

            let action = {
                CharacterAction.empty with
                    Turn = gameplay.Turn
                    Target = Some target
                    PhysicalAction = physicalAction
                    MentalAction = NoMentalAction
                    Element = Some Gall
            }

            action
        else
            let action = {
                CharacterAction.empty with
                    Turn = gameplay.Turn
                    Target = Some target
                    PhysicalAction = NoPhysicalAction
                    MentalAction = NoMentalAction
                    Element = Some Gall
            }

            action


    // response, prototype of defender AI
    static member turnDefenderPlan (attacker: Entity) attackerAction (defender: Entity) gameplay world =

        let canAct =
            let character = defender.GetCharacter world
            character.MajorWounds < MajorWounds.Down

        if canAct then

            let movesCombatant =
                match attackerAction.PhysicalAction with
                | Sequence moves -> moves
                | _ -> []


            let combatantName = attacker.Name
            let targetName = defender.Name

            let area = gameplay.Area

            let physicalAction =
                match Area.findPath combatantName targetName area with
                // no path
                | [] ->
                    NoPhysicalAction
                | path ->
                    let character = defender.GetCharacter world
                    let reach = Character.getReach character
                    let statDefender = Character.getStat Lymph character

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
                    |> Sequence

            let action = {
                CharacterAction.empty with
                    Turn = gameplay.Turn
                    Target = Some attacker
                    PhysicalAction = physicalAction
                    MentalAction = NoMentalAction
                    Element = Some Lymph
            }

            action

        else
            let action = {
                CharacterAction.empty with
                    Turn = gameplay.Turn
                    Target = Some attacker
                    PhysicalAction = NoPhysicalAction
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


// this is our gameplay MMCC message type.
type CombatMessage =
    | StartPlaying
    | FinishQuitting
    | Update
    | TurnBegin
    | CombatantAttacks of Entity
    | CombatantDefends of Entity * CharacterAction
    | TurnEnd of Entity * CharacterAction * Entity * CharacterAction
    | ActorAction of Entity * CharacterAction
    | ActorMove of String * String * int * Move list
    | TimeUpdate
    interface Message

// this is our gameplay MMCC command type.
type CombatCommand =
    | RollInitiative
    | GameEffect of GameEffect
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
    override this.Message (model, message, screen, world) =

        match message with
        | StartPlaying ->

            let gameplay = Combat.initial

            let gameplay = {
                gameplay with
                    Combatants =
                        world
                        |> World.getEntities Simulants.CombatCharacters
                        |> Seq.map (fun c -> c, [])
                        |> List.ofSeq
            }

            let signal : Signal = RollInitiative

            withSignal signal gameplay

        | FinishQuitting ->
            let gameplay = Combat.empty
            just gameplay

        | Update ->
            let gameplay = Combat.update model world
            just gameplay


        | TurnBegin ->
            let attacker, model =
                match model.CombatantID with
                | Some attacker ->
                    attacker, model
                | None ->
                    let attacker =
                        model.Combatants
                        |> List.head
                        |> fst
                    attacker,
                    { model with CombatantID = Some attacker }

            let nextCombatantID =

                let combatants = model.Combatants |> List.map fst

                let attackerIndex =
                    combatants
                    |> List.findIndex (fun c -> c = attacker)

                let index =
                    if attackerIndex + 1 < List.length combatants then
                        attackerIndex + 1
                    else
                        0

                combatants
                |> List.item index

            let model = {
                model with
                    CombatantID = Some nextCombatantID
                    Turn = model.Turn + 1
            }

            let resetCharacter =
                Character.stanceReset

            let signals : Signal list = [
                GameEffect (CharacterFunction (attacker, resetCharacter))
                CombatantAttacks attacker
            ]

            withSignals signals model

        | CombatantAttacks attacker ->

            let attackerAction =
                Combat.turnAttackerPlan attacker model world

            let signals : Signal list = [
                GameEffect (CharacterFunction (attacker, Combat.turnAttackerStanceChange))
                CombatantDefends (attacker, attackerAction)
            ]

            withSignals signals model

        | CombatantDefends (attacker, attackerAction) ->
            match attackerAction with
            | { Target = Some defender } as action ->
                let defenderAction =
                    Combat.turnDefenderPlan attacker action defender model world

                let signals : Signal list = [
                    GameEffect (CharacterFunction (defender, Combat.turnDefenderStanceChange))
                    TurnEnd (attacker, attackerAction, defender, defenderAction)
                ]

                withSignals signals model
            | _ ->
                just model

        | TurnEnd (attacker, attackerAction, defender, defenderAction) ->

            let attackerAction =
                let character = attacker.GetCharacter world
                Combat.applyStanceToAction character attackerAction

            let defenderAction =
                let character = defender.GetCharacter world
                Combat.applyStanceToAction character defenderAction

            // add karma betting

            let attackerAction = { attackerAction with Blocks = defenderAction.Successes }
            let defenderAction = { defenderAction with Blocks = attackerAction.Successes }

            let combatants =
                model.Combatants
                |> List.map (fun (entity, history) ->
                    if entity = attacker then
                        entity, attackerAction::history
                    elif entity = defender then
                        entity, defenderAction::history
                    else
                        entity, history
                )

            let model = { model with Combatants = combatants }

            let signals : Signal list = [
                ActorAction (attacker, attackerAction)
                ActorAction (defender, defenderAction)
            ]

            withSignals signals model

        | ActorAction (actor, action) ->

            let attack = action.PhysicalAction

            match attack with
            | NoPhysicalAction ->
                just model
            | FullPhysicalAction ->
                just model
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

                    match action.Target with
                    | Some target ->
                        List.map (fun i -> ActorMove (actor.Name, target.Name, i, moves)) indexes
                    | None ->
                        []

                withSignals signals model

        | ActorMove (actorID, targetID, moveID, moves) ->

            let actors = World.getEntities Simulants.CombatCharacters world

            let actor =
                actors
                |> Seq.find (fun entity -> entity.Name = actorID)

            let target =
                actors
                |> Seq.find (fun entity  -> entity.Name = targetID)

            let signals : Signal list =
                Move.handle moveID moves actor target model.Area world
                |> List.map (fun signal -> GameEffect signal)

            withSignals signals model


        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { model with GameplayTime = model.GameplayTime + gameDelta.Updates }
            just gameplay

    // here we handle the above commands
    override this.Command (model, command, screen, world) =

        match command with
        | StartQuitting ->
            let world = World.publish () screen.QuitEvent screen world
            just world

        | RollInitiative ->

            let world =
                model.Combatants
                |> List.map fst
                |> List.fold (fun (world : World) (entity : Entity) ->
                    let character = entity.GetCharacter world

                    let character = Character.stanceReset character
                    let gall, plasma = Character.getMaxInitiative character
                    let character = { character with Initiative = Random.rollInitiative (gall + plasma) }

                    let world = entity.SetCharacter character world
                    world
                ) world

            let combatants =
                model.Combatants
                |> List.sortBy (fun (entity, _) ->
                    let character = entity.GetCharacter world
                    character.Initiative
                )

            let model = { model with Combatants = combatants }

            let world = screen.SetCombat model world

            just world

        | GameEffect (CharacterFunction (entity, func)) ->
            let character = entity.GetCharacter world
            let character = func character
            let world = entity.SetCharacter character world
            just world

        | GameEffect (Damage (entity, damage)) ->

            let character = entity.GetCharacter world
            let character = Character.doDamage damage character
            let world = entity.SetCharacter character world

            just world

        | GameEffect (MoveInter (character, location)) ->

            let area = model.Area
            let area = Area.moveSite character location area
            let model = { model with Area = area }
            let world = screen.SetCombat model world
            just world

        | GameEffect (MoveIntra (character, location, distance)) ->

            let area = model.Area
            let area = Area.establishDistance distance character location area
            let model = { model with Area = area }
            let world = screen.SetCombat model world
            just world

    // here we describe the content of the game including the hud, the scene, and the player
    override this.Content (gameplay, screen) = [
        // the gui group

        Content.group Simulants.GameplayGui.Name [] [

            Content.button Simulants.GameplayGuiAdvanceTurn.Name [
                Entity.Position == v3 0.0f 150.0f 0.0f
                Entity.Size == v3 80.0f 20.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text == "Advance Turn"
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 10
                Entity.ClickEvent => TurnBegin
            ]

            let player = gameplay.DisplayLeft
            let enemy = gameplay.DisplayRight

            let playerHistory =
                gameplay.Combatants
                |> List.tryFind (fun (entity, _) -> player.ID = entity.Name)

            let enemyHistory =
                gameplay.Combatants
                |> List.tryFind (fun (entity, _) -> enemy.ID = entity.Name)


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


            match playerHistory, enemyHistory with
            | Some (_, playerLastTurn::_), Some (_, enemyLastTurn::_) ->

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

                richText "CombatLocations" [
                    Entity.Position == v3 0.0f -160.0f 0.0f
                    Entity.Size == v3 360.0f 40.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Text := $"{Area.getConnections player.ID gameplay.Area} -- {Area.getConnections enemy.ID gameplay.Area}"
                    Entity.TextColor == Color.FloralWhite
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                ]

            | _ -> ()

        ]

        // the scene group while playing
        match gameplay.GameplayState with
        | Playing ->
            Content.group Simulants.CombatCharacters.Name [] [
                character Character.player
                character Character.rat
            ]
        // no scene group otherwise
        | Quit -> ()
    ]