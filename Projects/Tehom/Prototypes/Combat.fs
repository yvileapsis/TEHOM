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
    | CharacterReset of Entity
    | CharacterStanceChange of Entity * Stance
    | Damage of Entity * int
    | TravelInter of String * String
    | TravelIntra of String * String * uint32

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
                [TravelInter moveInter; TravelIntra moveIntra]
            | Some moveInter, None ->
                [TravelInter moveInter]
            | None, Some moveIntra ->
                [TravelIntra moveIntra]
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

type Action =
    | FullMentalAction
    | FullPhysicalAction
    | StanceChange of Stance
    | PhysicalSequence of Move list

type TurnType =
    | Action
    | Reaction

type Check = {
    Action : Action

    Skill : int

    Element : Element option
    Successes : int
    Threshold : int

    Target : Entity option

    OpposedBy : Entity list
    OpposedSuccesses : int
}
with
    static member empty = {
        Action = PhysicalSequence []
        Skill = 0
        Element = None
        Successes = 0
        Threshold = 0
        Target = None
        OpposedBy = []
        OpposedSuccesses = 0
    }

type Turn = {
    Turn : int
    Type : TurnType
    Checks : Check list
}
with
    static member empty = {
        Turn = 0
        Type = Reaction
        Checks = []
    }

    static member applyStance combatant turn =
        let checks =
            turn.Checks
            |> List.foldMap (fun check combatant ->
                match check.Action with
                | StanceChange stance ->
                    let combatant = Character.stanceChange stance combatant
                    check, combatant
                | _ ->
                    match check.Element with
                    | Some element ->

                        let stat = Character.getStancedStat element combatant |> int
                        let successes = Random.rollDiceThreshold stat 3
                        let check = { check with Successes = successes }

                        check, combatant

                    | None ->

                        check, combatant
            ) combatant
            |> fst
        { turn with Checks = checks }

    // TODO: Simplest way to do it for now
    static member opposedTurns left right =
        let leftSuccesses =
            left.Checks
            |> List.fold (fun acc check -> acc + check.Successes) 0
        let rightSuccesses =
            right.Checks
            |> List.fold (fun acc check -> acc + check.Successes) 0

        let leftChecks =
            left.Checks
            |> List.map (fun check ->
                if List.isEmpty check.OpposedBy then
                    check
                else
                    { check with OpposedSuccesses = rightSuccesses }
            )

        let rightChecks =
            right.Checks
            |> List.map (fun check ->
                if List.isEmpty check.OpposedBy then
                    check
                else
                    { check with OpposedSuccesses = leftSuccesses }
            )

        { left with Checks = leftChecks }, { right with Checks = rightChecks }


type CombatState =
    | Playing
    | Quit

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Combat = {
    GameplayTime : int64
    GameplayState : CombatState

    Combatants : (Entity * Turn list) list
    CurrentCombatant : Entity option

    DisplayLeft : Character
    DisplayRight : Character

    Turn : int

    Area : Area
}
with
    // this represents the gameplay model in a vacant state, such as when the gameplay screen is not selected.
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit

        Combatants = List.empty
        CurrentCombatant = None

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

    // btw thinking with high enough air you should be able to tell enemy's stance
    // TODO: implement correct stance selection once skills are implemented
    static member turnAttackerStance =
        Character.stanceMove [Gall; Gall; Gall] [Lymph; Oil; Plasma] Character.stanceEmpty

    static member turnDefenderStance =

        Character.stanceMove [Lymph; Lymph; Lymph] [Gall; Oil; Plasma] Character.stanceEmpty


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


            let stanceChange = [{
                Check.empty with
                    Action = StanceChange Combat.turnAttackerStance
            }]

            let area = gameplay.Area

            let physicalAction =
                match Area.findPath combatantName targetName area with
                // no path
                | [] ->
                    []
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
                    |> PhysicalSequence
                    |> fun x -> [{
                        Check.empty with
                            Action = x
                            Element = Some Gall
                            Threshold = 1

                            Target = Some target

                            OpposedBy = [target]
                    }]

            let action = {
                Turn.empty with
                    Turn = gameplay.Turn
                    Type = Action
                    Checks = stanceChange @ physicalAction
            }

            action
        else
            let action = {
                Turn.empty with
                    Turn = gameplay.Turn
                    Type = Action
                    Checks = []
            }

            action


    // response, prototype of defender AI
    static member turnDefenderPlan (attacker: Entity) attackerAction (defender: Entity) gameplay world =

        let canAct =
            let character = defender.GetCharacter world
            character.MajorWounds < MajorWounds.Down

        if canAct then

            let movesCombatant =
                match attackerAction.Action with
                | PhysicalSequence moves -> moves
                | _ -> []

            let combatantName = attacker.Name
            let targetName = defender.Name

            let area = gameplay.Area

            let stanceChange = [{
                Check.empty with
                    Action = StanceChange Combat.turnDefenderStance
            }]

            let physicalAction =
                match Area.findPath combatantName targetName area with
                // no path
                | [] ->
                    []
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
                    |> PhysicalSequence
                    |> fun x -> [{
                        Check.empty with
                            Action = x
                            Element = Some Lymph
                            Threshold = 1

                            Target = Some attacker

                            OpposedBy = [attacker]
                    }]


            let action = {
                Turn.empty with
                    Type = Reaction
                    Turn = gameplay.Turn
                    Checks = stanceChange @ physicalAction
            }

            action

        else
            let action = {
                Turn.empty with
                    Type = Reaction
                    Turn = gameplay.Turn
                    Checks = []
            }

            action