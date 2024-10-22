namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open Move
open Action
open Character

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
    | Damage of Entity * Size : int * Damage : int
    | TravelInter of String * String
    | TravelIntra of String * String * uint32

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

    let getItemFromList list =
        Gen.randomItem list

    let rollInitiative max =
        Gen.random2 0 max

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

    static member unstoppable action = {
        Check.empty with
            Action = action
    }

    static member unopposed action threshold = {
        Check.empty with
            Action = action
            Threshold = threshold
    }

    static member opposed action threshold target = {
        Check.empty with
            Action = action
            Threshold = threshold
            OpposedBy = [target]
    }

    static member attack action threshold target = {
        Check.empty with
            Action = action
            Element = Some Gall
            Threshold = threshold
            OpposedBy = [target]
            Target = Some target
    }

    static member defence action threshold target = {
        Check.empty with
            Action = action
            Element = Some Lymph
            Threshold = threshold
            OpposedBy = [target]
            Target = Some target
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

    static member processMove moveID moves (attacker : Entity) (defender : Entity) area world =

        let bonusDamage moveID moves damage =
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
        | Sweep
        | Toss
        | Throw ->
            []
        | Strike weapon ->
            // weapon
            let damage = Weapon.getDamage weapon
            let size = Weapon.getSizeBoost weapon
            // moves
            let damage = bonusDamage moveID moves damage
            // attacker
            let attackerCharacter = attacker.GetCharacter world
            let size = size + Character.getSize attackerCharacter
            [Damage (defender, size, damage)]

    static member processTurn actor turn area world =
        turn.Checks
        |> List.fold (fun effects check ->
            match check.Action with
            | FullMentalAction ->
                effects

            | FullPhysicalAction ->
                effects

            | StanceChange stance ->
                let effect =
                    CharacterStanceChange (actor, stance)
                effects @ [ effect ]

            | PhysicalSequence moves ->
                let successes = check.Successes
                let blocks = check.OpposedSuccesses

                let effects' =
                    let indexes, moves =
                        moves
                        |> List.indexed
                        |> List.takeWhile (fun (i, move) ->
                            // positioning is always successful for now, but should check if within reach of enemy later
                            ((List.contains move Move.positioning) || (successes > blocks)) && (i < successes)
                        )
                        |> List.unzip

                    match check.Target with
                    | Some target ->
                        List.map (fun i ->
                            Turn.processMove i moves actor target area world
                        ) indexes
                        |> List.concat
                    | None ->
                        []
                effects @ effects'
            ) []


type CombatState =
    | Playing
    | Quit

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Combat = {
    GameplayTime : int64
    GameplayState : CombatState

    Turn : int
    Combatants : Entity list
    History : Map<Entity, Turn list>
    Area : Area

    DisplayLeft : Character
    DisplayRight : Character
}
with
    // this represents the gameplay model in a vacant state, such as when the gameplay screen is not selected.
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit

        Turn = 0
        Combatants = []
        History = Map.empty
        Area = Area.empty

        DisplayLeft = Character.empty
        DisplayRight = Character.empty
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Combat.empty with
            GameplayState = Playing
            DisplayLeft = CharacterContent.player
            DisplayRight = CharacterContent.rat
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