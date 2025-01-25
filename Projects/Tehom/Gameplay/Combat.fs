namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open Move
open Action
open Character

(*
TODO: Always do the minimum implementation

TODO:
    - The Seven Rooms
    - Taking key
    - Opening door
    - Taking pistol

TODO: Combat
    - Doesn't end
    - No skills
    - Can't select stance
    - No karma betting
    - No inventory
    - No cooldown
    - Body doesn't impact anything

TODO: Containers and notes

TODO: Debate

TODO: Visibility system + Lights

TODO: Tiniest vertical slice:
    * 7 Rooms:
    * Waitroom, barricaded windows, rolling hospital bed, locked exit door, you wake up here
    * Registration room, safe with useful stuff like a pistol maybe, code locked, code is gotten from a book
      (can be seen from waitroom through glass)
    * Main hall, chairs, first rat attacks
    * Electrical room to fix the lights, second rat attacks
    * Surgery room, surgery table, cat on the table, note (cat ate the key), opening it lets spider chandalier escape
    * Pharmacy shop, drugs you can use
    * Staircase, other floors blocked, but can move up and down.
    * Goal is to go to surgery room, take the key, return to waitroom, open the door and exit.
    * Optionally you can fix lights and fight spider chandalier.
*)

type GameEffect =
    | CharacterDo of Entity * (Character -> Character)
    | Damage of Entity * Size : int * Damage : int
    | TravelInter of Area : Entity * String * String
    | TravelIntra of Area : Entity * String * String * uint32
with
    static member travel actor target closeness distance (area : Entity) world =
        let model = area.GetArea world
        match Area.moveWithinReach actor target closeness distance model with
        | Some (fst, snd), Some (fst', snd', thd') ->
            [TravelInter (area, fst, snd); TravelIntra (area, fst', snd', thd')]
        | Some (fst, snd), None ->
            [TravelInter (area, fst, snd)]
        | None, Some (fst, snd, thd) ->
            [TravelIntra (area, fst, snd, thd)]
        | None, None ->
            []

type Check = {
    Action : Action
    Type : CheckType
    Cost : Cost
    Target : Entity option
    OpposedBy : Entity list
}
with
    static member empty = {
        Action = NoAction
        Type = NoType
        Cost = Cost.empty
        Target = None
        OpposedBy = []
    }

    static member unstoppable action = {
        Check.empty with
            Action = action
    }

    static member unopposed action cost = {
        Check.unstoppable action with
            Cost = cost
    }

    static member isUnstoppable check =
        (List.isEmpty check.OpposedBy) && check.Cost = Cost.empty

    static member isUnopposed check =
        List.isEmpty check.OpposedBy

    static member opposed action cost target = {
        Check.unopposed action cost with
            OpposedBy = [target]
    }

    static member targeted action cost target = {
        Check.opposed action cost target with
            Target = Some target
    }

    static member isOpposed check =
        List.notEmpty check.OpposedBy

    static member stance action =
        Check.unopposed action { Cost.empty with Stances = 1}

    static member moveAttack action target =
        let cost = { Cost.empty with StaminaPhysicalActive = 1 }
        { Check.targeted action cost target with Type = PhysicalActive }

    static member moveDefence action target =
        let cost = { Cost.empty with StaminaPhysicalReactive = 1 }
        { Check.targeted action cost target with Type = PhysicalReactive }

    static member applyCosts (actor : Entity) check (world : World) =
        let character = actor.GetCharacter world

        if Check.isUnstoppable check then
            true, world

        elif Check.isUnopposed check then
            if Character.canPay check.Cost character then
                let world = actor.SetCharacterWith (Character.pay check.Cost) world
                true, world
            else
                false, world

        else
            if Character.canPay check.Cost character then
                let world = actor.SetCharacterWith (Character.pay check.Cost) world

                let enemyStamina =
                    check.OpposedBy
                    |> List.map (fun entity -> entity.GetCharacter world)
                    |> List.map (Character.getStamina (CheckType.getOpposite check.Type))
                    |> List.max

                let actorStamina = Character.getStamina check.Type character

                (actorStamina > enemyStamina), world
            else
                false, world

    static member processCheck check before after (attacker : Entity) (area : Entity) world =

        match check.Action with
        | SkillSelect i ->
            // TODO: implement skills
            // CharacterDo (attacker, 0)
            []
        | StanceChange stance ->
            [ CharacterDo (attacker, Character.setStance stance) ]
        | KarmaBet bet ->
            [ CharacterDo (attacker, Character.removeKarma bet) ]
        | RollStance ->
            [ CharacterDo (attacker, Character.roll) ]
        | Move move' ->
            match move' with
            | Stride
            | Climb
            | Crawl
            | Jump
            | Roll
            | Sidestep
            | Dash
            | Swim ->
                let should =
                    after
                    |> List.map (fun check -> match check.Action with Move Stride-> true | _ -> false)
                    |> List.contains true
                    |> not

                if should then
                    match check.Target with
                    | Some target ->

                        let moves =
                            check::before
                            |> List.sumBy (fun check -> match check.Action with Move Stride -> 1u | _ -> 0u)

                        let attackerCharacter = attacker.GetCharacter world
                        let reach = Character.getReach attackerCharacter
                        let distance = moves * Character.getSpeed attackerCharacter

                        GameEffect.travel attacker.Name target.Name reach distance area world

                    | None ->
                        []
                else
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
                match check.Target with
                | Some target ->

                    let bonusDamage damage =
                        let damage =
                            match before with
                            | [] -> damage
                            | _ ->
                                before
                                |> List.rev
                                |> List.foldWhile (fun bonus move ->
                                    match move.Action with
                                    | Move Power -> Some (bonus + 5)
                                    | _ -> None
                                ) damage
                        let damage =
                            match after with
                            | _::after ->
                                after
                                |> List.foldWhile (fun bonus move ->
                                    match move.Action with
                                    | Move Press -> Some (bonus + 2)
                                    | _ -> None
                                ) damage
                            | _ -> damage
                        damage


                    // weapon
                    let damage = Weapon.getDamage weapon
                    let size = Weapon.getSizeBoost weapon
                    // moves
                    let damage = bonusDamage damage
                    // attacker
                    let attackerCharacter = attacker.GetCharacter world
                    let size = size + Character.getSize attackerCharacter
                    [Damage (target, size, damage)]

                | None ->
                    []
        | _ ->
            []

type Turn = {
    Turn : int
    Entity : Entity
    Checks : Check list
    Passed : bool list
}
with
    static member empty = {
        Turn = 0
        Entity = Entity
        Checks = []
        Passed = []
    }

    static member processCosts turn world =
        turn.Checks
        |> List.foldMap (fun check (world : World) ->
            let result, world = Check.applyCosts turn.Entity check world
            result, world
        ) world
        |> fun (checks, world) -> { turn with Passed = checks }, world

    static member zip turn =
        List.zip turn.Checks turn.Passed

    static member processEffects turn area world =
        let checks =
            Turn.zip turn
            |> List.filter snd
            |> List.map fst

        checks
        |> List.indexed
        |> List.collect (fun (i, check) ->
            let before, after = List.splitAt i checks
            Check.processCheck check before (List.tail after) turn.Entity area world
        )

    static member describe turn =
        Turn.zip turn
        |> List.map (fun (check, succeeded) ->
            Action.describe check.Action, succeeded
        )

type CombatState =
    | TurnNone
    | TurnAttacker of Attacker : Entity
    | TurnAttackPlan of Attacker : Entity
    | TurnDefender of AttackPlan : Turn
    | TurnDefencePlan of AttackPlan : Turn * Defender : Entity
    | TurnAttackKarmaBid of AttackPlan : Turn * DefencePlan : Turn
    | TurnDefenceKarmaBid of AttackPlan : Turn * DefencePlan : Turn
    | TurnExecute of AttackPlan : Turn * DefencePlan : Turn

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Combat = {
    CombatTime : int64
    CombatState : CombatState

    Turn : int
    Combatants : Entity list
    History : Map<Entity, Turn list>
    Area : Entity

    DisplayLeftEntity : Entity option
    DisplayLeftModel : Character option
    DisplayRightEntity : Entity option
    DisplayRightModel : Character option

    PossibleActions : (Action * bool) List
    PossibleTargets : (Entity * bool) list
    PlannedActions : Action list
    PlannedTarget : Entity option
    DistanceCurrentReach : uint32
    DistanceToTarget : uint32
}
with
    // this represents the gameplay model in a vacant state, such as when the gameplay screen is not selected.
    static member empty = {
        CombatTime = 0L
        CombatState = TurnNone

        Turn = 0
        Combatants = []
        History = Map.empty
        Area = Entity

        DisplayLeftEntity = None
        DisplayLeftModel = None
        DisplayRightEntity = None
        DisplayRightModel = None

        PossibleActions = []
        PossibleTargets = []
        PlannedActions = []
        PlannedTarget = None
        DistanceCurrentReach = 0u
        DistanceToTarget = 0u
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Combat.empty with
            DisplayLeftEntity = Some (Simulants.GameplayCharacters / CharacterContent.player.ID)
            DisplayRightEntity = Some (Simulants.GameplayCharacters / CharacterContent.rat.ID)
    }

    // this updates the gameplay model every frame that gameplay is active.
    static member update gameplay world =

        let leftCharacter =
            match gameplay.DisplayLeftEntity with
            | Some entity -> Some (entity.GetCharacter world)
            | None -> None

        let rightCharacter =
            match gameplay.DisplayRightEntity with
            | Some entity -> Some (entity.GetCharacter world)
            | None -> None

        { gameplay with DisplayLeftModel = leftCharacter; DisplayRightModel = rightCharacter }


    static member getCharacterMostWounds entities world =
        entities
        |> List.sortBy (fun (entity: Entity) ->
            let character = entity.GetCharacter world
            character.Wounds
        )
        |> List.last

    static member isCharacterControlled entity =
        let player = Simulants.GameplayCharacters / CharacterContent.player.ID
        entity = player


    static member advanceTurn turns' combat =

        let history =
            combat.History
            |> Map.map (fun entity turns ->
                match turns' |> List.tryFind (fun (turn : Turn) -> entity = turn.Entity) with
                | Some turn ->
                    turn::turns
                | None ->
                    turns
            )

        let attacker = List.head combat.Combatants
        let combatants = List.tail combat.Combatants

        let model = {
            combat with
                Combatants = combatants @ [attacker]
                Turn = combat.Turn + 1
                History = history
        }

        model

[<AutoOpen>]
module CombatExtensions =
    type Entity with
        member this.GetCombat world = this.GetModelGeneric<Combat> world
        member this.SetCombat value world = this.SetModelGeneric<Combat> value world
        member this.Combat = this.ModelGeneric<Combat> ()
        member this.ExecuteGameEffect effect world =
            match effect with
            | CharacterDo (entity, func) ->
                let world = entity.SetCharacterWith func world
                world

            | Damage (entity, size, damage) ->
                let func character =
                    let sizeDifference = Character.getSize character - size
                    let character = Character.doDamage sizeDifference damage character
                    character
                let world = entity.SetCharacterWith func world
                world

            | TravelInter (area, character, location) ->
                let world = area.SetAreaWith (Area.moveSite character location >> Area.iterateDisplay 10) world
                world

            | TravelIntra (area, character, location, distance) ->
                let world = area.SetAreaWith (Area.establishDistance distance character location >> Area.iterateDisplay 10) world
                world
        member this.ExecuteGameEffects effects world =
            List.fold (fun world effect -> this.ExecuteGameEffect effect world) world effects