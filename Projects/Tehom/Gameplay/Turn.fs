namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open Character

type GameEffect =
    | CharacterDo of Entity * (Character -> Character)
    | Damage of Entity * Size : Int32 * Damage : Int32
    | TravelInter of Area : Entity * String * String
    | TravelIntra of Area : Entity * String * String * UInt32
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
    Action : Tehom.Action
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
            [ CharacterDo (attacker, Character.removeFracture bet) ]
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

type Plan = {
    Entity : Entity
    PossibleActions : List<Tehom.Action * Boolean>
    PossibleTargets : List<Entity * Boolean>
    PlannedActions : List<Tehom.Action>
    PlannedTarget : Option<Entity>
    PlannedFractureBet : Int32

    DistanceCurrentReach : UInt32
    DistanceToTarget : UInt32

    Turn : Int32
    Checks : List<Check>
}
with
    static member empty = {
        Entity = Entity
        PossibleActions = []
        PossibleTargets = []
        PlannedActions = []
        PlannedTarget = None
        PlannedFractureBet = 0

        DistanceCurrentReach = 0u
        DistanceToTarget = 0u

        Turn = 0
        Checks = List.empty
    }

    static member make entity turnID = {
        Plan.empty with
            Entity = entity
            Turn = turnID
    }

    static member describe (plan : Plan) =
        plan.PlannedActions
        |> List.map Action.describe

    static member describe2 (plan : Plan) =
        plan.Checks
        |> List.map (fun check ->
            Action.describe check.Action
        )

    static member isCustom (plan : Plan) =
        let player = Simulants.GameplayCharacters / CharacterContent.player.ID
        plan.Entity = player


    static member setPlannedTarget target (plan : Plan) =
        let plan = {
            plan with
                PlannedTarget = Some target
        }
        plan

    static member addPlannedAction action (plan : Plan) =
        let plan = {
            plan with
                PlannedActions = plan.PlannedActions @ [ action ]
        }
        plan

    static member removePlannedAction i (plan : Plan) =
        let plan = {
            plan with
                PlannedActions = List.removeAt i plan.PlannedActions
        }
        plan

    static member modifyFractureBet i (plan : Plan) =
        let plan = {
            plan with
                PlannedFractureBet = plan.PlannedFractureBet + i
        }
        plan

    static member getCharacterMostWounds entities world =
        entities
        |> List.sortBy (fun (entity: Entity) ->
            let character = entity.GetCharacter world
            character.Wounds
        )
        |> List.last

    static member getPossibleTargets attacker combatants =
        // TODO: some sort of allegiance system
        combatants
        |> List.remove (fun entity -> entity = attacker)
        |> List.map (fun entity -> entity, true)

    // pick target with the most wounds, excluding combatant himself
    static member findTarget attacker combatants world =
        let targets =
            Plan.getPossibleTargets attacker combatants
            |> List.filter snd
            |> List.map fst
        Plan.getCharacterMostWounds targets world

    static member getDistanceBetweenAttackerAndTarget (area : Entity) (plan : Plan) world =

        let (Some target) = plan.PlannedTarget

        let attackerName = plan.Entity.Name
        let targetName = target.Name

        let area = area.GetArea world
        match Area.findPath attackerName targetName area with
        | _::_ as path ->

            let distance = List.last path |> snd

            distance
        | _ ->
            0u

    static member getPossibleActions (area : Entity) (plan : Plan) world =

        let character = plan.Entity.GetCharacter world
        let statCombatant = Character.getStat Gall character
        let plannedActions = plan.PlannedActions

        let isAttackExecutable attack =

            let (Some target) = plan.PlannedTarget

            let attackerName = plan.Entity.Name
            let targetName = target.Name

            let area = area.GetArea world
            match Area.findPath attackerName targetName area with
            | _::_ as path ->

                let distance = List.last path |> snd
                // in reach
                if Character.getCoveredDistance plannedActions character >= distance then
                    true
                else
                    false
            | _ ->
                false

        let moveWithinActionLimit move =
            int statCombatant > (List.length plannedActions)

        Character.getPossibleMoves character
        |> List.map (fun x ->
            let executable =
                if List.contains x Move.positioning then
                    moveWithinActionLimit x
                else
                    moveWithinActionLimit x
                    && isAttackExecutable x

            Move x, executable
        )

    static member tryMakeChecksAttackCustom (plan : Plan) =

        if List.notEmpty plan.PlannedActions && Option.isSome plan.PlannedTarget then

            // TODO: move to planning
            let (Some target) =
                plan.PlannedTarget

            let stance =
                [ Check.unstoppable (StanceChange Stance.attacker) ]

            let roll =
                [ Check.unstoppable RollStance ]

            let fracture =
                plan.PlannedFractureBet

            let karmaBet =
                [ Check.unstoppable (KarmaBet fracture) ]

            let checks =
                plan.PlannedActions
                // threshold should be generated for the action
                |> List.map (fun action -> Check.moveAttack action target)

            Some { plan with Checks = stance @ roll @ karmaBet @ checks }

        else
            None


    // attack in the case character has no programming
    static member attackDefault character moves attacks target =
        // TODO: better selection of positioning moves
        let move =
            List.init moves (fun _ -> Stride)

        // TODO: much better selection of weapons
        // TODO: much better selection of attack moves, power, press and such
        let attack =
            let weapon () =
                Character.getWeapons character
                |> Random.getItemFromList

            List.init attacks (fun _ -> Strike (weapon ()))

        move @ attack
        |> fun x -> [
            Check.unstoppable (StanceChange Stance.attacker)
            Check.unstoppable RollStance
            for i in x do Check.moveAttack (Move i) target
        ]

    // attack in the case character is programmed
    static member attackCustom character threshold target =
        let customAction =
            character.CustomActions
            |> List.sortByDescending (fun { Actions = actions } ->
                actions
                |> List.fold (fun priority action ->
                    match action with
                    | StanceChange { GallStance = gall; LymphStance = lymph } ->
                        // if healthy pick aggressive stance, otherwise pick defensive stance
                        if Character.isDamaged character then
                            lymph
                        else
                            gall
                    | _ ->
                        priority
                ) 0
            )
            |> List.head

        customAction
        |> _.Actions
        |> List.map (function
            | Move _ as action ->
                Check.moveAttack action target
            | StanceChange _ as action ->
                Check.stance action
            | action ->
                Check.unstoppable action
        )

    // plan attacker actions
    static member tryMakeChecksAttackDefault (plan : Plan) (area : Entity) world =

        let attacker = plan.Entity

        let character = attacker.GetCharacter world

        if Character.canAct character then

            let (Some target) = plan.PlannedTarget

            let attackerName = attacker.Name
            let targetName = target.Name

            let area = area.GetArea world
            match Area.findPath attackerName targetName area with
            | _::_ as path ->

                let statCombatant = Character.getStat Gall character
                let reach = Character.getReach character
                let speed = Character.getSpeed character

                let distance = List.last path |> snd

                // in reach
                if (reach >= distance) then
                    // custom actions
                    if Character.hasCustomActions character then
                        Plan.attackCustom character (int statCombatant) target
                    // no custom actions
                    else
                        Plan.attackDefault character 0 (int statCombatant) target
                // not in reach
                else
                    // can run to
                    if speed * statCombatant >= distance then
                        let movementMoves = round (float distance / float speed + 0.5) |> int
                        Plan.attackDefault character movementMoves (int statCombatant - movementMoves) target
                    // can't run to
                    else
                        Plan.attackDefault character (int statCombatant) 0 target

                |> fun checks -> Some { plan with Checks = checks }

            | _ ->
                None
        else
            None

    static member analyzeAttackerMoves attackerAction =
        match attackerAction.Action with
        | Move move ->
            if List.contains move Move.attacks then 1 else 0
        | _ ->
            0

    // defence in the case character has no programming
    static member defenceDefault character blocks attacks target =
        // TODO: better selection of positioning moves
        let defence =
            Random.getItemsFromList blocks Move.defence

        // TODO: much better selection of weapons
        // TODO: much better selection of attack moves, power, press and such
        let attack =
            let weapon () =
                Character.getWeapons character
                |> Random.getItemFromList

            if attacks > 0 then
                [ Ready ]
            else
                []
            @
            if attacks > 1 then
                List.init (attacks - 1) (fun _ -> Strike (weapon ()))
            else
                []

        defence @ attack
        |> fun x -> [
            Check.unstoppable (StanceChange Stance.attacker)
            Check.unstoppable RollStance
            for i in x do Check.moveDefence (Move i) target
        ]

    // defence in the case character is programmed
    static member defenceCustom character threshold target =
        let customAction =
            character.CustomActions
            |> List.sortByDescending (fun { Actions = actions } ->
                actions
                |> List.fold (fun priority action ->
                    match action with
                    | StanceChange { LymphStance = lymph } ->
                        lymph
                    | _ ->
                        priority
                ) 0
            )
            |> List.head

        customAction
        |> _.Actions
        |> List.map (function
            | Move _ as action ->
                Check.moveDefence action target
            | StanceChange _ as action ->
                Check.stance action
            | action ->
                Check.unstoppable action
        )

    // plan defender actions
    static member tryMakeChecksDefenceCustom (attacker: Entity) attackerAction (defence : Plan) (area : Entity) world =

        let defender = defence.Entity

        let character = defender.GetCharacter world

        if Character.canAct character then

            let target = attacker

            let defenderName = defender.Name
            let targetName = target.Name

            let area = area.GetArea world
            match Area.findPath defenderName targetName area with
            | _::_ as path ->

                let statCombatant = Character.getStat Gall character
                let reach = Character.getReach character
                let speed = Character.getSpeed character

                let distance = List.last path |> snd

                // in reach
                if (reach >= distance) then
                    // custom actions
                    if Character.hasCustomActions character then
                        Plan.defenceCustom character (int statCombatant) target
                    // no custom actions
                    else
                        let attackMoves = Plan.analyzeAttackerMoves attackerAction
                        Plan.defenceDefault character attackMoves (int statCombatant - attackMoves) target
                // not in reach
                // TODO: assumes enemy coordinate is static which is false
                else
                    Plan.defenceDefault character 0 0 target

                |> fun checks -> Some { defence with Checks = checks }
            | _ ->
                None
        else
            None

    static member plan combatants area (plan : Plan) (world : World)  =
        let plan = {
            plan with
                PossibleTargets = Plan.getPossibleTargets plan.Entity combatants
                PlannedTarget = Some (Plan.findTarget plan.Entity combatants world)
        }

        let plan = {
            plan with
                PossibleActions = Plan.getPossibleActions area plan world
        }

        let character = plan.Entity.GetCharacter world
        let plannedActions = plan.PlannedActions

        let plan = {
            plan with
                DistanceCurrentReach = Character.getCoveredDistance plannedActions character
                DistanceToTarget = Plan.getDistanceBetweenAttackerAndTarget area plan world
        }
        plan

    static member update combatants area (plan : Plan) (world : World) =
        let plan = {
            plan with
                PossibleTargets = Plan.getPossibleTargets plan.Entity combatants
        }

        let plan = {
            plan with
                PossibleActions = Plan.getPossibleActions area plan world
        }

        let character = plan.Entity.GetCharacter world
        let plannedActions = plan.PlannedActions

        let plan = {
            plan with
                DistanceCurrentReach = Character.getCoveredDistance plannedActions character
                DistanceToTarget = Plan.getDistanceBetweenAttackerAndTarget area plan world
        }
        plan

    static member tryFinalizeAttack (area : Entity) (plan : Plan) world =

        let actor = plan.Entity
        let character = actor.GetCharacter world

        let (Some target) = plan.PlannedTarget

        let attackerName = actor.Name
        let targetName = target.Name

        let areaModel = area.GetArea world

        if not (Character.canAct character) ||
            List.isEmpty (Area.findPath attackerName targetName areaModel) then
            None

        elif Plan.isCustom plan then
            Plan.tryMakeChecksAttackCustom plan

        else
            Plan.tryMakeChecksAttackDefault plan area world

    static member tryFinalizeDefence (area : Entity) (attack : Plan) (defence : Plan) world =

        let actor = defence.Entity
        let character = actor.GetCharacter world

        let (Some target) = defence.PlannedTarget

        let attackerName = actor.Name
        let targetName = target.Name

        let areaModel = area.GetArea world

        let action =
            attack.Checks
            |> List.tryFind (fun check -> not (List.isEmpty check.OpposedBy))
            |> _.Value

        if not (Character.canAct character) ||
            List.isEmpty (Area.findPath attackerName targetName areaModel) then
            None

        elif Plan.isCustom defence then
            Plan.tryMakeChecksAttackCustom defence

        else
            Plan.tryMakeChecksDefenceCustom attack.Entity action defence area world

type CheckState =
    | NotTested
    | Passed
    | Failed

type Turn = {
    Turn : Int32
    Entity : Entity
    OrderedChecks : List<Int32 * Check * CheckState>
}
with
    static member empty = {
        Turn = 0
        Entity = Entity
        OrderedChecks = []
    }

    static member processCosts i turn world =
        let checks, world =
            turn.OrderedChecks
            |> List.foldMap (fun (index, check, state) (world : World) ->
                if i = index then
                    let result, world = Check.applyCosts turn.Entity check world
                    let state = if result then Passed else Failed
                    (index, check, state), world
                else
                    (index, check, state), world
            ) world
        let turn = { turn with OrderedChecks = checks }
        turn, world

    static member processEffects i turn area world =
        let checks =
            turn.OrderedChecks
            |> List.choose (fun (index, check, state) -> if i = index && state = Passed then Some check else None)

        checks
        |> List.indexed
        |> List.collect (fun (i, check) ->
            let before, after = List.splitAt i checks
            Check.processCheck check before (List.tail after) turn.Entity area world
        )

    static member getSubTurns turn =
        turn.OrderedChecks
        |> List.map (fun (i, _, _) -> i)
        |> List.max

    static member describe (turn : Turn) =
        turn.OrderedChecks
        |> List.map (fun (i, check, state) ->
            Action.describe check.Action, (state = Passed)
        )

    static member makeFromPlan (plan : Plan) =
        let orderedChecks =
            plan.Checks
            |> List.foldMap (fun value (prevOpposed, index) ->
                let opposed = Check.isOpposed value
                let index =
                    if opposed = prevOpposed then index else index + 1
                (index, value, NotTested), (opposed, index)
            ) (false, 1)
            |> fst

        {
            Turn.empty with
                Turn = plan.Turn
                Entity = plan.Entity
                OrderedChecks = orderedChecks
        }