namespace Tehom

open System
open System.Numerics
open Prime
open Nu

type Path = Graph.Query.LPath<String, UInt32>

type Plan = {
    Turn : Int32
    Countering : Option<Plan>

    Entity : Entity

    Combatants : List<Entity>

    Area : Entity

    PossibleChecks : List<Check * Boolean>
    PossibleTargets : List<Entity * Path * Boolean>

    PlannedTarget : Option<Entity>
    PlannedFracture : Stamina
    PlannedStance : Stats

    DistanceCurrentReach : UInt32
    DistanceToTarget : UInt32

    Checks : List<Check>
}
with
    static member empty = {
        Turn = 0
        Countering = None

        Entity = Entity
        Combatants = []
        Area = Entity

        PossibleChecks = []
        PossibleTargets = []

        PlannedTarget = None
        PlannedFracture = Stamina.empty
        PlannedStance = Stance.empty

        DistanceCurrentReach = 0u
        DistanceToTarget = 0u

        Checks = List.empty
    }

    static member make turnID defence entity combatants area = {
        Plan.empty with
            Turn = turnID
            Countering = defence
            Entity = entity
            Combatants = combatants
            Area = area
    }

    static member updateBases entity combatants area plan = {
        plan with
            Entity = entity
            Combatants = combatants
            Area = area
    }

    static member describe (plan : Plan) =
        plan.Checks
        |> List.map (fun check ->
            Action.describe check.Action
        )

    static member isCustom (plan : Plan) =
        let player = Simulants.GameplayCharacters / CharacterContent.player.ID
        plan.Entity = player


    static member setPlannedTarget target (plan : Plan) =
        let plan = { plan with PlannedTarget = Some target }
        plan

    static member addPlannedAction check (plan : Plan) =
        let plan = { plan with Checks = plan.Checks @ [ check ] }
        plan

    static member removePlannedAction i (plan : Plan) =
        let plan = { plan with Checks = List.removeAt i plan.Checks }
        plan

    static member setFracture change (plan : Plan) =
        let plan = { plan with PlannedFracture = change }
        plan

    static member setStance change (plan : Plan) =
        let plan = { plan with PlannedStance = change }
        plan

    static member getCharacterMostWounds entities world =
        entities
        |> List.sortBy (fun (entity: Entity) ->
            let character = entity.GetCharacter world
            character.Wounds
        )
        |> List.last

    static member tryMakeChecksPlanned (plan : Plan) world =

        let character =
            plan.Entity.GetCharacter world

        if List.notEmpty plan.Checks then

            let checks = [
                if Character.verifyStance plan.PlannedStance character then
                    Check.unstoppable (StanceChange plan.PlannedStance)

                Check.unstoppable RollStance

                if Character.verifyFracture plan.PlannedFracture character then
                    Check.unstoppable (Fracture plan.PlannedFracture)

                for check in plan.Checks do check
            ]

            Some { plan with Checks = checks }

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
            Check.unstoppable (StanceChange Stats.attacker)
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
                    | StanceChange { Gall = gall; Lymph = lymph } ->
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
                        Plan.attackCustom character statCombatant target
                    // no custom actions
                    else
                        Plan.attackDefault character 0 statCombatant target
                // not in reach
                else
                    // can run to
                    if speed * (uint32 statCombatant) >= distance then
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

    static member countAttackerMoves (attack : Plan) =
        attack.Checks
        |> List.fold (fun state check ->
            match check.Action with
            | Move move when Move.isAttack move ->
                state + if List.notEmpty check.OpposedBy then 1 else 0
            | _ ->
                state
        ) 0

    static member countDefenderMoves (defence : Plan) =
        defence.Checks
        |> List.fold (fun state check ->
            match check.Action with
            | Move move when Move.isDefence move ->
                state + if List.notEmpty check.OpposedBy then 1 else 0
            | _ ->
                state
        ) 0

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
            Check.unstoppable (StanceChange Stats.attacker)
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
                    | StanceChange { Lymph = lymph } ->
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
    static member tryMakeChecksDefenceDefault (attack : Plan) (defence : Plan) (area : Entity) world =

        let defender = defence.Entity

        let character = defender.GetCharacter world

        if Character.canAct character then

            let target = attack.Entity

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
                        let attackMoves = Plan.countAttackerMoves attack
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

    static member canSeeTarget entity target world =
        true

    static member canAttackTarget entity target path world =
        match path with
        | _::_ ->
            true
        | _ ->
            false

    static member getPossibleTargets plan =
        plan.PossibleTargets

    static member updatePossibleTargets plan world =
        let targets =
            plan.Combatants

        // TODO: some sort of allegiance system
        // exclude combatant himself
        let targets =
            targets
            |> List.remove (fun entity -> entity = plan.Entity)

        // sort targets most wounds to least wounds
        let targets =
            targets
            |> List.sortBy (fun (entity : Entity) ->
                let character = entity.GetCharacter world
                character.Wounds
            )
            |> List.rev

        let area = plan.Area.GetArea world

        let targets =
            targets
            |> List.map (fun entity ->
                let actor = plan.Entity.Name
                let target = entity.Name
                entity, Area.findPath actor target area
            )

        // map if they're valid targets
        let targets =
            targets
            |> List.filter (fun (target, _) -> Plan.canSeeTarget plan.Entity target world)
            |> List.map (fun (target, path) -> target, path, Plan.canAttackTarget plan.Entity target path world)

        let plan = { plan with PossibleTargets = targets }
        plan

    static member setTargetDefault plan =
        let target =
            plan.PossibleTargets
            |> List.head
            |> fun (target, _, _) -> target

        let plan = { plan with PlannedTarget = Some target }
        plan

    static member getDistanceToTarget plan =
        plan.PossibleTargets
        |> List.find (fun (entity, _, _) -> Some entity = plan.PlannedTarget)
        |> fun (_, path, _) ->
            path
            |> List.last
            |> snd

    static member updatePossibleActions plan world =

        let character = plan.Entity.GetCharacter world
        let plannedActions = plan.Checks |> List.map _.Action
        let coveredDistance = Character.getCoveredDistance plannedActions character
        let distanceToTarget = Plan.getDistanceToTarget plan

        let totalCost =
            plan.Checks
            |> List.fold (fun total check -> total + check.Cost) Cost.empty

        let isAttackExecutable check =
            // in reach
            coveredDistance >= distanceToTarget

        let moveWithinActionLimit check =
            Character.canTheoreticallyPay (totalCost + check.Cost) character

        let moves =
            Character.getPossibleMoves character

        let checks =
            moves
            |> List.map (fun move ->
                let action = Move move
                let (Some target) = plan.PlannedTarget
                if Option.isNone plan.Countering then
                    Check.moveAttack action target
                else
                    Check.moveDefence action target
            )

        let checks =
            checks
            |> List.remove (fun check ->
                match plan.Countering with
                | Some attack ->
                    false
                | None ->
                    match check.Action with
                    | Move move ->
                        Move.isDefenceNotPositioning move
                    | _ ->
                        false
            )

        let attackMoves =
            match plan.Countering with
            | Some attack ->
                Plan.countAttackerMoves attack
            | None ->
                0
        let defenceMoves =
            Plan.countDefenderMoves plan

        let checks =
            checks
            |> List.map (fun check ->
                match plan.Countering with
                | Some _ ->
                    match check.Action with
                    | Move move when Move.isAttack move ->
                        check, defenceMoves >= attackMoves && isAttackExecutable check && moveWithinActionLimit check
                    | _ ->
                        check, isAttackExecutable check && moveWithinActionLimit check
                | None ->
                    check, isAttackExecutable check && moveWithinActionLimit check
            )

        let plan = { plan with PossibleChecks = checks }
        plan

    static member updateDistances plan world =
        let character = plan.Entity.GetCharacter world
        let plannedActions = plan.Checks |> List.map _.Action

        let plan = {
            plan with
                DistanceCurrentReach = Character.getCoveredDistance plannedActions character
                DistanceToTarget = Plan.getDistanceToTarget plan
        }
        plan

    static member initialize (plan : Plan) (world : World)  =
        let plan = Plan.updatePossibleTargets plan world
        let plan = Plan.setTargetDefault plan
        let plan = Plan.updatePossibleActions plan world
        let plan = Plan.updateDistances plan world
        plan

    static member updateDerivatives (plan : Plan) (world : World) =
        let plan = Plan.updatePossibleTargets plan world
        let plan = Plan.updatePossibleActions plan world
        let plan = Plan.updateDistances plan world
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
            Plan.tryMakeChecksPlanned plan world

        else
            Plan.tryMakeChecksAttackDefault plan area world

    static member tryFinalizeDefence (area : Entity) (attack : Plan) (defence : Plan) world =

        let actor = defence.Entity
        let character = actor.GetCharacter world

        let (Some target) = defence.PlannedTarget

        let attackerName = actor.Name
        let targetName = target.Name

        let areaModel = area.GetArea world

        if not (Character.canAct character) ||
            List.isEmpty (Area.findPath attackerName targetName areaModel) then
            None

        elif Plan.isCustom defence then
            Plan.tryMakeChecksPlanned defence world

        else
            Plan.tryMakeChecksDefenceDefault attack defence area world

type Turn = {
    Turn : Int32
    Entity : Entity
    Checks : List<Check>
}
with
    static member empty = {
        Turn = 0
        Entity = Entity
        Checks = []
    }

    static member processCosts i turn world =
        let checks, world =
            turn.Checks
            |> List.foldMap (fun check (world : World) ->
                if i = check.Order then
                    let result, world = Check.applyCosts turn.Entity check world
                    Check.applyState result check, world
                else
                    check, world
            ) world
        let turn = { turn with Checks = checks }
        turn, world

    static member processEffects i turn area world =
        let checks =
            turn.Checks
            |> List.choose (fun check -> if i = check.Order && check.State = Passed then Some check else None)

        checks
        |> List.indexed
        |> List.collect (fun (i, check) ->
            let before, after = List.splitAt i checks
            Check.processCheck check before (List.tail after) turn.Entity area world
        )

    static member getSubTurns turn =
        turn.Checks
        |> List.map _.Order
        |> List.max

    static member describe (turn : Turn) =
        turn.Checks
        |> List.map (fun check ->
            Action.describe check.Action, (check.State = Passed)
        )

    static member makeFromPlan (plan : Plan) =
        let checks =
            plan.Checks
            |> List.foldMap (fun value (prevOpposed, index) ->
                let opposed = Check.isOpposed value
                let index = if opposed = prevOpposed then index else index + 1
                { value with Order = index }, (opposed, index)
            ) (false, 1)
            |> fst

        {
            Turn.empty with
                Turn = plan.Turn
                Entity = plan.Entity
                Checks = checks
        }