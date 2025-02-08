namespace Tehom

open System
open System.Numerics
open Prime
open Nu

type Plan = {
    Turn : Int32

    Entity : Entity

    Combatants : List<Entity>

    Area : Entity

    PossibleActions : List<Tehom.Action * Boolean>
    PossibleTargets : List<Entity * Boolean>
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

        Entity = Entity
        Combatants = []
        Area = Entity

        PossibleActions = []
        PossibleTargets = []
        PlannedTarget = None
        PlannedFracture = Stamina.empty
        PlannedStance = Stance.empty

        DistanceCurrentReach = 0u
        DistanceToTarget = 0u

        Checks = List.empty
    }

    static member make turnID entity combatants area = {
        Plan.empty with
            Turn = turnID
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

    static member addPlannedAction action (plan : Plan) =
        let (Some target) = plan.PlannedTarget
        let check = Check.moveAttack action target
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

    static member getPossibleTargets attacker combatants world =
        // TODO: some sort of allegiance system
        combatants
        |> List.remove (fun entity -> entity = attacker)
        |> List.sortBy (fun (entity: Entity) ->
            let character = entity.GetCharacter world
            character.Wounds
        )
        |> List.rev
        |> List.map (fun entity -> entity, true)

    // pick target with the most wounds, excluding combatant himself
    static member findTarget combatants world =
        let targets =
            combatants
            |> List.filter snd
            |> List.map fst
        Plan.getCharacterMostWounds targets world

    static member getDistanceBetweenAttackerAndTarget (plan : Plan) world =

        let (Some target) = plan.PlannedTarget

        let attackerName = plan.Entity.Name
        let targetName = target.Name

        let area = plan.Area.GetArea world
        match Area.findPath attackerName targetName area with
        | _::_ as path ->

            let distance = List.last path |> snd

            distance
        | _ ->
            0u

    static member getPossibleActions (plan : Plan) world =

        let character = plan.Entity.GetCharacter world
        let statCombatant = Character.getStat Gall character
        let plannedActions = plan.Checks |> List.map _.Action

        let isAttackExecutable attack (target : Entity) =

            let attackerName = plan.Entity.Name
            let targetName = target.Name

            let area = plan.Area.GetArea world
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
                    match plan.PlannedTarget with
                    | Some target ->
                        moveWithinActionLimit x
                        && isAttackExecutable x target
                    | None ->
                        false

            Move x, executable
        )

    static member tryMakeChecksAttackCustom (plan : Plan) =

        if List.notEmpty plan.Checks then

            let stance =
                [ Check.unstoppable (StanceChange Stats.attacker) ]

            let roll =
                [ Check.unstoppable RollStance ]

            let fracture =
                plan.PlannedFracture

            let karmaBet =
                [ Check.unstoppable (FractureBet fracture) ]

            let checks =
                plan.Checks

            Some { plan with Checks = stance @ roll @ karmaBet @ checks }

        else
            None

    static member tryMakeChecksDefenceCustom (plan : Plan) =

        if List.notEmpty plan.Checks then

            let stance =
                [ Check.unstoppable (StanceChange Stats.attacker) ]

            let roll =
                [ Check.unstoppable RollStance ]

            let fracture =
                plan.PlannedFracture

            let karmaBet =
                [ Check.unstoppable (FractureBet fracture) ]

            let checks =
                plan.Checks

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
    static member tryMakeChecksDefenceDefault (attacker: Entity) attackerAction (defence : Plan) (area : Entity) world =

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

    static member updatePossibleTargets plan world =
        let plan = {
            plan with
                PossibleTargets = Plan.getPossibleTargets plan.Entity plan.Combatants world
        }
        plan

    static member setTargetDefault plan =
        let plan = {
            plan with
                PlannedTarget = Some (plan.PossibleTargets |> List.head |> fst)
        }
        plan

    static member updatePossibleActions plan world =
        let plan = {
            plan with
                PossibleActions = Plan.getPossibleActions plan world
        }
        plan

    static member updateDistances plan world =
        let character = plan.Entity.GetCharacter world
        let plannedActions = plan.Checks |> List.map _.Action
        let plan = {
            plan with
                DistanceCurrentReach = Character.getCoveredDistance plannedActions character
                DistanceToTarget = Plan.getDistanceBetweenAttackerAndTarget plan world
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
            Plan.tryMakeChecksDefenceCustom defence

        else
            Plan.tryMakeChecksDefenceDefault attack.Entity action defence area world

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