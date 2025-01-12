namespace Tehom

open Prime
open Nu
open Move
open Action
open Character
open Area

module AttackerAI =

    let getPossibleMoves character =

        let strikes =
            character
            |> Character.getWeapons
            |> List.map Strike

        strikes @ [ Power; Press ] @ [ Stride ]

    let getDistanceBetweenAttackerAndTarget (attacker : Entity) combat world =
        let area = combat.Area

        let (Some target) = combat.PlannedTarget

        let attackerName = attacker.Name
        let targetName = target.Name

        match Area.findPath attackerName targetName area with
        | _::_ as path ->

            let distance = List.last path |> snd

            distance
        | _ ->
            0u

    let getCoveredDistance actions character =
        let reach = Character.getReach character
        let speed = Character.getSpeed character

        reach +
        (actions
        |> List.fold (fun distance action ->
            match action with
            | Move Stride ->
                distance + speed
            | _ ->
                distance
        ) 0u)

    let getCoveredDistance2 (attacker : Entity) combat world =
        let character = attacker.GetCharacter world
        let plannedActions = combat.PlannedActions
        getCoveredDistance plannedActions character

    let getPossibleActions (attacker : Entity) combat world =

        let character = attacker.GetCharacter world
        let statCombatant = Character.getStat Gall character
        let plannedActions = combat.PlannedActions

        let isAttackExecutable attack =

            let area = combat.Area

            let (Some target) = combat.PlannedTarget

            let attackerName = attacker.Name
            let targetName = target.Name

            match Area.findPath attackerName targetName area with
            | _::_ as path ->

                let distance = List.last path |> snd
                // in reach
                if (getCoveredDistance plannedActions character >= distance) then
                    true
                else
                    false
            | _ ->
                false

        let moveWithinActionLimit move =
            int statCombatant > (List.length plannedActions)

        getPossibleMoves character
        |> List.map (fun x ->
            let executable =
                if List.contains x Move.positioning then
                    moveWithinActionLimit x
                else
                    moveWithinActionLimit x
                    && isAttackExecutable x

            Move x, executable
        )

    let getMovesMatrix (attacker : Entity) world =

        let character = attacker.GetCharacter world

        let statCombatant = Character.getStat Gall character |> int

        List.init statCombatant (fun _ -> getPossibleMoves character)

    let getPossibleTargets attacker combat world =
        // TODO: some sort of allegiance system
        combat.Combatants
        |> List.remove (fun entity -> entity = attacker)
        |> List.map (fun entity -> entity, true)

    // pick target with the most wounds, excluding combatant himself
    let findTarget attacker combat world =
        let targets =
            getPossibleTargets attacker combat world
            |> List.filter snd
            |> List.map fst
        Combat.getCharacterMostWounds targets world

    let customPlan (attacker: Entity) area combat world =

        if List.notEmpty combat.PlannedActions then
            // TODO: move to planning
            let (Some target) =
                combat.PlannedTarget
            let stance =
                [ Check.unstoppable (StanceChange Stance.attacker) ]
            let roll =
                [ Check.unstoppable RollStance ]

            let checks =
                combat.PlannedActions
                // threshold should be generated for the action
                |> List.map (fun action -> Check.moveAttack action target)

            Some {
                Turn.empty with
                    Turn = combat.Turn
                    Checks = stance @ roll @ checks
                    Entity = attacker
            }
        else
            None

    // attack in the case character has no programming
    let attackDefault character moves attacks target =
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
    let attackCustom character threshold target =
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
    let tryPlan (attacker: Entity) area gameplay world =

        let character = attacker.GetCharacter world

        if Character.canAct character then

            let target = findTarget attacker gameplay world

            let attackerName = attacker.Name
            let targetName = target.Name

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
                        attackCustom character (int statCombatant) target
                    // no custom actions
                    else
                        attackDefault character 0 (int statCombatant) target
                // not in reach
                else
                    // can run to
                    if speed * statCombatant >= distance then
                        let movementMoves = round (float distance / float speed + 0.5) |> int
                        attackDefault character movementMoves (int statCombatant - movementMoves) target
                    // can't run to
                    else
                        attackDefault character (int statCombatant) 0 target

                |> fun checks -> Some {
                    Turn.empty with
                        Turn = gameplay.Turn
                        Checks = checks
                        Entity = attacker
                }

            | _ ->
                None
        else
            None


module DefenderAI =

    let analyzeAttackerMoves attackerAction =
        match attackerAction.Action with
        | Move move ->
            if List.contains move Move.attacks then 1 else 0
        | _ ->
            0

    // defence in the case character has no programming
    let defenceDefault character blocks attacks target =
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
    let defenceCustom character threshold target =
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
    let tryPlan (attacker: Entity) attackerAction (defender: Entity) area gameplay world =

        let character = defender.GetCharacter world

        if Character.canAct character then

            let target = attacker

            let defenderName = defender.Name
            let targetName = target.Name

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
                        defenceCustom character (int statCombatant) target
                    // no custom actions
                    else
                        let attackMoves = analyzeAttackerMoves attackerAction
                        defenceDefault character attackMoves (int statCombatant - attackMoves) target
                // not in reach
                // TODO: assumes enemy coordinate is static which is false
                else
                    defenceDefault character 0 0 target

                |> fun checks -> Some {
                    Turn.empty with
                        Turn = gameplay.Turn
                        Checks = checks
                        Entity = defender
                }
            | _ ->
                None
        else
            None