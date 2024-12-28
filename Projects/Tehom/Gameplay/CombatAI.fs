namespace Tehom

open Prime
open Nu
open Move
open Action
open Character
open Area

module AttackerAI =

    // pick target with the most wounds, excluding combatant himself
    let findTarget attacker gameplay world =
        // TODO: some sort of allegiance system
        let otherCombatants =
            gameplay.Combatants
            |> List.remove (fun entity -> entity = attacker)
        Combat.getCharacterMostWounds otherCombatants world

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
            Check.attack (PhysicalSequence x) (List.length x) target
        ]

    // attack in the case character is programmed
    let attackCustom character threshold target =
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
        |> _.Actions
        |> List.map (function
            | PhysicalSequence seq ->
                let action =
                    seq
                    |> List.truncate threshold
                    |> PhysicalSequence
                Check.attack action threshold target
            | StanceChange _ as action ->
                Check.unstoppable action
            | _ ->
                Check.empty
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
                        Type = TurnType.Action
                        Checks = checks
                }

            | _ ->
                None
        else
            None


module DefenderAI =

    let analyzeAttackerMoves attackerAction =
        match attackerAction.Action with
        | PhysicalSequence moves ->
            moves
            |> List.filter (fun move -> List.contains move Move.attacks)
            |> List.length
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
            Check.defence (PhysicalSequence x) (List.length x) target
        ]

    // defence in the case character is programmed
    let defenceCustom character threshold target =
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
        |> _.Actions
        |> List.map (function
            | PhysicalSequence seq ->
                let action =
                    seq
                    |> List.truncate threshold
                    |> PhysicalSequence
                Check.defence action threshold target
            | StanceChange _ as action ->
                Check.unstoppable action
            | _ ->
                Check.empty
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
                    []
                |> fun checks -> Some {
                    Turn.empty with
                        Turn = gameplay.Turn
                        Type = TurnType.Reaction
                        Checks = checks
                }
            | _ ->
                None
        else
            None