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
            Check.attack (PhysicalSequence x) (moves + attacks) target
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
            | PhysicalSequence _ as action ->
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

    // response, prototype of defender AI
    let plan (attacker: Entity) attackerAction (defender: Entity) gameplay world =

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
                    Action = StanceChange Stance.defender
            }]

            let physicalAction =
                match Area.findPath combatantName targetName area with
                // no path
                | [] ->
                    []
                | path ->
                    let character = defender.GetCharacter world
                    let reach = Character.getReach character
                    let statDefender = Character.getStat Lymph character |> int

                    let distance = List.last path |> snd
                    if (reach >= distance) then
                        // in reach

                        let combatantBlockableMoves =
                            List.fold (fun number move ->
                                if List.contains move Move.attacks then number + 1u else number
                            ) 0u movesCombatant
                            |> int

                        Random.getItemsFromList (min statDefender combatantBlockableMoves) Move.defence
                        @
                        if (statDefender > combatantBlockableMoves) then
                            [ Ready ]
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