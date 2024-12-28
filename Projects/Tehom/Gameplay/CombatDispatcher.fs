namespace Tehom

open System
open System.Numerics
open Prime
open Nu

open Action

type CombatMessage =
    | StartPlaying
    | Update
    | TurnBegin
    | TurnAttack of Attacker : Entity
    | TurnDefence of Attacker : Entity * Attack : Turn
    | TurnEnd of Attacker : Entity * Attack : Turn * Defender : Entity * Defence : Turn
    | ProcessCharacterTurn of Entity * Turn
    | TimeUpdate
    interface Message

type CombatCommand =
    | RollInitiative
    | GameEffect of GameEffect
    interface Command

[<AutoOpen>]
module CombatExtensions =
    type Entity with
        member this.GetCombat world = this.GetModelGeneric<Combat> world
        member this.SetCombat value world = this.SetModelGeneric<Combat> value world
        member this.Combat = this.ModelGeneric<Combat> ()

type CombatDispatcher () =
    inherit Entity2dDispatcher<Combat, CombatMessage,CombatCommand> (false, false, false, Combat.initial)

    override this.Definitions (_, _) = [
        Entity.Size == Constants.Render.VirtualResolution.V3
        Screen.DeselectingEvent => FinishQuitting
        Screen.UpdateEvent => Update
        Screen.TimeUpdateEvent => TimeUpdate
    ]

    override this.Message (model, message, entity, world) =

        match message with
        | StartPlaying ->
            let gameplay =
                Combat.initial

            let combatants =
                world
                |> World.getEntities Simulants.GameplayCharacters
                |> List.ofSeq

            let history =
                combatants
                |> List.map (fun c -> c, [])
                |> Map.ofSeq

            let gameplay = { gameplay with Combatants = combatants; History = history }

            withSignals [
                RollInitiative
            ] gameplay

        | Update ->
            if model.Combatants = Combat.initial.Combatants then
                [StartPlaying], model
            else
                let model = Combat.update model world
                just model

        | TurnBegin ->
            match model.Combatants with
            | attacker::combatants ->

                let model = {
                    model with
                        Combatants = combatants @ [attacker]
                        Turn = model.Turn + 1
                }

                let signals : Signal list = [
                    GameEffect (CharacterReset attacker)
                    TurnAttack attacker
                ]

                withSignals signals model

            | _ ->
                just model

        | TurnAttack attacker ->
            match AttackerAI.tryPlan attacker model.Area model world with
            | Some attackerAction ->
                let signal : Signal =
                    TurnDefence (attacker, attackerAction)

                withSignal signal model
            | None ->
                just model

        // TODO: assumed one target
        | TurnDefence (attacker, attackerAction) ->
            attackerAction.Checks
            |> List.tryFind (fun check -> not (List.isEmpty check.OpposedBy))
            |> function
                | Some { OpposedBy = [defender] } as Some action ->
                    match DefenderAI.tryPlan attacker action defender model.Area model world with
                    | Some defenderAction ->
                        let signal : Signal =
                            TurnEnd (attacker, attackerAction, defender, defenderAction)

                        withSignal signal model
                    | None ->
                        just model
                | _ ->
                    just model

        | TurnEnd (attacker, attackerAction, defender, defenderAction) ->
            let attackerTurn =
                let character = attacker.GetCharacter world
                Turn.applyStance character attackerAction

            let defenderTurn =
                let character = defender.GetCharacter world
                Turn.applyStance character defenderAction

            // TODO: add karma betting

            let attackerTurn, defenderTurn = Turn.opposedTurns attackerTurn defenderTurn

            let history =
                model.History
                |> Map.map (fun entity turns ->
                    if entity = attacker then
                        attackerTurn::turns
                    elif entity = defender then
                        defenderTurn::turns
                    else
                        turns
                )

            let model = { model with History = history }

            let signals : Signal list = [
                ProcessCharacterTurn (attacker, attackerTurn)
                ProcessCharacterTurn (defender, defenderTurn)
            ]

            withSignals signals model

        | ProcessCharacterTurn (actor, turn) ->
            let signals : Signal list =
                Turn.processTurn actor turn model.Area world
                |> List.map (fun signal -> GameEffect signal)

            withSignals signals model

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { model with GameplayTime = model.GameplayTime + gameDelta.Updates }
            just gameplay

    override this.Command (model, command, entity, world) =

        match command with

        | RollInitiative ->

            let combatants, world =
                model.Combatants
                |> List.foldMap (fun (entity : Entity) (world : World)  ->
                    let character = entity.GetCharacter world

                    let character = Character.turnReset character

                    let maxInitiative = Character.getMaxInitiative character
                    let initiative = Random.rollInitiative maxInitiative
                    let character = Character.setInitiative initiative character

                    let world = entity.SetCharacter character world

                    (entity, initiative), world
                ) world

            let combatants =
                combatants
                |> List.sortBy snd
                |> List.rev
                |> List.map fst

            let model = { model with Combatants = combatants }

            let world = entity.SetCombat model world

            just world

        | GameEffect (CharacterReset entity) ->
            let character = entity.GetCharacter world
            let character = Character.turnReset character
            let world = entity.SetCharacter character world
            just world

        | GameEffect (CharacterStanceChange (entity, stance)) ->
            let character = entity.GetCharacter world
            let character = Character.stanceChange stance character
            let world = entity.SetCharacter character world
            just world

        | GameEffect (Damage (entity, size, damage)) ->
            let character = entity.GetCharacter world
            let sizeDifference = Character.getSize character - size
            let character = Character.doDamage sizeDifference damage character
            let world = entity.SetCharacter character world
            just world

        | GameEffect (TravelInter (character, location)) ->
            let area = model.Area
            let area = Area.moveSite character location area
            let model = { model with Area = area }
            let world = entity.SetCombat model world
            just world

        | GameEffect (TravelIntra (character, location, distance)) ->
            let area = model.Area
            let area = Area.establishDistance distance character location area
            let model = { model with Area = area }
            let world = entity.SetCombat model world
            just world


    override this.Content (model, _) = [

        Content.button Simulants.GameplayGuiAdvanceTurn.Name [
            Entity.PositionLocal == v3 0.0f 150.0f 0.0f
            Entity.Size == v3 80.0f 20.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
            Entity.Text == "Advance Turn"
            Entity.Font == Assets.Gui.ClearSansFont
            Entity.FontSizing == Some 10
            Entity.ClickEvent => TurnBegin
        ]

        let player = model.DisplayLeft
        let enemy = model.DisplayRight

        let playerHistory =
            model.History
            |> Map.toList
            |> List.tryFind (fun (entity, _) -> player.ID = entity.Name)

        let enemyHistory =
            model.History
            |> Map.toList
            |> List.tryFind (fun (entity, _) -> enemy.ID = entity.Name)

        let statsBox character = [
            let (gall, lymph, oil, plasma) = Character.getStats character
            let (gallStance, lymphStance, oilStance, plasmaStance) =
                character
                |> Character.getStance
                |> Stance.getStats
            let minorWounds = character.MinorWounds
            let majorWounds = character.MajorWounds

            let stat name text = Content.text name [
                Entity.Size == v3 80.0f 10.0f 0.0f
                Entity.Text := text
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 10
                Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
            ]

            stat "Minor Wounds" $"Wounds {minorWounds}"
            stat "Major Wounds" $"{majorWounds}"
            stat "Gall" $"{gall} {gallStance}"
            stat "Lymph" $"{lymph} {lymphStance}"
            stat "Oil" $"{oil} {oilStance}"
            stat "Plasma" $"{plasma} {plasmaStance}"
            stat "Stances" $"Stances {character.StancesLeft}"
            stat "Initiative" $"Initiative {character.Initiative}"
        ]

        Content.association "StatsBoxPlayer" [
            Entity.PositionLocal == v3 -160.0f 0.0f 0.0f
            Entity.Size == v3 80.0f 80.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
            Entity.Layout == Flow (FlowDownward, FlowUnlimited)
        ] (statsBox player)

        Content.association "StatsBoxEnemy" [
            Entity.PositionLocal == v3 200.0f 0.0f 0.0f
            Entity.Size == v3 80.0f 80.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
            Entity.Layout == Flow (FlowDownward, FlowUnlimited)
        ] (statsBox enemy)


        match playerHistory, enemyHistory with
        | Some (_, playerLastTurn::_), Some (_, enemyLastTurn::_) ->

            let playerMoves =
                playerLastTurn.Checks
                |> List.tryFind (fun x -> match x.Action with | PhysicalSequence _ -> true | _ -> false)
                |> function
                    | Some { Action = PhysicalSequence moves } -> moves
                    | _ -> []

            let enemyMoves =
                enemyLastTurn.Checks
                |> List.tryFind (fun x -> match x.Action with | PhysicalSequence _ -> true | _ -> false)
                |> function
                    | Some { Action = PhysicalSequence moves } -> moves
                    | _ -> []

            let playerSuccesses =
                playerLastTurn.Checks
                |> List.tryFind (fun x -> match x.Action with | PhysicalSequence _ -> true | _ -> false)
                |> fun check ->
                    match check with
                    | Some check -> check.Successes
                    | None -> 0

            let enemySuccesses =
                enemyLastTurn.Checks
                |> List.tryFind (fun x -> match x.Action with | PhysicalSequence _ -> true | _ -> false)
                |> fun check ->
                    match check with
                    | Some check -> check.Successes
                    | None -> 0


            let turnWinner =
                if playerSuccesses = enemySuccesses then
                    "Draw!"
                else if playerSuccesses > enemySuccesses then
                    $"{player.Name} advances!"
                else
                    $"{enemy.Name} advances!"

            let combatWinner =
                if Character.isDead player && Character.isDead enemy then
                    "Everyone died!"
                else if Character.isDead player then
                    $"{enemy.Name} won!"
                else if Character.isDead enemy then
                    $"{player.Name} won!"
                else
                    ""


            richText "CombatSummary" [
                Entity.PositionLocal == v3 0.0f 80.0f 0.0f
                Entity.Size == v3 240.0f 40.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text := $" {playerSuccesses} - {enemySuccesses} - {turnWinner}

                Turn: {model.Turn} {combatWinner}"
                Entity.TextColor == Color.FloralWhite
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 10
            ]

            Content.association "MovesPlayer" [
                Entity.PositionLocal == v3 -80.0f 0.0f 0.0f
                Entity.Size == v3 80.0f 80.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Layout == Flow (FlowDownward, FlowUnlimited)
            ] [
                for i, move in List.indexed playerMoves ->
                    Content.text $"Move{i}" [
                        Entity.Size == v3 80.0f 10.0f 0.0f
                        Entity.Text := $"{Move.getName move}"
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
                Entity.PositionLocal == v3 80.0f 0.0f 0.0f
                Entity.Size == v3 80.0f 80.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Layout == Flow (FlowDownward, FlowUnlimited)
            ] [
                for i, move in List.indexed enemyMoves ->
                    Content.text $"Move{i}" [
                        Entity.Size == v3 80.0f 10.0f 0.0f
                        Entity.Text := $"{Move.getName move}"
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
                Entity.PositionLocal == v3 0.0f -160.0f 0.0f
                Entity.Size == v3 360.0f 40.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text := $"{Area.getConnections player.ID model.Area} -- {Area.getConnections enemy.ID model.Area}"
                Entity.TextColor == Color.FloralWhite
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 10
            ]

        | _ -> ()

    ]