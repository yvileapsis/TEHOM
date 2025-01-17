namespace Tehom

open System
open System.Numerics
open Prime
open Nu

open Action

type CombatMessage =
    | Update
    | TurnProcess
    | UpdatePossibleActions
    | SetPlannedTarget of int
    | AddPlannedAction of int
    | RemovePlannedAction of int
    | TimeUpdate
    interface Message

type CombatCommand =
    | RollInitiative
    | GameEffect of GameEffect
    | ProcessCharacterTurn of Turn list
    interface Command

type CombatDispatcher () =
    inherit Entity2dDispatcher<Combat, CombatMessage, CombatCommand> (false, false, false, Combat.initial)

    override this.Definitions (_, _) = [
        Entity.Size == Constants.Render.VirtualResolution.V3 / 2f
        Screen.DeselectingEvent => FinishQuitting
        Screen.UpdateEvent => Update
        Screen.TimeUpdateEvent => TimeUpdate
        Entity.AlwaysUpdate == true
        Entity.RegisterEvent => RollInitiative
    ]

    override this.Message (model, message, entity, world) =

        match message with
        | Update ->
            if model.Combatants = Combat.initial.Combatants then
                [StartPlaying], model
            else
                let model = Combat.update model world
                just model

        | TurnProcess ->
            match model.CombatState with
            | TurnNone ->
                match model.Combatants with
                | attacker::_ ->
                    let signals : Signal list = [ GameEffect (CharacterDo (attacker, Character.turnReset)) ]
                    let model = { model with CombatState = TurnAttacker attacker }
                    withSignals signals model
                | _ ->
                    just model

            | TurnAttacker attacker ->
                let model =
                    if Combat.isCharacterControlled attacker then
                        let model = {
                            model with
                                PossibleTargets = AttackerAI.getPossibleTargets attacker model world
                                PlannedTarget = Some (AttackerAI.findTarget attacker model world)
                        }
                        let model = {
                            model with
                                PossibleActions = AttackerAI.getPossibleActions attacker model world
                                DistanceCurrentReach = AttackerAI.getCoveredDistance2 attacker model world
                                DistanceToTarget = AttackerAI.getDistanceBetweenAttackerAndTarget attacker model world
                        }
                        model
                    else
                        model
                let model = { model with CombatState = TurnAttackPlan attacker }
                just model

            | TurnAttackPlan attacker ->
                let plan =
                    if Combat.isCharacterControlled attacker then
                        AttackerAI.customPlan attacker model world
                    else
                        AttackerAI.tryPlan attacker model world

                let model = {
                    model with
                        PossibleTargets = []
                        PossibleActions = []
                        PlannedTarget = None
                        PlannedActions = []
                }

                match plan with
                | Some attackerAction ->
                    let model = { model with CombatState = TurnDefender attackerAction }
                    just model
                | None ->
                    let model = { model with CombatState = TurnAttacker attacker }
                    just model

            | TurnDefender attackPlan ->
                attackPlan.Checks
                |> List.tryFind (fun check -> not (List.isEmpty check.OpposedBy))
                |> function
                    | Some { OpposedBy = [defender] } ->
                        let model = { model with CombatState = TurnDefencePlan (attackPlan, defender) }
                        just model
                    | _ ->
                        just model

            | TurnDefencePlan (attackPlan, defender) ->
                let action =
                    attackPlan.Checks
                    |> List.tryFind (fun check -> not (List.isEmpty check.OpposedBy))
                    |> _.Value

                match DefenderAI.tryPlan attackPlan.Entity action defender model world with
                | Some defencePlan ->
                    let model = { model with CombatState = TurnAttackKarmaBid (attackPlan, defencePlan) }
                    just model
                | None ->
                    just model

            | TurnAttackKarmaBid (attackPlan, defencePlan) ->
                let attacker' = attackPlan.Entity.GetCharacter world
                let attackPlan = {
                    attackPlan with
                        Checks = (Check.unstoppable (KarmaBet attacker'.FractureCurrent))::attackPlan.Checks
                }
                let model = { model with CombatState = TurnDefenceKarmaBid (attackPlan, defencePlan) }
                just model

            | TurnDefenceKarmaBid (attackPlan, defencePlan) ->
                let defender' = defencePlan.Entity.GetCharacter world
                let defencePlan = {
                    defencePlan with
                        Checks = (Check.unstoppable (KarmaBet defender'.FractureCurrent))::defencePlan.Checks
                }
                let model = { model with CombatState = TurnExecute (attackPlan, defencePlan) }
                just model

            | TurnExecute (attackPlan, defencePlan)->

                let signals : Signal list = [
                    ProcessCharacterTurn [attackPlan; defencePlan]
                ]

                withSignals signals model

        | SetPlannedTarget i ->

            let target, possible = List.item i model.PossibleTargets

            if possible then

                let model = {
                    model with
                        PlannedTarget = Some target
                }

                just model
            else
                just model

        | UpdatePossibleActions ->

            let attacker =
                match model.CombatState with
                | TurnAttacker attacker -> attacker
                | TurnAttackPlan attacker -> attacker
                | _ -> failwith "todo"

            let model = {
                model with
                    PossibleTargets = AttackerAI.getPossibleTargets attacker model world
                    PossibleActions = AttackerAI.getPossibleActions attacker model world
                    DistanceCurrentReach = AttackerAI.getCoveredDistance2 attacker model world
                    DistanceToTarget = AttackerAI.getDistanceBetweenAttackerAndTarget attacker model world
            }
            just model

        | AddPlannedAction i ->
            let action, possible = List.item i model.PossibleActions

            if possible then
                let model = {
                    model with
                        PlannedActions = model.PlannedActions @ [ action ]
                }

                let signals : Signal list = [
                    UpdatePossibleActions
                ]

                withSignals signals model
            else
                just model

        | RemovePlannedAction i ->
            let model = {
                model with
                    PlannedActions = List.removeAt i model.PlannedActions
            }

            let signals : Signal list = [
                UpdatePossibleActions
            ]

            withSignals signals model

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { model with CombatTime = model.CombatTime + gameDelta.Updates }
            just gameplay

    override this.Command (model, command, entity, world) =

        match command with
        | RollInitiative ->

            let world =
                model.Combatants
                |> List.fold (fun (world : World) (entity : Entity) ->
                    entity.SetCharacterWith (Character.turnReset >> Character.rollInitiative) world
                ) world

            let combatants =
                model.Combatants
                |> List.sortBy (fun (entity : Entity) ->
                    let character = entity.GetCharacter world
                    character.Initiative
                )
                |> List.rev

            let model = { model with Combatants = combatants }

            let world = entity.SetCombat model world

            just world

        | GameEffect effect ->
            let world = entity.ExecuteGameEffect effect world
            just world

        | ProcessCharacterTurn list ->

            // horrifying code that needs to be optimized
            // takes apart turn, separates it into smaller subturns, orders those to specific execution-ready order
            // order is unopposed - opposed - unopposed
            // TODO: figure out better representation for this
            let split turn =
                turn.Checks
                |> List.fold (fun (prevOpposed, index, listOfLists) check ->
                    let opposed = Check.isOpposed check
                    match listOfLists with
                    | (_, turn)::tail when prevOpposed = opposed ->
                        opposed, index, (index, { turn with Checks = turn.Checks @ [ check ] })::tail
                    | listOfLists ->
                        opposed, index + 1, (index, { turn with Checks = [check] })::listOfLists
                ) (false, 0, [])
                |> fun (_, _, result) -> result
                |> Map.ofList

            let split turns =
                turns
                |> List.map split
                |> List.fold (fun bigmap map ->
                    map
                    |> Map.fold (fun bigmap key value ->
                        let value =
                            match Map.tryFind key bigmap with
                            | Some value' ->
                                (value::value')
                            | None ->
                                [value]
                        Map.add key value bigmap
                    ) bigmap
                ) Map.empty
                |> Map.toList

            let unsplit turns =
                turns
                |> List.concat
                |> List.fold (fun turns turn ->
                    match List.tryFindIndex (fun turn' -> turn'.Entity = turn.Entity) turns with
                    | Some index ->
                        let turn' = List.item index turns
                        let turn' = { turn' with Checks = turn'.Checks @ turn.Checks; Passed = turn'.Passed @ turn.Passed }
                        List.replace index turn' turns |> snd
                    | None ->
                        List.append [turn] turns
                ) List.empty
            // end of horrifying code

            let costs turns world =
                turns
                |> List.foldMap (fun turn (world : World) -> Turn.processCosts turn world) world

            let effects turns world =
                turns
                |> List.fold (fun world turn ->
                    let effects = Turn.processEffects turn model.Area world
                    List.fold (fun world effect -> entity.ExecuteGameEffect effect world ) world effects
                ) world

            let processing turns world =
                let turns, world = costs turns world
                let world = effects turns world
                turns, world

            let turns, world =
                list
                |> split
                |> List.map snd
                |> List.foldMap processing world
                |> fun (turns, world) ->
                    unsplit turns, world

            let model = entity.GetCombat world

            let model = Combat.advanceTurn turns model

            let model = { model with CombatState = TurnNone }

            let world = entity.SetCombat model world

            just world

    override this.TruncateModel model = {
        model with
            DisplayLeftModel = None
            DisplayRightModel = None
    }
    override this.UntruncateModel (model, model') = {
        model with
            DisplayLeftModel = model'.DisplayLeftModel
            DisplayRightModel = model'.DisplayRightModel
    }

    override this.Content (model, _) = [

        Content.staticSprite "Background" [
            Entity.Size == v3 480f 180f 0f
            Entity.StaticImage == Assets.Default.Black
            Entity.Color == Color.White.WithA 0.5f
        ]

        // Constant
        Content.button "AdvanceTurn" [
            Entity.Absolute == false
            Entity.PositionLocal == v3 0.0f 75.0f 0.0f
            Entity.Size == v3 40.0f 10.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
            Entity.Text == "Advance Turn"
            Entity.Font == Assets.Gui.ClearSansFont
            Entity.FontSizing == Some 5
            Entity.ClickEvent => TurnProcess
        ]

        let left = model.DisplayLeftModel
        let right = model.DisplayRightModel

        let findHistory entity =
            Map.tryFind entity model.History

        let leftHistory =
            Option.bind findHistory model.DisplayLeftEntity

        let rightHistory =
            Option.bind findHistory model.DisplayRightEntity

        let statsBox (character : Character) boxProperties textProperties =

            let stat name statName statValue = Content.composite name textProperties [
                Content.text "Name" (textProperties @ [
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                    Entity.Text := statName
                ])
                Content.text "Value" (textProperties @ [
                    Entity.Justification == Justified (JustifyRight, JustifyMiddle)
                    Entity.Text := statValue
                ])
            ]

            let location name text = ContentEx.richText name (textProperties @ [
                Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                Entity.Text := text
            ])

            let (gall, lymph, oil, plasma) =
                Character.getStats character

            let (gallStance, lymphStance, oilStance, plasmaStance) =
                character
                |> Character.getStance
                |> Stance.getStats

            let stats = [
                stat "MinorWounds" "Minor Wounds" $"{character.Injuries}"
                stat "MajorWounds" "Major Wounds" $"{character.Wounds}"
                stat "Gall" "Gall" $"{gall} {gallStance}"
                stat "Lymph" "Lymph" $"{lymph} {lymphStance}"
                stat "Oil" "Oil" $"{oil} {oilStance}"
                stat "Plasma" "Plasma" $"{plasma} {plasmaStance}"
                stat "Stances" "Stances" $"{character.StancesCurrent}"
                stat "Initiative" "Initiative" $"{character.Initiative}"
            ]

            Content.association "Stats" boxProperties stats

        // TODO: Display reach + stride distance, also display it on the map
        match model.CombatState with
        | TurnAttackPlan attacker when Combat.isCharacterControlled attacker ->
            let action (i, action, enabled) = Content.button $"Selectable{i}" [
                Entity.Absolute == false
                Entity.Size == v3 60.0f 5.0f 0.0f
                Entity.Text := $"{action}"
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 5
                Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                Entity.TextColor := if enabled then Color.FloralWhite else Color.Gray
                Entity.ClickEvent => AddPlannedAction i
            ]

            Content.association "PossibleActions" [
                Entity.Absolute == false
                Entity.PositionLocal == v3 -180.0f 0.0f 0.0f
                Entity.Size == v3 40.0f 40.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Layout == Flow (FlowDownward, FlowUnlimited)
            ] (
                model.PossibleActions
                |> List.indexed
                |> List.map (fun (i, (action, possible)) -> i, Action.describe action, possible)
                |> List.map action
            )

        | _ ->
            ()

        match model.CombatState with
        | TurnAttackPlan attacker when Combat.isCharacterControlled attacker ->
            let action (i, action, enabled) = Content.button $"Selectable{i}" [
                Entity.Absolute == false
                Entity.Size == v3 30.0f 5.0f 0.0f
                Entity.Text := $"{action}"
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 5
                Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                Entity.TextColor := if enabled then Color.FloralWhite else Color.Gray
                Entity.ClickEvent => SetPlannedTarget i
            ]

            Content.association "PossibleTargets" [
                Entity.Absolute == false
                Entity.PositionLocal == v3 -210.0f 0.0f 0.0f
                Entity.Size == v3 40.0f 40.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Layout == Flow (FlowDownward, FlowUnlimited)
            ] (
                model.PossibleTargets
                // TOOD: annoying, can't access world name, will need to store name
                |> List.indexed
                |> List.map (fun (i, (entity, possible)) -> i, $"{entity.Name}", possible)
                |> List.map action
            )

        | _ ->
            ()

        match model.PlannedTarget with
        | Some entity ->
            Content.button "PlannedTarget" [
                Entity.Absolute == false
                Entity.PositionLocal == v3 -40.0f 30.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Size == v3 40.0f 5.0f 0.0f
                Entity.Text := $"{entity.Name}"
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 5
                Entity.TextColor == Color.FloralWhite
            ]
        | None ->
            ()

        match model.CombatState with
        | TurnAttackPlan attacker when Combat.isCharacterControlled attacker ->
            Content.button "Distance" [
                Entity.Absolute == false
                Entity.PositionLocal == v3 -100.0f 30.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Size == v3 40.0f 5.0f 0.0f
                Entity.Text := $"{model.DistanceCurrentReach}"
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 5
                Entity.TextColor == Color.FloralWhite
            ]

            Content.button "Distance2" [
                Entity.Absolute == false
                Entity.PositionLocal == v3 40.0f 30.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Size == v3 40.0f 5.0f 0.0f
                Entity.Text := $"{model.DistanceToTarget}"
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 5
                Entity.TextColor == Color.FloralWhite
            ]
        | _ ->
            ()

        match left with
        | Some entity ->
            Content.composite "Left" [] [

                statsBox entity [
                    Entity.Absolute == false
                    Entity.Elevation == 10.0f
                    Entity.PositionLocal == v3 -110.0f 0.0f 0.0f
                    Entity.Size == v3 60.0f 40.0f 0.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                ] [
                    Entity.Absolute == false
                    Entity.Size == v3 60.0f 5.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.TextColor == Color.FloralWhite
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 5
                ]

                let actions =

                    match model.CombatState, leftHistory with
                    | TurnAttackPlan attacker, _ when Combat.isCharacterControlled attacker ->

                        let action (i, action) = Content.button $"ActionSelectable{i}" [
                            Entity.Absolute == false
                            Entity.Size == v3 60.0f 5.0f 0.0f
                            Entity.Text := $"{action}"
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing == Some 5
                            Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                            Entity.TextColor == Color.FloralWhite
                            Entity.ClickEvent => RemovePlannedAction i
                        ]

                        model.PlannedActions
                        |> List.map Action.describe
                        |> List.indexed
                        |> List.map action

                    | _, Some (lastTurn::_) ->

                        let action (i, (action, success)) = Content.text $"Action{i}" [
                            Entity.Absolute == false
                            Entity.Size == v3 60.0f 5.0f 0.0f
                            Entity.Text := $"{action}"
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing == Some 5
                            Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                            Entity.TextColor := if success then Color.FloralWhite else Color.Gray
                        ]

                        lastTurn
                        |> Turn.describe
                        |> List.indexed
                        |> List.map action

                    | _ -> []

                Content.association "Actions" [
                    Entity.Absolute == false
                    Entity.PositionLocal == v3 -40.0f 0.0f 0.0f
                    Entity.Size == v3 60.0f 40.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                ] actions
            ]
        | None -> ()

        match right with
        | Some entity ->
            Content.composite "Right" [] [

                statsBox entity [
                    Entity.Absolute == false
                    Entity.Elevation == 10.0f
                    Entity.PositionLocal == v3 110.0f 0.0f 0.0f
                    Entity.Size == v3 60.0f 40.0f 0.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                ] [
                    Entity.Absolute == false
                    Entity.Size == v3 60.0f 5.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.TextColor == Color.FloralWhite
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 5
                ]

                let actions =
                    match rightHistory with
                    | Some (lastTurn::_) ->

                        let action (i, (action, success)) = Content.text $"Action{i}" [
                            Entity.Absolute == false
                            Entity.Size == v3 60.0f 5.0f 0.0f
                            Entity.Text := $"{action}"
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing == Some 5
                            Entity.Justification == Justified (JustifyRight, JustifyMiddle)
                            Entity.TextColor := if success then Color.FloralWhite else Color.Gray
                        ]

                        lastTurn
                        |> Turn.describe
                        |> List.indexed
                        |> List.map action

                    | _ -> []

                Content.association "Actions" [
                    Entity.Absolute == false
                    Entity.PositionLocal == v3 40.0f 0.0f 0.0f
                    Entity.Size == v3 60.0f 40.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                ] actions
            ]
        | None -> ()

        match leftHistory, rightHistory with
        | Some (leftLastTurn::_), Some (rightLastTurn::_) ->

            let left = left.Value
            let right = right.Value

            let turnTypeLeft = List.last leftLastTurn.Checks
            let turnTypeLeft = turnTypeLeft.Type

            let turnTypeRight = List.last rightLastTurn.Checks
            let turnTypeRight = turnTypeRight.Type

            let leftSuccesses = Character.getStamina turnTypeLeft left
            let rightSuccesses = Character.getStamina turnTypeRight right

            let turnResult =
                if leftSuccesses = rightSuccesses then
                    "Draw!"
                else if leftSuccesses > rightSuccesses then
                    $"{left.Name} advances!"
                else
                    $"{right.Name} advances!"

            let combatResult =
                if Character.isDead left && Character.isDead right then
                    "Everyone died!"
                else if Character.isDead left then
                    $"{right.Name} won!"
                else if Character.isDead right then
                    $"{left.Name} won!"
                else
                    ""

            ContentEx.richText "CombatSummary" [
                Entity.Absolute == false
                Entity.PositionLocal == v3 0.0f 40.0f 0.0f
                Entity.Size == v3 120.0f 20.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text := $" {leftSuccesses} - {rightSuccesses} - {turnResult}

                Turn: {model.Turn} {combatResult}"
                Entity.TextColor == Color.FloralWhite
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 5
            ]


        | _ -> ()

    ]