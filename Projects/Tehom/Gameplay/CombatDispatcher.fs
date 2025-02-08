namespace Tehom

open System
open System.Numerics
open Prime
open Nu

type CombatMessage =
    | Update
    | ReorderCombatants
    | TurnProcess
    | TurnFinish of List<Turn>
    | SetPlannedTarget of Int32
    | AddPlannedAction of Int32
    | RemovePlannedAction of Int32
    | SetPlannedFracture of Stamina
    | SetPlannedStance of Stats
    | TimeUpdate
    interface Message

type CombatCommand =
    | ResetCombatants
    | GameEffect of GameEffect
    | ProcessCharacterTurn of Turn list
    interface Command

type CombatDispatcher () =
    inherit Entity2dDispatcher<Combat, CombatMessage, CombatCommand> (false, false, false, Combat.initial)

    override this.Definitions (_, _) = [
        Entity.Size == Constants.Render.DisplayVirtualResolution.V3 / 2f
        Screen.DeselectingEvent => FinishQuitting
        Screen.UpdateEvent => Update
        Screen.TimeUpdateEvent => TimeUpdate
        Entity.AlwaysUpdate == true
        Entity.RegisterEvent => ResetCombatants
    ]

    override this.Message (model, message, entity, world) =

        match message with
        | Update ->
            let signals : List<Signal> = []
            // TODO: remove when I figure out which event runs when entity is created if any
            //       if no such event, create it as a facet lol
            let signals, model =
                if model.FirstRun = false then
                    signals @ [
                        ResetCombatants
                        ReorderCombatants
                    ], { model with FirstRun = true }
                else
                    signals, model

            let model = Combat.update model world
            signals, model

        | TurnProcess ->
            match model.CombatState with
            | TurnNone ->
                match model.Combatants with
                | attacker::_ ->
                    let signals : Signal list = [ GameEffect (CharacterDo (attacker, Character.resetTurn)); TurnProcess ]
                    let model = { model with CombatState = TurnAttackerBegin attacker }
                    withSignals signals model
                | _ ->
                    just model

            | TurnAttackerBegin attacker ->
                let plan = Plan.make model.Turn None attacker model.Combatants model.Area
                let plan = Plan.initialize plan world

                let model = { model with CombatState = TurnAttackerPlanning plan }

                [ if Plan.isPlanned plan then () else TurnProcess ], model

            | TurnAttackerPlanning attack ->
                match Plan.tryFinalizeAttack model.Area attack world with
                | Some plan ->
                    let model = { model with CombatState = TurnAttackerFinish plan }
                    [TurnProcess], model
                | None ->
                    just model

            | TurnAttackerFinish attack ->
                let defenders =
                    attack.Checks
                    |> List.collect _.OpposedBy

                // TODO: assumes one defender
                match List.tryHead defenders with
                | Some defender ->
                    let model = { model with CombatState = TurnDefenderBegin (attack, defender) }
                    [TurnProcess], model
                | _ ->
                    just model

            | TurnDefenderBegin (attack, defender) ->
                let plan = Plan.make model.Turn (Some attack) defender model.Combatants model.Area
                let plan = Plan.initialize plan world

                let model = { model with CombatState = TurnDefenderPlanning (attack, plan) }

                [ if Plan.isPlanned plan then () else TurnProcess ], model

            | TurnDefenderPlanning (attack, defence) ->
                match Plan.tryFinalizeDefence model.Area attack defence world with
                | Some defence ->
                    let model = { model with CombatState = TurnDefenderFinish (attack, defence) }
                    [TurnProcess], model
                | None ->
                    just model

            | TurnDefenderFinish (attack, defence) ->
                let model = { model with CombatState = TurnExecute (attack, defence) }
                [TurnProcess], model

            | TurnExecute (attack, defence)->
                let attack = Turn.makeFromPlan attack
                let defence = Turn.makeFromPlan defence

                let signals : Signal list = [
                    ProcessCharacterTurn [attack; defence]
                ]

                withSignals signals model

            | Won entity ->
                let model = { model with CombatState = Done entity }
                [TurnProcess], model

            | Done _ ->
                just model

        | SetPlannedTarget i ->
            let model =
                Combat.updatePlan (fun plan ->
                    let target, path, possible = List.item i plan.PossibleTargets
                    let plan = if possible then Plan.setPlannedTarget target plan else plan
                    let plan = Plan.updateBases plan.Entity model.Combatants model.Area plan
                    let plan = Plan.updateDerivatives plan world
                    plan
                ) model

            just model

        | AddPlannedAction i ->
            let model =
                Combat.updatePlan (fun plan ->
                    let check, possible = List.item i plan.PossibleChecks
                    let plan = if possible then Plan.addPlannedAction check plan else plan
                    let plan = Plan.updateBases plan.Entity model.Combatants model.Area plan
                    let plan = Plan.updateDerivatives plan world
                    plan
                ) model

            just model

        | RemovePlannedAction i ->
            let model =
                Combat.updatePlan (fun plan ->
                    let plan = Plan.removePlannedAction i plan
                    let plan = Plan.updateBases plan.Entity model.Combatants model.Area plan
                    let plan = Plan.updateDerivatives plan world
                    plan
                ) model

            just model

        | SetPlannedFracture fracture ->
            let model = Combat.updatePlan (Plan.setFracture fracture) model
            just model

        | SetPlannedStance stance ->
            let model = Combat.updatePlan (Plan.setStance stance) model
            just model

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { model with CombatTime = model.CombatTime + gameDelta.Updates }
            just gameplay

        | ReorderCombatants ->
            let model = Combat.orderCombatantsByInitiative model world
            just model

        | TurnFinish turns ->

            let model = Combat.advanceTurn turns model

            let aliveEntities =
                model.Combatants
                |> List.filter (fun entity ->
                    entity.GetCharacter world
                    |> Character.canAct
                )

            let model = { model with CombatState = if List.length aliveEntities > 1 then TurnNone else Won (List.head aliveEntities) }

            just model

    override this.Command (model, command, entity, world) =

        match command with
        | ResetCombatants ->

            let world =
                model.Combatants
                |> List.fold (fun (world : World) (entity : Entity) ->
                    entity.SetCharacterWith Character.resetCombat world
                ) world

            just world

        | GameEffect effect ->
            let world = entity.ExecuteGameEffect effect world
            just world

        | ProcessCharacterTurn turns ->

            // TODO: secondary fracture betting happens here as part of a turn, not a plan
            let orderMax =
                turns
                |> List.map Turn.getSubTurns
                |> List.max

            let turns, world =
                // turn processing is a little weird in that it synchorniously processes actions on the same sub-turn
                List.init (1 + orderMax) id
                |> List.fold (fun (turns, world) i ->

                    let turns, world =
                        List.foldMap (fun turn (world : World) -> Turn.processCosts i turn world) world turns

                    let effects =
                        List.collect (fun turn -> Turn.processEffects i turn model.Area world) turns

                    let world =
                        List.fold (fun world effect -> entity.ExecuteGameEffect effect world) world effects

                    turns, world
                ) (turns, world)

            [TurnFinish turns], world

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

        match model.CombatState with
        | Done _ ->
            ()
        | _ ->

            // TODO: Display reach + stride distance, also display it on the map
            let planLeft =
                match model.CombatState with
                | TurnAttackerPlanning attack when Plan.isPlanned attack ->
                    Some attack
                | TurnDefenderPlanning (attack, defence) when Plan.isPlanned defence ->
                    Some defence
                | _ ->
                    None

            let planRight =
                match model.CombatState with
                | TurnDefenderPlanning (attack, defence) when Plan.isPlanned defence ->
                    Some attack
                | _ ->
                    None

            Content.staticSprite "Background" [
                Entity.Size == v3 480f 280f 0f
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

            let historyLeft =
                Option.bind findHistory model.DisplayLeftEntity

            let historyRight =
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

                let (gall, lymph, oil, plasma) =
                    Character.getStats character
                    |> Stats.getStats

                let (gallStance, lymphStance, oilStance, plasmaStance) =
                    character
                    |> Character.getStance
                    |> Stats.getStats

                let stats = [
                    stat "MinorWounds" "Minor Wounds" $"{character.Injuries}"
                    stat "MajorWounds" "Major Wounds" $"{character.Wounds}"
                    stat "Gall" "Gall" $"{gall} {gallStance}"
                    stat "Lymph" "Lymph" $"{lymph} {lymphStance}"
                    stat "Oil" "Oil" $"{oil} {oilStance}"
                    stat "Plasma" "Plasma" $"{plasma} {plasmaStance}"
                    stat "Stances" "Stances" $"{character.StancesCurrent}"
                    stat "Fracture" "Fracture" $"{character.FractureCurrent}"
                    stat "Initiative" "Initiative" $"{character.Initiative}"
                ]

                Content.association "Stats" boxProperties stats

            match planLeft with
            | Some plan ->
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
                    plan.PossibleChecks
                    |> List.indexed
                    |> List.map (fun (i, (action, possible)) -> i, Action.describe action.Action, possible)
                    |> List.map action
                )

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
                    Plan.getPossibleTargets plan
                    // TOOD: annoying, can't access world name, will need to store name
                    |> List.indexed
                    |> List.map (fun (i, (entity, path, possible)) -> i, $"{entity.Name}", possible)
                    |> List.map action
                )

                let stat name display current change =
                    Content.association $"{name}" [
                        Entity.Size == v3 90.0f 5.0f 0.0f
                    ] [
                        Content.button $"Value" [
                            Entity.Absolute == false
                            Entity.Size == v3 30.0f 5.0f 0.0f
                            Entity.Text := $"{name}: {display}"
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing == Some 5
                            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                            Entity.TextColor := Color.FloralWhite
                        ]
                        Content.button $"Increase" [
                            Entity.Absolute == false
                            Entity.Size == v3 10f 5f 0f
                            Entity.PositionLocal == v3 20f 0f 0f
                            Entity.Text := $"+"
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing == Some 5
                            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                            Entity.TextColor := Color.FloralWhite
                            Entity.ClickEvent => SetPlannedFracture (current + change)
                        ]
                        Content.button $"Decrease" [
                            Entity.Absolute == false
                            Entity.Size == v3 10f 5f 0f
                            Entity.PositionLocal == v3 -20f 0f 0f
                            Entity.Text := $"-"
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing == Some 5
                            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                            Entity.TextColor := Color.FloralWhite
                            Entity.ClickEvent => SetPlannedFracture (current - change)
                        ]
                    ]

                Content.association "Fracture" [
                    Entity.Absolute == false
                    Entity.PositionLocal == v3 -180.0f -100.0f 0.0f
                    Entity.Size == v3 40.0f 40.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                ] [
                    stat "Fire" plan.PlannedFracture.PhysicalActive plan.PlannedFracture Stamina.physicalActive
                    stat "Water" plan.PlannedFracture.PhysicalReactive plan.PlannedFracture Stamina.physicalReactive
                    stat "Air" plan.PlannedFracture.MentalActive plan.PlannedFracture Stamina.mentalActive
                    stat "Earth" plan.PlannedFracture.MentalReactive plan.PlannedFracture Stamina.mentalReactive
                ]

                let stat name display current change =
                    Content.association $"{name}" [
                        Entity.Size == v3 90.0f 5.0f 0.0f
                    ] [
                        Content.button $"Value" [
                            Entity.Absolute == false
                            Entity.Size == v3 30f 5f 0.0f
                            Entity.Text := $"{name}: {display}"
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing == Some 5
                            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                            Entity.TextColor := Color.FloralWhite
                        ]
                        Content.button $"Increase" [
                            Entity.Absolute == false
                            Entity.Size == v3 10f 5f 0.0f
                            Entity.PositionLocal == v3 20f 0f 0f
                            Entity.Text := $"+"
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing == Some 5
                            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                            Entity.TextColor := Color.FloralWhite
                            Entity.ClickEvent => SetPlannedStance (current + change)
                        ]
                        Content.button $"Decrease" [
                            Entity.Absolute == false
                            Entity.Size == v3 10f 5f 0.0f
                            Entity.PositionLocal == v3 -20f 0f 0f
                            Entity.Text := $"-"
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing == Some 5
                            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                            Entity.TextColor := Color.FloralWhite
                            Entity.ClickEvent => SetPlannedStance (current - change)
                        ]
                    ]

                Content.association "StanceChange" [
                    Entity.Absolute == false
                    Entity.PositionLocal == v3 -80.0f -100.0f 0.0f
                    Entity.Size == v3 40.0f 40.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                ] [
                    stat "Gall" plan.PlannedStance.Gall plan.PlannedStance Stance.gall
                    stat "Lymph" plan.PlannedStance.Lymph plan.PlannedStance Stance.lymph
                    stat "Oil" plan.PlannedStance.Oil plan.PlannedStance Stance.oil
                    stat "Plasma" plan.PlannedStance.Plasma plan.PlannedStance Stance.plasma
                ]

                match plan.PlannedTarget with
                | Some target ->
                    Content.button "PlannedTarget" [
                        Entity.Absolute == false
                        Entity.PositionLocal == v3 -40.0f 30.0f 0.0f
                        Entity.Elevation == 10.0f
                        Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                        Entity.Size == v3 40.0f 5.0f 0.0f
                        Entity.Text := $"{target.Name}"
                        Entity.Font == Assets.Gui.ClearSansFont
                        Entity.FontSizing == Some 5
                        Entity.TextColor == Color.FloralWhite
                    ]
                | None ->
                    ()

                Content.button "Distance" [
                    Entity.Absolute == false
                    Entity.PositionLocal == v3 -100.0f 30.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Size == v3 40.0f 5.0f 0.0f
                    Entity.Text := $"{plan.DistanceCurrentReach}"
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
                    Entity.Text := $"{plan.DistanceToTarget}"
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

                        match planLeft, historyLeft with
                        | Some plan, _  ->

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

                            plan
                            |> Plan.describe
                            |> List.indexed
                            |> List.map action

                        | _, Some (turn::_) ->

                            let action (i, (action, success)) = Content.text $"Action{i}" [
                                Entity.Absolute == false
                                Entity.Size == v3 60.0f 5.0f 0.0f
                                Entity.Text := $"{action}"
                                Entity.Font == Assets.Gui.ClearSansFont
                                Entity.FontSizing == Some 5
                                Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                                Entity.TextColor := if success then Color.FloralWhite else Color.Gray
                            ]

                            turn
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
                        match planRight, historyRight with
                        | Some plan, _ ->

                            let action (i, action) = Content.text $"ActionSelectable{i}" [
                                Entity.Absolute == false
                                Entity.Size == v3 60.0f 5.0f 0.0f
                                Entity.Text := $"{action}"
                                Entity.Font == Assets.Gui.ClearSansFont
                                Entity.FontSizing == Some 5
                                Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                                Entity.TextColor == Color.FloralWhite
                            ]

                            plan
                            |> Plan.describe
                            |> List.indexed
                            |> List.map action

                        | _, Some (lastTurn::_) ->

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

            match historyLeft, historyRight with
            | Some (leftLastTurn::_), Some (rightLastTurn::_) ->

                let left = left.Value
                let right = right.Value

                let turnTypeLeft = List.last leftLastTurn.Checks
                let turnTypeLeft = turnTypeLeft.Type

                let turnTypeRight = List.last rightLastTurn.Checks
                let turnTypeRight = turnTypeRight.Type

                let successesLeft = Character.getStamina turnTypeLeft left
                let successesRight = Character.getStamina turnTypeRight right

                let turnResult =
                    if successesLeft = successesRight then
                        "Draw!"
                    else if successesLeft > successesRight then
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
                    Entity.Text := $" {successesLeft} - {successesRight} - {turnResult}

                    Turn: {model.Turn} {combatResult}"
                    Entity.TextColor == Color.FloralWhite
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 5
                ]


            | _ -> ()

    ]