namespace Tehom

open System
open System.Numerics
open Prime
open Nu

open Action

type CombatMessage =
    | Update
    | ReorderCombatants
    | TurnProcess
    | UpdatePossibleActions
    | SetPlannedTarget of Int32
    | AddPlannedAction of Int32
    | RemovePlannedAction of Int32
    | ChangePlannedFractureBet of Int32
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
                let plan = Plan.make attacker
                let plan = Plan2.plan plan.Entity model world plan

                let model = { model with CombatState = TurnAttackerPlanning plan }

                [ if Plan2.shouldBePlanned plan then () else TurnProcess ], model

            | TurnAttackerPlanning attack ->
                match Plan2.finalize attack model world with
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
                let plan = Plan.make defender
                let plan = Plan2.plan plan.Entity model world plan

                let model = { model with CombatState = TurnDefenderPlanning (attack, plan) }

                [ if Plan2.shouldBePlanned plan then () else TurnProcess ], model

            | TurnDefenderPlanning (attack, defence) ->

                match Plan2.finalizeDefence attack defence model world with
                | Some defence ->
                    let model = { model with CombatState = TurnDefenderFinish (attack, defence) }
                    [TurnProcess], model
                | None ->
                    just model

            | TurnDefenderFinish (attack, defence) ->
                let model = { model with CombatState = TurnExecute (attack, defence) }
                [TurnProcess], model

            | TurnExecute (attack, defence)->

                let attack = Plan2.makeTurn attack
                let defence = Plan2.makeTurn defence

                let signals : Signal list = [
                    ProcessCharacterTurn [attack; defence]
                ]

                withSignals signals model

            | Won entity ->
                let model = { model with CombatState = Done entity }
                [TurnProcess], model

            | Done entity ->
                just model

        | SetPlannedTarget i ->
            let model =
                Combat.updatePlan (fun plan ->
                    let target, possible = List.item i plan.PossibleTargets
                    if possible then
                        Plan2.setPlannedTarget target plan
                    else
                        plan
                ) model

            just model

        | UpdatePossibleActions ->
            let model =
                Combat.updatePlan (fun plan -> Plan2.update plan.Entity model world plan) model

            just model

        | AddPlannedAction i ->
            let model =
                Combat.updatePlan (fun plan ->
                    let action, possible = List.item i plan.PossibleActions
                    let plan = Plan2.addPlannedAction action plan
                    let plan = Plan2.update plan.Entity model world plan
                    plan
                ) model

            just model

        | RemovePlannedAction i ->
            let model =
                Combat.updatePlan (fun plan ->
                    let plan = Plan2.removePlannedAction i plan
                    let plan = Plan2.update plan.Entity model world plan
                    plan
                ) model

            just model

        | ChangePlannedFractureBet i ->
            let model =
                Combat.updatePlan (fun plan ->
                    let plan = Plan2.modifyFractureBet i plan
                    plan
                ) model

            just model

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { model with CombatTime = model.CombatTime + gameDelta.Updates }
            just gameplay

        | ReorderCombatants ->

            let model = {
                model with
                    Combatants =
                        model.Combatants
                        |> List.sortBy (fun (entity : Entity) ->
                            entity.GetCharacterWith Character.getInitiative world
                        )
                        |> List.rev
            }

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

            // TODO: secondary fracture betting happens here as part of a turn, not a plan

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

            let aliveEntities =
                model.Combatants
                |> List.filter (fun entity ->
                    entity.GetCharacter world
                    |> Character.canAct
                )

            let model = { model with CombatState = if List.length aliveEntities > 1 then TurnNone else Won (List.head aliveEntities) }

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

        match model.CombatState with
        | Done entity ->
            ()
        | _ ->

            // TODO: Display reach + stride distance, also display it on the map
            let plan =
                match model.CombatState with
                | TurnAttackerPlanning attack when Plan2.shouldBePlanned attack ->
                    Some attack
                | TurnDefenderPlanning (attack, defence) when Plan2.shouldBePlanned defence ->
                    Some defence
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
                    stat "Fracture" "Fracture" $"{character.FractureCurrent}"
                    stat "Initiative" "Initiative" $"{character.Initiative}"
                ]

                Content.association "Stats" boxProperties stats

            match plan with
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
                    plan.PossibleActions
                    |> List.indexed
                    |> List.map (fun (i, (action, possible)) -> i, Action.describe action, possible)
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
                    plan.PossibleTargets
                    // TOOD: annoying, can't access world name, will need to store name
                    |> List.indexed
                    |> List.map (fun (i, (entity, possible)) -> i, $"{entity.Name}", possible)
                    |> List.map action
                )

                Content.association "Fracture Bet" [
                    Entity.Absolute == false
                    Entity.PositionLocal == v3 -180.0f -100.0f 0.0f
                    Entity.Size == v3 40.0f 40.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                ] [
                    Content.button $"Fracture" [
                        Entity.Absolute == false
                        Entity.Size == v3 30.0f 5.0f 0.0f
                        Entity.Text := $"{plan.PlannedFractureBet}"
                        Entity.Font == Assets.Gui.ClearSansFont
                        Entity.FontSizing == Some 5
                        Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                        Entity.TextColor := Color.FloralWhite
                    ]
                    Content.button $"Increase" [
                        Entity.Absolute == false
                        Entity.Size == v3 30.0f 5.0f 0.0f
                        Entity.Text := $"+"
                        Entity.Font == Assets.Gui.ClearSansFont
                        Entity.FontSizing == Some 5
                        Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                        Entity.TextColor := Color.FloralWhite
                        Entity.ClickEvent => ChangePlannedFractureBet 1
                    ]
                    Content.button $"Decrease" [
                        Entity.Absolute == false
                        Entity.Size == v3 30.0f 5.0f 0.0f
                        Entity.Text := $"-"
                        Entity.Font == Assets.Gui.ClearSansFont
                        Entity.FontSizing == Some 5
                        Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                        Entity.TextColor := Color.FloralWhite
                        Entity.ClickEvent => ChangePlannedFractureBet -1
                    ]
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

                        match plan, leftHistory with
                        | Some attack, _ when Plan2.shouldBePlanned attack  ->

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

                            attack.PlannedActions
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