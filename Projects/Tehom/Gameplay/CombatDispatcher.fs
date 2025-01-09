namespace Tehom

open System
open System.Numerics
open Prime
open Nu

open Action

type CombatMessage =
    | StartPlaying
    | Update
    | TurnProcess
    | ProcessCharacterTurn of Entity * Turn
    | SelectNext of int
    | TimeUpdate
    interface Message

type CombatCommand =
    | RollInitiative
    | UpdateGraph
    | GameEffect of GameEffect
    interface Command

[<AutoOpen>]
module CombatExtensions =
    type Entity with
        member this.GetCombat world = this.GetModelGeneric<Combat> world
        member this.SetCombat value world = this.SetModelGeneric<Combat> value world
        member this.Combat = this.ModelGeneric<Combat> ()

type CombatDispatcher () =
    inherit Entity2dDispatcher<Combat, CombatMessage, CombatCommand> (false, false, false, Combat.initial)

    override this.Definitions (_, _) = [
        Entity.Size == Constants.Render.VirtualResolution.V3 / 2f
        Screen.DeselectingEvent => FinishQuitting
        Screen.UpdateEvent => Update
        Screen.TimeUpdateEvent => TimeUpdate
        Entity.AlwaysUpdate == true
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
                [UpdateGraph], model

        | TurnProcess ->
            match model.CombatState with
            | TurnNone ->
                match model.Combatants with
                | attacker::_ ->
                    let signals : Signal list = [ GameEffect (CharacterReset attacker) ]
                    let model = { model with CombatState = TurnAttacker attacker }
                    withSignals signals model
                | _ ->
                    just model

            | TurnAttacker attacker ->
                let model =
                    if Combat.isCharacterControlled attacker then
                        let model = { model with Selections = AttackerAI.getMovesMatrix attacker world }
                        model
                    else
                        model
                let model = { model with CombatState = TurnAttackPlan attacker }
                just model

            | TurnAttackPlan attacker ->
                let plan =
                    if Combat.isCharacterControlled attacker then
                        AttackerAI.customPlan attacker model.Area model world |> Some
                    else
                        AttackerAI.tryPlan attacker model.Area model world

                let model = { model with Selections = List.empty }

                match plan with
                | Some attackerAction ->
                    let model = { model with CombatState = TurnDefender (attacker, attackerAction) }
                    just model
                | None ->
                    just model

            | TurnDefender (attacker, attackPlan) ->
                attackPlan.Checks
                |> List.tryFind (fun check -> not (List.isEmpty check.OpposedBy))
                |> function
                    | Some { OpposedBy = [defender] } ->
                        let model = { model with CombatState = TurnDefencePlan (attacker, attackPlan, defender) }
                        just model
                    | _ ->
                        just model

            | TurnDefencePlan (attacker, attackPlan, defender) ->
                let action =
                    attackPlan.Checks
                    |> List.tryFind (fun check -> not (List.isEmpty check.OpposedBy))
                    |> _.Value

                match DefenderAI.tryPlan attacker action defender model.Area model world with
                | Some defencePlan ->
                    let model = { model with CombatState = TurnExecute (attacker, attackPlan, defender, defencePlan) }
                    just model
                | None ->
                    just model

            | TurnAttackKarmaBid ->
                failwith "todo"

            | TurnDefenceKarmaBid ->
                failwith "todo"

            | TurnExecute (attacker, attackPlan, defender, defencePlan)->
                let attackerTurn =
                    let character = attacker.GetCharacter world
                    Turn.applyStance character attackPlan

                let defenderTurn =
                    let character = defender.GetCharacter world
                    Turn.applyStance character defencePlan

                // TODO: add karma betting

                let attackerTurn, defenderTurn = Turn.opposedTurns attackerTurn defenderTurn

                let model = Combat.advanceTurn attacker defender attackerTurn defenderTurn model

                let model = { model with CombatState = TurnNone }

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

        | SelectNext i ->
            match List.tryItem i model.Selections with
            | Some (head::tail) ->
                let list = tail @ [head]

                let selections =
                    model.Selections
                    |> List.indexed
                    |> List.map (fun (j, list') -> if i = j then list else list')

                let model = { model with Selections = selections }
                just model
            | _ ->
                just model

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { model with CombatTime = model.CombatTime + gameDelta.Updates }
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

        | UpdateGraph ->
            let graph = entity / "Graph"
            let graphSites = graph.GetGraph world
            let graphSites = { graphSites with Graph = model.Area.Sites }
            let world = graph.SetGraph graphSites world
            just world

    override this.TruncateModel model = {
        model with
            DisplayLeftModel = None
            DisplayRightModel = None
            Area = Area.empty
    }
    override this.UntruncateModel (model, model') = {
        model with
            DisplayLeftModel = model'.DisplayLeftModel
            DisplayRightModel = model'.DisplayRightModel
            Area = model'.Area
    }

    override this.Content (model, _) = [

        Content.staticSprite "Background" [
            Entity.Size == v3 280f 80f 0f
            Entity.StaticImage == Assets.Default.Black
            Entity.Color == Color.White.WithA 0.5f
        ]

        Content.entity<GraphDispatcher> "Graph" [
            Entity.PositionLocal == v3 90f 90f 0f
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
                stat "MinorWounds" "Minor Wounds" $"{character.MinorWounds}"
                stat "MajorWounds" "Major Wounds" $"{character.MajorWounds}"
                stat "Gall" "Gall" $"{gall} {gallStance}"
                stat "Lymph" "Lymph" $"{lymph} {lymphStance}"
                stat "Oil" "Oil" $"{oil} {oilStance}"
                stat "Plasma" "Plasma" $"{plasma} {plasmaStance}"
                stat "Stances" "Stances" $"{character.StancesLeft}"
                stat "Initiative" "Initiative" $"{character.Initiative}"

                let connections = Area.getConnections character.ID model.Area
                location "CombatLocation" $"Location {connections}"
            ]

            Content.association "Stats" boxProperties stats



        match left with
        | Some entity ->
            Content.composite "Left" [] [

                statsBox entity [
                    Entity.Absolute == false
                    Entity.Elevation == 10.0f
                    Entity.PositionLocal == v3 -100.0f 0.0f 0.0f
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
                            Entity.Size == v3 40.0f 5.0f 0.0f
                            Entity.Text := $"{action}"
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing == Some 5
                            Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                            Entity.TextColor == Color.FloralWhite
                            Entity.ClickEvent => SelectNext i
                        ]

                        model.Selections
                        |> List.map (List.head >> Move.getName)
                        |> List.indexed
                        |> List.map action

                    | _, Some (lastTurn::_) ->

                        let action (i, (action, success)) = Content.text $"Action{i}" [
                            Entity.Absolute == false
                            Entity.Size == v3 40.0f 5.0f 0.0f
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
                    Entity.Size == v3 40.0f 40.0f 0.0f
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
                    Entity.PositionLocal == v3 100.0f 0.0f 0.0f
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
                            Entity.Size == v3 40.0f 5.0f 0.0f
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
                    Entity.Size == v3 40.0f 40.0f 0.0f
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

            let leftSuccesses = Turn.getSuccesses leftLastTurn
            let rightSuccesses = Turn.getSuccesses rightLastTurn

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