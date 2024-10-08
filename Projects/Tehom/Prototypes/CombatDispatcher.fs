namespace Tehom

open System
open Prime
open Nu
open FGL
open Character
open Area

// this is our gameplay MMCC message type.
type CombatMessage =
    | StartPlaying
    | FinishQuitting
    | Update
    | TurnBegin
    | CombatantAttacks of Entity
    | CombatantDefends of Entity * Turn
    | TurnEnd of Entity * Turn * Entity * Turn
    | CharacterTurn of Entity * Turn
    | TimeUpdate
    interface Message

// this is our gameplay MMCC command type.
type CombatCommand =
    | RollInitiative
    | GameEffect of GameEffect
    | StartQuitting
    interface Command

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module CombatExtensions =
    type Screen with
        member this.GetCombat world = this.GetModelGeneric<Combat> world
        member this.SetCombat value world = this.SetModelGeneric<Combat> value world
        member this.Combat = this.ModelGeneric<Combat> ()
        member this.QuitEvent = Events.QuitEvent --> this

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type CombatDispatcher () =
    inherit ScreenDispatcher<Combat, CombatMessage, CombatCommand> (Combat.empty)

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Combat.initial
        else Combat.empty

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) = [
        Screen.SelectEvent => StartPlaying
        Screen.DeselectingEvent => FinishQuitting
        Screen.UpdateEvent => Update
        Screen.TimeUpdateEvent => TimeUpdate
    ]

    // here we handle the above messages
    override this.Message (model, message, screen, world) =

        match message with
        | StartPlaying ->

            let gameplay = Combat.initial

            let gameplay = {
                gameplay with
                    Combatants =
                        world
                        |> World.getEntities Simulants.CombatCharacters
                        |> Seq.map (fun c -> c, [])
                        |> List.ofSeq
            }

            let signal : Signal = RollInitiative

            withSignal signal gameplay

        | FinishQuitting ->
            let gameplay = Combat.empty
            just gameplay

        | Update ->
            let gameplay = Combat.update model world
            just gameplay

        | TurnBegin ->

            let combatants =
                model.Combatants
                |> List.sortBy (fun (entity, _) ->
                    let character = entity.GetCharacter world
                    character.Initiative
                )
                |> List.rev

            let model = { model with Combatants = combatants }

            let attacker, model =
                match model.CurrentCombatant with
                | Some attacker ->
                    attacker, model
                | None ->
                    let attacker =
                        model.Combatants
                        |> List.head
                        |> fst
                    attacker,
                    { model with CurrentCombatant = Some attacker }

            let nextCombatantID =

                let combatants = model.Combatants |> List.map fst

                let attackerIndex =
                    combatants
                    |> List.findIndex (fun c -> c = attacker)

                let index =
                    if attackerIndex + 1 < List.length combatants then
                        attackerIndex + 1
                    else
                        0

                combatants
                |> List.item index

            let model = {
                model with
                    CurrentCombatant = Some nextCombatantID
                    Turn = model.Turn + 1
            }

            let signals : Signal list = [
                GameEffect (CharacterReset attacker)
                CombatantAttacks attacker
            ]

            withSignals signals model

        | CombatantAttacks attacker ->

            let attackerAction =
                Combat.turnAttackerPlan attacker model world

            let signal : Signal =
                CombatantDefends (attacker, attackerAction)

            withSignal signal model

        // TODO: assumed one target
        | CombatantDefends (attacker, attackerAction) ->
            attackerAction.Checks
            |> List.tryFind (fun check ->
                not (List.isEmpty check.OpposedBy)
            )
            |> fun check ->
                match check with
                | Some { OpposedBy = [defender] } as Some action ->
                    let defenderAction =
                        Combat.turnDefenderPlan attacker action defender model world

                    let signal : Signal =
                        TurnEnd (attacker, attackerAction, defender, defenderAction)

                    withSignal signal model
                | _ ->
                    just model

        | TurnEnd (attacker, attackerAction, defender, defenderAction) ->

            let attackerTurn =
                let character = attacker.GetCharacter world
                Turn.applyStance character attackerAction

            let defenderTurn =
                let character = defender.GetCharacter world
                Turn.applyStance character defenderAction

            // add karma betting

            let attackerTurn, defenderTurn = Turn.opposedTurns attackerTurn defenderTurn

            let combatants =
                model.Combatants
                |> List.map (fun (entity, history) ->
                    if entity = attacker then
                        entity, attackerTurn::history
                    elif entity = defender then
                        entity, defenderTurn::history
                    else
                        entity, history
                )

            let model = { model with Combatants = combatants }

            let signals : Signal list = [
                CharacterTurn (attacker, attackerTurn)
                CharacterTurn (defender, defenderTurn)
            ]

            withSignals signals model

        | CharacterTurn (actor, turn) ->

            let signals : Signal list =
                turn.Checks
                |> List.fold (fun signals check ->
                    match check.Action with
                    | FullMentalAction ->
                        signals
                    | FullPhysicalAction ->
                        signals
                    | StanceChange stance ->
                        let signal : Signal =
                            GameEffect (CharacterStanceChange (actor, stance))
                        signals @ [ signal ]
                    | PhysicalSequence moves ->

                        let successes = check.Successes
                        let blocks = check.OpposedSuccesses

                        let signals' : Signal list =
                            let indexes, moves =
                                moves
                                |> List.indexed
                                |> List.takeWhile (fun (i, move) ->
                                    // positioning is always successful for now, but should check if within reach of enemy later
                                    ((List.contains move Move.positioning) || (successes > blocks)) && (i < successes)
                                )
                                |> List.unzip

                            match check.Target with
                            | Some target ->
                                List.map (fun i -> Move.handle i moves actor target model.Area world) indexes
                                |> List.concat
                                |> List.map (fun signal -> GameEffect signal)
                            | None ->
                                []
                        signals @ signals'
                    ) []

            withSignals signals model

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { model with GameplayTime = model.GameplayTime + gameDelta.Updates }
            just gameplay

    // here we handle the above commands
    override this.Command (model, command, screen, world) =

        match command with
        | StartQuitting ->
            let world = World.publish () screen.QuitEvent screen world
            just world

        | RollInitiative ->

            let world =
                model.Combatants
                |> List.map fst
                |> List.fold (fun (world : World) (entity : Entity) ->
                    let character = entity.GetCharacter world

                    let character = Character.turnReset character
                    let gall, plasma = Character.getMaxInitiative character
                    let character = { character with Initiative = Random.rollInitiative (gall + plasma) }

                    let world = entity.SetCharacter character world
                    world
                ) world

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

        | GameEffect (Damage (entity, damage)) ->

            let character = entity.GetCharacter world
            let character = Character.doDamage damage character
            let world = entity.SetCharacter character world

            just world

        | GameEffect (TravelInter (character, location)) ->

            let area = model.Area
            let area = Area.moveSite character location area
            let model = { model with Area = area }
            let world = screen.SetCombat model world
            just world

        | GameEffect (TravelIntra (character, location, distance)) ->

            let area = model.Area
            let area = Area.establishDistance distance character location area
            let model = { model with Area = area }
            let world = screen.SetCombat model world
            just world

    // here we describe the content of the game including the hud, the scene, and the player
    override this.Content (gameplay, screen) = [
        // the gui group

        Content.group Simulants.GameplayGui.Name [] [

            Content.button Simulants.GameplayGuiAdvanceTurn.Name [
                Entity.Position == v3 0.0f 150.0f 0.0f
                Entity.Size == v3 80.0f 20.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text == "Advance Turn"
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 10
                Entity.ClickEvent => TurnBegin
            ]

            let player = gameplay.DisplayLeft
            let enemy = gameplay.DisplayRight

            let playerHistory =
                gameplay.Combatants
                |> List.tryFind (fun (entity, _) -> player.ID = entity.Name)

            let enemyHistory =
                gameplay.Combatants
                |> List.tryFind (fun (entity, _) -> enemy.ID = entity.Name)


            let statsBox character = [
                let (gall, lymph, oil, plasma) = Character.getStats character
                let (gallStance, lymphStance, oilStance, plasmaStance) = Character.getStance character
                let minorWounds = character.MinorWounds
                let majorWounds = character.MajorWounds

                Content.text "Minor Wounds" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"Wounds {minorWounds}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]

                Content.text "Major Wounds" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"{majorWounds}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]

                Content.text "Gall" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"{gall} {gallStance}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]
                Content.text "Lymph" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"{lymph} {lymphStance}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]
                Content.text "Oil" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"{oil} {oilStance}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]
                Content.text "Plasma" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"{plasma} {plasmaStance}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]
                Content.text "Stances" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"Stances {character.StancesLeft}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]
                Content.text "Initiative" [
                    Entity.Size == v3 80.0f 10.0f 0.0f
                    Entity.Text := $"Initiative {character.Initiative}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                ]
            ]

            Content.association "StatsBoxPlayer" [
                Entity.Position == v3 -160.0f 0.0f 0.0f
                Entity.Size == v3 80.0f 80.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Layout == Flow (FlowDownward, FlowUnlimited)
            ] (statsBox player)

            Content.association "StatsBoxEnemy" [
                Entity.Position == v3 200.0f 0.0f 0.0f
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
                    if player.MajorWounds = MajorWounds.Dead && enemy.MajorWounds = MajorWounds.Dead then
                        "Everyone died!"
                    else if player.MajorWounds = MajorWounds.Dead then
                        $"{enemy.Name} won!"
                    else if enemy.MajorWounds = MajorWounds.Dead then
                        $"{player.Name} won!"
                    else
                        ""


                richText "CombatSummary" [
                    Entity.Position == v3 0.0f 80.0f 0.0f
                    Entity.Size == v3 240.0f 40.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Text := $" {playerSuccesses} - {enemySuccesses} - {turnWinner}

                    Turn: {gameplay.Turn} {combatWinner}"
                    Entity.TextColor == Color.FloralWhite
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                ]

                Content.association "MovesPlayer" [
                    Entity.Position == v3 -80.0f 0.0f 0.0f
                    Entity.Size == v3 80.0f 80.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                ] [
                    for i, move in List.indexed playerMoves ->
                        Content.text $"MovesPlayer{i}" [
                            Entity.Size == v3 80.0f 10.0f 0.0f
                            Entity.Text := $"{move}"
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
                    Entity.Position == v3 80.0f 0.0f 0.0f
                    Entity.Size == v3 80.0f 80.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                ] [
                    for i, move in List.indexed enemyMoves ->
                        Content.text $"MovesEnemy{i}" [
                            Entity.Size == v3 80.0f 10.0f 0.0f
                            Entity.Text := $"{move}"
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
                    Entity.Position == v3 0.0f -160.0f 0.0f
                    Entity.Size == v3 360.0f 40.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Text := $"{Area.getConnections player.ID gameplay.Area} -- {Area.getConnections enemy.ID gameplay.Area}"
                    Entity.TextColor == Color.FloralWhite
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 10
                ]

            | _ -> ()

        ]

        // the scene group while playing
        match gameplay.GameplayState with
        | Playing ->
            Content.group Simulants.CombatCharacters.Name [] [
                character Character.player
                character Character.rat
            ]
        // no scene group otherwise
        | Quit -> ()
    ]