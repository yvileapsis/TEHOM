namespace Tehom

open Nu

module TehomChoices =

    open TehomID
    open Actor
    open Ability
    open Action
    open TehomActors

    // needs objects
    (*
    Verb -> Set<Action>
    Action -> Set<Ability * Limb * Composed>
    Ability -> Set<Object>
    *)

    type Choice = {
        Action: ActionID
        Ability: AbilityID
        Limb: Limb
        Object: ActorID
    }
    with
        static member empty = {
            Action = ActionID TehomID.empty
            Ability = AbilityID TehomID.empty
            Limb = Limb.default'
            Object = ActorID TehomID.empty
        }


    // should be map of actorabilities -> objects
    type Choices = Choices of Set<Choice>
    with
        static member unwrap (Choices choices) = choices
        static member choices (subject: ActorID) (tehomActors : TehomActors) =

            let { Actors = actors; Compositions = compositions; Abilities = abilities; Actions = actions } = tehomActors

                // filtering by limbs presupposed here, when composition is created.
            let limbs = Limb.allChildrenByID compositions subject

            let objects abilityID limb =
                if Map.containsKey abilityID abilities then
                    let levels = abilities[abilityID].Levels
                    levels
                    |> Set.map (fun (up, down, _) ->
                        Limb.custom compositions up down limb
                        |> Set.map (Limb.unwrap >> List.head >> fst)
                    )
                    |> Set.unionMany
                else
                    Set.empty

            let actions abilityID =
                if Map.containsKey abilityID abilities then

                    let abilityGroup = abilities[abilityID].AbilityGroup

                    actions
                    |> Map.filter (fun _ value ->
                        List.contains abilityGroup value.Order
                    )
                    |> Map.keys
                    |> Set.ofSeq
                else
                   Set.empty

            let abilities limb =
                let abilities actorID =
                    if Map.containsKey actorID actors then
                        actors[actorID].Abilities
                    else
                        Set.empty
                limb
                |> Limb.unwrap
                |> List.head
                |> fst
                |> abilities

            limbs
            |> Set.map (fun limb -> { Choice.empty with Limb = limb })

            |> Set.map (fun choice ->
                Set.map (fun ability -> { choice with Ability = ability }) (abilities choice.Limb)
            )
            |> Set.unionMany

            |> Set.map (fun choice ->
                Set.map (fun verb -> { choice with Action = verb }) (actions choice.Ability)
            )
            |> Set.unionMany

            |> Set.map (fun choice ->
                Set.map (fun object -> { choice with Object = object }) (objects choice.Ability choice.Limb)
            )
            |> Set.unionMany
            |> Choices

        static member filter (subject: ActorID) (action : ActionID option) (ability : AbilityID option)
            (limb : Limb Option) (object : ActorID option) (Choices choices : Choices) =

            let (|Empty|_|) a = if Set.isEmpty a then Some () else None

            let filterBy compare = function
                | None -> id
                | Some a -> Set.filter (fun (b : Choice) -> a === compare b)

            let actionFilter = filterBy (function { Action = x } -> x) action
            let abilityFilter = filterBy (function { Ability = x } -> x) ability
            let limbFilter = filterBy (function { Limb = x } -> x) limb
            let objectFilter = filterBy (function { Object = x } -> x) object

            match choices with
                | Empty -> Error $"Failed at subject %A{subject}"
                | choices ->
            match actionFilter choices with
                | Empty -> Error $"Failed at action %A{action.Value}"
                | choices ->
            match abilityFilter choices with
                | Empty -> Error $"Failed at ability %A{ability.Value}"
                | choices ->
            match limbFilter choices with
                | Empty -> Error $"Failed at limb %A{limb}"
                | choices ->
            match objectFilter choices with
                | Empty -> Error $"Failed at object %A{object.Value}"
                | choices -> Ok (Choices choices)
        static member best (Choices choices : Choices) = Set.maxElement choices

    let subject = ActorID (ID "cat")
    let action = ActionID (ID "look")
    let object = ActorID (ID "key")


    (*
    possibilities

    TODO: make action
    TODO: filter actorAbiltiies by abilities by action
    TODO: filter actorAbilities by composed
    TODO: pick best for a think             <- per action

    handling

    *)

    let whatever =
        let getChildrenSet actor (compositions: Map<TehomID, Composition>) =
            let (Composition composition) = compositions[actor]
            composition
            |> Map.keys
            |> Set.ofSeq

        let getParentsSet actor compositions =
            compositions
            |> Map.values
            |> Seq.map Map.keys
            |> Seq.filter (Seq.contains actor)
            |> Seq.map Set.ofSeq
            |> Set.unionMany

        let concat func1 func2 actor (compositions: Map<TehomID, Composition>) =
            let func1 actor = func1 actor compositions
            let func2 actor = func2 actor compositions

            func1 actor
            |> Set.map func2
            |> Set.unionMany
        ()

    // Rough list of actions 'alive' entities should be capable of, each action should require a certain 'Ability'.
    type EntityActions =
        | Look
        | Go

        | Use
        | Equip
        | UnEquip
        | Take
        | Drop
        | Give

        | Attack
        | Say

    // Will be reworked, rough draft of actions
    // Somewhere in this module or later there would happen mapping between UI-sent actions and actions player entity does.
    type PlayerActions =
        | Wait
        | Look
        | Explore
        | Go

        | Self
        | Party

        | Use
        | Equip
        | UnEquip
        | Take
        | Drop
        | Give

        | Attack
        | Say

type TehomChoice = TehomChoices.Choice
type TehomChoices = TehomChoices.Choices