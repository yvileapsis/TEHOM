namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open Character

(*
TODO: Always do the minimum implementation

TODO:
    - Equipping pistol
    - Opening doors

TODO: Combat
    - No skills
    - Can't select stance
    - No karma betting
    - No inventory
    - No cooldown
    - Body doesn't impact anything

TODO: Containers and notes

TODO: Debate

TODO: Visibility system + Lights

TODO: Tiniest vertical slice:
    * 7 Rooms:
    * Waitroom, barricaded windows, rolling hospital bed, locked exit door, you wake up here
    * Registration room, safe with useful stuff like a pistol maybe, code locked, code is gotten from a book
      (can be seen from waitroom through glass)
    * Main hall, chairs, first rat attacks
    * Electrical room to fix the lights, second rat attacks
    * Surgery room, surgery table, cat on the table, note (cat ate the key), opening it lets spider chandalier escape
    * Pharmacy shop, drugs you can use
    * Staircase, other floors blocked, but can move up and down.
    * Goal is to go to surgery room, take the key, return to waitroom, open the door and exit.
    * Optionally you can fix lights and fight spider chandalier.
*)

type CombatState =
    | TurnNone
    | TurnAttackerBegin of Attacker : Entity
    | TurnAttackerPlanning of AttackPlan : Plan
    | TurnAttackerFinish of AttackPlan : Plan
    | TurnDefenderBegin of AttackTurn : Plan * Defender : Entity
    | TurnDefenderPlanning of AttackTurn : Plan * DefenceTurn : Plan
    | TurnDefenderFinish of AttackTurn : Plan * DefenceTurn : Plan
    | TurnExecute of AttackTurn : Plan * DefenceTurn : Plan
    | Won of Winner : Entity
    | Done of Winner : Entity

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Combat = {
    CombatTime : Int64
    CombatState : CombatState

    FirstRun : Boolean

    Turn : Int32
    Combatants : List<Entity>
    History : Map<Entity, List<Turn>>
    Area : Entity

    DisplayLeftEntity : Option<Entity>
    DisplayLeftModel : Option<Character>
    DisplayRightEntity : Option<Entity>
    DisplayRightModel : Option<Character>
}
with
    // this represents the gameplay model in a vacant state, such as when the gameplay screen is not selected.
    static member empty = {
        CombatTime = 0L
        CombatState = TurnNone

        FirstRun = false

        Turn = 0
        Combatants = []
        History = Map.empty
        Area = Entity

        DisplayLeftEntity = None
        DisplayLeftModel = None
        DisplayRightEntity = None
        DisplayRightModel = None
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Combat.empty with
            DisplayLeftEntity = Some (Simulants.GameplayCharacters / CharacterContent.player.ID)
            DisplayRightEntity = Some (Simulants.GameplayCharacters / CharacterContent.rat.ID)
    }

    // this updates the gameplay model every frame that gameplay is active.
    static member update model world =

        let leftCharacter =
            match model.DisplayLeftEntity with
            | Some entity -> Some (entity.GetCharacter world)
            | None -> None

        let rightCharacter =
            match model.DisplayRightEntity with
            | Some entity -> Some (entity.GetCharacter world)
            | None -> None

        { model with DisplayLeftModel = leftCharacter; DisplayRightModel = rightCharacter }

    static member orderCombatantsByInitiative model world =
        let combatants =
            model.Combatants
            |> List.sortBy (fun (entity : Entity) ->
                entity.GetCharacterWith Character.getInitiative world
            )
            |> List.rev
        let model = { model with Combatants = combatants }
        model

    static member advanceTurn turns' combat =

        let history =
            combat.History
            |> Map.map (fun entity turns ->
                match turns' |> List.tryFind (fun (turn : Turn) -> entity = turn.Entity) with
                | Some turn ->
                    turn::turns
                | None ->
                    turns
            )

        let attacker = List.head combat.Combatants
        let combatants = List.tail combat.Combatants

        let model = {
            combat with
                Combatants = combatants @ [attacker]
                Turn = combat.Turn + 1
                History = history
        }

        model

    static member updatePlan updater model =
        match model.CombatState with
        | TurnAttackerPlanning plan ->
            { model with CombatState = TurnAttackerPlanning (updater plan) }
        | TurnDefenderPlanning (attack, plan) ->
            { model with CombatState = TurnDefenderPlanning (attack, updater plan) }
        | _ ->
            model


[<AutoOpen>]
module CombatExtensions =
    type Entity with
        member this.GetCombat world = this.GetModelGeneric<Combat> world
        member this.SetCombat value world = this.SetModelGeneric<Combat> value world
        member this.Combat = this.ModelGeneric<Combat> ()
        member this.ExecuteGameEffect effect world =
            match effect with
            | CharacterDo (entity, func) ->
                let world = entity.SetCharacterWith func world
                world

            | Damage (entity, size, damage) ->
                let func character =
                    let sizeDifference = Character.getSize character - size
                    let character = Character.doDamage sizeDifference damage character
                    character
                let world = entity.SetCharacterWith func world
                world

            | TravelInter (area, character, location) ->
                let world = area.SetAreaWith (Area.moveSite character location >> Area.iterateDisplay 10) world
                world

            | TravelIntra (area, character, location, distance) ->
                let world = area.SetAreaWith (Area.establishDistance distance character location >> Area.iterateDisplay 10) world
                world
        member this.ExecuteGameEffects effects world =
            List.fold (fun world effect -> this.ExecuteGameEffect effect world) world effects