namespace Tehom

open System
open System.Numerics
open Prime
open Nu

module Random =
    let rollDie () = Gen.random2 1 7
    let rollDice count =
        Seq.init count (fun _ -> rollDie ())
    let rollDiceThreshold count threshold =
        rollDice count
        |> Seq.fold (fun state x -> if x > threshold then state + 1 else state) 0

    let getItemsFromList length list =
        let gen () = Gen.randomItem list
        List.init length (fun _ -> gen ())

    let getItemFromList list =
        Gen.randomItem list

    let rollInitiative max =
        Gen.random2 0 max

module Weapon =
    type Type =
        | Small
        | Medium
        | Large
(*
    type Requirement =
        | True
        | And of Requirement * Requirement
        | Or of Requirement * Requirement
        | Not of Requirement
        | HasLimb of string
*)
    type Weapon = {
        Type : Type
        Name : String
        Damage : Int32
        Range : UInt32
        Reload : Int32
        ReloadLeft : Int32
        RequiredLimb : String
    }
    with
        static member empty = {
            Type = Small
            Name = String.empty
            Damage = 0
            Range = 0u
            Reload = 0
            ReloadLeft = 0
            RequiredLimb = String.empty
        }

        // only humans attack with fists
        static member fist limb = {
            Weapon.empty with
                Type = Small
                Name = $"Fist Strike ({limb})"
                Damage = 3
                RequiredLimb = limb
        }

        // only humans kick
        static member kick limb = {
            Weapon.empty with
                Type = Medium
                Name = $"Kick Strike ({limb})"
                Damage = 4
                RequiredLimb = limb
        }

        // charactertics differ a lot based on body stats
        static member bite limb = {
            Weapon.empty with
                Type = Small
                Name = "Bite"
                Damage = 5
                RequiredLimb = limb
        }

        // charactertics differ a lot based on body stats
        static member claw limb = {
            Weapon.empty with
                Type = Small
                Name = "Claw Swipe"
                Damage = 5
                RequiredLimb = limb
        }

        static member getRange weapon = weapon.Range
        static member getDamage weapon = weapon.Damage
        static member getSizeBoost weapon =
            match weapon.Type with
            | Small -> -1
            | Medium -> 0
            | Large -> 1


type Weapon = Weapon.Weapon

module Move =

    type Move =
        | Block
        | Burst
        | Cast
        | Climb
        | Crawl
        | Crouch
        | Dash
        | Delay
        | Dodge
        | Fire
        | Grab
        | Jump
        | Knockout
        | Power
        | Press
        | Ready
        | Retarget
        | Roll
        | Sidestep
        | Slam
        | Spin
        | Stride
        | Strike of Weapon
        | Sweep
        | Swim
        | Toss
        | Throw
    with
        static member positioning = [
            Climb; Crawl; Crouch; Dash; Jump; Roll; Sidestep; Stride; Swim;
        ]
        static member attacks = [
            Fire; Grab; Knockout; Power; Press; Retarget; Slam; Strike Weapon.empty; Throw; Toss;
        ]
        static member defence = [
            Block; Crouch; Dodge; Jump; Roll; Spin;
        ]
        static member special = [
            Burst; Ready; Sweep;
        ]
        static member getName move =
            match move with
            | Strike weapon -> weapon.Name
            | x -> $"{x}"

type Element =
    | Gall // Bile
    | Lymph
    | Oil
    | Plasma // Hema
    // GLOPS
    // HOBLS

type CheckType =
    | NoType
    | PhysicalActive
    | PhysicalReactive
    | MentalActive
    | MentalReactive
with
    static member getOpposite checkType =
        match checkType with
        | NoType -> NoType
        | PhysicalActive -> PhysicalReactive
        | PhysicalReactive -> PhysicalActive
        | MentalActive -> MentalReactive
        | MentalReactive -> MentalActive

type Move = Move.Move

// TODO: figure out where this should be
type Stance = {
    GallStance : Int32
    LymphStance : Int32
    OilStance : Int32
    PlasmaStance : Int32
}
with
    static member empty = {
        GallStance = 0
        LymphStance = 0
        OilStance = 0
        PlasmaStance = 0
    }

    static member getStats stance =
        stance.GallStance, stance.LymphStance, stance.OilStance, stance.PlasmaStance

    static member make toStat fromStats (stance : Stance) =

        let moveOne toStat fromStat stance =
            let stance : Stance =
                match fromStat with
                | Gall -> { stance with GallStance = stance.GallStance - 1 }
                | Lymph -> { stance with LymphStance = stance.LymphStance - 1 }
                | Oil -> { stance with OilStance = stance.OilStance - 1 }
                | Plasma-> { stance with PlasmaStance = stance.PlasmaStance - 1 }

            let stance : Stance =
                match toStat with
                | Gall -> { stance with GallStance = stance.GallStance + 1 }
                | Lymph -> { stance with LymphStance = stance.LymphStance + 1 }
                | Oil -> { stance with OilStance = stance.OilStance + 1 }
                | Plasma-> { stance with PlasmaStance = stance.PlasmaStance + 1 }

            stance

        List.init (List.length fromStats) (fun _ -> toStat)
        |> List.zip fromStats
        |> List.fold (fun stance (fromStat, toStat) -> moveOne toStat fromStat stance) stance

    // btw thinking with high enough air you should be able to tell enemy's stance
    // TODO: implement correct stance selection once skills are implemented
    static member attacker =
        Stance.make Gall [Lymph; Oil; Plasma] Stance.empty
    static member defender =
        Stance.make Lymph [Gall; Oil; Plasma] Stance.empty

module Action =
    type Action =
        | NoAction
        | FullMentalAction
        | FullPhysicalAction
        | SkillSelect of int
        | StanceChange of Stance
        | RollStance
        | KarmaBet of int
        | Move of Move

    with
        static member describe action =
            match action with
            | NoAction -> failwith "todo"
            | FullMentalAction -> failwith "todo"
            | FullPhysicalAction -> failwith "todo"
            | StanceChange stance -> $"Stance {Stance.getStats stance}"
            | Move moves -> $"{moves |> Move.getName}"
            | SkillSelect i -> $"Betting {i} Karma"
            | KarmaBet i -> $"Betting {i} Karma"
            | RollStance -> "Rolling Stats"


    type CustomAction = {
        Name : String
        Actions : List<Action>
    }

type Cost = {
    StaminaMentalActive : Int32
    StaminaPhysicalActive : Int32
    StaminaMentalReactive : Int32
    StaminaPhysicalReactive : Int32
    ActionsPhysicalActive : Int32
    ActionsMentalActive : Int32
    ActionsPhysicalReactive : Int32
    ActionsMentalReactive : Int32
    Stances : Int32
}
with
    static member empty = {
        StaminaMentalActive = 0
        StaminaPhysicalActive = 0
        StaminaMentalReactive = 0
        StaminaPhysicalReactive = 0
        ActionsPhysicalActive = 0
        ActionsMentalActive = 0
        ActionsPhysicalReactive = 0
        ActionsMentalReactive = 0
        Stances = 0
    }

type Action = Action.Action

module Character =

    type Wounds =
        | Healthy = 0
        | Hurt = 1
        | Bruised = 2
        | Wounded = 3
        | Injured = 4
        | Critical = 5
        | Down = 6
        | Dying = 7
        | Dead = 8

    type Stat =
        | Crippled = 0
        | Feeble = 1
        | Poor = 2
        | Average = 3
        | Gifted = 4
        | Superior = 5
        | Exceptional = 6
        | Transcendent = 7
        | Divine = 8
        | Impossible = 9
    // C F P A S G E T D I

    type Stats = Stat * Stat * Stat * Stat

    type Character = {
        ID : String
        Name : String
        // Static stats

        Gall : Stat
        Lymph : Stat
        Oil : Stat
        Plasma : Stat

        ActionsPhysicalActiveBase : Int32
        ActionsMentalActiveBase : Int32
        ActionsPhysicalReactiveBase : Int32
        ActionsMentalReactiveBase : Int32

        StancesBase : Int32
        FractureBase : Int32

        Edges : List<Stat>
        CustomActions : List<Action.CustomAction>

        // depends on body type
        Body : Body
        Weapons : List<Weapon>
        Items : List<String>

        // Dynamic stats
        Wounds : Wounds
        Injuries : Int32

        Initiative : Int32

        StaminaPhysicalActiveBase : Int32
        StaminaMentalActiveBase : Int32
        StaminaPhysicalReactiveBase : Int32
        StaminaMentalReactiveBase : Int32

        // TODO: worth considering NOT having limits on the number of actions, relying on stamina only
        StaminaPhysicalActiveCurrent : Int32
        StaminaMentalActiveCurrent : Int32
        StaminaPhysicalReactiveCurrent : Int32
        StaminaMentalReactiveCurrent : Int32

        ActionsPhysicalActiveCurrent : Int32
        ActionsMentalActiveCurrent : Int32
        ActionsPhysicalReactiveCurrent : Int32
        ActionsMentalReactiveCurrent : Int32

        StancesCurrent : Int32
        FractureCurrent : Int32

        Stance : Stance

    }
    with
        static member getElement element stats =
            let (gall, lymph, oil, plasma) = stats
            match element with
            | Gall -> gall
            | Lymph -> lymph
            | Oil -> oil
            | Plasma -> plasma

        static member statsEmpty : Stats = Stat.Crippled, Stat.Crippled, Stat.Crippled, Stat.Crippled

        static member stanceVerify (stance : Stance) (stats : Stats) =
            let (gall, lymph, oil, plasma) = stats
            let (gallChange, lymphChange, oilChange, plasmaChange) = Stance.getStats stance
            gallChange + lymphChange + oilChange + plasmaChange |> int = 0
            && - int gallChange < int gall
            && - int lymphChange < int lymph
            && - int oilChange < int oil
            && - int plasmaChange < int plasma

        static member setStance (stance : Stance) character =
            let verified = Character.stanceVerify stance (Character.getStats character)
            if (verified) then
                { character with Stance = stance }
            else
                character

        static member removeFracture remove (character : Character) =
            let fracture = max 0 (character.FractureCurrent - remove)
            let character = {
                character with
                    FractureCurrent = fracture
            }
            character

        static member getMaxInitiative character =
            let (gall, _, _, plasma) = Character.getStats character
            int gall + int plasma

        static member getInitiative character =
            character.Initiative

        static member setInitiative initiative character =
            { character with Initiative = initiative }

        static member rollInitiative character =
            let maxInitiative = Character.getMaxInitiative character
            let initiative = Random.rollInitiative maxInitiative
            { character with Initiative = initiative }

        static member resetTurn (character : Character) =
            let character = Character.setStance Stance.empty character
            let character = {
                character with
                    StaminaMentalActiveCurrent = 0
                    StaminaPhysicalActiveCurrent = 0
                    StaminaMentalReactiveCurrent = 0
                    StaminaPhysicalReactiveCurrent = 0
                    ActionsPhysicalActiveCurrent = character.ActionsPhysicalActiveBase
                    ActionsMentalActiveCurrent = character.ActionsMentalActiveBase
                    ActionsPhysicalReactiveCurrent = character.ActionsPhysicalReactiveBase
                    ActionsMentalReactiveCurrent = character.ActionsMentalReactiveBase
                    StancesCurrent = character.StancesBase
            }
            character

        static member resetCombat (character : Character) =
            let character = Character.resetTurn character
            let character = Character.rollInitiative character
            let character = {
                character with
                    FractureCurrent = character.FractureBase
            }
            character

        static member canPay (cost : Cost) character =
            character.StaminaMentalActiveCurrent >= cost.StaminaMentalActive
            && character.StaminaPhysicalActiveCurrent >= cost.StaminaPhysicalActive
            && character.StaminaMentalReactiveCurrent >= cost.StaminaMentalReactive
            && character.StaminaPhysicalReactiveCurrent >= cost.StaminaPhysicalReactive
            && character.ActionsPhysicalActiveCurrent >= cost.ActionsPhysicalActive
            && character.ActionsMentalActiveCurrent >= cost.ActionsMentalActive
            && character.ActionsPhysicalReactiveCurrent >= cost.ActionsPhysicalReactive
            && character.ActionsMentalReactiveCurrent >= cost.ActionsMentalReactive
            && character.StancesCurrent >= cost.Stances

        static member pay (cost : Cost) character =
            let character = {
                character with
                    StaminaMentalActiveCurrent = character.StaminaMentalActiveCurrent - cost.StaminaMentalActive
                    StaminaPhysicalActiveCurrent = character.StaminaPhysicalActiveCurrent - cost.StaminaPhysicalActive
                    StaminaMentalReactiveCurrent = character.StaminaMentalReactiveCurrent - cost.StaminaMentalReactive
                    StaminaPhysicalReactiveCurrent = character.StaminaPhysicalReactiveCurrent - cost.StaminaPhysicalReactive
                    ActionsPhysicalActiveCurrent = character.ActionsPhysicalActiveCurrent - cost.ActionsPhysicalActive
                    ActionsMentalActiveCurrent = character.ActionsMentalActiveCurrent - cost.ActionsMentalActive
                    ActionsPhysicalReactiveCurrent = character.ActionsPhysicalReactiveCurrent - cost.ActionsPhysicalReactive
                    ActionsMentalReactiveCurrent = character.ActionsMentalReactiveCurrent - cost.ActionsMentalReactive
                    StancesCurrent = character.StancesCurrent - cost.Stances
            }
            character

        static member getStats character : Stats =
            character.Gall, character.Lymph, character.Oil, character.Plasma

        static member getStance character : Stance =
            character.Stance

        static member getStancedStats character : Stats =
            let (gall, lymph, oil, plasma) = Character.getStats character
            let (gallChange, lymphChange, oilChange, plasmaChange) =
                Character.getStance character
                |> Stance.getStats
            gall + enum gallChange, lymph + enum lymphChange, oil + enum oilChange, plasma + enum plasmaChange

        static member getStat element character =
            character
            |> Character.getStats
            |> Character.getElement element
            |> uint32

        static member getStancedStat element character =
            character
            |> Character.getStancedStats
            |> Character.getElement element
            |> uint32

        static member doDamage size damage character =
            let (_, lymph, oil, _) = Character.getStats character

            let lymph = int lymph
            let oil = int oil

            let check mult =
                (mult + size) * lymph > damage

            let wounds =
                if check 1 then
                    None
                elif check 2 then
                    if (character.Injuries < oil) then
                        Some 0
                    else
                        Some 1
                else
                    if check 3 then
                        Some 1
                    elif check 4 then
                        Some 2
                    elif check 5 then
                        Some 3
                    elif check 6 then
                        Some 4
                    elif check 7 then
                        Some 5
                    else
                        Some 6

            let maxDamage newValue =
                 Wounds.Dead
                 |> int
                 |> min newValue
                 |> enum

            match wounds with
            | None ->
                character
            | Some 0 ->
                { character with Injuries = character.Injuries + 1 }
            | Some wounds ->
                { character with Wounds = maxDamage (wounds + int character.Wounds) }

        static member getSize character =
            character.Body.Size

        static member getSpeed character =
            let multiplier = Body.Gait.multiplier character.Body.Gait
            let baseSpeed =
                // mouse to beagle
                if character.Body.Size <= -2 then
                    150
                // human child
                elif character.Body.Size = -1 then
                    300
                // human
                elif character.Body.Size = 0 then
                    600
                // bear
                elif character.Body.Size = 1 then
                    900
                // elephant
                elif character.Body.Size = 2 then
                    1200
                // blue whale
                elif character.Body.Size = 3 then
                    1500
                //
                elif character.Body.Size = 4 then
                    1800
                // leviathan
                else
                    2100

            float baseSpeed * multiplier |> uint32

        static member getReach character =
            if character.Body.Size <= -2 then
                25u
            // human child
            elif character.Body.Size = -1 then
                50u
            // human
            elif character.Body.Size = 0 then
                75u
            // bear
            elif character.Body.Size = 1 then
                100u
            // elephant
            elif character.Body.Size = 2 then
                150u
            // blue whale
            elif character.Body.Size = 3 then
                200u
            //
            elif character.Body.Size = 4 then
                300u
            // leviathan
            else
                400u

        static member getRange weapon character =
            let reach = Character.getReach character
            let weapon = Weapon.getRange weapon
            max reach weapon

        static member canAct character =
            character.Wounds < Wounds.Down

        static member isDead character =
            character.Wounds = Wounds.Dead

        static member isDamaged character =
            let (_, _, oil, _) = Character.getStats character
            let oil = int oil
            character.Injuries >= oil || character.Wounds > Wounds.Wounded

        static member getWeapons character =
            character.Weapons

        static member hasCustomActions character =
            not (List.isEmpty character.CustomActions)

        static member roll character =
            let (gall, lymph, oil, plasma) = Character.getStancedStats character
            let character = {
                character with
                    StaminaPhysicalActiveBase = Random.rollDiceThreshold (int gall) 3
                    StaminaPhysicalReactiveBase = Random.rollDiceThreshold (int lymph) 3
                    StaminaMentalActiveBase = Random.rollDiceThreshold (int oil) 3
                    StaminaMentalReactiveBase = Random.rollDiceThreshold (int plasma) 3
            }
            let character = {
                character with
                    StaminaPhysicalActiveCurrent = character.StaminaPhysicalActiveBase
                    StaminaPhysicalReactiveCurrent = character.StaminaPhysicalReactiveBase
                    StaminaMentalActiveCurrent = character.StaminaMentalActiveBase
                    StaminaMentalReactiveCurrent = character.StaminaMentalReactiveBase
            }
            character

        static member getStamina actionType character =
            match actionType with
            | PhysicalActive -> character.StaminaPhysicalActiveBase
            | PhysicalReactive -> character.StaminaPhysicalReactiveBase
            | MentalActive -> character.StaminaMentalActiveBase
            | MentalReactive -> character.StaminaMentalReactiveBase
            | NoType -> 0

        static member addItem item character =
            let items = character.Items
            let character = {
                character with
                    Items = item::items
            }
            character

        static member empty = {
            ID = String.empty
            Name = String.empty
            Gall = Stat.Crippled
            Lymph = Stat.Crippled
            Oil = Stat.Crippled
            Plasma = Stat.Crippled

            Stance = Stance.empty

            Body = Body.empty
            Weapons = []
            Items = []

            Edges = []
            CustomActions = []

            StaminaMentalActiveBase = 0
            StaminaPhysicalActiveBase = 0
            StaminaMentalReactiveBase = 0
            StaminaPhysicalReactiveBase = 0

            StaminaMentalActiveCurrent = 0
            StaminaPhysicalActiveCurrent = 0
            StaminaMentalReactiveCurrent = 0
            StaminaPhysicalReactiveCurrent = 0

            ActionsPhysicalReactiveBase = 0
            ActionsMentalReactiveBase = 0
            ActionsPhysicalReactiveCurrent = 0
            ActionsMentalReactiveCurrent = 0

            ActionsPhysicalActiveBase = 0
            ActionsPhysicalActiveCurrent = 0
            ActionsMentalActiveBase = 0
            ActionsMentalActiveCurrent = 0

            StancesBase = 0
            StancesCurrent = 0
            FractureBase = 0
            FractureCurrent = 0

            Wounds = Wounds.Healthy
            Injuries = 0

            Initiative = 0
        }

type Character = Character.Character