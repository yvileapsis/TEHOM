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
        count / 2
        //rollDice count
        //|> Seq.fold (fun state x -> if x > threshold then state + 1 else state) 0

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

    // Used for whether attacking with said weapon is a strike or a fire
    type RangeType =
        | Melee
        | Ranged
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
        RangeType : RangeType
        Name : String
        Damage : Int32
        Range : UInt32

        Ready : Int32
        ReadyLeft : Int32
        Reload : Int32
        ReloadLeft : Int32
        Uses : Int32
        UsesLeft : Int32

        RequiredLimb : String
    }
    with
        static member empty = {
            Type = Small
            RangeType = Melee
            Name = String.empty
            Damage = 0
            Range = 0u

            Ready = 0
            ReadyLeft = 0
            Reload = 0
            ReloadLeft = 0
            Uses = 0
            UsesLeft = 0

            RequiredLimb = String.empty
        }

        // only humans attack with fists
        static member fist limb = {
            Weapon.empty with
                Type = Small
                Name = $"Fist Strike ({limb})"
                Damage = 3
                RequiredLimb = limb
                Ready = 1
        }

        // only humans kick
        static member kick limb = {
            Weapon.empty with
                Type = Medium
                Name = $"Kick Strike ({limb})"
                Damage = 4
                RequiredLimb = limb
                Ready = 2
        }

        // charactertics differ a lot based on body stats
        static member bite limb = {
            Weapon.empty with
                Type = Small
                Name = "Bite"
                Damage = 5
                RequiredLimb = limb
                Ready = 1
        }

        // charactertics differ a lot based on body stats
        static member claw limb = {
            Weapon.empty with
                Type = Small
                Name = "Claw Swipe"
                Damage = 5
                RequiredLimb = limb
                Ready = 1
        }

        static member pistol limb = {
            Weapon.empty with
                Type = Small
                Name = "Glock 22"
                Damage = 22
                RequiredLimb = limb
                Range = 300u
                Ready = 1
                Reload = 3
                Uses = 15
                UsesLeft = 15
        }

        static member getRange weapon = weapon.Range
        static member getDamage weapon = weapon.Damage
        static member getSizeBoost weapon =
            match weapon.Type with
            | Small -> -1
            | Medium -> 0
            | Large -> 1

        static member isRanged weapon =
            weapon.RangeType = RangeType.Ranged

type Weapon = Weapon.Weapon


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
    | Fire of Weapon
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
        Fire Weapon.empty; Grab; Knockout; Power; Press; Retarget; Slam; Strike Weapon.empty; Throw; Toss;
    ]

    static member defence = [
        Block; Crouch; Dodge; Jump; Roll; Spin;
    ]

    static member special = [
        Burst; Ready; Sweep;
    ]

    static member isAttack move =
        match move with
        | Fire _
        | Grab
        | Knockout
        | Power
        | Press
        | Retarget
        | Slam
        | Strike _
        | Throw
        | Toss  -> true
        | _ -> false

    static member isDefence move =
        match move with
        | Block
        | Crouch
        | Dodge
        | Jump
        | Roll
        | Spin -> true
        | _ -> false

    static member isDefenceNotPositioning move =
        match move with
        | Block
        | Dodge -> true
        | _ -> false

    static member isPositioning move =
        match move with
        | Climb
        | Crawl
        | Crouch
        | Dash
        | Jump
        | Roll
        | Sidestep
        | Stride
        | Swim -> true
        | _ -> false

    static member getName move =
        match move with
        | Strike weapon -> weapon.Name
        | x -> $"{x}"

    static member getRange move =
        match move with
        | Fire weapon ->
            Weapon.getRange weapon
        | Strike weapon ->
            Weapon.getRange weapon
        | _ ->
            0u

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

type Stats = {
    Gall : Int32
    Lymph : Int32
    Oil : Int32
    Plasma : Int32
}
with
    static member empty = {
        Gall = 0
        Lymph = 0
        Oil = 0
        Plasma = 0
    }

    static member getStats stance =
        stance.Gall, stance.Lymph, stance.Oil, stance.Plasma

    static member getElement element stats =
        match element with
        | Gall -> stats.Gall
        | Lymph -> stats.Lymph
        | Oil -> stats.Oil
        | Plasma -> stats.Plasma

    static member gall = { Stats.empty with Gall = 1 }
    static member lymph = { Stats.empty with Lymph = 1 }
    static member oil = { Stats.empty with Oil = 1 }
    static member plasma = { Stats.empty with Plasma = 1 }

    static member average = {
        Gall = 3
        Lymph = 3
        Oil = 3
        Plasma = 3
    }

    static member gifted = {
        Gall = 4
        Lymph = 4
        Oil = 4
        Plasma = 4
    }

    static member verify (stance : Stats) (stats : Stats) =
        let (gall, lymph, oil, plasma) = Stats.getStats stats
        let (gallChange, lymphChange, oilChange, plasmaChange) = Stats.getStats stance
        gallChange + lymphChange + oilChange + plasmaChange |> int = 0
        && - int gallChange < int gall
        && - int lymphChange < int lymph
        && - int oilChange < int oil
        && - int plasmaChange < int plasma

    static member (+) (stamina, stamina') = {
        stamina with
            Gall = stamina.Gall + stamina'.Gall
            Lymph = stamina.Lymph + stamina'.Lymph
            Oil = stamina.Oil + stamina'.Oil
            Plasma = stamina.Plasma + stamina'.Plasma
    }

    static member (-) (stamina, stamina') = {
        stamina with
            Gall = stamina.Gall - stamina'.Gall
            Lymph = stamina.Lymph - stamina'.Lymph
            Oil = stamina.Oil - stamina'.Oil
            Plasma = stamina.Plasma - stamina'.Plasma
    }

    static member make toStat fromStats (stance : Stats) =

        let moveOne toStat fromStat stance =
            let stance : Stats =
                match fromStat with
                | Gall -> { stance with Gall = stance.Gall - 1 }
                | Lymph -> { stance with Lymph = stance.Lymph - 1 }
                | Oil -> { stance with Oil = stance.Oil - 1 }
                | Plasma-> { stance with Plasma = stance.Plasma - 1 }

            let stance : Stats =
                match toStat with
                | Gall -> { stance with Gall = stance.Gall + 1 }
                | Lymph -> { stance with Lymph = stance.Lymph + 1 }
                | Oil -> { stance with Oil = stance.Oil + 1 }
                | Plasma-> { stance with Plasma = stance.Plasma + 1 }

            stance

        List.init (List.length fromStats) (fun _ -> toStat)
        |> List.zip fromStats
        |> List.fold (fun stance (fromStat, toStat) -> moveOne toStat fromStat stance) stance

    // btw thinking with high enough air you should be able to tell enemy's stance
    // TODO: implement correct stance selection once skills are implemented
    static member attacker =
        Stats.make Gall [Lymph; Oil; Plasma] Stats.empty
    static member defender =
        Stats.make Lymph [Gall; Oil; Plasma] Stats.empty

type Stance = Stats

type Stamina = {
    MentalActive : Int32
    PhysicalActive : Int32
    MentalReactive : Int32
    PhysicalReactive : Int32
}
with
    static member empty = {
        MentalActive = 0
        PhysicalActive = 0
        MentalReactive = 0
        PhysicalReactive = 0
    }

    static member mentalActive = { Stamina.empty with MentalActive = 1 }
    static member physicalActive = { Stamina.empty with PhysicalActive = 1 }
    static member mentalReactive = { Stamina.empty with MentalReactive = 1 }
    static member physicalReactive = { Stamina.empty with PhysicalReactive = 1 }

    static member sum stamina =
        stamina.MentalActive + stamina.PhysicalActive + stamina.MentalReactive + stamina.PhysicalReactive

    static member (+) (stamina, stamina') = {
        stamina with
            MentalActive = stamina.MentalActive + stamina'.MentalActive
            PhysicalActive = stamina.PhysicalActive + stamina'.PhysicalActive
            MentalReactive = stamina.MentalReactive + stamina'.MentalReactive
            PhysicalReactive = stamina.PhysicalReactive + stamina'.PhysicalReactive
    }

    static member (-) (stamina, stamina') = {
        stamina with
            MentalActive = stamina.MentalActive - stamina'.MentalActive
            PhysicalActive = stamina.PhysicalActive - stamina'.PhysicalActive
            MentalReactive = stamina.MentalReactive - stamina'.MentalReactive
            PhysicalReactive = stamina.PhysicalReactive - stamina'.PhysicalReactive
    }

type Action =
    | NoAction
    | FullMentalAction
    | FullPhysicalAction
    | SkillSelect of int
    | StanceChange of Stance
    | RollStance
    | Fracture of Stamina
    | Move of Move

with
    static member describe action =
        match action with
        | NoAction -> failwith "todo"
        | FullMentalAction -> failwith "todo"
        | FullPhysicalAction -> failwith "todo"
        | StanceChange stance -> $"Stance {Stats.getStats stance}"
        | Move moves -> $"{moves |> Move.getName}"
        | SkillSelect i -> $"Betting {i} Karma"
        | Fracture i -> $"Betting {i.PhysicalActive} {i.PhysicalReactive} {i.MentalActive} {i.MentalReactive} Karma"
        | RollStance -> "Rolling Stats"

type CustomAction = {
    Name : String
    Actions : List<Action>
}

type Cost = {
    Stamina : Stamina
    ActionsPhysicalActive : Int32
    ActionsMentalActive : Int32
    ActionsPhysicalReactive : Int32
    ActionsMentalReactive : Int32
    Stances : Int32
}
with
    static member empty = {
        Stamina = Stamina.empty
        ActionsPhysicalActive = 0
        ActionsMentalActive = 0
        ActionsPhysicalReactive = 0
        ActionsMentalReactive = 0
        Stances = 0
    }

    static member (+) (cost, cost') = {
        cost with
            Stamina = cost.Stamina + cost'.Stamina
            ActionsPhysicalActive = cost.ActionsPhysicalActive + cost'.ActionsPhysicalActive
            ActionsMentalActive = cost.ActionsMentalActive + cost'.ActionsMentalActive
            ActionsPhysicalReactive = cost.ActionsPhysicalReactive + cost'.ActionsPhysicalReactive
            ActionsMentalReactive = cost.ActionsMentalReactive + cost'.ActionsMentalReactive
            Stances = cost.Stances + cost'.Stances
    }

    static member (-) (cost, cost') = {
        cost with
            Stamina = cost.Stamina - cost'.Stamina
            ActionsPhysicalActive = cost.ActionsPhysicalActive - cost'.ActionsPhysicalActive
            ActionsMentalActive = cost.ActionsMentalActive - cost'.ActionsMentalActive
            ActionsPhysicalReactive = cost.ActionsPhysicalReactive - cost'.ActionsPhysicalReactive
            ActionsMentalReactive = cost.ActionsMentalReactive - cost'.ActionsMentalReactive
            Stances = cost.Stances - cost'.Stances
    }

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

type Character = {
    ID : String
    Name : String
    // Static stats

    Stats : Stats

    ActionsPhysicalActiveBase : Int32
    ActionsMentalActiveBase : Int32
    ActionsPhysicalReactiveBase : Int32
    ActionsMentalReactiveBase : Int32

    StancesBase : Int32
    FractureBase : Int32

    Edges : List<Stat>
    CustomActions : List<CustomAction>

    // depends on body type
    Body : Body
    Weapons : List<Weapon>
    Items : List<String>

    // Dynamic stats
    Wounds : Wounds
    Injuries : Int32

    Initiative : Int32

    StaminaBase : Stamina

    // TODO: worth considering NOT having limits on the number of actions, relying on stamina only
    StaminaCurrent : Stamina

    ActionsPhysicalActiveCurrent : Int32
    ActionsMentalActiveCurrent : Int32
    ActionsPhysicalReactiveCurrent : Int32
    ActionsMentalReactiveCurrent : Int32

    StancesCurrent : Int32
    FractureCurrent : Int32

    Stance : Stance

}
with

    static member setStance (stance : Stats) character =
        let verified = Stats.verify stance (Character.getStats character)
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

    static member spendFractureOnStamina stamina (character : Character) =
        let count = Stamina.sum stamina
        let character = Character.removeFracture count character
        { character with StaminaCurrent = character.StaminaCurrent + stamina }

    static member getMaxInitiative character =
        let (gall, _, _, plasma) =
            character
            |> Character.getStats
            |> Stats.getStats

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
        let character = Character.setStance Stats.empty character
        let character = {
            character with
                StaminaCurrent = Stamina.empty
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
        character.StaminaCurrent.MentalActive >= cost.Stamina.MentalActive
        && character.StaminaCurrent.MentalReactive >= cost.Stamina.MentalReactive
        && character.StaminaCurrent.PhysicalActive >= cost.Stamina.PhysicalActive
        && character.StaminaCurrent.PhysicalReactive >= cost.Stamina.PhysicalReactive
        && character.ActionsMentalActiveCurrent >= cost.ActionsMentalActive
        && character.ActionsPhysicalReactiveCurrent >= cost.ActionsPhysicalReactive
        && character.ActionsMentalReactiveCurrent >= cost.ActionsMentalReactive
        && character.StancesCurrent >= cost.Stances

    static member canTheoreticallyPay (cost : Cost) character =
        character.Stats.Plasma >= cost.Stamina.MentalActive
        && character.Stats.Oil >= cost.Stamina.MentalReactive
        && character.Stats.Gall >= cost.Stamina.PhysicalActive
        && character.Stats.Lymph >= cost.Stamina.PhysicalReactive
        && character.StancesCurrent >= cost.Stances

    static member verifyStance stance character =
        stance <> Stance.empty
        && Stance.verify stance character.Stats

    static member verifyFracture fracture character =
        fracture <> Stamina.empty
        && Stamina.sum fracture >= character.FractureCurrent

    static member pay (cost : Cost) character =
        let character = {
            character with
                StaminaCurrent = character.StaminaCurrent - cost.Stamina
                ActionsPhysicalActiveCurrent = character.ActionsPhysicalActiveCurrent - cost.ActionsPhysicalActive
                ActionsMentalActiveCurrent = character.ActionsMentalActiveCurrent - cost.ActionsMentalActive
                ActionsPhysicalReactiveCurrent = character.ActionsPhysicalReactiveCurrent - cost.ActionsPhysicalReactive
                ActionsMentalReactiveCurrent = character.ActionsMentalReactiveCurrent - cost.ActionsMentalReactive
                StancesCurrent = character.StancesCurrent - cost.Stances
        }
        character

    static member getStats character : Stats =
        character.Stats

    static member getStance character : Stats =
        character.Stance

    static member getStancedStats character : Stats =
        let stats = Character.getStats character
        let stance = Character.getStance character
        stats + stance

    static member getStat element character =
        character
        |> Character.getStats
        |> Stats.getElement element

    static member getStancedStat element character =
        character
        |> Character.getStancedStats
        |> Stats.getElement element

    static member doDamage size damage character =
        let (_, lymph, oil, _) =
            character
            |> Character.getStats
            |> Stats.getStats

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
        let (_, _, oil, _) =
            character
            |> Character.getStats
            |> Stats.getStats
        character.Injuries >= oil || character.Wounds > Wounds.Wounded

    static member getWeapons character =
        character.Weapons

    static member hasCustomActions character =
        not (List.isEmpty character.CustomActions)

    static member roll character =
        let (gall, lymph, oil, plasma) =
            character
            |> Character.getStancedStats
            |> Stats.getStats
        let stamina = {
            Stamina.empty with
                PhysicalActive = Random.rollDiceThreshold (int gall) 3
                PhysicalReactive = Random.rollDiceThreshold (int lymph) 3
                MentalActive = Random.rollDiceThreshold (int oil) 3
                MentalReactive = Random.rollDiceThreshold (int plasma) 3
        }
        let character = {
            character with
                StaminaBase = stamina
                StaminaCurrent = stamina
        }
        character

    static member getStamina actionType (character : Character) =
        match actionType with
        | PhysicalActive -> character.StaminaBase.PhysicalActive
        | PhysicalReactive -> character.StaminaBase.PhysicalReactive
        | MentalActive -> character.StaminaBase.MentalActive
        | MentalReactive -> character.StaminaBase.MentalReactive
        | NoType -> 0

    static member addItem item character =
        let items = character.Items
        let character = {
            character with
                Items = item::items
        }
        character

    static member addWeapon weapon character =
        let weapons = character.Weapons
        let character = {
            character with
                Weapons = weapon::weapons
        }
        character

    static member getPossibleMoves character = [
        for i in Character.getWeapons character do
            if Weapon.isRanged i then
                Fire i
            else
                Strike i

        Power; Press

        Stride

        Block; Crouch; Dodge; Jump; Roll; Spin;
    ]

    static member getCoveredDistance actions character =
        let speed = Character.getSpeed character

        let distance =
            actions
            |> List.fold (fun distance action ->
                match action with
                | Move Stride ->
                    distance + speed
                | _ ->
                    distance
            ) 0u

        distance

    static member empty = {
        ID = String.empty
        Name = String.empty

        Stats = Stats.empty
        Stance = Stats.empty

        Body = Body.empty
        Weapons = []
        Items = []

        Edges = []
        CustomActions = []

        StaminaBase = Stamina.empty

        StaminaCurrent = Stamina.empty

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