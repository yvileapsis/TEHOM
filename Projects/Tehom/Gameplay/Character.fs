namespace Tehom

open System
open Prime

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
        Name : string
        Damage : int
        Range : uint32
        Reload : int
        ReloadLeft : int
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

type Move = Move.Move

// TODO: figure out where this should be
type Stance = {
    GallStance : int
    LymphStance : int
    OilStance : int
    PlasmaStance : int
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
        | FullMentalAction
        | FullPhysicalAction
        | StanceChange of Stance
        | PhysicalSequence of Move list

    with
        static member describe action =
            match action with
            | FullMentalAction -> failwith "todo"
            | FullPhysicalAction -> failwith "todo"
            | StanceChange stance -> $"Stance Change to {stance}"
            | PhysicalSequence moves -> $"{moves |> List.head |> Move.getName}"


    type CustomAction = {
        Name : String
        Actions : Action list
    }


type Action = Action.Action

module Character =

    type MajorWounds =
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

        PhysicalActions : int
        MentalActions : int
        Stances : int
        Fracture : int

        Edges : Stat list
        CustomActions : Action.CustomAction list

        // depends on body type
        Body : Body
        Weapons : Weapon list

        // Dynamic stats
        MajorWounds : MajorWounds
        MinorWounds : int

        Initiative : int

        PhysicalActionsLeft : int
        MentalActionsLeft : int
        StancesLeft : int
        FractureLeft : int

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

        static member stanceChange (stance : Stance) character =
            let verified = Character.stanceVerify stance (Character.getStats character)
            if (character.StancesLeft > 0 && verified) then
                {
                    character with
                        Stance = stance
                        StancesLeft = character.StancesLeft - 1
                }
            else
                character

        static member turnReset (character : Character) =
            let character = Character.stanceChange Stance.empty character
            let character = {
                character with
                    PhysicalActionsLeft = character.PhysicalActions
                    MentalActionsLeft = character.MentalActions
                    StancesLeft = character.Stances
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
                else if check 2 then
                    if (character.MinorWounds < oil) then
                        Some 0
                    else
                        Some 1
                else
                    if check 3 then
                        Some 1
                    else if check 4 then
                        Some 2
                    else if check 5 then
                        Some 3
                    else if check 6 then
                        Some 4
                    else if check 7 then
                        Some 5
                    else
                        Some 6

            let maxDamage newValue =
                 MajorWounds.Dead
                 |> int
                 |> min newValue
                 |> enum

            match wounds with
            | None ->
                character
            | Some 0 ->
                { character with MinorWounds = character.MinorWounds + 1 }
            | Some wounds ->
                { character with MajorWounds = maxDamage (wounds + int character.MajorWounds) }


        static member getMaxInitiative character =
            let (gall, _, _, plasma) = Character.getStats character
            int gall + int plasma

        static member getInitiative character =
            character.Initiative

        static member setInitiative initiative character =
            { character with Initiative = initiative }

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
            character.MajorWounds < MajorWounds.Down

        static member isDead character =
            character.MajorWounds = MajorWounds.Dead

        static member isDamaged character =
            let (_, _, oil, _) = Character.getStats character
            let oil = int oil
            character.MinorWounds >= oil || character.MajorWounds > MajorWounds.Wounded

        static member getWeapons character =
            character.Weapons

        static member hasCustomActions character =
            not (List.isEmpty character.CustomActions)

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
            Edges = []
            CustomActions = []

            PhysicalActions = 1
            PhysicalActionsLeft = 0
            MentalActions = 1
            MentalActionsLeft = 0
            Stances = 0
            StancesLeft = 0
            Fracture = 0
            FractureLeft = 0

            MajorWounds = MajorWounds.Healthy
            MinorWounds = 0

            Initiative = 0
        }

type Character = Character.Character