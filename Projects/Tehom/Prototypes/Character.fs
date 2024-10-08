namespace Tehom

open System
open Prime

module Character =

    type Element =
        | Gall
        | Lymph
        | Oil
        | Plasma

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

    type Gait =
        | Sluggish
        | Slow
        | Moderate
        | Fast
        | Speedy
    with
        static member multiplier = function
            | Sluggish -> 0.5
            | Slow -> 0.75
            | Moderate -> 1.0
            | Fast -> 1.25
            | Speedy -> 1.5

    type Stats = Stat * Stat * Stat * Stat

    type Stance = int * int * int * int

    type Character = {
        // Static stats
        ID : String
        Name : String

        Gall : Stat
        Lymph : Stat
        Oil : Stat
        Plasma : Stat

        Edges : Stat list

        PhysicalActions : int
        MentalActions : int
        Stances : int
        Fracture : int

        // Dynamic stats
        Damage : int
        Size : int
        Gait : Gait

        MajorWounds : MajorWounds
        MinorWounds : int

        Initiative : int

        GallStance : int
        LymphStance : int
        OilStance : int
        PlasmaStance : int

        PhysicalActionsLeft : int
        MentalActionsLeft : int
        StancesLeft : int
        FractureLeft : int
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
        static member stanceEmpty : Stance = 0, 0, 0, 0
        static member stanceMove toStat fromStat (stance : Stance) =

            let moveOne toStat fromStat stance =
                let (gall, lymph, oil, plasma) = stance

                let stance : Stance =
                    match fromStat with
                    | Gall -> gall - 1, lymph, oil, plasma
                    | Lymph -> gall, lymph - 1, oil, plasma
                    | Oil -> gall, lymph, oil - 1, plasma
                    | Plasma-> gall, lymph, oil, plasma - 1

                let (gall, lymph, oil, plasma) = stance

                let stance : Stance =
                    match toStat with
                    | Gall -> gall + 1, lymph, oil, plasma
                    | Lymph -> gall, lymph + 1, oil, plasma
                    | Oil -> gall, lymph, oil + 1, plasma
                    | Plasma-> gall, lymph, oil, plasma + 1

                stance

            List.zip toStat fromStat
            |> List.fold (fun stance (toStat, fromStat) -> moveOne toStat fromStat stance) stance

        static member stanceVerify (stance : Stance) (stats : Stats) =
            let (gall, lymph, oil, plasma) = stats
            let (gallChange, lymphChange, oilChange, plasmaChange) = stance
            gallChange + lymphChange + oilChange + plasmaChange |> int = 0
            && - int gallChange < int gall
            && - int lymphChange < int lymph
            && - int oilChange < int oil
            && - int plasmaChange < int plasma

        static member stanceChange (stance : Stance) character =
            if (character.StancesLeft > 0) then
                let (gallChange, lymphChange, oilChange, plasmaChange) = stance
                {
                    character with
                        GallStance = gallChange
                        LymphStance = lymphChange
                        OilStance = oilChange
                        PlasmaStance = plasmaChange
                        StancesLeft = character.StancesLeft - 1
                }
            else
                character

        static member turnReset (character : Character) =
            let character = Character.stanceChange Character.stanceEmpty character
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
            character.GallStance, character.LymphStance, character.OilStance, character.PlasmaStance

        static member getStancedStats character : Stats =
            let (gall, lymph, oil, plasma) = Character.getStats character
            let (gallChange, lymphChange, oilChange, plasmaChange) = Character.getStance character
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

        static member getDamage character =
            character.Damage

        static member doDamage damage character =
            let (_, lymph, oil, _) = Character.getStats character

            let lymph = int lymph
            let oil = int oil

            let maxDamage newValue =
                 MajorWounds.Dead
                 |> int
                 |> min newValue
                 |> enum

            if (damage < lymph) then character
            else if (damage < 2 * lymph) then
                if (character.MinorWounds < oil) then
                    { character with MinorWounds = character.MinorWounds + 1 }
                else
                    { character with MajorWounds = maxDamage (1 + int character.MajorWounds) }
            else if (damage < 3 * (int lymph)) then
                { character with MajorWounds = maxDamage (1 + int character.MajorWounds) }
            else if (damage < 4 * lymph) then
                { character with MajorWounds = maxDamage (2 + int character.MajorWounds) }
            else if (damage < 5 * lymph) then
                { character with MajorWounds = maxDamage (3 + int character.MajorWounds) }
            else if (damage < 6 * lymph) then
                { character with MajorWounds = maxDamage (4 + int character.MajorWounds) }
            else if (damage < 7 * lymph) then
                { character with MajorWounds = maxDamage (5 + int character.MajorWounds) }
            else
                { character with MajorWounds = maxDamage (6 + int character.MajorWounds) }

        static member getMaxInitiative character =
            let (gall, _, _, plasma) = Character.getStats character
            int gall, int plasma

        static member getSpeed character =
            let multiplier = Gait.multiplier character.Gait
            let baseSpeed =
                // mouse to beagle
                if character.Size <= -2 then
                    150
                // human child
                elif character.Size = -1 then
                    300
                // human
                elif character.Size = 0 then
                    600
                // bear
                elif character.Size = 1 then
                    900
                // elephant
                elif character.Size = 2 then
                    1200
                // blue whale
                elif character.Size = 3 then
                    1500
                //
                elif character.Size = 4 then
                    1800
                // leviathan
                else
                    2100

            float baseSpeed * multiplier |> uint32

        static member getReach character =
            if character.Size <= -2 then
                25u
            // human child
            elif character.Size = -1 then
                50u
            // human
            elif character.Size = 0 then
                75u
            // bear
            elif character.Size = 1 then
                100u
            // elephant
            elif character.Size = 2 then
                150u
            // blue whale
            elif character.Size = 3 then
                200u
            //
            elif character.Size = 4 then
                300u
            // leviathan
            else
                400u


        static member empty = {
            ID = String.empty
            Name = String.empty
            Gall = Stat.Crippled
            Lymph = Stat.Crippled
            Oil = Stat.Crippled
            Plasma = Stat.Crippled

            GallStance = 0
            LymphStance = 0
            OilStance = 0
            PlasmaStance = 0

            Damage = 0
            Size = 0
            Gait = Moderate

            Edges = []

            PhysicalActions = 0
            PhysicalActionsLeft = 0
            MentalActions = 0
            MentalActionsLeft = 0
            Stances = 0
            StancesLeft = 0
            Fracture = 0
            FractureLeft = 0

            MajorWounds = MajorWounds.Healthy
            MinorWounds = 0

            Initiative = 0
        }

        static member player = {
            Character.empty with
                ID = "player"
                Name = "Player"
                Gall = Stat.Gifted
                Lymph = Stat.Gifted
                Oil = Stat.Gifted
                Plasma = Stat.Gifted

                Damage = 5
                PhysicalActions = 1
                MentalActions = 1
                Stances = 2
        }

        static member enemy = {
            Character.empty with
                ID = "enemy"
                Name = "Enemy"
                Gall = Stat.Average
                Lymph = Stat.Average
                Oil = Stat.Average
                Plasma = Stat.Average

                Damage = 5
                PhysicalActions = 1
                MentalActions = 1
                Stances = 1
        }

        static member rat = {
            Character.enemy with
                Gall = Stat.Gifted
                Lymph = Stat.Average
                Oil = Stat.Average
                Plasma = Stat.Feeble

                ID = "rat"
                Name = "Rat"
                Size = -2
                Gait = Speedy
        }

type Character = Character.Character