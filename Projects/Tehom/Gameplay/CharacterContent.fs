namespace Tehom

open Prime
open Nu
open Character

module CharacterContent =

    let player = {
        Character.empty with
            ID = "player"
            Name = "Player"
            Gall = Stat.Gifted
            Lymph = Stat.Gifted
            Oil = Stat.Gifted
            Plasma = Stat.Gifted

            Body = Body.human

            StancesBase = 2
            FractureBase = 15

            Weapons = [
                Weapon.bite "head"
                Weapon.fist "armLeft"
                Weapon.fist "armRight"
                Weapon.kick "legLeft"
                Weapon.kick "legRight"
            ]
    }

    let enemy = {
        Character.empty with
            ID = "enemy"
            Name = "Enemy"
            Gall = Stat.Average
            Lymph = Stat.Average
            Oil = Stat.Average
            Plasma = Stat.Average
            StancesBase = 1
    }

    let rat = {
        enemy with
            Gall = Stat.Gifted
            Lymph = Stat.Average
            Oil = Stat.Average
            Plasma = Stat.Feeble

            ID = "rat"
            Name = "Rat"

            Body = Body.rat

            Weapons = [
                Weapon.bite "head"
                Weapon.claw "armLeft"
                Weapon.claw "armRight"
            ]

            FractureBase = 5

            CustomActions = [
                { Name = "Aggressive"; Actions = [
                    StanceChange (Stance.make Gall [Plasma; Plasma; Plasma] Stance.empty)
                    RollStance
                    for i in [ Jump; Power; Strike (Weapon.bite "head"); Press ] do Move i
                ] }
                { Name = "Neutral"; Actions = [
                    RollStance
                    for i in [ Strike (Weapon.bite "head"); Press; Press; Press ] do Move i
                ] }
                { Name = "Defence"; Actions = [
                    StanceChange (Stance.make Lymph [Gall; Gall; Gall; Gall; Plasma; Plasma] Stance.empty)
                    RollStance
                    for i in [ Dodge; Dodge; Dodge; Dodge; ] do Move i
                ] }
            ]
    }