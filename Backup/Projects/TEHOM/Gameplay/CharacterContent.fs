namespace Tehom

open Prime
open Nu

module CharacterContent =

    let player = {
        Character.empty with
            ID = "player"
            Name = "Player"
            Stats = Stats.gifted

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
            Stats = Stats.average
            StancesBase = 1
    }

    let rat = {
        enemy with
            Stats = {
                Gall = 4
                Lymph = 3
                Oil = 3
                Plasma = 1
            }

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
                    StanceChange (Stats.make Gall [Plasma; Plasma; Plasma] Stats.empty)
                    RollStance
                    for i in [ Jump; Power; Strike (Weapon.bite "head"); Press ] do Move i
                ] }
                { Name = "Neutral"; Actions = [
                    RollStance
                    for i in [ Strike (Weapon.bite "head"); Press; Press; Press ] do Move i
                ] }
                { Name = "Defence"; Actions = [
                    StanceChange (Stats.make Lymph [Gall; Gall; Gall; Gall; Plasma; Plasma] Stats.empty)
                    RollStance
                    for i in [ Dodge; Dodge; Dodge; Dodge; ] do Move i
                ] }
            ]
    }