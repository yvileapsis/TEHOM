namespace Tehom

open Prime
open Nu
open Move
open Action
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

            Stances = 2

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
            Stances = 1
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

            CustomActions = [
                { Name = "Aggressive"; Actions = [
                    StanceChange (Stance.make Gall [Plasma; Plasma; Plasma] Stance.empty)
                    PhysicalSequence [ Jump; Power; Strike (Weapon.bite "head"); Press ]
                ] }
                { Name = "Neutral"; Actions = [
                    PhysicalSequence [ Strike (Weapon.bite "head"); Press; Press; Press ]
                ] }
                { Name = "Defence"; Actions = [
                    StanceChange (Stance.make Lymph [Gall; Gall; Gall; Gall; Plasma; Plasma] Stance.empty)
                    PhysicalSequence [ Dodge; Dodge; Dodge; Dodge; ]
                ] }
            ]
    }