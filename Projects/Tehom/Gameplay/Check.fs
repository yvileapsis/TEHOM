namespace Tehom

open System
open System.Numerics
open Prime
open Nu

type GameEffect =
    | CharacterDo of Entity * (Character -> Character)
    | Damage of Entity * Size : Int32 * Damage : Int32
    | TravelInter of Area : Entity * String * String
    | TravelIntra of Area : Entity * String * String * UInt32
with
    static member travel actor target closeness distance (area : Entity) world =
        let model = area.GetArea world
        match Area.moveWithinReach actor target closeness distance model with
        | Some (fst, snd), Some (fst', snd', thd') ->
            [TravelInter (area, fst, snd); TravelIntra (area, fst', snd', thd')]
        | Some (fst, snd), None ->
            [TravelInter (area, fst, snd)]
        | None, Some (fst, snd, thd) ->
            [TravelIntra (area, fst, snd, thd)]
        | None, None ->
            []

type CheckState =
    | NotTested
    | Passed
    | Failed

type Check = {
    Order : Int32
    Action : Tehom.Action
    Type : CheckType
    State : CheckState
    Cost : Cost
    Target : Entity option
    OpposedBy : Entity list
}
with
    static member empty = {
        Order = 0
        Action = NoAction
        Type = NoType
        State = NotTested
        Cost = Cost.empty
        Target = None
        OpposedBy = []
    }

    static member unstoppable action = {
        Check.empty with
            Action = action
    }

    static member unopposed action cost = {
        Check.unstoppable action with
            Cost = cost
    }

    static member isUnstoppable check =
        (List.isEmpty check.OpposedBy) && check.Cost = Cost.empty

    static member isUnopposed check =
        List.isEmpty check.OpposedBy

    static member opposed action cost target = {
        Check.unopposed action cost with
            OpposedBy = [target]
    }

    static member targeted action cost target = {
        Check.opposed action cost target with
            Target = Some target
    }

    static member isOpposed check =
        List.notEmpty check.OpposedBy

    static member stance action =
        Check.unopposed action { Cost.empty with Stances = 1}

    static member moveAttack action target =
        let cost = { Cost.empty with Stamina = Stamina.physicalActive }
        { Check.targeted action cost target with Type = PhysicalActive }

    static member moveDefence action target =
        let cost = { Cost.empty with Stamina = Stamina.physicalReactive }
        { Check.targeted action cost target with Type = PhysicalReactive }

    static member applyCosts (actor : Entity) check (world : World) =
        let character = actor.GetCharacter world

        if Check.isUnstoppable check then
            true, world

        elif Check.isUnopposed check then
            if Character.canPay check.Cost character then
                let world = actor.SetCharacterWith (Character.pay check.Cost) world
                true, world
            else
                false, world

        else
            if Character.canPay check.Cost character then
                let world = actor.SetCharacterWith (Character.pay check.Cost) world

                let enemyStamina =
                    check.OpposedBy
                    |> List.map (fun entity -> entity.GetCharacter world)
                    |> List.map (Character.getStamina (CheckType.getOpposite check.Type))
                    |> List.max

                let actorStamina = Character.getStamina check.Type character

                (actorStamina > enemyStamina), world
            else
                false, world

    static member applyState bool check =
        { check with State = if bool then Passed else Failed }

    static member processCheck check before after (attacker : Entity) (area : Entity) world =

        match check.Action with
        | SkillSelect _ ->
            // TODO: implement skills
            // CharacterDo (attacker, 0)
            []
        | StanceChange stance ->
            [ CharacterDo (attacker, Character.setStance stance) ]
        | FractureBet bet ->
            [ CharacterDo (attacker, Character.spendFractureOnStamina bet) ]
        | RollStance ->
            [ CharacterDo (attacker, Character.roll) ]
        | Move move' ->
            match move' with
            | Stride
            | Climb
            | Crawl
            | Jump
            | Roll
            | Sidestep
            | Dash
            | Swim ->
                let should =
                    after
                    |> List.map (fun check -> match check.Action with Move Stride-> true | _ -> false)
                    |> List.contains true
                    |> not

                if should then
                    match check.Target with
                    | Some target ->

                        let moves =
                            check::before
                            |> List.sumBy (fun check -> match check.Action with Move Stride -> 1u | _ -> 0u)

                        let attackerCharacter = attacker.GetCharacter world
                        let reach = Character.getReach attackerCharacter
                        let distance = moves * Character.getSpeed attackerCharacter

                        GameEffect.travel attacker.Name target.Name reach distance area world

                    | None ->
                        []
                else
                    []
            | Block
            | Crouch
            | Delay
            | Dodge
            | Spin ->
                []
            | Cast
            | Power
            | Press
            | Ready
            | Retarget ->
                []
            | Burst
            | Fire ->
                []
            | Grab
            | Knockout
            | Slam
            | Sweep
            | Toss
            | Throw ->
                []
            | Strike weapon ->
                match check.Target with
                | Some target ->

                    let bonusDamage damage =
                        let damage =
                            match before with
                            | [] -> damage
                            | _ ->
                                before
                                |> List.rev
                                |> List.foldWhile (fun bonus move ->
                                    match move.Action with
                                    | Move Power -> Some (bonus + 5)
                                    | _ -> None
                                ) damage
                        let damage =
                            match after with
                            | _::after ->
                                after
                                |> List.foldWhile (fun bonus move ->
                                    match move.Action with
                                    | Move Press -> Some (bonus + 2)
                                    | _ -> None
                                ) damage
                            | _ -> damage
                        damage


                    // weapon
                    let damage = Weapon.getDamage weapon
                    let size = Weapon.getSizeBoost weapon
                    // moves
                    let damage = bonusDamage damage
                    // attacker
                    let attackerCharacter = attacker.GetCharacter world
                    let size = size + Character.getSize attackerCharacter
                    [Damage (target, size, damage)]

                | None ->
                    []
        | _ ->
            []