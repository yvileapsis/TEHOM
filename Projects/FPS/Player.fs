namespace MyGame
open System
open System.Numerics
open FParsec
open Prime
open Nu

module Player =

    type JumpState =
        { LastTime : int64
          LastTimeOnGround : int64 }

        static member initial =
            { LastTime = 0L
              LastTimeOnGround = 0L }

    type ObstructedState =
        { ObstructedTime : int64 }

    type AttackState =
        { AttackTime : int64
          FollowUpBuffered : bool
          AttackedCharacters : Entity Set }

        static member make time =
            { AttackTime = time
              FollowUpBuffered = false
              AttackedCharacters = Set.empty }

    type InjuryState =
        { InjuryTime : int64 }

    type WoundState =
        { WoundTime : int64 }

    type ActionState =
        | NormalState
        | ObstructedState of ObstructedState
        | AttackState of AttackState
        | InjuryState of InjuryState
        | WoundState of WoundState

    type [<ReferenceEquality; SymbolicExpansion>] Player = {
        Position : Vector3
        Rotation : Quaternion
        PositionPrevious : Vector3 FQueue
        RotationPrevious : Quaternion FQueue
        LinearVelocityPrevious : Vector3 FQueue
        AngularVelocityPrevious : Vector3 FQueue
        HitPoints : int
        ActionState : ActionState
        JumpState : JumpState
        CharacterCollisions : Entity Set
        WeaponCollisions : Entity Set
        WalkSpeed : single
        TurnSpeed : single
        JumpSpeed : single

        Inventory : List<Entity>
        InventoryDisplay : List<String>
        InventoryOpen : Boolean

        Interactable : Option<Entity>
        InteractableDisplay : String
    }
    with
        member this.PositionInterp position =
            if not (FQueue.isEmpty this.PositionPrevious) then
                let positions = FQueue.conj position this.PositionPrevious
                Seq.sum positions / single positions.Length
            else position

        member this.RotationInterp rotation =
            if not (FQueue.isEmpty this.RotationPrevious) then
                let rotations = FQueue.conj rotation this.RotationPrevious
                if rotations.Length > 1 then
                    let unnormalized = Quaternion.Slerp (Seq.head rotations, Seq.last rotations, 0.5f)
                    unnormalized.Normalized
                else rotation
            else rotation

        member this.LinearVelocityInterp linearVelocity =
            if not (FQueue.isEmpty this.LinearVelocityPrevious) then
                let linearVelocities = FQueue.conj linearVelocity this.LinearVelocityPrevious
                Seq.sum linearVelocities / single linearVelocities.Length
            else linearVelocity

        member this.AngularVelocityInterp angularVelocity =
            if not (FQueue.isEmpty this.AngularVelocityPrevious) then
                let angularVelocities = FQueue.conj angularVelocity this.AngularVelocityPrevious
                Seq.sum angularVelocities / single angularVelocities.Length
            else angularVelocity

        member this.CharacterProperties =
            CharacterProperties.defaultProperties


        static member computeTraversalAnimations rotation linearVelocity angularVelocity character =
            match character.ActionState with
            | NormalState ->
                let rotationInterp = character.RotationInterp rotation
                let linearVelocityInterp = character.LinearVelocityInterp linearVelocity
                let angularVelocityInterp = character.AngularVelocityInterp angularVelocity
                let forwardness = (linearVelocityInterp * 32.0f).Dot rotationInterp.Forward
                let backness = (linearVelocityInterp * 32.0f).Dot -rotationInterp.Forward
                let rightness = (linearVelocityInterp * 32.0f).Dot rotationInterp.Right
                let leftness = (linearVelocityInterp * 32.0f).Dot -rotationInterp.Right
                let turnRightness = if angularVelocityInterp.Y < 0.0f then -angularVelocityInterp.Y * 48.0f else 0.0f
                let turnLeftness = if angularVelocityInterp.Y > 0.0f then angularVelocityInterp.Y * 48.0f else 0.0f
                let animations =
                    [Animation.make 0L None "Armature|Idle" Loop 1.0f 0.5f None]
                let animations =
                    if forwardness >= 0.01f then Animation.make 0L None "Armature|WalkForward" Loop 1.0f forwardness None :: animations
                    elif backness >= 0.01f then Animation.make 0L None "Armature|WalkBack" Loop 1.0f backness None :: animations
                    else animations
                let animations =
                    if rightness >= 0.01f then Animation.make 0L None "Armature|WalkRight" Loop 1.0f rightness None :: animations
                    elif leftness >= 0.01f then Animation.make 0L None "Armature|WalkLeft" Loop 1.0f leftness None :: animations
                    else animations
                let animations =
                    if turnRightness >= 0.01f then Animation.make 0L None "Armature|TurnRight" Loop 1.0f turnRightness None :: animations
                    elif turnLeftness >= 0.01f then Animation.make 0L None "Armature|TurnLeft" Loop 1.0f turnLeftness None :: animations
                    else animations
                animations
            | _ -> []

        static member tryUpdateActionAnimation time character world =
            match character.ActionState with
            | NormalState -> None
            | ObstructedState obstructed ->
                let animation = Animation.loop obstructed.ObstructedTime None "Armature|Idle"
                Some (animation, false)
            | AttackState attack ->
                let localTime = time - attack.AttackTime
                match localTime with
                | 7L -> World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.SlashSound world
                | 67L -> World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.Slash2Sound world
                | _ -> ()
                let (animationTime, animationName) =
                    if localTime <= 55L
                    then (attack.AttackTime, "Armature|AttackVertical")
                    else (attack.AttackTime + 55L, "Armature|AttackHorizontal")
                let animation = Animation.once animationTime None animationName
                Some (animation, false)
            | InjuryState injury ->
                let animation = Animation.once injury.InjuryTime None "Armature|WalkBack"
                Some (animation, false)
            | WoundState wound ->
                let localTime = time - wound.WoundTime
                let animation = Animation.loop wound.WoundTime None "Armature|WalkBack"
                let invisible = localTime / 5L % 2L = 0L
                Some (animation, invisible)

        static member updateInterps position rotation linearVelocity angularVelocity character =

            // update interps
            let character =
                { character with
                    PositionPrevious = (if character.PositionPrevious.Length >= Constants.Gameplay.CharacterInterpolationSteps then character.PositionPrevious |> FQueue.tail else character.PositionPrevious) |> FQueue.conj position
                    RotationPrevious = (if character.RotationPrevious.Length >= Constants.Gameplay.CharacterInterpolationSteps then character.RotationPrevious |> FQueue.tail else character.RotationPrevious) |> FQueue.conj rotation
                    LinearVelocityPrevious = (if character.LinearVelocityPrevious.Length >= Constants.Gameplay.CharacterInterpolationSteps then character.LinearVelocityPrevious |> FQueue.tail else character.LinearVelocityPrevious) |> FQueue.conj linearVelocity
                    AngularVelocityPrevious = (if character.AngularVelocityPrevious.Length >= Constants.Gameplay.CharacterInterpolationSteps then character.AngularVelocityPrevious |> FQueue.tail else character.AngularVelocityPrevious) |> FQueue.conj angularVelocity }

            // ensure previous positions interp aren't stale (such as when an entity is moved in the editor with existing previous position state)
            let character =
                let positionInterp = character.PositionInterp position
                if Vector3.Distance (positionInterp, position) > Constants.Gameplay.CharacterPositionInterpDistanceMax
                then { character with PositionPrevious = List.init Constants.Gameplay.CharacterInterpolationSteps (fun _ -> position) |> FQueue.ofList }
                else character

            // fin
            character

        static member updateMotion time position (rotation : Quaternion) grounded (playerPosition : Vector3) character world =

            // update jump state
            let lastTimeOnGround = if grounded then time else character.JumpState.LastTimeOnGround
            let character = { character with Player.JumpState.LastTimeOnGround = lastTimeOnGround }

            // update traversal

            // player traversal
            if character.ActionState = NormalState || not grounded then

                // compute new position
                let forward = rotation.Forward
                let right = rotation.Right
                let walkSpeed = character.WalkSpeed * if grounded then 1.0f else 0.75f

                let walkVelocity =
                    (if World.isKeyboardKeyDown KeyboardKey.W world then forward else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.S world then -forward else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.A world then -right else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.D world then right else v3Zero)

                let walkSpeed =
                    if World.isKeyboardKeyDown KeyboardKey.LShift world then 2f * walkSpeed else walkSpeed

                let position =
                    if walkVelocity <> v3Zero then
                        position + walkVelocity.Normalized * walkSpeed
                    else
                        position

                (position, rotation, walkVelocity, character)

            else (position, rotation, v3Zero, character)


        static member updateAction time (position : Vector3) (rotation : Quaternion) (playerPosition : Vector3) character =
            character

        static member updateState time character =
            match character.ActionState with
            | NormalState -> character
            | ObstructedState _ -> character
            | AttackState attack ->
                let actionState =
                    let localTime = time - attack.AttackTime
                    if localTime < 55 || localTime < 130 && attack.FollowUpBuffered
                    then AttackState attack
                    else NormalState
                { character with ActionState = actionState }
            | InjuryState injury ->
                let actionState =
                    let localTime = time - injury.InjuryTime
                    let injuryTime = 30
                    if localTime < injuryTime
                    then InjuryState injury
                    else NormalState
                { character with ActionState = actionState }
            | WoundState _ -> character

        static member updateAnimations time position rotation linearVelocity angularVelocity character world =
            ignore<Vector3> position
            let traversalAnimations = Player.computeTraversalAnimations rotation linearVelocity angularVelocity character
            let (animations, invisible) =
                match Player.tryUpdateActionAnimation time character world with
                | Some (animation, invisible) -> (animation :: traversalAnimations, invisible)
                | None -> (traversalAnimations, false)
            (animations, invisible)

        static member updateAttackedCharacters time character =
            match character.ActionState with
            | AttackState attack ->
                let localTime = time - attack.AttackTime
                let attack =
                    match localTime with
                    | 55L -> { attack with AttackedCharacters = Set.empty } // reset attack tracking at start of buffered attack
                    | _ -> attack
                if localTime >= 20 && localTime < 30 || localTime >= 78 && localTime < 88 then
                    let attackingCharacters = Set.difference character.WeaponCollisions attack.AttackedCharacters
                    let attack = { attack with AttackedCharacters = Set.union attack.AttackedCharacters character.WeaponCollisions }
                    (attackingCharacters, { character with ActionState = AttackState attack })
                else (Set.empty, { character with ActionState = AttackState attack })
            | _ -> (Set.empty, character)

        static member updateInputKey time keyboardKeyData character =


            // jumping
            if keyboardKeyData.KeyboardKey = KeyboardKey.Space && not keyboardKeyData.Repeated then
                let sinceJump = time - character.JumpState.LastTime
                let sinceOnGround = time - character.JumpState.LastTimeOnGround
                if sinceJump >= 12L && sinceOnGround < 10L && character.ActionState = NormalState then
                    let character = { character with Player.JumpState.LastTime = time }
                    (true, character)
                else (false, character)

            // attacking
            elif keyboardKeyData.KeyboardKey = KeyboardKey.RShift && not keyboardKeyData.Repeated then
                let character =
                    match character.ActionState with
                    | NormalState ->
                        { character with ActionState = AttackState (AttackState.make time) }
                    | AttackState attack ->
                        let localTime = time - attack.AttackTime
                        if localTime > 10L && not attack.FollowUpBuffered
                        then { character with ActionState = AttackState { attack with FollowUpBuffered = true }}
                        else character
                    | ObstructedState _ | InjuryState _ | WoundState _ -> character
                (false, character)
            else (false, character)

        static member initial = {
            Position = Vector3.Zero
            Rotation = Quaternion.Identity
            PositionPrevious = FQueue.empty
            RotationPrevious = FQueue.empty
            LinearVelocityPrevious = FQueue.empty
            AngularVelocityPrevious = FQueue.empty
            HitPoints = 5
            ActionState = NormalState
            JumpState = JumpState.initial
            CharacterCollisions = Set.empty
            WeaponCollisions = Set.empty
            WalkSpeed = 0.08f
            TurnSpeed = 0.07f
            JumpSpeed = 5.0f
            InventoryOpen = false
            Inventory = []
            InventoryDisplay = []
            Interactable = None
            InteractableDisplay = String.empty
        }