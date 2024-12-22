namespace MyGame
open System
open System.Numerics
open Prime
open Nu

open Player
open SDL2;

type PlayerMessage =
    | CharacterPenetration of BodyPenetrationData
    | CharacterSeparationExplicit of BodySeparationExplicitData
    | CharacterSeparationImplicit of BodySeparationImplicitData
    | WeaponPenetration of BodyPenetrationData
    | WeaponSeparationExplicit of BodySeparationExplicitData
    | WeaponSeparationImplicit of BodySeparationImplicitData
    | UpdateInputKey of KeyboardKeyData
    | Update
    | MouseMove of MouseMoveData
    interface Message

type PlayerCommand =
    | Register
    | UpdateTransform of Vector3 * Quaternion * Quaternion
    | UpdateEyeTransform of Quaternion
    | UpdateAnimations of Vector3 * Quaternion * Animation array * bool
    | SyncWeaponTransform
    | PublishAttacks of Entity Set
    | PublishDie
    | Jump
    | RotateMove of MouseMoveData
    | Shoot
    interface Command

[<AutoOpen>]
module PlayerExtensions =
    type Entity with
        member this.GetPlayer world = this.GetModelGeneric<Player> world
        member this.SetPlayer value world = this.SetModelGeneric<Player> value world
        member this.Player = this.ModelGeneric<Player> ()
        member this.AttackEvent = Events.AttackEvent --> this
        member this.DieEvent = Events.DieEvent --> this

type PlayerDispatcher () =
    inherit Entity3dDispatcher<Player, PlayerMessage, PlayerCommand> (true, false, false, Player.initial)

    static let [<Literal>] BulletForce = 25.0f

    static member Facets =
        [typeof<RigidBodyFacet>]

    override this.Definitions (character, _) = [
        Entity.Size == v3Dup 2.0f
        Entity.Offset == v3 0.0f 1.0f 0.0f
        Entity.Static == false
        Entity.BodyType == KinematicCharacter
        Entity.SleepingAllowed == true
        Entity.CharacterProperties == character.CharacterProperties
        Entity.BodyShape == CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
        Entity.Observable == true
        Entity.FollowTargetOpt := None
        Entity.RegisterEvent => Register
        Game.KeyboardKeyDownEvent =|> fun evt -> UpdateInputKey evt.Data
        Entity.UpdateEvent => Update
        Entity.BodyPenetrationEvent =|> fun evt -> CharacterPenetration evt.Data
        Entity.BodySeparationExplicitEvent =|> fun evt -> CharacterSeparationExplicit evt.Data
        Entity.BodySeparationImplicitEvent =|> fun evt -> CharacterSeparationImplicit evt.Data
        Game.PostUpdateEvent => SyncWeaponTransform
        Game.MouseMoveEvent =|> fun evt -> MouseMove evt.Data
    ]

    override this.Message (character, message, entity, world) =

        match message with
        | UpdateInputKey keyboardKeyData ->
            let (jump, character) = Player.updateInputKey world.UpdateTime keyboardKeyData character
            withSignals (if jump then [Jump] else []) character

        | Update ->


            // update character
            let time = world.UpdateTime
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let linearVelocity = entity.GetLinearVelocity world
            let angularVelocity = entity.GetAngularVelocity world
            let bodyId = entity.GetBodyId world
            let grounded = World.getBodyGrounded bodyId world
            let playerPosition = Simulants.GameplayPlayer.GetPosition world

            let character = Player.updateInterps position rotation linearVelocity angularVelocity character


            // compute new rotation
            let turnSpeed = character.TurnSpeed * if grounded then 1.0f else 0.75f
            let turnVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)

            let rotation =
                if turnVelocity <> 0.0f then
                    rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity)
                else
                    rotation

            let head = entity / "Head"

            // compute new rotation
            let turnSpeed = character.TurnSpeed * if grounded then 1.0f else 0.75f
            let turnVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.Down world then -turnSpeed else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.Up world then turnSpeed else 0.0f)

            let rotationHead = head.GetRotationLocal world
            let rotationHead =
                if turnVelocity <> 0.0f then
                    rotationHead * Quaternion.CreateFromYawPitchRoll (0f, turnVelocity, 0f)
                else
                    rotationHead

            let (position, rotation, linearVelocity, character) = Player.updateMotion time position rotation grounded playerPosition character world

            let angularVelocity = v3 0f 0f 0f

            let character = Player.updateAction time position rotation playerPosition character
            let character = Player.updateState time character
            let (attackedCharacters, character) = Player.updateAttackedCharacters time character
            let (animations, invisible) = Player.updateAnimations time position rotation linearVelocity angularVelocity character world

            // deploy signals from update
            let signals = [
                UpdateTransform (position, rotation, rotationHead) :> Signal
                UpdateAnimations (position, rotation, Array.ofList animations, invisible)
                if World.isMouseButtonDown MouseLeft world then Shoot
            ]
            let signals = match character.ActionState with WoundState wound when wound.WoundTime = world.UpdateTime - 60L -> PublishDie :> Signal :: signals | _ -> signals
            let signals = if attackedCharacters.Count > 0 then PublishAttacks attackedCharacters :> Signal :: signals else signals
            withSignals signals character

        | CharacterPenetration penetrationData ->
            match penetrationData.BodyShapePenetratee.BodyId.BodySource with
            | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world ->
                let characterPenetratee = penetratee.GetCharacter world
                just character
            | _ -> just character

        | CharacterSeparationExplicit separationData ->
            match separationData.BodyShapeSeparatee.BodyId.BodySource with
            | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                let character = { character with CharacterCollisions = Set.remove separatee character.CharacterCollisions }
                just character
            | _ -> just character

        | CharacterSeparationImplicit separationData ->
            match separationData.BodyId.BodySource with
            | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                let character = { character with CharacterCollisions = Set.remove separatee character.CharacterCollisions }
                just character
            | _ -> just character

        | WeaponPenetration penetrationData ->
            match penetrationData.BodyShapePenetratee.BodyId.BodySource with
            | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world && penetratee <> entity ->
                let characterPenetratee = penetratee.GetCharacter world
                if characterPenetratee.CharacterType = Character.Enemy then
                    let character = { character with WeaponCollisions = Set.add penetratee character.WeaponCollisions }
                    just character
                else just character
            | _ -> just character

        | WeaponSeparationExplicit separationData ->
            match separationData.BodyShapeSeparatee.BodyId.BodySource with
            | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                let character = { character with WeaponCollisions = Set.remove separatee character.WeaponCollisions }
                just character
            | _ -> just character

        | WeaponSeparationImplicit separationData ->
            match separationData.BodyId.BodySource with
            | :? Entity as separatee when separatee.Is<CharacterDispatcher> world ->
                let character = { character with WeaponCollisions = Set.remove separatee character.WeaponCollisions }
                just character
            | _ -> just character
        | MouseMove mouseMoveData ->
            withSignals [RotateMove mouseMoveData] character

    override this.Command (character, command, entity, world) =

        match command with
        | Register ->
            // let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
            // let weapon = entity / Constants.Gameplay.CharacterWeaponName
            // let world = animatedModel.SetAnimations [|Animation.loop GameTime.zero None "Armature|Idle"|] world
            // let world = animatedModel.AnimateBones world
            // let world = weapon.AutoBounds world
            withSignal SyncWeaponTransform world

        | UpdateTransform (position, rotationBody, rotationHead) ->
            let world = entity.SetPosition position world
            let world = entity.SetRotation rotationBody world
            let head = entity / "Head"
            let world = head.SetRotationLocal rotationHead world
            just world

        | UpdateAnimations (position, rotation, animations, invisible) ->
            // let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
            // let weapon = entity / Constants.Gameplay.CharacterWeaponName
            // let world = animatedModel.SetPosition (character.PositionInterp position) world
            // let world = animatedModel.SetRotation (character.RotationInterp rotation) world
            // let world = animatedModel.SetAnimations animations world
            // let world = animatedModel.SetVisible (not invisible) world
            // let world = weapon.SetVisible (not invisible) world
            just world

        | SyncWeaponTransform ->
            // let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
            // let weapon = entity / Constants.Gameplay.CharacterWeaponName
            // match animatedModel.TryGetBoneTransformByName Constants.Gameplay.CharacterWeaponHandBoneName world with
            // | Some weaponHandBoneTransform ->
            //     let weaponTransform =
            //        Matrix4x4.CreateTranslation (v3 -0.1f 0.0f 0.02f) *
            //        Matrix4x4.CreateFromAxisAngle (v3Forward, MathF.PI_OVER_2) *
            //        weaponHandBoneTransform
            //    let world = weapon.SetPosition weaponTransform.Translation world
            //    let world = weapon.SetRotation weaponTransform.Rotation world
            //    just world
            // | None ->
            just world

        | PublishAttacks attackedCharacters ->
            let world =
                Set.fold (fun world attackedCharacter ->
                    World.publish attackedCharacter entity.AttackEvent entity world)
                    world attackedCharacters
            just world

        | PublishDie ->
            let world = World.publish entity entity.DieEvent entity world
            just world

        | Jump ->
            let bodyId = entity.GetBodyId world
            let world = World.jumpBody true character.JumpSpeed bodyId world
            just world

        | RotateMove _ ->

            if world.Advancing && entity.GetEnabled world then

                let position = entity.GetPosition world
                let rotation = entity.GetRotation world
                let linearVelocity = entity.GetLinearVelocity world
                let angularVelocity = entity.GetAngularVelocity world
                let bodyId = entity.GetBodyId world
                let grounded = World.getBodyGrounded bodyId world

                let character = Player.updateInterps position rotation linearVelocity angularVelocity character

                // compute new rotation
                let turnSpeed = character.TurnSpeed * if grounded then 1.0f else 0.75f

                let mousePosition = World.getMousePosition2dScreen world

                let turnVelocity =
                    (- mousePosition.X) * turnSpeed * 0.1f

                let rotation =
                    if turnVelocity <> 0.0f then
                        rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity)
                    else
                        rotation

                let head = entity / "Head"

                // compute new rotation
                let turnSpeed = character.TurnSpeed * if grounded then 1.0f else 0.75f
                let turnVelocity =
                    (mousePosition.Y) * turnSpeed * 0.1f

                let rotationHead = head.GetRotationLocal world
                let rotationHead =
                    if turnVelocity <> 0.0f then
                        rotationHead * Quaternion.CreateFromYawPitchRoll (0f, turnVelocity, 0f)
                    else
                        rotationHead

//                let rotation = if turnVelocity <> 0.0f then rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity) else rotation

                withSignals [UpdateTransform (position, rotation, rotationHead)] world
            else

                just world

        | Shoot ->

            let (bullet, world) = World.createEntity<BulletDispatcher> NoOverlay None entity.Group world // OPTIMIZATION: NoOverlay to avoid reflection.

            let barrel = entity / "Head" / "Barrel"
            let position = barrel.GetPosition world
            let rotation = barrel.GetRotation world
            let forward = rotation.Forward

            let world = bullet.SetPosition position world
            let world = World.applyBodyLinearImpulse (2f * forward) None (bullet.GetBodyId world) world

            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.ShotSound world
            just world

    override this.RayCast (ray, entity, world) =
        let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
        match animatedModel.RayCast ray world with
        | [||] ->
            let weapon = entity / Constants.Gameplay.CharacterWeaponName
            weapon.RayCast ray world
        | intersections -> intersections

    override this.Content (character, _) = [
         // hearts
        for i in 0 .. dec 5 do
            Content.staticSprite ("Heart+" + string i) [
                Entity.Position == v3 (-284.0f + single i * 32.0f) -144.0f 0.0f
                Entity.Size == v3 32.0f 32.0f 0.0f
                Entity.StaticImage := if character.HitPoints >= inc i then Assets.Gameplay.HeartFull else Assets.Gameplay.HeartEmpty
                Entity.MountOpt == None
            ]

        if false then
            // animated model
            Content.entity<AnimatedModelDispatcher> Constants.Gameplay.CharacterAnimatedModelName [
                Entity.Visible == false
                Entity.Size == v3Dup 2.0f
                Entity.Offset == v3 0.0f 1.0f 0.0f
                Entity.MaterialProperties == MaterialProperties.defaultProperties
                Entity.AnimatedModel == Assets.Gameplay.JoanModel
                Entity.Pickable == false
            ]

            // weapon
            Content.entity<RigidModelDispatcher> Constants.Gameplay.CharacterWeaponName [
                Entity.Offset == v3 0.0f 0.5f 0.0f
                Entity.StaticModel := character.WeaponModel
                Entity.BodyType == Static
                Entity.BodyShape == BoxShape { Size = v3 0.3f 1.2f 0.3f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.6f 0.0f)); PropertiesOpt = None }
                Entity.Sensor == true
                Entity.NavShape == EmptyNavShape
                Entity.Pickable == false
                Entity.BodyPenetrationEvent =|> fun evt -> WeaponPenetration evt.Data
                Entity.BodySeparationExplicitEvent =|> fun evt -> WeaponSeparationExplicit evt.Data
                Entity.BodySeparationImplicitEvent =|> fun evt -> WeaponSeparationImplicit evt.Data
            ]

        Content.composite "Head" [
            Entity.PositionLocal == v3 0.0f 1.65f 0.0f
            Entity.RotationLocal ==  Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -10f, 0f)
        ] [
            Content.staticModelHierarchy "Pistol" [
                //Entity.PositionLocal == v3 0.1f -0.1f -0.3f

                Entity.PositionLocal == v3 0.15f -0.15f -0.25f
                Entity.ScaleLocal == v3 0.1f 0.1f 0.2f
//                Entity.RotationLocal == Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 180f, 0f, 0f)
                Entity.StaticModel == Assets.Default.StaticModel
            ]

            Content.entity "Barrel" [
//                Entity.PositionLocal == v3 0.1f -0.1f -0.3f
                Entity.PositionLocal == v3 0.15f -0.15f -0.25f
            ]

            let followPlayer = true

            if followPlayer then
                ContentEx.cameraFirstPerson "Camera" []
        ]
    ]