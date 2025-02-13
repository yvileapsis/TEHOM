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
    | InventoryAddItem of Entity
    | InventoryDropItem of Int32
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
    | InteractableActivate of Entity
    | InteractableDrop of Entity
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

    override this.Definitions (character, entity) = [
        Entity.Size == v3Dup 2.0f
        Entity.Offset == v3 0.0f 1.0f 0.0f
        Entity.Static == false
        Entity.BodyType == KinematicCharacter
        Entity.SleepingAllowed == true
        Entity.CharacterProperties == character.CharacterProperties
        Entity.BodyShape == CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
        Entity.Observable == true
        Entity.RegisterEvent => Register
        Game.KeyboardKeyDownEvent =|> fun evt -> UpdateInputKey evt.Data
        Entity.UpdateEvent => Update
        Entity.BodyPenetrationEvent =|> fun evt -> CharacterPenetration evt.Data
        Entity.BodySeparationExplicitEvent =|> fun evt -> CharacterSeparationExplicit evt.Data
        Entity.BodySeparationImplicitEvent =|> fun evt -> CharacterSeparationImplicit evt.Data
        Game.PostUpdateEvent => SyncWeaponTransform
        Entity.TakeItemEvent =|> fun evt -> InventoryAddItem evt.Data
    ]

    override this.Message (model, message, entity, world) =

        match message with
        | UpdateInputKey keyboardKeyData ->
            let (jump, model) = Player.updateInputKey world.UpdateTime keyboardKeyData model
            
            let model =
                if keyboardKeyData.KeyboardKey = KeyboardKey.Tab
                    && not keyboardKeyData.Repeated then
                    { model with InventoryOpen = not model.InventoryOpen }
                else
                    model

            let signals : List<Signal> = [
                if jump then Jump
                if keyboardKeyData.KeyboardKey = KeyboardKey.E then
                    match model.Interactable with
                    | Some entity ->
                        match entity.Parent with
                        | :? Entity as interactable ->
                            InteractableActivate interactable
                        | _ ->
                            ()
                    | _ ->
                        ()
            ]

            withSignals signals model

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

            // compute new rotation
            let turnSpeed = model.TurnSpeed * if grounded then 1.0f else 0.75f
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
            let turnSpeed = model.TurnSpeed * if grounded then 1.0f else 0.75f
            let turnVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.Down world then -turnSpeed else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.Up world then turnSpeed else 0.0f)

            let rotationHead = head.GetRotationLocal world
            let rotationHead =
                if turnVelocity <> 0.0f then
                    rotationHead * Quaternion.CreateFromYawPitchRoll (0f, turnVelocity, 0f)
                else
                    rotationHead

            let (position, rotation, linearVelocity, character) = Player.updateMotion time position rotation grounded playerPosition model world

            let angularVelocity = v3 0f 0f 0f

            let character = Player.updateState time character
            let (attackedCharacters, model) = Player.updateAttackedCharacters time character
            let (animations, invisible) = Player.updateAnimations time position rotation linearVelocity angularVelocity model world

            // deploy signals from update
            let signals = [
                UpdateTransform (position, rotation, rotationHead) :> Signal
                UpdateAnimations (position, rotation, Array.ofList animations, invisible)
                if World.isMouseButtonDown MouseLeft world && not model.InventoryOpen then Shoot
            ]
            let signals = match model.ActionState with WoundState wound when wound.WoundTime = world.UpdateTime - 60L -> PublishDie :> Signal :: signals | _ -> signals
            let signals = if attackedCharacters.Count > 0 then PublishAttacks attackedCharacters :> Signal :: signals else signals

            let model = {
                model with
                    InventoryDisplay =
                        model.Inventory
                        |> List.map (fun entity ->
                            entity.GetInventoryItemDisplayName world
                        )
            }

            let screenCenterRay =
                let eyeCenter = World.getEye3dCenter world
                let eyeRotation = World.getEye3dRotation world
                let eyeFieldOfView = World.getEye3dFieldOfView world
                let mousePosition = World.getMousePosition world
                Viewport.mouseToWorld3d eyeCenter eyeRotation eyeFieldOfView mousePosition world.RasterViewport

            let intersections =
                let ray = screenCenterRay
                let origin = ray.Origin
                let finale = ray.Origin + 100f * ray.Direction
                let segment = segment3 origin finale
                let array = World.rayCast3dBodies segment 0xFFFFFFFF false world
                let array =
                    array
                    |> Array.choose (fun x ->
                        // TODO: remake as correct body intersection instead of name checking
                        match x.BodyShapeIntersected.BodyId.BodySource with
                        | :? Entity as intersected when intersected.Name = Simulants.Interactable ->
//                            match intersected.Parent with
//                            | :? Entity as intersected' ->
                                Some intersected
//                            | _ ->
//                                None
                        | _ ->
                            None
                    )
                array

            let model =
                match Array.tryHead intersections with
                | Some entity ->
                    { model with
                        Interactable = Some entity
                        InteractableDisplay = entity.GetInteractableDisplayName world
                        }
                | None ->
                    { model with Interactable = None }

            withSignals signals model

        | CharacterPenetration penetrationData ->
            match penetrationData.BodyShapePenetratee.BodyId.BodySource with
            | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world ->
                let characterPenetratee = penetratee.GetCharacter world
                just model
            | _ -> just model

        | CharacterSeparationExplicit separationData ->
            match separationData.BodyShapeSeparatee.BodyId.BodySource with
            | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                let character = { model with CharacterCollisions = Set.remove separatee model.CharacterCollisions }
                just character
            | _ -> just model

        | CharacterSeparationImplicit separationData ->
            match separationData.BodyId.BodySource with
            | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                let character = { model with CharacterCollisions = Set.remove separatee model.CharacterCollisions }
                just character
            | _ -> just model

        | WeaponPenetration penetrationData ->
            match penetrationData.BodyShapePenetratee.BodyId.BodySource with
            | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world && penetratee <> entity ->
                let character = { model with WeaponCollisions = Set.add penetratee model.WeaponCollisions }
                just character
            | _ -> just model

        | WeaponSeparationExplicit separationData ->
            match separationData.BodyShapeSeparatee.BodyId.BodySource with
            | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                let character = { model with WeaponCollisions = Set.remove separatee model.WeaponCollisions }
                just character
            | _ -> just model

        | WeaponSeparationImplicit separationData ->
            match separationData.BodyId.BodySource with
            | :? Entity as separatee when separatee.Is<CharacterDispatcher> world ->
                let character = { model with WeaponCollisions = Set.remove separatee model.WeaponCollisions }
                just character
            | _ -> just model

        | InventoryAddItem s ->
            let model = { model with Inventory = List.append [s] model.Inventory }
            just model

        | InventoryDropItem i ->
            let item = List.item i model.Inventory
            let model = { model with Inventory = List.removeAt i model.Inventory }
            withSignals [InteractableDrop item] model

    override this.Command (model, command, entity, world) =

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
            let world = World.jumpBody true model.JumpSpeed bodyId world
            just world

        | RotateMove { Position = mousePosition } ->

            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let linearVelocity = entity.GetLinearVelocity world
            let angularVelocity = entity.GetAngularVelocity world
            let bodyId = entity.GetBodyId world
            let grounded = World.getBodyGrounded bodyId world

            // compute new rotation
            let turnSpeed = model.TurnSpeed * if grounded then 1.0f else 0.75f

            let turnVelocity =
                (- mousePosition.X) * turnSpeed * 0.1f

            let rotation =
                if turnVelocity <> 0.0f then
                    rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity)
                else
                    rotation

            let head = entity / "Head"

            // compute new rotation
            let turnSpeed = model.TurnSpeed * if grounded then 1.0f else 0.75f
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


        | Shoot ->

            let (bullet, world) = World.createEntity<BulletDispatcher> NoOverlay None entity.Group world // OPTIMIZATION: NoOverlay to avoid reflection.

            let barrel = entity / "Head" / "Barrel"
            let position = barrel.GetPosition world
            let rotation = barrel.GetRotation world
            let forward = rotation.Forward

            let world = bullet.SetPosition position world
            let world = World.applyBodyLinearImpulse (16f * forward) None (bullet.GetBodyId world) world

            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.ShotSound world
            just world

        | UpdateEyeTransform quaternion ->
            failwith "todo"

        | InteractableActivate entity' ->
            let eventTrace = EventTrace.debug "PlayerDispatcher" "Activate" "" EventTrace.empty
            let world = World.publishPlus entity entity'.ActivateEvent eventTrace entity true false world
            just world

        | InteractableDrop entity' ->
            let eventTrace = EventTrace.debug "PlayerDispatcher" "Drop" "" EventTrace.empty
            let world = World.publishPlus entity entity'.DropEvent eventTrace entity true false world
            just world

    override this.RayCast (ray, entity, world) =
        let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
        match animatedModel.RayCast ray world with
        | [||] ->
            let weapon = entity / Constants.Gameplay.CharacterWeaponName
            weapon.RayCast ray world
        | intersections -> intersections

    override this.Content (model, _) = [
         // hearts
        for i in 0 .. dec 5 do
            Content.staticSprite ("Heart+" + string i) [
                Entity.Position == v3 (-284.0f + single i * 32.0f) -144.0f 0.0f
                Entity.Size == v3 32.0f 32.0f 0.0f
                Entity.StaticImage := if model.HitPoints >= inc i then Assets.Gameplay.HeartFull else Assets.Gameplay.HeartEmpty
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
                Entity.PositionLocal == v3 0.15f -0.15f -0.45f
            ]

            ContentEx.cameraFirstPerson "Camera1st" [
                Entity.EnabledLocal := not model.InventoryOpen
                Entity.MouseMoveEvent =|> fun evt -> RotateMove evt.Data
            ]
            ContentEx.camera "CameraInventory" [
                Entity.EnabledLocal := model.InventoryOpen
            ]
        ]

        Content.composite<StaticSpriteDispatcher> "Crosshair1st" [
            Entity.EnabledLocal := not model.InventoryOpen
            Entity.VisibleLocal := not model.InventoryOpen
            Entity.Size == v3 16f 16f 0f
            Entity.StaticImage == Assets.Gui.Crosshair
            Entity.MountOpt == None
        ] [
            if not model.InventoryOpen && model.Interactable.IsSome then
                Content.staticSprite "Prompt" [
                    Entity.Absolute == true
                    Entity.Size == v3 16f 16f 0f
                    Entity.StaticImage == Assets.Gui.CrosshairPrompt
                ]
        ]

        Content.composite<CursorDispatcher> "CrosshairInventory" [
            Entity.EnabledLocal := model.InventoryOpen
            Entity.VisibleLocal := model.InventoryOpen
            Entity.Size == v3 16f 16f 0f
            Entity.StaticImage == Assets.Gui.Crosshair
            Entity.MountOpt == None
            Entity.AlwaysUpdate == true
        ] [
            if model.InventoryOpen && model.Interactable.IsSome then
                Content.staticSprite "Prompt" [
                    Entity.PositionLocal == v3 8f -8f 0f
                    Entity.Absolute == true
                    Entity.Size == v3 16f 16f 0f
                    Entity.StaticImage == Assets.Gui.CrosshairPrompt
                ]
        ]

        if model.InventoryOpen then
            Content.composite "InventoryMenu" [
                Entity.MountOpt == None
            ] [
                Content.staticSprite "Background" [
                    Entity.Size == Constants.Render.DisplayVirtualResolution.V3 / 2f
                    Entity.StaticImage == Assets.Default.Black
                    Entity.Color == Color.Black.WithA 0.5f
                ]

                Content.association "List" [
                    Entity.PositionLocal == v3 -60f 60f 0f
                    Entity.Layout == Flow (FlowDownward, FlowUnlimited)
                ] [
                    for (i, name) in List.indexed model.InventoryDisplay do
                        Content.button $"Item{i}" [
                            Entity.Size == v3 80.0f 10.0f 0.0f
                            Entity.FontSizing == Some 8
                            Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                            Entity.TextColor == Color.FloralWhite
                            Entity.Text := $"{name}"
                            Entity.ClickEvent => InventoryDropItem i
                        ]
                ]
            ]

        if model.Interactable.IsSome then
            Content.text "Name" [
                Entity.Absolute == true
                Entity.PositionLocal == v3 0f -120.0f 0.0f
                Entity.Size == v3 128.0f 32.0f 0.0f
                Entity.Text := $"{model.InteractableDisplay}"
                Entity.FontSizing == Some 10
                Entity.MountOpt == None
            ]

            Content.text "Take" [
                Entity.Absolute == true
                Entity.PositionLocal == v3 0f -130.0f 0.0f
                Entity.Size == v3 128.0f 32.0f 0.0f
                Entity.Text := $"E) Take"
                Entity.FontSizing == Some 10
                Entity.MountOpt == None
            ]


    ]