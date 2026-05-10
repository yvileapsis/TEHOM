namespace VoxelForge
open System
open System.Numerics
open Prime
open Nu

type [<SymbolicExpansion>] FirstPersonPlayer =
    { Yaw : single
      Pitch : single
      PreviousMousePositionOpt : Vector2 option
      LastGroundedTime : int64
      LastJumpTime : int64 }

    static member val initial =
        { Yaw = 0.0f
          Pitch = 0.0f
          PreviousMousePositionOpt = None
          LastGroundedTime = Int64.MinValue
          LastJumpTime = Int64.MinValue }

[<AutoOpen>]
module FirstPersonPlayerExtensions =
    type Entity with
        member this.GetFirstPersonPlayer world : FirstPersonPlayer = this.GetModelGeneric<FirstPersonPlayer> world
        member this.SetFirstPersonPlayer value world = this.SetModelGeneric<FirstPersonPlayer> value world
        member this.FirstPersonPlayer = this.ModelGeneric<FirstPersonPlayer> ()

[<RequireQualifiedAccess>]
module FirstPersonPlayerLogic =

    let [<Literal>] private EyeHeight = 1.62f
    let [<Literal>] private WalkSpeed = 5.0f
    let [<Literal>] private SprintSpeed = 8.0f
    let [<Literal>] private AirControlScalar = 0.65f
    let [<Literal>] private JumpSpeed = 5.5f
    let [<Literal>] private MouseSensitivity = 0.0025f
    let [<Literal>] private MouseDeltaLimit = 250.0f
    let [<Literal>] private CoyoteUpdates = 10L
    let [<Literal>] private JumpCooldownUpdates = 12L

    let private pitchLimit = degToRadF 85.0f
    let private eyeOffset = v3 0.0f EyeHeight 0.0f

    let private characterProperties =
        StairSteppingCharacterProperties
            { StairSteppingCharacterProperties.defaultProperties with
                SlopeMax = degToRadF 50.0f
                StairStepUp = v3 0.0f 0.35f 0.0f
                StairStepDownStickToFloor = v3 0.0f -0.45f 0.0f
                StairStepForwardTest = 0.18f }

    let private clampPitch (pitch : single) =
        Math.Clamp (pitch, -pitchLimit, pitchLimit)

    let private computeLook (player : FirstPersonPlayer) (world : World) =
        let mousePosition = World.getMousePosition world
        let struct (yaw, pitch) =
            match player.PreviousMousePositionOpt with
            | Some previousMousePosition when world.Advancing ->
                let mouseDelta = mousePosition - previousMousePosition
                if abs mouseDelta.X <= MouseDeltaLimit && abs mouseDelta.Y <= MouseDeltaLimit then
                    struct
                        (player.Yaw - mouseDelta.X * MouseSensitivity,
                         clampPitch (player.Pitch - mouseDelta.Y * MouseSensitivity))
                else struct (player.Yaw, player.Pitch)
            | Some _ | None -> struct (player.Yaw, player.Pitch)
        { player with Yaw = yaw; Pitch = pitch; PreviousMousePositionOpt = Some mousePosition }

    let private computeMoveVelocity (grounded : bool) (rotation : Quaternion) (linearVelocity : Vector3) (world : World) =
        let forward = rotation.Forward.WithY 0.0f
        let right = rotation.Right.WithY 0.0f
        let forward = if forward.LengthSquared () > 0.0f then forward.Normalized else v3Forward
        let right = if right.LengthSquared () > 0.0f then right.Normalized else v3Right
        let movement =
            (if World.isKeyboardKeyDown KeyboardKey.W world then forward else v3Zero) +
            (if World.isKeyboardKeyDown KeyboardKey.S world then -forward else v3Zero) +
            (if World.isKeyboardKeyDown KeyboardKey.A world then -right else v3Zero) +
            (if World.isKeyboardKeyDown KeyboardKey.D world then right else v3Zero)
        let movement = if movement.LengthSquared () > 1.0f then movement.Normalized else movement
        let speed = if World.isKeyboardKeyDown KeyboardKey.LShift world then SprintSpeed else WalkSpeed
        let speed = speed * (if grounded then 1.0f else AirControlScalar)
        movement * speed + linearVelocity * v3Up

    let private tryJump (grounded : bool) (player : FirstPersonPlayer) (entity : Entity) (world : World) =
        if World.isKeyboardKeyPressed KeyboardKey.Space world then
            let time = world.UpdateTime
            let canJump =
                time >= player.LastJumpTime + JumpCooldownUpdates &&
                time <= player.LastGroundedTime + CoyoteUpdates
            if canJump then
                World.jumpBody (not grounded) JumpSpeed (entity.GetBodyId world) world
                { player with LastJumpTime = time }
            else player
        else player

    let syncCamera (entity : Entity) (player : FirstPersonPlayer) (world : World) =
        let eyeRotation = Quaternion.CreateFromYawPitchRoll (player.Yaw, player.Pitch, 0.0f)
        World.setEye3dCenter (entity.GetPosition world + eyeOffset) world
        World.setEye3dRotation eyeRotation world
        World.setEye3dFieldOfView 0.75f world

    let update (entity : Entity) (world : World) =
        let bodyId = entity.GetBodyId world
        let grounded = World.getBodyGrounded bodyId world
        let player = entity.GetFirstPersonPlayer world
        let player = if grounded then { player with LastGroundedTime = world.UpdateTime } else player
        let player = computeLook player world
        let bodyRotation = Quaternion.CreateFromAxisAngle (v3Up, player.Yaw)
        entity.SetRotation bodyRotation world
        if world.Advancing then
            let linearVelocity = entity.GetLinearVelocity world
            entity.SetLinearVelocity (computeMoveVelocity grounded bodyRotation linearVelocity world) world
            let player = tryJump grounded player entity world
            entity.SetFirstPersonPlayer player world
            syncCamera entity player world
        else
            entity.SetFirstPersonPlayer player world
            syncCamera entity player world

    let definitions =
        [Entity.Size == v3 0.7f 1.7f 0.7f
         Entity.Offset == v3 0.0f 0.85f 0.0f
         Entity.Presence == Omnipresent
         Entity.AlwaysUpdate == true
         Entity.Static == false
         Entity.Visible == false
         Entity.Pickable == false
         Entity.MountOpt == None
         Entity.BodyType == KinematicCharacter
         Entity.BodyShape == CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
         Entity.CharacterProperties == characterProperties
         Entity.Substance == Mass 70.0f
         Entity.Friction == 0.6f
         Entity.LinearDamping == 0.0f
         Entity.AngularFactor == v3Zero
         Entity.SleepingAllowed == false
         Entity.CollisionCategories == "1"
         Entity.CollisionMask == Constants.Physics.CollisionWildcard]

type FirstPersonPlayerDispatcher () =
    inherit Entity3dDispatcher<FirstPersonPlayer, Message, Command> (true, false, false, FirstPersonPlayer.initial)

    static member Facets =
        [typeof<RigidBodyFacet>]

    override this.Definitions (_, _) =
        FirstPersonPlayerLogic.definitions

    override this.Update (entity, world) =
        FirstPersonPlayerLogic.update entity world
