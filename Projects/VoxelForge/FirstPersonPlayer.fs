namespace VoxelForge
open System
open System.Numerics
open SDL
open Prime
open Nu

type [<SymbolicExpansion>] FirstPersonPlayer =
    { Yaw : single
      Pitch : single
      PreviousMousePositionOpt : Vector2 option
      DuckAmount : single
      Ducked : bool
      JumpReleased : bool
      LastGroundedTime : int64
      LastJumpTime : int64 }

    static member val initial =
        { Yaw = 0.0f
          Pitch = 0.0f
          PreviousMousePositionOpt = None
          DuckAmount = 0.0f
          Ducked = false
          JumpReleased = true
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

    let [<Literal>] private StandingEyeHeight = 1.62f
    let [<Literal>] private DuckEyeHeight = 1.0f
    let [<Literal>] private WalkSpeed = 5.0f
    let [<Literal>] private SprintSpeed = 8.0f
    let [<Literal>] private DuckSpeed = 2.0f
    let [<Literal>] private GroundAcceleration = 10.0f
    let [<Literal>] private AirAcceleration = 10.0f
    let [<Literal>] private AirSpeedCap = 0.75f
    let [<Literal>] private Friction = 4.0f
    let [<Literal>] private StopSpeed = 2.0f
    let [<Literal>] private SurfaceFriction = 1.0f
    let [<Literal>] private JumpSpeed = 5.5f
    let [<Literal>] private JumpForwardBoost = 1.0f
    let [<Literal>] private DuckRate = 8.0f
    let [<Literal>] private MouseSensitivity = 0.0025f
    let [<Literal>] private MouseDeltaLimit = 250.0f
    let [<Literal>] private JumpCooldownUpdates = 2L
    let [<Literal>] private CharacterRadius = 0.35f
    let [<Literal>] private StandingBodyHeight = 1.0f
    let [<Literal>] private StandingBodyCenter = 0.85f
    let [<Literal>] private DuckBodyHeight = 0.4f
    let [<Literal>] private DuckBodyCenter = 0.55f
    let [<Literal>] private UnduckProbeRadiusScalar = 0.8f
    let [<Literal>] private UnduckProbePadding = 0.03f

    let private pitchLimit = degToRadF 85.0f
    let private standingBodyShape =
        CapsuleShape { Height = StandingBodyHeight; Radius = CharacterRadius; TransformOpt = Some (Affine.makeTranslation (v3 0.0f StandingBodyCenter 0.0f)); PropertiesOpt = None }
    let private duckBodyShape =
        CapsuleShape { Height = DuckBodyHeight; Radius = CharacterRadius; TransformOpt = Some (Affine.makeTranslation (v3 0.0f DuckBodyCenter 0.0f)); PropertiesOpt = None }

    let private characterProperties =
        StairSteppingCharacterProperties
            { StairSteppingCharacterProperties.defaultProperties with
                SlopeMax = degToRadF 50.0f
                StairStepUp = v3 0.0f 0.38f 0.0f
                StairStepDownStickToFloor = v3 0.0f -0.7f 0.0f
                StairStepForwardTest = 0.24f }

    let private clampPitch (pitch : single) =
        Math.Clamp (pitch, -pitchLimit, pitchLimit)

    let private computeLook (player : FirstPersonPlayer) (world : World) =
        let inputFocused =
            World.tryGetWindowFlags world
            |> Option.exists (fun flags ->
                flags &&& SDL_WindowFlags.SDL_WINDOW_INPUT_FOCUS <> LanguagePrimitives.EnumOfValue 0UL)
        if world.Advancing && inputFocused then
            World.trySetMouseGrabbed true world
            World.setCursorVisible false world
            let mouseCenter = World.getMouseCenter world
            let mousePosition = World.getMousePosition world
            let struct (yaw, pitch) =
                match player.PreviousMousePositionOpt with
                | Some previousMousePosition ->
                    let mouseDelta = mousePosition - previousMousePosition
                    if abs mouseDelta.X <= MouseDeltaLimit && abs mouseDelta.Y <= MouseDeltaLimit then
                        struct
                            (player.Yaw - mouseDelta.X * MouseSensitivity,
                             clampPitch (player.Pitch - mouseDelta.Y * MouseSensitivity))
                    else struct (player.Yaw, player.Pitch)
                | None -> struct (player.Yaw, player.Pitch)
            World.trySetMousePosition mouseCenter world
            { player with Yaw = yaw; Pitch = pitch; PreviousMousePositionOpt = Some mouseCenter }
        else
            World.trySetMouseGrabbed false world
            World.setCursorVisible true world
            { player with PreviousMousePositionOpt = None }

    let private getFrameTime (world : World) =
        GameTime.toSeconds world.GameDelta |> single |> max 0.0f

    let private getEyeOffset (player : FirstPersonPlayer) =
        let eyeHeight = StandingEyeHeight + (DuckEyeHeight - StandingEyeHeight) * player.DuckAmount
        v3 0.0f eyeHeight 0.0f

    let private canUnduck (entity : Entity) (world : World) =
        let bodyId = entity.GetBodyId world
        let collisionCategory = Physics.categorizeCollisionMask (entity.GetCollisionCategories world)
        let collisionMask = Physics.categorizeCollisionMask (entity.GetCollisionMask world)
        let position = entity.GetPosition world
        let duckTop = DuckBodyCenter + DuckBodyHeight * 0.5f + CharacterRadius + UnduckProbePadding
        let standingTop = StandingBodyCenter + StandingBodyHeight * 0.5f + CharacterRadius + UnduckProbePadding
        let probeHeight = standingTop - duckTop
        let probeRadius = CharacterRadius * UnduckProbeRadiusScalar
        let probeOffsets =
            [|v3Zero
              v3 probeRadius 0.0f 0.0f
              v3 -probeRadius 0.0f 0.0f
              v3 0.0f 0.0f probeRadius
              v3 0.0f 0.0f -probeRadius|]
        probeOffsets
        |> Array.forall (fun offset ->
            let probeRay = ray3 (position + offset + v3 0.0f duckTop 0.0f) (v3 0.0f probeHeight 0.0f)
            World.rayCastBodies3d probeRay collisionCategory collisionMask false world
            |> Array.forall (fun intersection -> intersection.BodyShapeIntersected.BodyId = bodyId))

    let private applyDuckBodyShape (entity : Entity) (ducked : bool) (world : World) =
        let desiredBodyShape = if ducked then duckBodyShape else standingBodyShape
        if entity.GetBodyShape world <> desiredBodyShape then entity.SetBodyShape desiredBodyShape world

    let private computeDuck (entity : Entity) (player : FirstPersonPlayer) (world : World) =
        let wantsDuck = World.isKeyboardKeyDown KeyboardKey.LCtrl world
        let ducked = wantsDuck || not (canUnduck entity world)
        applyDuckBodyShape entity ducked world
        let target = if ducked then 1.0f else 0.0f
        let step = DuckRate * getFrameTime world
        let duckAmount =
            if player.DuckAmount < target then min target (player.DuckAmount + step)
            elif player.DuckAmount > target then max target (player.DuckAmount - step)
            else player.DuckAmount
        { player with DuckAmount = duckAmount; Ducked = ducked }

    let private computeWishMove (player : FirstPersonPlayer) (rotation : Quaternion) (world : World) =
        let forward = rotation.Forward.WithY 0.0f
        let right = rotation.Right.WithY 0.0f
        let forward = if forward.LengthSquared () > 0.0f then forward.Normalized else v3Forward
        let right = if right.LengthSquared () > 0.0f then right.Normalized else v3Right
        let forwardMove =
            (if World.isKeyboardKeyDown KeyboardKey.W world then 1.0f else 0.0f) -
            (if World.isKeyboardKeyDown KeyboardKey.S world then 1.0f else 0.0f)
        let sideMove =
            (if World.isKeyboardKeyDown KeyboardKey.D world then 1.0f else 0.0f) -
            (if World.isKeyboardKeyDown KeyboardKey.A world then 1.0f else 0.0f)
        let wishVelocity = forward * forwardMove + right * sideMove
        let wishMagnitude = min 1.0f (wishVelocity.Length ())
        if wishMagnitude > 0.0f then
            let wishDirection = wishVelocity / wishVelocity.Length ()
            let standingSpeed = if World.isKeyboardKeyDown KeyboardKey.LShift world then SprintSpeed else WalkSpeed
            let maxSpeed = standingSpeed + (DuckSpeed - standingSpeed) * player.DuckAmount
            struct (wishDirection, wishMagnitude * maxSpeed)
        else struct (v3Zero, 0.0f)

    let private applyFriction (frameTime : single) (velocity : Vector3) =
        let horizontalVelocity = velocity.WithY 0.0f
        let speed = horizontalVelocity.Length ()
        if speed <= 0.0001f then velocity.WithY velocity.Y
        else
            let control = if speed < StopSpeed then StopSpeed else speed
            let drop = control * Friction * SurfaceFriction * frameTime
            let newSpeed = max 0.0f (speed - drop)
            let scale = newSpeed / speed
            v3 (velocity.X * scale) velocity.Y (velocity.Z * scale)

    let private accelerate (frameTime : single) (acceleration : single) (wishDirection : Vector3) (wishSpeed : single) (velocity : Vector3) =
        if wishSpeed <= 0.0f then velocity
        else
            let currentSpeed = Vector3.Dot (velocity, wishDirection)
            let addSpeed = wishSpeed - currentSpeed
            if addSpeed <= 0.0f then velocity
            else
                let accelerationSpeed = min addSpeed (acceleration * frameTime * wishSpeed * SurfaceFriction)
                velocity + wishDirection * accelerationSpeed

    let private airAccelerate (frameTime : single) (wishDirection : Vector3) (wishSpeed : single) (velocity : Vector3) =
        if wishSpeed <= 0.0f then velocity
        else
            let wishSpeedCapped = min AirSpeedCap wishSpeed
            let currentSpeed = Vector3.Dot (velocity, wishDirection)
            let addSpeed = wishSpeedCapped - currentSpeed
            if addSpeed <= 0.0f then velocity
            else
                let accelerationSpeed = min addSpeed (AirAcceleration * frameTime * wishSpeed * SurfaceFriction)
                velocity + wishDirection * accelerationSpeed

    let private clipGroundVelocity (bodyId : BodyId) (velocity : Vector3) (world : World) =
        match World.getBodyToGroundContactNormalOpt bodyId world with
        | Some normal when normal.LengthSquared () > 0.0f ->
            let normal = normal.Normalized
            let backoff = Vector3.Dot (velocity, normal)
            let velocity = if backoff < 0.0f then velocity - normal * backoff else velocity
            if normal.Y >= 0.95f && velocity.Y > 0.0f then velocity.WithY 0.0f else velocity
        | Some _ | None ->
            if velocity.Y > 0.0f then velocity.WithY 0.0f else velocity

    let private computeMoveVelocity (player : FirstPersonPlayer) (bodyId : BodyId) (grounded : bool) (jumping : bool) (rotation : Quaternion) (linearVelocity : Vector3) (world : World) =
        let frameTime = getFrameTime world
        let struct (wishDirection, wishSpeed) = computeWishMove player rotation world
        if grounded && not jumping then
            linearVelocity
            |> applyFriction frameTime
            |> accelerate frameTime GroundAcceleration wishDirection wishSpeed
            |> fun velocity -> clipGroundVelocity bodyId velocity world
        else airAccelerate frameTime wishDirection wishSpeed linearVelocity

    let private tryJump (grounded : bool) (player : FirstPersonPlayer) (rotation : Quaternion) (linearVelocity : Vector3) (world : World) =
        let time = world.UpdateTime
        let jumpDown = World.isKeyboardKeyDown KeyboardKey.Space world
        let jumping =
            grounded &&
            jumpDown &&
            player.JumpReleased &&
            time >= player.LastJumpTime + JumpCooldownUpdates
        if jumping then
            let struct (wishDirection, wishSpeed) = computeWishMove player rotation world
            let forwardBoost = if wishSpeed > 0.0f then wishDirection * JumpForwardBoost else v3Zero
            let linearVelocity = (linearVelocity + forwardBoost).WithY (max JumpSpeed linearVelocity.Y)
            struct ({ player with LastJumpTime = time; JumpReleased = false }, linearVelocity, true)
        else
            let player = if not jumpDown then { player with JumpReleased = true } else player
            struct (player, linearVelocity, false)

    let syncCamera (entity : Entity) (player : FirstPersonPlayer) (world : World) =
        let eyeRotation = Quaternion.CreateFromYawPitchRoll (player.Yaw, player.Pitch, 0.0f)
        World.setEye3dCenter (entity.GetPosition world + getEyeOffset player) world
        World.setEye3dRotation eyeRotation world
        World.setEye3dFieldOfView 0.75f world

    let update (entity : Entity) (world : World) =
        let bodyId = entity.GetBodyId world
        let grounded = World.getBodyGrounded bodyId world
        let player = entity.GetFirstPersonPlayer world
        let player = if grounded then { player with LastGroundedTime = world.UpdateTime } else player
        let player = computeLook player world
        let player = if world.Advancing then computeDuck entity player world else player
        let bodyRotation = Quaternion.CreateFromAxisAngle (v3Up, player.Yaw)
        entity.SetRotation bodyRotation world
        if world.Advancing then
            let linearVelocity = entity.GetLinearVelocity world
            let struct (player, linearVelocity, jumping) = tryJump grounded player bodyRotation linearVelocity world
            let linearVelocity = computeMoveVelocity player bodyId grounded jumping bodyRotation linearVelocity world
            entity.SetLinearVelocity linearVelocity world
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
         Entity.BodyShape == standingBodyShape
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
