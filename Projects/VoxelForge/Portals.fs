namespace VoxelForge
open System
open System.Numerics
open Prime
open Nu

type PortalId =
    | Blue
    | Orange

type PortalSurface =
    { Id : PortalId
      Center : Vector3
      Rotation : Quaternion
      HalfExtents : Vector2
      PairId : PortalId }

type PortalPair =
    { Blue : PortalSurface
      Orange : PortalSurface
      RecursionLimit : int }

type PortalPlayerTracking =
    { PreviousSignedDistances : Map<PortalId, single>
      LastTeleportTime : int64
      LastExitPortalOpt : PortalId option }

    static member empty =
        { PreviousSignedDistances = Map.empty
          LastTeleportTime = Int64.MinValue
          LastExitPortalOpt = None }

type PortalTraversalResult =
    { Position : Vector3
      EyeRotation : Quaternion
      LinearVelocity : Vector3
      Tracking : PortalPlayerTracking }

[<AutoOpen>]
module PortalApertureExtensions =
    type Entity with
        member this.GetPortalSourceId world : int64 = this.Get (nameof this.PortalSourceId) world
        member this.SetPortalSourceId value world = this.Set (nameof this.PortalSourceId) value world
        member this.PortalSourceId = lens (nameof this.PortalSourceId) this this.GetPortalSourceId this.SetPortalSourceId
        member this.GetPortalDestinationCenter world : Vector3 = this.Get (nameof this.PortalDestinationCenter) world
        member this.SetPortalDestinationCenter value world = this.Set (nameof this.PortalDestinationCenter) value world
        member this.PortalDestinationCenter = lens (nameof this.PortalDestinationCenter) this this.GetPortalDestinationCenter this.SetPortalDestinationCenter
        member this.GetPortalDestinationRotation world : Quaternion = this.Get (nameof this.PortalDestinationRotation) world
        member this.SetPortalDestinationRotation value world = this.Set (nameof this.PortalDestinationRotation) value world
        member this.PortalDestinationRotation = lens (nameof this.PortalDestinationRotation) this this.GetPortalDestinationRotation this.SetPortalDestinationRotation
        member this.GetPortalHalfExtents world : Vector2 = this.Get (nameof this.PortalHalfExtents) world
        member this.SetPortalHalfExtents value world = this.Set (nameof this.PortalHalfExtents) value world
        member this.PortalHalfExtents = lens (nameof this.PortalHalfExtents) this this.GetPortalHalfExtents this.SetPortalHalfExtents
        member this.GetPortalRecursionLimit world : int = this.Get (nameof this.PortalRecursionLimit) world
        member this.SetPortalRecursionLimit value world = this.Set (nameof this.PortalRecursionLimit) value world
        member this.PortalRecursionLimit = lens (nameof this.PortalRecursionLimit) this this.GetPortalRecursionLimit this.SetPortalRecursionLimit
        member this.GetPortalOneSided world : bool = this.Get (nameof this.PortalOneSided) world
        member this.SetPortalOneSided value world = this.Set (nameof this.PortalOneSided) value world
        member this.PortalOneSided = lens (nameof this.PortalOneSided) this this.GetPortalOneSided this.SetPortalOneSided
        member this.GetPortalTint world : Color = this.Get (nameof this.PortalTint) world
        member this.SetPortalTint value world = this.Set (nameof this.PortalTint) value world
        member this.PortalTint = lens (nameof this.PortalTint) this this.GetPortalTint this.SetPortalTint

[<RequireQualifiedAccess>]
module PortalLogic =

    let [<Literal>] TeleportCooldownUpdates = 5L
    let [<Literal>] ExitSurfaceDistance = 0.04f
    let [<Literal>] ExitRearmDistance = 0.25f
    let [<Literal>] CapsulePadding = 0.18f

    let defaultHalfExtents = v2 0.75f 1.15f

    let portalIdToInt64 portalId =
        match portalId with
        | Blue -> 1L
        | Orange -> 2L

    let portalTint portalId =
        match portalId with
        | Blue -> color 0.35f 0.7f 1.0f 1.0f
        | Orange -> color 1.0f 0.55f 0.18f 1.0f

    let private lookRotation (forward : Vector3) =
        let forward = if forward.LengthSquared () > 0.0f then forward.Normalized else v3Forward
        Quaternion.CreateFromRotationMatrix (Matrix4x4.CreateWorld (v3Zero, forward, v3Up))

    let pairAtGround (groundCenter : Vector3) =
        let centerY = groundCenter.Y + defaultHalfExtents.Y
        let centerZ = groundCenter.Z - 4.0f
        let blue =
            { Id = Blue
              Center = v3 (groundCenter.X - 2.0f) centerY centerZ
              Rotation = lookRotation v3Right
              HalfExtents = defaultHalfExtents
              PairId = Orange }
        let orange =
            { Id = Orange
              Center = v3 (groundCenter.X + 2.0f) centerY centerZ
              Rotation = lookRotation v3Left
              HalfExtents = defaultHalfExtents
              PairId = Blue }
        { Blue = blue
          Orange = orange
          RecursionLimit = 2 }

    let defaultPair =
        pairAtGround (v3 0.0f 7.02f 0.0f)

    let portals pair =
        [|pair.Blue; pair.Orange|]

    let pairedPortal (pair : PortalPair) portal =
        match portal.PairId with
        | Blue -> pair.Blue
        | Orange -> pair.Orange

    let signedDistance (position : Vector3) (portal : PortalSurface) =
        Vector3.Dot (position - portal.Center, portal.Rotation.Forward)

    let private clampToPortalFront (minimumDistance : single) (position : Vector3) (portal : PortalSurface) =
        let distance = signedDistance position portal
        if distance < minimumDistance
        then position + portal.Rotation.Forward * (minimumDistance - distance)
        else position

    let private pointOnPortalPlane (position : Vector3) (signedDistance : single) (portal : PortalSurface) =
        position - portal.Rotation.Forward * signedDistance

    let isPointWithinAperture (point : Vector3) (portal : PortalSurface) padding =
        let offset = point - portal.Center
        let x = Vector3.Dot (offset, portal.Rotation.Right)
        let y = Vector3.Dot (offset, portal.Rotation.Up)
        abs x <= portal.HalfExtents.X + padding &&
        abs y <= portal.HalfExtents.Y + padding

    let private makePortalMatrix (portal : PortalSurface) =
        let mutable matrix = Matrix4x4.CreateFromQuaternion portal.Rotation
        matrix.Translation <- portal.Center
        matrix

    let private rotationTransfer (source : PortalSurface) (destination : PortalSurface) =
        Matrix4x4.CreateFromQuaternion source.Rotation.Inverted *
        Matrix4x4.CreateRotationY MathF.PI *
        Matrix4x4.CreateFromQuaternion destination.Rotation

    let transferMatrix (source : PortalSurface) (destination : PortalSurface) =
        let sourceMatrix = makePortalMatrix source
        let destinationMatrix = makePortalMatrix destination
        let (_, sourceInverse) = Matrix4x4.Invert sourceMatrix
        sourceInverse * Matrix4x4.CreateRotationY MathF.PI * destinationMatrix

    let transferPosition (source : PortalSurface) (destination : PortalSurface) (position : Vector3) =
        Vector3.Transform (position, transferMatrix source destination)

    let transferDirection (source : PortalSurface) (destination : PortalSurface) (direction : Vector3) =
        Vector3.TransformNormal (direction, rotationTransfer source destination)

    let transferRotation (source : PortalSurface) (destination : PortalSurface) (rotation : Quaternion) =
        let transfer = rotationTransfer source destination
        let forward = Vector3.TransformNormal (rotation.Forward, transfer)
        let up = Vector3.TransformNormal (rotation.Up, transfer)
        let forward = if forward.LengthSquared () > 0.0f then forward.Normalized else destination.Rotation.Forward
        let up = if up.LengthSquared () > 0.0f then up.Normalized else destination.Rotation.Up
        Quaternion.CreateFromRotationMatrix (Matrix4x4.CreateWorld (v3Zero, forward, up))

    let signedDistances (position : Vector3) (pair : PortalPair) =
        portals pair
        |> Array.map (fun portal -> portal.Id, signedDistance position portal)
        |> Map.ofArray

    let yawPitchFromRotation (rotation : Quaternion) =
        let forward = rotation.Forward
        let forward = if forward.LengthSquared () > 0.0f then forward.Normalized else v3Forward
        let y = Math.Clamp (forward.Y, -1.0f, 1.0f)
        let yaw = MathF.Atan2 (-forward.X, -forward.Z)
        let pitch = MathF.Asin y
        struct (yaw, pitch)

    let tryResolvePlayerTraversal
        (pair : PortalPair)
        (tracking : PortalPlayerTracking)
        (time : int64)
        (playerPosition : Vector3)
        (eyePosition : Vector3)
        (eyeRotation : Quaternion)
        (linearVelocity : Vector3) =
        let currentDistances = signedDistances eyePosition pair
        let canTeleport = time >= tracking.LastTeleportTime + TeleportCooldownUpdates
        let mutable resultOpt = None
        let mutable i = 0
        let portalArray = portals pair
        while Option.isNone resultOpt && i < portalArray.Length do
            let portal = portalArray[i]
            let destination = pairedPortal pair portal
            let previousDistance =
                match tracking.PreviousSignedDistances.TryFind portal.Id with
                | Some distance -> distance
                | None -> currentDistances[portal.Id]
            let currentDistance = currentDistances[portal.Id]
            let exitingPortal =
                match tracking.LastExitPortalOpt with
                | Some exitPortalId when exitPortalId = portal.Id && currentDistance <= ExitRearmDistance -> true
                | Some _ | None -> false
            let crossing = previousDistance > 0.0f && currentDistance <= 0.0f
            let triggerPoint = pointOnPortalPlane eyePosition currentDistance portal
            if canTeleport && not exitingPortal && crossing && isPointWithinAperture triggerPoint portal CapsulePadding then
                let transformedEyePosition =
                    transferPosition portal destination eyePosition
                    |> fun position -> clampToPortalFront ExitSurfaceDistance position destination
                let transformedEyeOffset = transferDirection portal destination (eyePosition - playerPosition)
                let transformedPosition = transformedEyePosition - transformedEyeOffset
                let transformedRotation = transferRotation portal destination eyeRotation
                let transformedVelocity = transferDirection portal destination linearVelocity
                let tracking =
                    { PreviousSignedDistances = signedDistances transformedEyePosition pair
                      LastTeleportTime = time
                      LastExitPortalOpt = Some destination.Id }
                resultOpt <-
                    Some
                        { Position = transformedPosition
                          EyeRotation = transformedRotation
                          LinearVelocity = transformedVelocity
                          Tracking = tracking }
            i <- inc i
        match resultOpt with
        | Some result -> result
        | None ->
            let lastExitPortalOpt =
                match tracking.LastExitPortalOpt with
                | Some exitPortalId ->
                    match currentDistances.TryFind exitPortalId with
                    | Some distance when distance <= ExitRearmDistance -> Some exitPortalId
                    | Some _ | None -> None
                | None -> None
            { Position = playerPosition
              EyeRotation = eyeRotation
              LinearVelocity = linearVelocity
              Tracking =
                { tracking with
                    PreviousSignedDistances = currentDistances
                    LastExitPortalOpt = lastExitPortalOpt } }

type PortalApertureDispatcher () =
    inherit Entity3dDispatcher<unit, Message, Command> (false, false, false, ())

    static member Facets =
        []

    static member Properties =
        [define Entity.PortalSourceId 0L
         define Entity.PortalDestinationCenter v3Zero
         define Entity.PortalDestinationRotation Quaternion.Identity
         define Entity.PortalHalfExtents PortalLogic.defaultHalfExtents
         define Entity.PortalRecursionLimit 2
         define Entity.PortalOneSided true
         define Entity.PortalTint Color.White]

    override this.Definitions (_, _) =
        [Entity.Presence == Omnipresent
         Entity.AlwaysRender == true
         Entity.Static == true
         Entity.Size == v3 (PortalLogic.defaultHalfExtents.X * 2.0f) (PortalLogic.defaultHalfExtents.Y * 2.0f) 0.035f
         Entity.Scale == v3 (PortalLogic.defaultHalfExtents.X * 2.0f) (PortalLogic.defaultHalfExtents.Y * 2.0f) 0.035f
         Entity.Pickable == false
         Entity.CastShadow == false
         Entity.PortalSourceId == 0L
         Entity.PortalDestinationCenter == v3Zero
         Entity.PortalDestinationRotation == Quaternion.Identity
         Entity.PortalHalfExtents == PortalLogic.defaultHalfExtents
         Entity.PortalRecursionLimit == 2
         Entity.PortalOneSided == true
         Entity.PortalTint == Color.White]

    override this.Render (renderPass, entity, world) =
        if renderPass.IsNormalPass then
            Log.infoOnce ("VoxelForge portal aperture render message emitted.")
            let pair = PortalLogic.defaultPair
            let sourcePortalId = entity.GetPortalSourceId world
            let defaultPortalOpt =
                if entity.Name = Simulants.BluePortalAperture.Name then Some pair.Blue
                elif entity.Name = Simulants.OrangePortalAperture.Name then Some pair.Orange
                else None
            let struct (sourcePortalId, destinationCenter, destinationRotation, halfExtents, recursionLimit, oneSided, tint) =
                if sourcePortalId <> 0L then
                    struct
                        (entity.GetPortalSourceId world,
                         entity.GetPortalDestinationCenter world,
                         entity.GetPortalDestinationRotation world,
                         entity.GetPortalHalfExtents world,
                         entity.GetPortalRecursionLimit world,
                         entity.GetPortalOneSided world,
                         entity.GetPortalTint world)
                else
                    match defaultPortalOpt with
                    | Some portal ->
                        let destination = PortalLogic.pairedPortal pair portal
                        struct
                            (PortalLogic.portalIdToInt64 portal.Id,
                             destination.Center,
                             destination.Rotation,
                             portal.HalfExtents,
                             pair.RecursionLimit,
                             entity.GetPortalOneSided world,
                             PortalLogic.portalTint portal.Id)
                    | None ->
                        struct
                            (entity.GetPortalSourceId world,
                             entity.GetPortalDestinationCenter world,
                             entity.GetPortalDestinationRotation world,
                             entity.GetPortalHalfExtents world,
                             entity.GetPortalRecursionLimit world,
                             entity.GetPortalOneSided world,
                             entity.GetPortalTint world)
            let transform = entity.GetTransform world
            World.enqueueRenderMessage3d
                (RenderPortal3d
                    { SourcePortalId = sourcePortalId
                      SourceCenter = entity.GetPosition world
                      SourceRotation = entity.GetRotation world
                      SourceModelMatrix = transform.AffineMatrix
                      SourceHalfExtents = halfExtents
                      DestinationCenter = destinationCenter
                      DestinationRotation = destinationRotation
                      RecursionLimit = recursionLimit
                      OneSided = oneSided
                      Tint = tint
                      RenderPass = renderPass })
                world
