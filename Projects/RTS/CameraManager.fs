namespace MyGame

open System
open System.Numerics
open Prime
open Nu

type [<ReferenceEquality; SymbolicExpansion>] CameraManager = {
      SpeedMinimum : float32
      SpeedMaximum : float32
      RotationSpeed : float32

      Angle : float32
      DistanceMinimum : float32
      DistanceMaximum : float32

      ZoomCurrent : float32
      ZoomStep : float32

      Rotating : (float32 * float32) option
      FastMoving : Vector2 option
      FastMovingDistanceMinimum : Vector2
}
with
    static member initial = {
        SpeedMinimum = 0.5f
        SpeedMaximum = 2.0f
        RotationSpeed = 0.1f

        Angle = 45f
        DistanceMinimum = 4f
        DistanceMaximum = 12f
        ZoomCurrent = 1.0f
        ZoomStep = 0.05f

        Rotating = None
        FastMoving = None
        FastMovingDistanceMinimum = v2 4f 4f
    }

type CameraManagerMessage =
    | Update
    | MouseMove of MouseMoveData
    | Zoom of MouseWheelData
    | RotateClickDown of MouseButtonData
    | RotateClickUp of MouseButtonData
    | RotateMove of MouseMoveData
    | FastMoveClickDown of MouseButtonData
    | FastMoveClickUp of MouseButtonData
    interface Message

type CameraManagerCommand =
    | Register
    | UpdateTransform of Vector3 * Quaternion
    interface Command

[<AutoOpen>]
module CameraManagerExtensions =
    type Entity with
        member this.GetCameraManager world = this.GetModelGeneric<CameraManager> world
        member this.SetCameraManager value world = this.SetModelGeneric<CameraManager> value world
        member this.CameraManager = this.ModelGeneric<CameraManager> ()


type CameraManagerDispatcher () =
    inherit Entity3dDispatcher<CameraManager, CameraManagerMessage, CameraManagerCommand> (false, false, false, CameraManager.initial)

    override this.Definitions (character, _) = [
        Entity.Rotation == Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 45f, 0f, 0f)
        Entity.UpdateEvent => Update
        Game.MouseWheelEvent =|> fun evt -> Zoom evt.Data
        Game.MouseMiddleDownEvent =|> fun evt -> RotateClickDown evt.Data
        Game.MouseMiddleUpEvent =|> fun evt -> RotateClickUp evt.Data
        Game.MouseMoveEvent =|> fun evt -> MouseMove evt.Data
        Game.MouseRightDownEvent =|> fun evt -> FastMoveClickDown evt.Data
        Game.MouseRightUpEvent =|> fun evt -> FastMoveClickUp evt.Data
    ]

    override this.Message (character, message, entity, world) =

        match message with
        | Update ->

            let position = entity.GetPosition world
            let rotation = entity.GetRotation world

            let forward = rotation.Forward
            let right = rotation.Right
            let speed = character.SpeedMinimum

            let walkVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.W world then forward * speed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.S world then -forward * speed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.A world then -right * speed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.D world then right * speed else v3Zero)

            let position = if walkVelocity <> v3Zero then position + walkVelocity else position

            let position =
                match character.FastMoving with
                | Some positionStart ->

                    let mousePosition = World.getMousePosition2dScreen world

                    let speed =
                        if ((mousePosition - positionStart).Length () > character.FastMovingDistanceMinimum.Length ()) then
                            0.01f * character.SpeedMinimum * (mousePosition - positionStart)
                        else
                            v2Zero

                    let walkVelocity =
                        forward * speed.Y + right * speed.X

                    let position = if walkVelocity <> v3Zero then position + walkVelocity else position

                    position
                | None ->
                    position

            let turnSpeed = character.RotationSpeed

            let turnVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)

            let rotation = if turnVelocity <> 0.0f then rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity) else rotation

            withSignals [UpdateTransform (position, rotation)] character

        | MouseMove mouseMoveData ->
            withSignals [RotateMove mouseMoveData] character

        | RotateClickDown _ ->
            let mousePosition = World.getMousePosition2dScreen world

            let character = { character with Rotating = Some (mousePosition.X, mousePosition.X) }

            just character

        | RotateClickUp _ ->
            match character.Rotating with
            | Some (positionStart, _) ->
                let position = entity.GetPosition world

                let mousePosition = World.getMousePosition2dScreen world

                if (positionStart = mousePosition.X) then
                    let character = { character with Rotating = None; ZoomCurrent = 1f }

                    let rotation = Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 45f, 0f, 0f)

                    withSignals [UpdateTransform (position, rotation)] character
                else
                    let character = { character with Rotating = None }

                    just character

            | None ->
                just character

        | RotateMove _ ->
            match character.Rotating with
            | Some (positionStart, positionLast) ->
                let position = entity.GetPosition world
                let rotation = entity.GetRotation world

                let turnSpeed = character.RotationSpeed

                let mousePosition = World.getMousePosition2dScreen world

                let turnVelocity =
                    (mousePosition.X - positionLast) * turnSpeed * 0.1f

                let rotation = if turnVelocity <> 0.0f then rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity) else rotation

                let character = { character with Rotating = Some (positionStart, mousePosition.X) }

                withSignals [UpdateTransform (position, rotation)] character

            | None ->
                just character
        | FastMoveClickDown _ ->
            let mousePosition = World.getMousePosition2dScreen world

            let character = { character with FastMoving = Some mousePosition }

            just character

        | FastMoveClickUp _ ->

            let character = { character with FastMoving = None }

            just character

        | Zoom mouseWheelData ->

            let zoom = character.ZoomCurrent
            let step = character.ZoomStep

            let zoomVelocity =
                mouseWheelData.Travel

            let zoom =
                zoom - zoomVelocity * step
                |> min 1f
                |> max 0f

            let character = {
                character with
                    ZoomCurrent = zoom
            }

            just character



    override this.Command (character, command, entity, world) =

        match command with
        | Register ->
            just world
        | UpdateTransform (position, rotation) ->
            let world = entity.SetPosition position world
            let world = entity.SetRotation rotation world
            just world

    override this.Content (character, entity) = [

        let distance = character.DistanceMinimum + character.ZoomCurrent * (character.DistanceMaximum - character.DistanceMinimum)
        let verticalPosition = distance * sin (Math.DegreesToRadians character.Angle)
        let horizontalPosition = distance * cos (Math.DegreesToRadians character.Angle)

        ContentEx.camera "Camera" [
            Entity.PositionLocal := v3 0f verticalPosition horizontalPosition
            Entity.RotationLocal := Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -character.Angle, 0f)
        ]

    ]