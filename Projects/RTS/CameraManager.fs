namespace MyGame

open System
open System.Numerics
open Prime
open Nu

type CameraRotation = {
    PositionStart : float32
    PositionLast : float32
}

type CameraQuickMove = {
    PositionStart : Vector2
}

type [<ReferenceEquality; SymbolicExpansion>] CameraManager = {
      SpeedMinimum : float32
      SpeedMaximum : float32
      QuickMoveMargin : Vector2

      RotationSpeed : float32

      Angle : float32

      DistanceMinimum : float32
      DistanceMaximum : float32

      ZoomCurrent : float32
      ZoomStep : float32

      ActiveRotation : CameraRotation option
      ActiveQuickMove : CameraQuickMove option
}
with
    static member initial = {
        SpeedMinimum = 0.5f
        SpeedMaximum = 2.0f
        QuickMoveMargin = v2 4f 4f

        RotationSpeed = 0.1f

        Angle = 45f

        DistanceMinimum = 4f
        DistanceMaximum = 12f

        ZoomCurrent = 1.0f
        ZoomStep = 0.05f

        ActiveRotation = None
        ActiveQuickMove = None
    }

type CameraManagerMessage =
    | Update
    | MouseMove of MouseMoveData
    | MouseButton of MouseButtonData
    | MouseWheel of MouseWheelData
    | Move
    | Zoom of float32
    | RotationStart
    | RotationProcess of CameraRotation
    | RotationFinish of CameraRotation
    | QuickMoveStart
    | QuickMoveProcess of CameraQuickMove
    | QuickMoveFinish
    interface Message

type CameraManagerCommand =
    | Register
    | SetTransform of Vector3 * Quaternion
    | AddTransform of Vector3 * Quaternion
    interface Command

[<AutoOpen>]
module CameraManagerExtensions =
    type Entity with
        member this.GetCameraManager world = this.GetModelGeneric<CameraManager> world
        member this.SetCameraManager value world = this.SetModelGeneric<CameraManager> value world
        member this.CameraManager = this.ModelGeneric<CameraManager> ()


type CameraManagerDispatcher () =
    inherit Entity3dDispatcher<CameraManager, CameraManagerMessage, CameraManagerCommand> (false, false, false, CameraManager.initial)

    override this.Definitions (_, _) = [
        Entity.Rotation == Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 45f, 0f, 0f)
        Entity.UpdateEvent => Update
        Game.MouseWheelEvent =|> fun evt -> MouseWheel evt.Data
        Game.MouseMoveEvent =|> fun evt -> MouseMove evt.Data
        Game.MouseMiddleDownEvent =|> fun evt -> MouseButton evt.Data
        Game.MouseMiddleUpEvent =|> fun evt -> MouseButton evt.Data
        Game.MouseRightDownEvent =|> fun evt -> MouseButton evt.Data
        Game.MouseRightUpEvent =|> fun evt -> MouseButton evt.Data
    ]

    override this.Message (model, message, entity, world) =

        match message with
        | Update ->
            let signals : Signal list = [
                Move
                if model.ActiveQuickMove.IsSome then QuickMoveProcess model.ActiveQuickMove.Value
            ]

            withSignals signals model

        | MouseButton mouseButtonData ->
            let signals : Signal list = [
                if mouseButtonData.Button = MouseMiddle && mouseButtonData.Down then RotationStart
                if mouseButtonData.Button = MouseMiddle && not mouseButtonData.Down && model.ActiveRotation.IsSome
                then RotationFinish model.ActiveRotation.Value
                if mouseButtonData.Button = MouseRight && mouseButtonData.Down then QuickMoveStart
                if mouseButtonData.Button = MouseRight && not mouseButtonData.Down then QuickMoveFinish
            ]

            withSignals signals model

        | MouseMove _ ->
            let signals : Signal list = [
                if model.ActiveRotation.IsSome then RotationProcess model.ActiveRotation.Value
            ]

            withSignals signals model

        | MouseWheel mouseWheelData ->
            let signals : Signal list = [
                Zoom mouseWheelData.Travel
            ]

            withSignals signals model

        | Move ->
            let rotation = entity.GetRotation world

            let forward = rotation.Forward
            let right = rotation.Right
            let speed = model.SpeedMinimum

            let walkVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.W world then forward * speed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.S world then -forward * speed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.A world then -right * speed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.D world then right * speed else v3Zero)

            let position = if walkVelocity <> v3Zero then walkVelocity else v3Zero

            let turnSpeed = model.RotationSpeed

            let turnVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)

            let rotation = if turnVelocity <> 0.0f then Quaternion.CreateFromAxisAngle (v3Up, turnVelocity) else Quaternion.Identity

            let signals : Signal list = [
                AddTransform (position, rotation)
            ]

            withSignals signals model

        | Zoom travel ->
            let zoom = model.ZoomCurrent
            let step = model.ZoomStep

            let zoom =
                zoom - travel * step
                |> min 1f
                |> max 0f

            let character = { model with ZoomCurrent = zoom }
            just character

        | RotationStart ->
            let mousePosition = World.getMousePosition2dScreen world
            let activeRotation = { PositionStart = mousePosition.X; PositionLast = mousePosition.X}
            let character = { model with ActiveRotation = Some activeRotation }
            just character

        | RotationProcess activeRotation ->
            let turnSpeed = model.RotationSpeed

            let mousePosition = World.getMousePosition2dScreen world

            let turnVelocity =
                (mousePosition.X - activeRotation.PositionLast) * turnSpeed * 0.1f

            let signals : Signal list = [
                if turnVelocity <> 0.0f then AddTransform (v3Zero, Quaternion.CreateFromAxisAngle (v3Up, turnVelocity))
            ]

            let activeRotation = { activeRotation with PositionLast = mousePosition.X }

            let character = { model with ActiveRotation = Some activeRotation }

            withSignals signals character

        | RotationFinish activeRotation ->
            let mousePosition = World.getMousePosition2dScreen world

            if (activeRotation.PositionStart = mousePosition.X) then
                let character = { model with ActiveRotation = None; ZoomCurrent = 1f }

                let position = entity.GetPosition world
                let rotation = Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 45f, 0f, 0f)

                withSignals [SetTransform (position, rotation)] character
            else
                let character = { model with ActiveRotation = None }

                just character

        | QuickMoveStart ->
            let mousePosition = World.getMousePosition2dScreen world
            let activeQuickMove = { PositionStart = mousePosition }
            let character = { model with ActiveQuickMove = Some activeQuickMove }
            just character

        | QuickMoveProcess activeQuickMove ->
            let rotation = entity.GetRotation world

            let forward = rotation.Forward
            let right = rotation.Right

            let mousePosition = World.getMousePosition2dScreen world

            let speed =
                if ((mousePosition - activeQuickMove.PositionStart).Length () > model.QuickMoveMargin.Length ()) then
                    0.01f * model.SpeedMinimum * (mousePosition - activeQuickMove.PositionStart)
                else
                    v2Zero

            let signals : Signal list = [
                if speed <> v2Zero then AddTransform (forward * speed.Y + right * speed.X, Quaternion.Identity)
            ]

            withSignals signals model

        | QuickMoveFinish ->
            let character = { model with ActiveQuickMove = None }
            just character

    override this.Command (_, command, entity, world) =

        match command with
        | Register ->
            just world

        | SetTransform (position, rotation) ->
            let world = entity.SetPosition position world
            let world = entity.SetRotation rotation world
            just world

        | AddTransform (positionDelta, rotationDelta) ->
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let world = entity.SetPosition (position + positionDelta) world
            let world = entity.SetRotation (rotation * rotationDelta) world
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