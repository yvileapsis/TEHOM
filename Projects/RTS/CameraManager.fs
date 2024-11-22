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

      PositionPrevious : Vector3 FQueue
      RotationPrevious : Quaternion FQueue

      Position : Vector3
      Rotation : Quaternion
      RotationDefault : Quaternion
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

        PositionPrevious = FQueue.empty
        RotationPrevious = FQueue.empty

        Position = v3Zero
        Rotation = Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 45f, 0f, 0f)
        RotationDefault = Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 45f, 0f, 0f)
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
        Entity.Absolute == true
        Entity.UpdateEvent => Update
        Game.MouseWheelEvent =|> fun evt -> MouseWheel evt.Data
        Game.MouseMoveEvent =|> fun evt -> MouseMove evt.Data
        Game.MouseMiddleDownEvent =|> fun evt -> MouseButton evt.Data
        Game.MouseMiddleUpEvent =|> fun evt -> MouseButton evt.Data
        Game.MouseRightDownEvent =|> fun evt -> MouseButton evt.Data
        Game.MouseRightUpEvent =|> fun evt -> MouseButton evt.Data
    ]

    override this.Message (model, message, _, world) =

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
            let rotation = model.Rotation
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

            let model = {
                model with
                    Position = model.Position + position
                    Rotation = model.Rotation * rotation
            }

            just model

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

            let activeRotation = { activeRotation with PositionLast = mousePosition.X }

            let model = {
                model with
                    ActiveRotation = Some activeRotation
                    Rotation = model.Rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity)
            }

            just model

        | RotationFinish activeRotation ->
            let mousePosition = World.getMousePosition2dScreen world

            if (activeRotation.PositionStart = mousePosition.X) then

                let model = {
                    model with
                        ActiveRotation = None
                        ZoomCurrent = 1f
                        Rotation = model.RotationDefault
                }

                just model
            else
                let model = {
                    model with
                        ActiveRotation = None
                }

                just model

        | QuickMoveStart ->
            let mousePosition = World.getMousePosition2dScreen world
            let activeQuickMove = { PositionStart = mousePosition }
            let character = { model with ActiveQuickMove = Some activeQuickMove }
            just character

        | QuickMoveProcess activeQuickMove ->
            let rotation = model.Rotation

            let forward = rotation.Forward
            let right = rotation.Right

            let mousePosition = World.getMousePosition2dScreen world

            let speed =
                if ((mousePosition - activeQuickMove.PositionStart).Length () > model.QuickMoveMargin.Length ()) then
                    0.01f * model.SpeedMinimum * (mousePosition - activeQuickMove.PositionStart)
                else
                    v2Zero

            let model = {
                model with
                    Position = model.Position + forward * speed.Y + right * speed.X
            }

            just model

        | QuickMoveFinish ->
            let character = { model with ActiveQuickMove = None }
            just character

    override this.Command (_, command, _, world) =

        match command with
        | Register ->
            just world

    override this.Content (character, _) = [

        Content.composite "Object" [
            Entity.Position := character.Position
            Entity.Rotation := character.Rotation
        ] [

            let distance = character.DistanceMinimum + character.ZoomCurrent * (character.DistanceMaximum - character.DistanceMinimum)
            let angle = Math.DegreesToRadians character.Angle

            ContentEx.camera "Subject" [
                Entity.PositionLocal := distance * v3 0f (sin angle) (cos angle)
                Entity.RotationLocal := Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -character.Angle, 0f)
            ]
        ]

    ]