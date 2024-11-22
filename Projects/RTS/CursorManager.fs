namespace MyGame

open System
open System.Numerics
open Prime
open Nu

type [<ReferenceEquality; SymbolicExpansion>] CursorManager = {
      Selection : bool
}
with
    static member initial = {
        Selection = false
    }

type CursorManagerMessage =
    | Update
    interface Message

type CursorManagerCommand =
    | Register
    interface Command

[<AutoOpen>]
module CursorManagerExtensions =
    type Entity with
        member this.GetCameraManager world = this.GetModelGeneric<CameraManager> world
        member this.SetCameraManager value world = this.SetModelGeneric<CameraManager> value world
        member this.CameraManager = this.ModelGeneric<CameraManager> ()

type CursorManagerDispatcher () =
    inherit GuiDispatcher<CursorManager, CursorManagerMessage, CursorManagerCommand> (CursorManager.initial)

    override this.Definitions (_, _) = [
        Entity.UpdateEvent => Update
        Entity.RegisterEvent => Register
    ]

    override this.Message (model, message, _, _) =

        match message with
        | Update ->
            just model

    override this.Command (_, command, _, world) =

        match command with
        | Register ->
            just world

    override this.Content (_, _) = [

        ContentEx.cursor "Cursor" [
            Entity.StaticImage == Assets.Gameplay.CursorSprite
            Entity.Size == v3 16f 16f 0f
        ]
    ]