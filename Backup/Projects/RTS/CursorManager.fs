namespace MyGame

open System
open System.Numerics
open Prime
open Nu

type [<ReferenceEquality; SymbolicExpansion>] CursorManager = {
      Cursor : CursorDirectionType
      CursorPriority : Map<CursorDirectionType, int>
}
with
    static member initial = {
        Cursor = CursorNormal
        CursorPriority = Map.ofSeq [
            CursorNormal, 10
            CursorUp, 0
            CursorDown, 0
            CursorLeft, 0
            CursorRight, 0
        ]
    }

    static member resetDirection =
        [ CursorUp, 0; CursorDown, 0; CursorLeft, 0; CursorRight, 0; ]

    static member direction (positionDelta : Vector2) =
        let size = (v3iDup 3).V2
        if positionDelta.Y > positionDelta.X * size.Y / size.X && positionDelta.Y > - positionDelta.X * size.Y / size.X then
            [ CursorUp, 20; CursorDown, 0; CursorLeft, 0; CursorRight, 0; ]
        elif positionDelta.Y < positionDelta.X * size.Y / size.X && positionDelta.Y < - positionDelta.X * size.Y / size.X then
            [ CursorUp, 0; CursorDown, 20; CursorLeft, 0; CursorRight, 0; ]
        elif positionDelta.Y > positionDelta.X * size.Y / size.X && positionDelta.Y < - positionDelta.X * size.Y / size.X then
            [ CursorUp, 0; CursorDown, 0; CursorLeft, 20; CursorRight, 0; ]
        elif positionDelta.Y < positionDelta.X * size.Y / size.X && positionDelta.Y > - positionDelta.X * size.Y / size.X then
            [ CursorUp, 0; CursorDown, 0; CursorLeft, 0; CursorRight, 20; ]
        else
            [ CursorUp, 0; CursorDown, 0; CursorLeft, 0; CursorRight, 0; ]

type CursorManagerMessage =
    | Update
    | SetCursor of (CursorDirectionType * int) list
    interface Message

type CursorManagerCommand =
    | Register
    interface Command

[<AutoOpen>]
module CursorManagerExtensions =
    type Entity with
        member this.GetCursorManager world = this.GetModelGeneric<CursorManager> world
        member this.SetCursorManager value world = this.SetModelGeneric<CursorManager> value world
        member this.CursorManager = this.ModelGeneric<CursorManager> ()

type CursorManagerDispatcher () =
    inherit GuiDispatcher<CursorManager, CursorManagerMessage, CursorManagerCommand> (CursorManager.initial)

    override this.Definitions (_, _) = [
        Entity.UpdateEvent => Update
        Entity.RegisterEvent => Register
        Events.SetCursorEvent --> Simulants.GameplayManagers --> Address.Wildcard =|> fun evt -> SetCursor evt.Data
    ]

    override this.Message (model, message, _, _) =

        match message with
        | Update ->
            just model

        | SetCursor newPriorities ->

            let priorities =
                newPriorities
                |> List.fold (fun acc (cursor, priority) -> Map.add cursor priority acc) model.CursorPriority

            let highest =
                priorities
                |> Map.toSeq
                |> Seq.maxBy snd
                |> fst

            let model = { model with Cursor = highest; CursorPriority = priorities }
            just model

    override this.Command (model, command, _, world) =

        match command with
        | Register ->
            just world

    override this.Content (model, _) = [

        let cursor =
            match model.Cursor with
            | CursorNormal -> Assets.Gameplay.CursorSprite
            | CursorUp -> Assets.Gameplay.CursorUpSprite
            | CursorDown -> Assets.Gameplay.CursorDownSprite
            | CursorLeft -> Assets.Gameplay.CursorLeftSprite
            | CursorRight -> Assets.Gameplay.CursorRightSprite

        ContentEx.cursor "Cursor" [
            Entity.StaticImage := cursor
            Entity.Size == v3 16f 16f 0f
        ]
    ]