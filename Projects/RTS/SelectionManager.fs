namespace MyGame
open System
open System.Numerics
open Prime
open Nu

type [<ReferenceEquality; SymbolicExpansion>] SelectionManager = {
      Entities : Entity list
      PositionStart : Vector3
      PositionEnd : Vector3
      Selection : bool
}
with
    static member initial = {
        Entities = List.Empty
        Selection = false
        PositionStart = Vector3.Zero
        PositionEnd = Vector3.Zero
    }

type SelectionManagerMessage =
    | Update
    | SetEntities of Entity list
    | ClickUnclick of MouseButtonData
    | MouseMove of MouseMoveData
    interface Message

type SelectionManagerCommand =
    | Register
    | PublishSelectionEvent
    interface Command

[<AutoOpen>]
module SelectionManagerExtensions =
    type Entity with
        member this.GetSelectionManager world = this.GetModelGeneric<SelectionManager> world
        member this.SetSelectionManager value world = this.SetModelGeneric<SelectionManager> value world
        member this.SelectionManager = this.ModelGeneric<SelectionManager> ()
        member this.SelectionEvent = Events.SelectionEvent --> this


type SelectionManagerDispatcher () =
    inherit GuiDispatcher<SelectionManager, SelectionManagerMessage, SelectionManagerCommand> (SelectionManager.initial)

    override this.Definitions (character, _) = [
        Entity.Visible := character.Selection
        Entity.Position := (character.PositionStart + character.PositionEnd) / 2f
        Entity.Size := abs (character.PositionEnd - character.PositionStart)
        Game.MouseLeftDownEvent =|> fun evt -> ClickUnclick evt.Data
        Game.MouseLeftUpEvent =|> fun evt -> ClickUnclick evt.Data
        Game.MouseMoveEvent =|> fun evt -> MouseMove evt.Data
        Events.BeginSelectingEvent --> Simulants.Gameplay =|> fun evt -> SetEntities evt.Data
    ]

    override this.Message (character, message, entity, world) =

        match message with
        | ClickUnclick data ->
            if data.Down then
//                let position = data.Position / (single Constants.Render.VirtualScalar)
                let position = World.getMousePosition2dScreen world
                let character = {
                    character with
                        Selection = true
                        PositionStart = (v3 position.X position.Y 0f)
                        PositionEnd = (v3 position.X position.Y 0f)

                }
(*

            let firstIntersection =
                let ray = World.getMouseRay3dWorld world
                let origin = ray.Origin
                let finale = ray.Origin + 20f * ray.Direction
                let array = World.rayCast3dBodies origin finale 0xFFFFFFFF 0xFFFFFFFF false world
                Array.tryHead array

            match firstIntersection with
            | Some intersectionData ->
                match intersectionData.BodyShapeIntersected.BodyId.BodySource with
                | :? Entity as intersected when intersected.Is<CharacterDispatcher> world ->
                    // character select

                    let character = intersected.GetCharacter world

                    let character = { character with Selected = not character.Selected }

                    let world = intersected.SetCharacter character world

                    [PublishSelectionEvent], world
                | _ ->
                    [PublishSelectionEvent], world


*)

                just character
            else
                let position = World.getMousePosition2dScreen world
                let character = {
                    character with
                        Selection = false
                        PositionEnd = (v3 position.X position.Y 0f)
                }
                [PublishSelectionEvent], character
        | MouseMove data ->
            if character.Selection then
                let position = World.getMousePosition2dScreen world
                let character = {
                    character with
                        PositionEnd = (v3 position.X position.Y 0f)
                }
                just character
            else
                just character
        | Update ->
            just character
        | SetEntities entities ->
            let character = { character with Entities = entities }
            just character

    override this.Command (character, command, entity, world) =

        match command with
        | PublishSelectionEvent ->

            let transform = entity.GetTransform world
            let perimeter = transform.Perimeter.Box2

            let entities =
                character.Entities
                |> List.filter (fun entity ->
                    let v3 = entity.GetPosition world
                    let position =
                        (World.position3dToPosition2d v3 world).V2 / (single Constants.Render.VirtualScalar)
                    perimeter.Intersects position
                )
            let world = World.publish entities entity.SelectionEvent entity world
            just world
        | _ ->
            just world

    override this.Content (character, entity) = [

        Content.staticSprite "Image" [
            Entity.Size := abs (character.PositionEnd - character.PositionStart)
            Entity.StaticImage == Assets.Default.White
            Entity.Color == Color.Cyan.WithA 0.5f
        ]

    ]