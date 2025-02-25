namespace Tehom

open System
open System.Numerics
open Prime
open Nu

type Menu = {
    State : String
    LastMousePosition : Vector2 option
    LastPosition : Vector3 option
    Position : Vector3
}
with
    static member empty = {
        State = "Test"
        LastMousePosition = None
        LastPosition = None
        Position = v3Zero
    }

type MenuMessage =
    | ClickDown
    | ClickUp
    | UpdatePosition
    | Update
    | TimeUpdate
    interface Message

type MenuCommand =
    | ResetCombatants
    interface Command

type MenuDispatcher () =
    inherit Entity2dDispatcher<Menu, MenuMessage, MenuCommand> (false, false, false, Menu.empty)

    override this.Definitions (model, _) = [
        Entity.Position := model.Position
        Entity.Size == Constants.Render.DisplayVirtualResolution.V3 / 2f
        Screen.DeselectingEvent => FinishQuitting
        Game.UpdateEvent => Update
        Game.PostUpdateEvent => UpdatePosition
        Screen.TimeUpdateEvent => TimeUpdate
        Entity.AlwaysUpdate == true
        Entity.RegisterEvent => ResetCombatants
    ]

    override this.Message (model, message, entity, world) =
        match message with
        | Update ->
            let inPerimeter =
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                perimeter.Intersects mousePositionWorld

            let signals : Signal list = [
                if inPerimeter && World.isMouseButtonPressed MouseLeft world then ClickDown
                if World.isMouseButtonUp MouseLeft world then ClickUp
            ]

            withSignals signals model

        | TimeUpdate ->
            just model

        | ClickDown ->
            let mousePosition = World.getMousePosition2dScreen world
            [], {
                model with
                    LastPosition = Some model.Position
                    LastMousePosition = Some mousePosition
            }

        | ClickUp ->
            [], {
                model with
                    LastPosition = None
                    LastMousePosition = None
            }

        | UpdatePosition ->
            if model.LastPosition.IsSome then
                let mousePosition = World.getMousePosition2dScreen world - model.LastMousePosition.Value
                let position = model.LastPosition.Value
                let position = position + v3 mousePosition.X mousePosition.Y 0f
                let model = {
                    model with
                        Position = position
                }
                just model
            else
                just model

    override this.Command (model, command, entity, world) =

        match command with
        | ResetCombatants ->
            just world

    override this.Content (model, _) = [

        Content.staticSprite "Background" [
            Entity.Size == Constants.Render.DisplayVirtualResolution.V3 / 2f
            Entity.StaticImage == Assets.Default.Black
            Entity.Color == Color.White.WithA 0.5f
        ]
    ]