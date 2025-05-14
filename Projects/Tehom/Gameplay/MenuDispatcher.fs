namespace Tehom

open System
open System.Numerics
open Prime
open Nu

(*

Entity Traits -> Sensing system -> Descriptions + Intensity

Intensity is limited

*)

type Menu = {
    GameplayTime : Int64
    State : String
    LastMousePosition : Vector2 option
    LastPosition : Vector3 option
    Position : Vector3
    Limiter : Int32
}
with
    static member empty = {
        GameplayTime = 0
        State = "Test"
        LastMousePosition = None
        LastPosition = None
        Position = v3 0f 20f 0f
        Limiter = 0
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
        Game.TimeUpdateEvent => TimeUpdate
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
            let gameDelta = world.GameDelta
            let model = { model with GameplayTime = model.GameplayTime + gameDelta.Updates }
            let model =
                if model.GameplayTime % 10L = 0L then
                    { model with Limiter = model.Limiter + 10 }
                else
                    model
            just model

        | ClickDown ->
            let mousePosition = World.getMousePosition2dScreen world
            [], {
                model with
                    LastPosition = Some model.Position
                    LastMousePosition = Some mousePosition
                    Limiter = model.Limiter + 10
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

        let text = [
            100, "You wake up on a rusted **gurney**, its thin mattress stiff with age. The wheels are misaligned, one of them jammed with something brittle, as if shattered glass had been ground into the mechanism. A faint, lingering warmth clings to the sheets beneath you—like someone else had just been here."
            90, "The air smells of old antiseptic, overtaken by dust and something faintly metallic."
            80, "The windows are barricaded, wooden planks bolted over the glass, gaps thin enough to let in only slivers of light. Some of the boards are deeply gouged."
            70, "A **security camera**, lens cracked. The power light is off, but it still seems to be aimed directly at you."
            60, "At the far end of the hall, a **heavy metal door** stands locked, secured by a **keypad** mechanism. The numbers are worn, dulled by countless presses. Someone scratched something into the metal just beneath the keypad, but it has been deliberately scraped away. Only faint indentations remain."
            50, "A wall-mounted **payphone** is bolted to the wall nearby. The handset dangles from its cord, swaying slightly. A dial tone hums softly from the receiver."
            40, "An old **vending machine**, dark and unpowered, its glass front smeared with handprints. One of the buttons has been jammed inward, as if someone pressed it too hard. The snack behind it is missing."
            30, "A directory sign, most of the text scratched away—except for one word: DOORS. Someone has circled it in ink, pressing so hard that the plastic beneath is slightly warped."
            20, "A toppled wheelchair, its footrests twisted as if someone forced their way out of it in a hurry. The wheels are caked in dried grime, except for one small, spotless streak."
            10, "A row of waiting chairs lines the wall. The plastic seats are cracked, their metal frames rusted. One chair near the back is missing entirely, but the dust outline where it once sat is perfectly clean."
        ]

        let text =
            text
            |> List.sortBy fst
            |> List.rev
            |> List.choose (fun (value, str) ->
                let value = 100 - value
                if value < model.Limiter then
                    Some str
                else
                    None
            )
            |> List.join "\n\n"


        Content.staticSprite "Background" [
            Entity.Size == Constants.Render.DisplayVirtualResolution.V3 / 2f
            Entity.StaticImage == Assets.Default.Black
            Entity.Color == Color.White.WithA 0.5f
        ]

        ContentEx.richText "Text" [
            Entity.FontSizing == Some 5
            Entity.PositionLocal == v3 0f 60f 0f
            Entity.Size == v3 300f 32f 0f
            Entity.TextColor == Color.FloralWhite
            Entity.Text := text
        ]
    ]