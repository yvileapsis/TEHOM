namespace Tehom

open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module TextInputFacetModule =

    type Entity with
        member this.GetFocused world : bool = this.Get (nameof this.Focused) world
        member this.SetFocused (value : bool) world = this.Set (nameof this.Focused) value world
        member this.Focused = lens (nameof this.Focused) this this.GetFocused this.SetFocused

        member this.GetCaret world : int = this.Get (nameof this.Caret) world
        member this.SetCaret (value : int) world = this.Set (nameof this.Caret) value world
        member this.Caret = lens (nameof this.Caret) this this.GetCaret this.SetCaret

        member this.TextInputChangedEvent = stoa<string> "Changed/Event" --> this

    type TextInputFacet () =
        inherit Facet (false)
        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity

            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        Resolve, world |> entity.SetFocused true
                    else Resolve, world
                else Cascade, world |> entity.SetFocused false
            else Cascade, world |> entity.SetFocused false

        static let handleMouseLeftUp evt world =
            Cascade, world

        static let handleKeyboardKeyDown evt world =
            let entity = evt.Subscriber : Entity

            if entity.GetFocused world then

                let publishEvent world =
                    let text = entity.GetText world
                    let eventTrace = EventTrace.debug "TextInputFacet" "handleKeyboardKeyDown" "TextInput" EventTrace.empty
                    let world = World.publishPlus text entity.TextInputChangedEvent eventTrace entity true false world
                    world

                let moveCaret left world =
                    let text = entity.GetText world
                    let length = String.length text
                    let caret = entity.GetCaret world

                    let caret = caret + (if left then -1 else 1)
                    let caret = if caret < 0 then 0 else caret
                    let caret = if caret > length then length else caret

                    let world = entity.SetCaret caret world
                    world

                let removeKeyFromInput backspace world =

                    let caret = entity.GetCaret world
                    let caret = if backspace then caret - 1 else caret

                    let removeCharFromString (string: String) =
                        if caret >= 0 && caret < String.length string then
                            String (Array.removeAt caret (Array.ofSeq string))
                        else
                            string

                    let text = removeCharFromString (entity.GetText world)

                    let world = entity.SetText text world
                    let world = if backspace then moveCaret true world else world
                    world

                match evt.Data.KeyboardKey with
                | KeyboardKey.Backspace -> Resolve, publishEvent (removeKeyFromInput true world)
                | KeyboardKey.Delete -> Resolve, publishEvent (removeKeyFromInput false world)
                | KeyboardKey.Left -> Resolve, moveCaret true world
                | KeyboardKey.Right -> Resolve, moveCaret false world
                | _ -> Resolve, world
            else
                Resolve, world

        static let handleTextInput evt world =
            let entity = evt.Subscriber : Entity

            if entity.GetFocused world then
                let publishEvent world =
                    let text = entity.GetText world
                    let eventTrace = EventTrace.debug "TextInputFacet" "handleKeyboardKeyDown" "TextInput" EventTrace.empty
                    let world = World.publishPlus text entity.TextInputChangedEvent eventTrace entity true false world
                    world

                let moveCaret left world =
                    let text = entity.GetText world
                    let length = String.length text
                    let caret = entity.GetCaret world

                    let caret = caret + (if left then -1 else 1)
                    let caret = if caret < 0 then 0 else caret
                    let caret = if caret > length then length else caret

                    let world = entity.SetCaret caret world
                    world

                let rec addKeyToInput world =
                    let caret = entity.GetCaret world

                    let addCharToString char (string: String) =
                        String (Array.insertAt caret char (Array.ofSeq string))

                    let text = addCharToString evt.Data.TextInput (entity.GetText world)

                    let world = entity.SetText text world
                    let world = moveCaret false world
                    world

                let world = addKeyToInput world

                Resolve, publishEvent world
            else
                Cascade, world

        static member Properties = [
            define Entity.Focused false
            define Entity.Caret 0
            // TODO: allow for repeated key events after a short delay
        ]

        override this.Register (entity, world) =
            world
            |> World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof TextInputFacet)
            |> World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof TextInputFacet)
            |> World.sense handleTextInput Nu.Game.Handle.TextInputEvent entity (nameof TextInputFacet)
            |> World.sense handleKeyboardKeyDown Nu.Game.Handle.KeyboardKeyDownEvent entity (nameof TextInputFacet)

        override this.Render (_, entity, world) =

            if (entity.GetFocused world) then

                // TODO: there got to be a better way
                let string =
                    let caret = entity.GetCaret world
                    let length = String.length (entity.GetText world)
                    (String.replicate caret " ") + "_"

                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter // gui currently ignores rotation and scale
                let horizon = transform.Horizon
                let mutable textTransform = Transform.makeDefault false // centered-ness and offset are already baked into perimeter
                let margin = (entity.GetTextMargin world).V3
                let offset = (entity.GetTextOffset world).V3
                let shift = entity.GetTextShift world
                textTransform.Position <- perimeter.Min + margin + offset + v3 0f 0f 0f
                textTransform.Size <- perimeter.Size - margin * 2.0f
                textTransform.Elevation <- transform.Elevation + shift
                textTransform.Absolute <- transform.Absolute
                let font = entity.GetFont world

                World.enqueueLayeredOperation2d
                    { Elevation = textTransform.Elevation
                      Horizon = horizon
                      AssetTag = font
                      RenderOperation2d =
                        RenderText
                            { Transform = textTransform
                              Text = string
                              Font = font
                              Color = if transform.Enabled then entity.GetTextColor world else entity.GetTextDisabledColor world
                              Justification = entity.GetJustification world }}
                    world


[<AutoOpen>]
module TextInputDispatcherModule =

    /// Gives an entity the base behavior of a gui text control.
    type TextInputDispatcher () =
        inherit GuiDispatcher ()

        static member Facets =
            [typeof<TextFacet>
             typeof<TextInputFacet>
             typeof<BackdroppableFacet>]

        static member Properties =
            [define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))]


[<AutoOpen>]
module TextInput =
    let textInput entityName initializers = Content.entity<TextInputDispatcher> entityName initializers