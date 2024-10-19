namespace Tehom

open System
open Nu
open NuMark

[<AutoOpen>]
module GlyphFacetModule =
/// Augments an entity with text.
    type GlyphFacet () =
        inherit Facet (false, false, false)

        static member Properties =
            [define Entity.Text ""
             define Entity.Font Assets.Default.Font
             define Entity.FontSizing None
             define Entity.FontStyling Set.empty
             define Entity.Justification (Justified (JustifyCenter, JustifyMiddle))
             define Entity.TextMargin v2Zero
             define Entity.TextColor Color.White
             define Entity.TextColorDisabled (Color (0.75f, 0.75f, 0.75f, 0.75f))
             define Entity.TextOffset v2Zero
             define Entity.TextShift 0.5f]

        override this.Render (_, entity, world) =
            let text = entity.GetText world
            if not (String.IsNullOrWhiteSpace text) then

                let text = text |> Seq.filter (fun c -> c <> '\n' && c <> '\r')

                World.enqueueLayeredOperations2d (Seq.indexed text
                |> Seq.map (fun (i, c) ->

                    let localPos = v3 ((float32 (i % 36)) * 10.0f - 350.0f) (- (float32 (i / 36)) * 10.0f + 110.0f) 0.0f
                    let mutable transform = entity.GetTransform world

                    let perimeter = transform.Perimeter // gui currently ignores rotation and scale
                    let horizon = transform.Horizon
                    let mutable textTransform = Transform.makeDefault ()
                    let margin = (entity.GetTextMargin world).V3
                    let offset = (entity.GetTextOffset world).V3
                    let shift = entity.GetTextShift world
                    textTransform.Position <- perimeter.Center + margin + offset + localPos
                    textTransform.Size <- perimeter.Size - margin * 2.0f
                    textTransform.Elevation <- transform.Elevation + shift
                    textTransform.Absolute <- transform.Absolute
                    let font = entity.GetFont world
                    let fontSizing = entity.GetFontSizing world
                    let fontStyling = entity.GetFontStyling world


                    { Elevation = textTransform.Elevation
                      Horizon = horizon
                      AssetTag = font
                      RenderOperation2d =
                        RenderText
                            { Transform = textTransform
                              ClipOpt = ValueSome textTransform.Bounds2d.Box2
                              Text = string c
                              Font = font
                              FontSizing = fontSizing
                              FontStyling = fontStyling
                              Color = if transform.Enabled then entity.GetTextColor world else entity.GetTextColorDisabled world
                              Justification = entity.GetJustification world }}

                )) world

        override this.GetAttributesInferred (_, _) =
            AttributesInferred.important Constants.Engine.EntityGuiSizeDefault v3Zero



[<AutoOpen>]
module GlyphDispatcherModule =

    /// Gives an entity the base behavior of a gui text control.
    type GlyphDispatcher () =
        inherit GuiDispatcher ()

        static member Facets =
            [typeof<BackdroppableFacet>
             typeof<GlyphFacet>]

        static member Properties =
            [define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))]


[<AutoOpen>]
module Glyph =
    let glyph entityName initializers = Content.entity<GlyphDispatcher> entityName initializers


[<AutoOpen>]
module RichTextFacetModule =

    /// Augments an entity with rich text.
    type RichTextFacet () =
        inherit Facet (false, false, false)

        static member Properties = [
            define Entity.Text ""
            define Entity.Font Assets.Default.Font
            define Entity.FontSizing None
            define Entity.FontStyling Set.empty
            define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))
            define Entity.TextMargin v2Zero
            define Entity.TextColor Color.Black
            define Entity.TextColorDisabled (Color (0.25f, 0.25f, 0.25f, 0.75f))
            define Entity.TextOffset v2Zero
            define Entity.TextShift 0.5f
        ]

        override this.Render (_, entity, world) =
            let text = entity.GetText world
            if not (String.IsNullOrWhiteSpace text) then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter // gui currently ignores rotation and scale
                let horizon = transform.Horizon

                let mutable textTransform = Transform.makeDefault ()  // centered-ness and offset are already baked into perimeter
                let margin = (entity.GetTextMargin world).V3
                let offset = (entity.GetTextOffset world).V3

                let shift = entity.GetTextShift world
                textTransform.Position <- perimeter.Min + margin + offset
                textTransform.Size <- perimeter.Size - margin * 2.0f
                textTransform.Elevation <- transform.Elevation + shift
                textTransform.Absolute <- transform.Absolute

                let font = entity.GetFont world
                let fontSizing = entity.GetFontSizing world
                let fontStyling = entity.GetFontStyling world
                let color = if transform.Enabled then entity.GetTextColor world else entity.GetTextColorDisabled world
                let justification = entity.GetJustification world

                let parsingResult = parseNuMark text

                let alterStyling blockStyle =
                    Set.fold (fun set x ->
                        match x with
                        | Bold -> Set.add FontStyle.Bold set
                        | Italics -> Set.add FontStyle.Italic set
                        | Underlined -> Set.add FontStyle.Underline set
                        | Strikethrough -> Set.add FontStyle.Strikethrough set
                        | _ -> set
                    ) fontStyling blockStyle

                let defaultJustify =
                    match justification with
                    | Justified (JustifyLeft, _) -> JustifyLeft
                    | Justified (JustifyRight, _) -> JustifyRight
                    | Justified (JustifyCenter, _) -> JustifyCenter
                    | Justified (JustifyFull, _) -> JustifyFull
                    | _ -> JustifyLeft

                let alterJustify entityJustification =
                    match entityJustification with
                    | Left -> JustifyLeft
                    | Right -> JustifyRight
                    | Center -> JustifyCenter
                    | Full -> JustifyFull
                    | Unjustified -> defaultJustify

                let paragraphList =
                    match parsingResult with
                    | Ok text ->
                        text
                        |> List.map (function
                            | Paragraph (justify, paragraph) ->
                                {
                                    Blocks =
                                        List.map (fun block -> {
                                            Text = block.Text
                                            Color = color
                                            Font = font
                                            FontSizing = fontSizing
                                            FontStyling = alterStyling block.Style
                                        }) paragraph
                                    Justification = alterJustify justify
                                }
                            | _ ->
                                {
                                    Blocks = List.empty
                                    Justification = defaultJustify
                                }
                        )
                    | Error (text, error) -> [
                        {
                            Blocks = [{
                                Text = text
                                Color = color
                                Font = font
                                FontSizing = fontSizing
                                FontStyling = fontStyling
                            }]
                            Justification = defaultJustify
                        }
                        {
                            Blocks = [{
                                Text = $"{error}"
                                Color = color
                                Font = font
                                FontSizing = fontSizing
                                FontStyling = fontStyling
                            }]
                            Justification = defaultJustify
                        }
                    ]

                World.enqueueLayeredOperation2d {
                    Elevation = textTransform.Elevation
                    Horizon = horizon
                    AssetTag = font
                    RenderOperation2d = RenderRichText {
                        Transform = textTransform
                        ClipOpt = ValueNone
                        Entries = paragraphList
                    }
                } world

        override this.GetAttributesInferred (_, _) =
            AttributesInferred.important Constants.Engine.EntityGuiSizeDefault v3Zero


[<AutoOpen>]
module RichTextDispatcherModule =

    /// Gives an entity the base behavior of a gui text control.
    type RichTextDispatcher () =
        inherit GuiDispatcher ()

        static member Facets =
            [typeof<BackdroppableFacet>
             typeof<RichTextFacet>]

        static member Properties =
            [define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))]


[<AutoOpen>]
module RichText =
    let richText entityName initializers = Content.entity<RichTextDispatcher> entityName initializers


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
        inherit Facet (false, false, false)
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
                let mutable textTransform = Transform.makeDefault () // centered-ness and offset are already baked into perimeter
                let margin = (entity.GetTextMargin world).V3
                let offset = (entity.GetTextOffset world).V3
                let shift = entity.GetTextShift world
                textTransform.Position <- perimeter.Min + margin + offset + v3 0f 0f 0f
                textTransform.Size <- perimeter.Size - margin * 2.0f
                textTransform.Elevation <- transform.Elevation + shift
                textTransform.Absolute <- transform.Absolute
                let font = entity.GetFont world
                let fontSizing = entity.GetFontSizing world
                let fontStyling = entity.GetFontStyling world

                World.enqueueLayeredOperation2d {
                    Elevation = textTransform.Elevation
                    Horizon = horizon
                    AssetTag = font
                    RenderOperation2d = RenderText {
                        Transform = textTransform
                        ClipOpt = ValueNone
                        Text = string
                        Font = font
                        FontSizing = fontSizing
                        FontStyling = fontStyling
                        Color = if transform.Enabled then entity.GetTextColor world else entity.GetTextColorDisabled world
                        Justification = entity.GetJustification world
                    }
                } world


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