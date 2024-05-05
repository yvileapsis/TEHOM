namespace Tehom

open System
open Nu
open NuMark

[<AutoOpen>]
module RichTextFacetModule =

    /// Augments an entity with rich text.
    type RichTextFacet () =
        inherit Facet (false)

        static member Properties = [
            define Entity.Text ""
            define Entity.Font Assets.Default.Font
            define Entity.FontSizing None
            define Entity.FontStyling Set.empty
            define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))
            define Entity.TextMargin v2Zero
            define Entity.TextColor Color.Black
            define Entity.TextDisabledColor (Color (0.25f, 0.25f, 0.25f, 0.75f))
            define Entity.TextOffset v2Zero
            define Entity.TextShift 0.5f
        ]

        override this.Render (_, entity, world) =
            let text = entity.GetText world
            if not (String.IsNullOrWhiteSpace text) then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter // gui currently ignores rotation and scale
                let horizon = transform.Horizon

                let mutable textTransform = Transform.makeDefault false // centered-ness and offset are already baked into perimeter
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
                let color = if transform.Enabled then entity.GetTextColor world else entity.GetTextDisabledColor world
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

                let alterJustify = function
                    | Left -> JustifyLeft
                    | Right -> JustifyRight
                    | Center -> JustifyCenter
                    // incorrect
                    | Full -> JustifyFull

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