namespace Tehom

open System
open Nu
open Tehom.NuMark

[<AutoOpen>]
module RichTextFacetModule =

    /// Augments an entity with text.
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

                let parsedText =
                    NuMark.parseNuMark text
                    |> List.map (function
                        | NuMark.Paragraph (justification, line) ->
                            {
                                Blocks = line
                                |> List.map (fun block -> {
                                    Text = block.Text
                                    Color = Color.White
                                    Font = Assets.Gui.MontSerratFont
                                    FontSizing = Some 10
                                    FontStyling = Set.fold (fun set x ->
                                        match x with
                                        | Bold -> Set.add FontStyle.Bold set
                                        | Italics -> Set.add FontStyle.Italic set
                                        | Underlined -> Set.add FontStyle.Underline set
                                        | Strikethrough -> Set.add FontStyle.Strikethrough set
                                        | _ -> set
                                    ) Set.empty block.Style
                                })
                                Justification = justification
                            }
                        | _ ->
                            {
                                Blocks = List.empty
                                Justification = Justification.Unjustified true
                            }
                    )


                let deffont = entity.GetFont world
                let deffontSizing = entity.GetFontSizing world
                let deffontStyling = entity.GetFontStyling world
                let defcolor = if transform.Enabled then entity.GetTextColor world else entity.GetTextDisabledColor world
                let defjustification = entity.GetJustification world
                World.enqueueLayeredOperation2d {
                    Elevation = textTransform.Elevation
                    Horizon = horizon
                    AssetTag = deffont
                    RenderOperation2d = RenderRichText {
                        Transform = textTransform
                        Entries = parsedText
                    }
                } world

        override this.GetAttributesInferred (_, _) =
            AttributesInferred.important Constants.Engine.EntitySizeGuiDefault v3Zero


[<AutoOpen>]
module RichTextDispatcherModule =

    /// Gives an entity the base behavior of a gui text control.
    type RichTextDispatcher () =
        inherit GuiDispatcher ()

        static member Facets =
            [typeof<RichTextFacet>
             typeof<BackdroppableFacet>]

        static member Properties =
            [define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))]


[<AutoOpen>]
module RichText =
    let richText entityName initializers = Content.entity<RichTextDispatcher> entityName initializers