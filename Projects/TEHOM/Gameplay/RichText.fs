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

                let parsedText = NuMark.parseNuMark text


                let deffont = entity.GetFont world
                let deffontSizing = entity.GetFontSizing world
                let deffontStyling = entity.GetFontStyling world
                let defcolor = if transform.Enabled then entity.GetTextColor world else entity.GetTextDisabledColor world
                let defjustification = entity.GetJustification world


                let toDraw =
                    let (NuMark.NodeList list) = parsedText
                    list
                    |> List.map (fun (Paragraph (x, y)) -> x, y)
                    |> List.map (fun (justification, text) ->
                        let call = {
                            Transform = textTransform
                            Text = ""
                            Font = deffont
                            FontSizing = deffontSizing
                            FontStyling = deffontStyling
                            Color = defcolor
                            Justification = justification
                        }
                        match text with
                        | String x -> { call with Text = x }
                        | Bold (String x) -> { call with Text = x; FontStyling = Set.ofList [ FontStyle.Bold ] }
                        | Formatted (_, Strikethrough (String x)) -> { call with Text = x; FontStyling = Set.ofList [ FontStyle.Strikethrough ]; Color = Color.Red }
                        | _ -> { call with Text = "Not Handled" }

                    )


                World.enqueueLayeredOperation2d {
                    Elevation = textTransform.Elevation
                    Horizon = horizon
                    AssetTag = deffont
                    RenderOperation2d = RenderRichText toDraw
                } world

        override this.GetAttributesInferred (_, _) =
            AttributesInferred.make Constants.Engine.EntityGuiSizeDefault v3Zero


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