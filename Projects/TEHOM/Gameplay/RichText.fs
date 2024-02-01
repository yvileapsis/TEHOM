namespace Tehom

open System
open Nu

[<AutoOpen>]
module RichTextFacetModule =

    /// Augments an entity with text.
    type TextFacet () =
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
                World.enqueueLayeredOperation2d {
                    Elevation = textTransform.Elevation
                    Horizon = horizon
                    AssetTag = font
                    RenderOperation2d = RenderText {
                        Transform = textTransform
                        Text = text
                        Font = font
                        FontSizing = fontSizing
                        FontStyling = fontStyling
                        Color = if transform.Enabled then entity.GetTextColor world else entity.GetTextDisabledColor world
                        Justification = entity.GetJustification world
                    }
                } world

        override this.GetAttributesInferred (_, _) =
            AttributesInferred.make Constants.Engine.EntityGuiSizeDefault v3Zero