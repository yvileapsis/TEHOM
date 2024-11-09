// Nu Game Engine Extensions

namespace Nu
open System
open Nu

/// Augments an entity with rich text.
type CameraFacet () =
    inherit Facet (false, false, false)

    static let synchronize evt world =
        let entity = evt.Subscriber : Entity

        if entity.GetEnabled world then

            let position = entity.GetPosition world
            let rotation = entity.GetRotation world

            let world = World.setEye3dCenter position world
            let world = World.setEye3dRotation rotation world

            Cascade, world

        else

            Cascade, world

    override this.Register (entity, world) =
        world
        |> World.sense synchronize Nu.Game.Handle.RegisterEvent entity (nameof CameraFacet)
        |> World.sense synchronize Nu.Game.Handle.PreUpdateEvent entity (nameof CameraFacet)

    override this.Render (_, _, _) =
        // TODO: add guizmos for display
        ()

/// Gives an entity the base behavior of a gui text control.
type CameraDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets = [
        typeof<CameraFacet>
    ]

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

/// Gives an entity the base behavior of a gui text control.
type GlyphDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<BackdroppableFacet>
         typeof<GlyphFacet>]

    static member Properties =
        [define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))]

[<RequireQualifiedAccess>]
module ContentEx =

    let camera entityName initializers = Content.entity<CameraDispatcher> entityName initializers

    let glyph entityName initializers = Content.entity<GlyphDispatcher> entityName initializers