// Nu Game Engine Extensions

namespace Nu
open System
open Nu
open SDL2

/// Augments an entity with rich text.
type CameraFacet () =
    inherit Facet (false, false, false)

    static let synchronize (entity : Entity) world =
        if entity.GetEnabled world then

            let position = entity.GetPosition world
            let rotation = entity.GetRotation world

            let world = World.setEye3dCenter position world
            let world = World.setEye3dRotation rotation world

            world
        else
            world

    override this.Register (entity, world) =
        world
        |> synchronize entity
        |> World.sense (fun evt world ->
            let entity = evt.Subscriber : Entity
            Cascade, synchronize entity world
        ) Nu.Game.Handle.PreUpdateEvent entity (nameof CameraFacet)

    override this.Render (_, _, _) =
        // TODO: add guizmos for display
        ()

/// Gives an entity the base behavior of a gui text control.
type CameraDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets = [
        typeof<CameraFacet>
    ]

/// Augments an entity with rich text.
type CursorFacet () =
    inherit Facet (false, false, false)

    static let register evt world =
        let entity = evt.Subscriber : Entity

        if entity.GetEnabled world then

            SDL.SDL_ShowCursor 0 |> ignore

            Cascade, world

        else
            SDL.SDL_ShowCursor 1 |> ignore

            Cascade, world

    static let unregister evt world =
        let entity = evt.Subscriber : Entity
        SDL.SDL_ShowCursor 1 |> ignore
        Cascade, world

    static let synchronize evt world =
        let entity = evt.Subscriber : Entity

        if entity.GetEnabled world then

            let position = World.getMousePosition2dScreen world

            let world = entity.SetPosition position.V3 world

            Cascade, world

        else

            Cascade, world

    static member Properties =
        [define Entity.InsetOpt None
         // TODO: add default cursor
         define Entity.StaticImage Assets.Default.StaticSprite
         define Entity.Color Color.One
         define Entity.Blend Transparent
         define Entity.Emission Color.Zero
         define Entity.Flip FlipNone
         define Entity.Elevation 100f]

    override this.Register (entity, world) =
        world
        |> World.sense register Nu.Game.Handle.RegisterEvent entity (nameof CameraFacet)
        |> World.sense unregister Nu.Game.Handle.UnregisteringEvent entity (nameof CameraFacet)
        |> World.sense synchronize Nu.Game.Handle.MouseMoveEvent entity (nameof CameraFacet)

    override this.Render (_, entity, world) =
        let mutable transform = entity.GetTransform world
        let staticImage = entity.GetStaticImage world
        let insetOpt = match entity.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
        let clipOpt = ValueNone : Box2 voption
        let color = entity.GetColor world
        let blend = entity.GetBlend world
        let emission = entity.GetEmission world
        let flip = entity.GetFlip world
        // TODO: replace with special operation for cursor
        let perimeter = transform.Perimeter
        transform.Position <- perimeter.BottomRight
        World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)

/// Gives an entity the base behavior of a gui text control.
type CursorDispatcher () =
    inherit Entity2dDispatcher (false, false, false)

    static member Facets = [
        typeof<CursorFacet>
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
                          Justification = entity.GetJustification world
                          CursorOpt = None }}

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

[<AutoOpen>]
module HoverFacetExtensions =
    type Entity with
        member this.GetHover world : bool = this.Get (nameof this.Hover) world
        member this.SetHover (value : bool) world = this.Set (nameof this.Hover) value world
        member this.Hover = lens (nameof this.Hover) this this.GetHover this.SetHover
        member this.GetHoverOffset world = this.Get (nameof this.HoverOffset) world
        member this.SetHoverOffset (value) world = this.Set (nameof this.HoverOffset) value world
        member this.HoverOffset = lens (nameof this.HoverOffset) this this.GetHoverOffset this.SetHoverOffset
        member this.GetHoverImage world : Image AssetTag = this.Get (nameof this.HoverImage) world
        member this.SetHoverImage (value : Image AssetTag) world = this.Set (nameof this.HoverImage) value world
        member this.HoverImage = lens (nameof this.HoverImage) this this.GetHoverImage this.SetHoverImage
        member this.GetHoverSoundOpt world : Sound AssetTag option = this.Get (nameof this.HoverSoundOpt) world
        member this.SetHoverSoundOpt (value : Sound AssetTag option) world = this.Set (nameof this.HoverSoundOpt) value world
        member this.HoverSoundOpt = lens (nameof this.HoverSoundOpt) this this.GetHoverSoundOpt this.SetHoverSoundOpt
        member this.GetHoverSoundVolume world : single = this.Get (nameof this.HoverSoundVolume) world
        member this.SetHoverSoundVolume (value : single) world = this.Set (nameof this.HoverSoundVolume) value world
        member this.HoverSoundVolume = lens (nameof this.HoverSoundVolume) this this.GetHoverSoundVolume this.SetHoverSoundVolume
        member this.HoverEvent = stoa<unit> "Hover/Event"  --> this
        member this.HoveredEvent = stoa<unit> "Hovered/Event"  --> this
        member this.UnhoveredEvent = stoa<unit> "Unhovered/Event" --> this

/// Augments an entity with button behavior.
type HoverFacet () =
    inherit Facet (false, false, false)

    static let handleMouseMove evt world =
        let entity = evt.Subscriber : Entity
        if entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
            let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
            if perimeter.Intersects mousePositionWorld then
                if transform.Enabled then
                    let world =
                        if entity.GetHover world = false then
                            match entity.GetHoverSoundOpt world with
                            | Some clickSound -> World.playSound (entity.GetHoverSoundVolume world) clickSound world
                            | None -> ()

                            let eventTrace = EventTrace.debug "HoverFacet" "handleMouseMove" "" EventTrace.empty
                            World.publishPlus () entity.HoveredEvent eventTrace entity true false world
                        else
                            world
                    let world = entity.SetHover true world
                    let struct (_, _, world) = entity.TrySet (nameof Entity.HoverOffset) (entity.GetHoverOffset world) world
                    let eventTrace = EventTrace.debug "HoverFacet" "handleMouseMove" "" EventTrace.empty
                    let world = World.publishPlus () entity.HoverEvent eventTrace entity true false world
                    (Cascade, world)
                else (Cascade, world)
            else
                let world =
                    if entity.GetHover world = true then
                        let eventTrace = EventTrace.debug "HoverFacet" "handleMouseMove" "" EventTrace.empty
                        World.publishPlus () entity.UnhoveredEvent eventTrace entity true false world
                    else
                        world

                let world = entity.SetHover false world
                (Cascade, world)
        else (Cascade, world)

    static let handleMouseLeftUp evt world =
        let entity = evt.Subscriber : Entity
        let wasDown = entity.GetDown world
        let world = entity.SetDown false world
        let struct (_, _, world) = entity.TrySet (nameof Entity.TextOffset) v2Zero world
        if entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
            let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
            if perimeter.Intersects mousePositionWorld then
                if transform.Enabled && wasDown then
                    let eventTrace = EventTrace.debug "ButtonFacet" "handleMouseLeftUp" "Up" EventTrace.empty
                    let world = World.publishPlus () entity.UpEvent eventTrace entity true false world
                    let eventTrace = EventTrace.debug "ButtonFacet" "handleMouseLeftUp" "Click" EventTrace.empty
                    let world = World.publishPlus () entity.ClickEvent eventTrace entity true false world
                    match entity.GetClickSoundOpt world with
                    | Some clickSound -> World.playSound (entity.GetClickSoundVolume world) clickSound world
                    | None -> ()
                    (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)
        else (Cascade, world)

    static member Properties =
        [define Entity.SliceMargin Constants.Gui.SliceMarginDefault
         define Entity.ColorDisabled Constants.Gui.ColorDisabledDefault
         define Entity.Hover false
         define Entity.HoverOffset v2Zero
         define Entity.HoverImage Assets.Default.ButtonUp
         define Entity.HoverSoundOpt (Some Assets.Default.Sound)
         define Entity.HoverSoundVolume Constants.Audio.SoundVolumeDefault]

    override this.Register (entity, world) =
        let world = World.sense handleMouseMove Nu.Game.Handle.MouseMoveEvent entity (nameof ButtonFacet) world
        world

    override this.Render (_, entity, world) =
        let mutable transform = entity.GetTransform world
        let sliceMargin = entity.GetSliceMargin world
        let spriteImage =
            if entity.GetDown world then
                entity.GetDownImage world
            elif entity.GetHover world then
                entity.GetHoverImage world
            else
                entity.GetUpImage world
        let color = if transform.Enabled then Color.One else entity.GetColorDisabled world
        World.renderGuiSpriteSliced transform.Absolute transform.Perimeter sliceMargin spriteImage transform.Offset transform.Elevation color world

    override this.GetAttributesInferred (entity, world) =
        match Metadata.tryGetTextureSizeF (entity.GetUpImage world) with
        | ValueSome size -> AttributesInferred.important size.V3 v3Zero
        | ValueNone -> AttributesInferred.important Constants.Engine.EntityGuiSizeDefault v3Zero

/// Gives an entity the base behavior of a gui button.
type ButtonExDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<TextFacet>
         typeof<ButtonFacet>
         typeof<HoverFacet>]


[<RequireQualifiedAccess>]
module ContentEx =

    let camera entityName initializers = Content.entity<CameraDispatcher> entityName initializers

    let cursor entityName initializers = Content.entity<CursorDispatcher> entityName initializers

    let glyph entityName initializers = Content.entity<GlyphDispatcher> entityName initializers

    let buttonEx entityName initializers = Content.entity<ButtonExDispatcher> entityName initializers