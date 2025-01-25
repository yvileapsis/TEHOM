// Nu Game Engine Extensions

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Numerics
open System.Threading
open SDL2
open ImGuiNET
open Prime

/// Augments an entity with a camera facet, synchronizing world's 3d eye position and rotation with that of the entity.
type CameraFacet () =
    inherit Facet (false, false, false)

    static let postUpdate evt (world : World) =
        let entity = evt.Subscriber : Entity
        if world.Advancing && entity.GetEnabled world then

            let position = entity.GetPosition world
            let rotation = entity.GetRotation world

            let world = World.setEye3dCenter position world
            let world = World.setEye3dRotation rotation world

            Cascade, world
        else
            Cascade, world

    override this.Register (entity, world) =
        let world = World.sense postUpdate Nu.Game.Handle.PostUpdateEvent entity (nameof CameraFacet) world
        world

    override this.Render (_, _, _) =
        // TODO: add guizmos for display
        ()

    static member Properties =
        [define Entity.AlwaysUpdate true]

/// Augments an entity with a wrapper for SDL relative mouse mode, convenient for FPS games.
type MouseRelativeModeFacet () =
    inherit Facet (false, false, false)

    static let synchronize evt (world : World) =
        let entity = evt.Subscriber : Entity

        if world.Advancing && entity.GetEnabled world then

            SDL.SDL_WarpMouseInWindow (IntPtr.Zero, 960, 540) |> ignore

            Cascade, world
        else
            Cascade, world

    override this.Register (entity, world) =
        world
        |> World.sense synchronize Nu.Game.Handle.MouseMoveEvent entity (nameof MouseRelativeModeFacet)

    override this.Update (entity, world) =
        if world.Advancing && entity.GetEnabled world then
            SDL.SDL_SetRelativeMouseMode (SDL.SDL_bool.SDL_TRUE) |> ignore
        else
            SDL.SDL_SetRelativeMouseMode (SDL.SDL_bool.SDL_FALSE) |> ignore
        world

    static member Properties =
        [define Entity.AlwaysUpdate true]

/// Augments an entity with a custom cursor.
type CursorFacet () =
    inherit Facet (false, false, false)

    static let mouseMove evt (world : World) =
        let entity = evt.Subscriber : Entity

        if world.Advancing && entity.GetEnabled world then

            let position = World.getMousePosition2dScreen world

            let world = entity.SetPosition position.V3 world

            Cascade, world
        else
            Cascade, world

    static member Properties =
        [define Entity.AlwaysUpdate true
         define Entity.InsetOpt None
         // TODO: add default cursor
         define Entity.StaticImage Assets.Default.StaticSprite
         define Entity.Color Color.One
         define Entity.Blend Transparent
         define Entity.Emission Color.Zero
         define Entity.Flip FlipNone
         define Entity.Elevation 100f]

    override this.Register (entity, world) =
        world
        |> World.sense mouseMove Nu.Game.Handle.MouseMoveEvent entity (nameof CursorFacet)

    override this.Update (entity, world) =

        if world.Advancing && entity.GetEnabled world then
            SDL.SDL_ShowCursor 0 |> ignore
        else
            SDL.SDL_ShowCursor 1 |> ignore

        world

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

/// Augments an entity with a hoverable button behavior.
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

/// Augments an entity with a static billboard.
type Text3dFacet () =
    inherit Facet (false, false, false)

    static member Properties = [
        define Entity.Text ""
        define Entity.Font Assets.Default.Font
        define Entity.FontSizing (Some 40)
        define Entity.FontStyling Set.empty
        define Entity.Justification (Justified (JustifyCenter, JustifyMiddle))
        define Entity.TextMargin v2Zero
        define Entity.TextColor Color.White
        define Entity.TextColorDisabled Constants.Gui.ColorDisabledDefault
        define Entity.InsetOpt None
        define Entity.MaterialProperties MaterialProperties.empty
        define Entity.Material Material.empty
        define Entity.RenderStyle Deferred
        define Entity.ShadowOffset Constants.Engine.BillboardShadowOffsetDefault
    ]

    override this.Render (renderPass, entity, world) =
        let mutable transform = entity.GetTransform world
        let castShadow = transform.CastShadow
        if not renderPass.IsShadowPass || castShadow then
            let affineMatrix = transform.AffineMatrix
            let presence = transform.Presence
            let insetOpt = entity.GetInsetOpt world
            let properties = entity.GetMaterialProperties world
            let material = entity.GetMaterial world
            let shadowOffset = entity.GetShadowOffset world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)

            let margin = (entity.GetTextMargin world).V3
            let color = if transform.Enabled then entity.GetTextColor world else entity.GetTextColorDisabled world
            let font = entity.GetFont world
            let fontSizing = entity.GetFontSizing world
            let fontStyling = entity.GetFontStyling world
            let text = entity.GetText world

            let typeset = {
                Text = text
                Font = font
                FontSizing = fontSizing
                FontStyling = fontStyling
                Color = color
            }

            World.enqueueRenderMessage3d
                (RenderText3d
                    { CastShadow = castShadow
                      Presence = presence
                      ModelMatrix = affineMatrix
                      InsetOpt = insetOpt
                      MaterialProperties = properties
                      Material = material
                      ShadowOffset = shadowOffset
                      RenderType = renderType
                      RenderPass = renderPass
                      Typeset = typeset
                   })
                world

///
type GlyphMatrixFacet () =
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

module NuMark =

    #r "nuget: FParsec"
    open FParsec

    type Justification =
        | Left
        | Center
        | Right
        | Full
        | Unjustified

    type Style =
        | Bold
        | Italics
        | Strikethrough
        | Underlined
        | Subscript
        | Superscript

    type Block = {
        Text: string
        Size: string
        Color: string
        Style: Set<Style>
    }
    with
        static member empty = {
            Text = ""
            Size = ""
            Color = ""
            Style = Set.empty
        }

    type Line = Block list

    type UserState = {
        Stack: string list
    }
    with
       static member Default = {
           Stack = List.empty
       }


    let parseLine till : Parser<Line, UserState> =

        let specialChars = ['~'; '^'; '*'; '_'; '{'; '}'; '\\'; '\n'; '\r']

        let many1CharsTillOptional chars till =
            attempt (many1CharsTill chars till)
            <|> many1Chars chars

        let text =
            let escaped = choice [
                pstring @"\n" >>% ' '
                pstring @"\r" >>% ' '
                pstring @"\*" >>% '*'
                pstring @"\_" >>% '_'
                pstring @"\~" >>% '~'
                pstring @"\\" >>% '\\'
            ]

            choice [
                many1CharsTillOptional escaped till <?> "escaped"
                many1CharsTillOptional (noneOf specialChars) till <?> "text"
                many1CharsTillOptional (newline >>% ' ') till <?> "singular newline"
                many1CharsTillOptional (anyOf specialChars) till <?> "special symbol"
            ]
            |>> fun x -> [{ Block.empty with Text = x }]

        let styles, stylesRef = createParserForwardedToRef()

        let block =
            choice [
                attempt styles
                text
            ]

        let insides till =
            many1Till block till
            |>> List.concat

        let style styleType symbol =
            pstring symbol
            >>? insides (followedByString symbol)
            |>> List.map (fun x -> { x with Style = Set.add styleType x.Style })
            .>>? pstring symbol
            <?> $"style {symbol}"

        stylesRef.Value <- choice [
            style Bold "**"
            style Italics "*"
            style Strikethrough "~~"
            style Superscript "~"
            style Underlined "__"
            style Subscript "_"
        ]

        insides till
        |>> List.fold (fun (x : Line) y ->
            match x with
            | [] -> [y]
            | head::tail ->
                if head.Style = y.Style then
                    { head with Text = head.Text + y.Text }::tail
                else
                    y::x
        ) List.empty
        |>> List.rev

    type Node =
        | Paragraph of Justification * Line
        | Header of int * Node
        | Quote of int * Node
        | Indent of int * Node
        | List of int * Node
        | Code
        | Codeblock
        | Table
        | Image
        | HorizontalLine

    type Text = Node list

    let parseText : Parser<Text, _> =
        let node, nodeRef = createParserForwardedToRef()

        let line =
            let endings = choice [
                followedBy (pstring " ||" .>>? (newline .>> newline))
                followedBy (pstring " ||" .>>? eof)
                followedBy (pstring "||" .>>? (newline .>> newline))
                followedBy (pstring "||" .>>? eof)
                followedBy (newline >>. newline)
                followedBy eof
            ]

            let line = parseLine endings //(followedByString " ||" <|> followedByString "||" <|> followedByNewline <|> (followedBy eof))

            let full =
                pstring "||"
                >>? optional (pstring " ")
                >>? line
                .>>? optional (pstring " ")
                .>>? pstring "||"
                |>> fun x -> Center, x

            let left =
                pstring "||"
                >>? optional (pstring " ")
                >>? line
                |>> fun x -> Left, x

            let right =
                line
                .>>? optional (pstring " ")
                .>>? pstring "||"
                |>> fun x -> Right, x

            let center =
                line
                |>> fun x -> Unjustified, x

            optional spaces
            >>. choice [
                full
                left
                right
                center
            ]
            |>> Paragraph

        nodeRef.Value <- choice [
            line
        ]

        sepEndBy1 (node) (newline)

    let parseNuMark string =
        match runParserOnString parseText UserState.Default "" string with
        | Success (x, _, _) -> Result.Ok x
        | Failure (x, y, _) -> Result.Error (x, y)

open NuMark

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
module VoxelFacetExtensions =
    type Entity with

        member this.GetVoxelMaterialProperties world : TerrainMaterialProperties = this.Get (nameof this.VoxelMaterialProperties) world
        member this.SetVoxelMaterialProperties (value : TerrainMaterialProperties) world = this.Set (nameof this.VoxelMaterialProperties) value world
        member this.VoxelMaterialProperties = lens (nameof this.VoxelMaterialProperties) this this.GetVoxelMaterialProperties this.SetVoxelMaterialProperties
        member this.GetVoxelMaterial world : TerrainMaterial = this.Get (nameof this.VoxelMaterial) world
        member this.SetVoxelMaterial (value : TerrainMaterial) world = this.Set (nameof this.VoxelMaterial) value world
        member this.VoxelMaterial = lens (nameof this.VoxelMaterial) this this.GetVoxelMaterial this.SetVoxelMaterial
        member this.GetVoxelChunk world : VoxelChunk = this.Get (nameof this.VoxelChunk) world
        member this.SetVoxelChunk (value : VoxelChunk) world = this.Set (nameof this.VoxelChunk) value world
        member this.VoxelChunk = lens (nameof this.VoxelChunk) this this.GetVoxelChunk this.SetVoxelChunk

/// Augments an entity with voxel model and physics.
type VoxelFacet () =
    inherit Facet (true, false, false)

    static member Properties =
        [define Entity.Size (v3 16.0f 16.0f 16.0f)
         define Entity.Presence Omnipresent
         define Entity.Static true
         define Entity.AlwaysRender true
         define Entity.BodyEnabled true
         define Entity.Friction 0.5f
         define Entity.Restitution 0.0f
         define Entity.CollisionCategories "1"
         define Entity.CollisionMask Constants.Physics.CollisionWildcard
         define Entity.InsetOpt None
         define Entity.TerrainMaterialProperties TerrainMaterialProperties.defaultProperties
         define Entity.TerrainMaterial
            (BlendMaterial
                { TerrainLayers =
                    [|{ AlbedoImage = Assets.Default.TerrainLayer0Albedo
                        RoughnessImage = Assets.Default.TerrainLayer0Roughness
                        AmbientOcclusionImage = Assets.Default.TerrainLayer0AmbientOcclusion
                        NormalImage = Assets.Default.TerrainLayer0Normal
                        HeightImage = Assets.Default.TerrainLayer0Height }
                      { AlbedoImage = Assets.Default.TerrainLayer1Albedo
                        RoughnessImage = Assets.Default.TerrainLayer1Roughness
                        AmbientOcclusionImage = Assets.Default.TerrainLayer1AmbientOcclusion
                        NormalImage = Assets.Default.TerrainLayer1Normal
                        HeightImage = Assets.Default.TerrainLayer1Height }|]
                  BlendMap =
                      RedsMap
                        [|Assets.Default.TerrainLayer0Blend
                          Assets.Default.TerrainLayer1Blend|]})
         define Entity.TintImageOpt None
         define Entity.NormalImageOpt None
         // TODO: implement
         define Entity.VoxelChunk (SlicesVoxel Assets.Default.Black)
         define Entity.Segments v2iOne
         define Entity.Observable false
         nonPersistent Entity.AwakeTimeStamp 0L
         computed Entity.Awake (fun (entity : Entity) world -> entity.GetAwakeTimeStamp world = world.UpdateTime) None
         computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None]

    override this.Register (entity, world) =
//        let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity (nameof VoxelFacet) world
//        let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity (nameof VoxelFacet) world
//        let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Friction)) entity (nameof VoxelFacet) world
//        let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Restitution)) entity (nameof VoxelFacet) world
//        let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity (nameof VoxelFacet) world
//        let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionMask)) entity (nameof VoxelFacet) world
//        let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.HeightMap)) entity (nameof VoxelFacet) world
        let world = entity.SetAwakeTimeStamp world.UpdateTime world
        world

    (*
    override this.RegisterPhysics (entity, world) =
        match entity.TryGetTerrainResolution world with
        | Some resolution ->
            let mutable transform = entity.GetTransform world
            let terrainShape =
                { Resolution = resolution
                  Bounds = transform.Bounds3d
                  HeightMap = entity.GetHeightMap world
                  TransformOpt = None
                  PropertiesOpt = None }
            let bodyProperties =
                { Center = if entity.GetIs2d world then transform.PerimeterCenter else transform.Position
                  Rotation = transform.Rotation
                  Scale = transform.Scale
                  BodyShape = TerrainShape terrainShape
                  BodyType = Static
                  SleepingAllowed = true
                  Enabled = entity.GetBodyEnabled world
                  Friction = entity.GetFriction world
                  Restitution = entity.GetRestitution world
                  LinearVelocity = v3Zero
                  LinearDamping = 0.0f
                  AngularVelocity = v3Zero
                  AngularDamping = 0.0f
                  AngularFactor = v3Zero
                  Substance = Mass 0.0f
                  GravityOverride = None
                  CharacterProperties = CharacterProperties.defaultProperties
                  CollisionDetection = Discontinuous
                  CollisionCategories = Physics.categorizeCollisionMask (entity.GetCollisionCategories world)
                  CollisionMask = Physics.categorizeCollisionMask (entity.GetCollisionMask world)
                  Sensor = false
                  Observable = entity.GetObservable world
                  Awake = entity.GetAwake world
                  BodyIndex = (entity.GetBodyId world).BodyIndex }
            World.createBody false (entity.GetBodyId world) bodyProperties world
        | None -> world

    override this.UnregisterPhysics (entity, world) =
        World.destroyBody false (entity.GetBodyId world) world
        *)

    override this.Render (renderPass, entity, world) =

        // compute intersection function based on render pass
        let intersects =
            let interiorOpt = ValueSome (World.getGameEye3dFrustumInterior Game world)
            let exterior = World.getGameEye3dFrustumExterior Game world
            let imposter = World.getGameEye3dFrustumImposter Game world
            let lightBoxOpt = ValueSome (World.getLight3dBox world)
            fun probe light presence bounds ->
                match renderPass with
                | NormalPass -> Presence.intersects3d interiorOpt exterior imposter lightBoxOpt probe light presence bounds
                | LightMapPass (_, lightMapBounds) -> not probe && not light && lightMapBounds.Intersects bounds
                | ShadowPass (_, _, _, frustum) -> not probe && not light && frustum.Intersects bounds
                | ReflectionPass (_, _) -> false

        // render unculled surfaces
        let bounds = entity.GetBounds world
        if intersects false false Exterior bounds then

            let mutable transform = entity.GetTransform world
            let terrainDescriptor =
                { Bounds = transform.Bounds3d
                  InsetOpt = entity.GetInsetOpt world
                  MaterialProperties = entity.GetTerrainMaterialProperties world
                  Material = entity.GetTerrainMaterial world
                  TintImageOpt = entity.GetTintImageOpt world
                  NormalImageOpt = entity.GetNormalImageOpt world
                  VoxelChunk = entity.GetVoxelChunk world
                  Segments = entity.GetSegments world
                  Voxel = true }
            World.enqueueRenderMessage3d
                (RenderVoxel
                    { Visible = transform.Visible
                      VoxelDescriptor = terrainDescriptor
                      RenderPass = renderPass })
                world

/// Gives an entity the base behavior of a camera, synchronizing world's 3d eye position and rotation with itself.
type CameraDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets = [
        typeof<CameraFacet>
    ]

/// Gives an entity the base behavior of a first-person camera.
type CameraFirstPersonDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets = [
        typeof<CameraFacet>
        typeof<MouseRelativeModeFacet>
    ]

/// Gives an entity the base behavior of a cursor.
type CursorDispatcher () =
    inherit Entity2dDispatcher (false, false, false)

    static member Facets = [
        typeof<CursorFacet>
    ]

/// Gives an entity the base behavior of a gui text control.
type RichTextDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<BackdroppableFacet>
         typeof<RichTextFacet>]

    static member Properties =
        [define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))]

/// Gives an entity the base behavior of a gui button.
type ButtonExDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<TextFacet>
         typeof<ButtonFacet>
         typeof<HoverFacet>]

/// Gives an entity the base behavior of a static billboard.
type Text3dDispatcher () =
    inherit VuiDispatcher ()

    static member Facets =
        [typeof<Text3dFacet>]

/// Gives an entity the base behavior of a gui text control.
type GlyphMatrixDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<BackdroppableFacet>
         typeof<GlyphMatrixFacet>]

    static member Properties =
        [define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))]

/// Gives an entity the base behavior of a voxel model.
type VoxelDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static member Facets =
        [typeof<VoxelFacet>]

[<AutoOpen>]
module CustomFilter3dConfigDispatcherExtensions =
    type Entity with
        member this.GetCustomFilter3dConfig world : CustomFilter3dConfig = this.Get (nameof this.CustomFilter3dConfig) world
        member this.SetCustomFilter3dConfig (value : CustomFilter3dConfig) world = this.Set (nameof this.CustomFilter3dConfig) value world
        member this.CustomFilter3dConfig = lens (nameof this.CustomFilter3dConfig) this this.GetCustomFilter3dConfig this.SetCustomFilter3dConfig

type CustomFilter3dConfigDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Properties =
        [define Entity.CustomFilter3dConfig CustomFilter3dConfig.defaultConfig
         define Entity.Presence Omnipresent
         define Entity.AlwaysUpdate true]

    override this.Update (entity, world) =
        let config = entity.GetCustomFilter3dConfig world
        World.enqueueRenderMessage3d (ConfigureCustomFilter3d config) world
        world

[<RequireQualifiedAccess>]
module ContentEx =
    let camera entityName initializers = Content.entity<CameraDispatcher> entityName initializers
    let cameraFirstPerson entityName initializers = Content.entity<CameraFirstPersonDispatcher> entityName initializers
    let cursor entityName initializers = Content.entity<CursorDispatcher> entityName initializers
    let richText entityName initializers = Content.entity<RichTextDispatcher> entityName initializers
    let buttonEx entityName initializers = Content.entity<ButtonExDispatcher> entityName initializers
    let text3d entityName definitions = Content.entity<Text3dDispatcher> entityName definitions
    let glyph entityName initializers = Content.entity<GlyphMatrixDispatcher> entityName initializers
    let voxel entityName definitions = Content.entity<VoxelDispatcher> entityName definitions
    let customFilter3dConfig entityName definitions = Content.entity<CustomFilter3dConfigDispatcher> entityName definitions