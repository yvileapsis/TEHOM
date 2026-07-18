// Nu Slug Showcase.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace SlugShowcase
open System
open System.Numerics
open Prime
open Nu

/// A button that uses Slug outlines for its label instead of TextDispatcher.
type SlugButtonDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<ButtonFacet>
         typeof<SlugTextFacet>]

[<AutoOpen>]
module SlugShowcaseExtensions =
    type Game with
        member this.GetSlugDemoScene world : SlugDemoScene = this.Get (nameof Game.SlugDemoScene) world
        member this.SetSlugDemoScene (value : SlugDemoScene) world = this.Set (nameof Game.SlugDemoScene) value world
        member this.SlugDemoScene = lens (nameof Game.SlugDemoScene) this this.GetSlugDemoScene this.SetSlugDemoScene

[<RequireQualifiedAccess>]
module SlugShowcaseView =

    let private pageSize = 5

    let private doSlugButton name args world =
        let init updateResult (entity : Entity) world =
            World.monitor (fun _ world -> updateResult tautology world; Cascade) entity.ClickEvent entity world
        World.doEntityPlus<SlugButtonDispatcher, _> false init name args world

    let private button name label position size fontSize active world =
        let args =
            [Entity.Position .= position
             Entity.Size .= size
             Entity.Elevation .= 50.0f
             Entity.SlugFont .= SlugDemo.font
             Entity.FontSizing .= Some fontSize
             Entity.Text .= label
             Entity.TextColor @= if active then SlugDemo.cyan else SlugDemo.muted
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)]
        doSlugButton name args world

    let private drawNavigation scene (setScene : SlugDemoScene -> unit) world =
        let sceneIndex = SlugDemoScene.index scene
        let page = sceneIndex / pageSize
        let pageCount = (SlugDemoScene.all.Length + pageSize - 1) / pageSize
        let pageStart = page * pageSize
        let previousPage = (page + pageCount - 1) % pageCount
        let nextPage = (page + 1) % pageCount
        if button "NavPreviousPage" "<" (v3 -302.0f 164.0f 0.0f) (v3 28.0f 22.0f 0.0f) 11.0f false world then
            setScene SlugDemoScene.all[previousPage * pageSize]
        for slot in 0 .. pageSize - 1 do
            let index = pageStart + slot
            if index < SlugDemoScene.all.Length then
                let destination = SlugDemoScene.all[index]
                let x = -240.0f + single slot * 120.0f
                if button ("NavScene" + string slot) (SlugDemoScene.shortLabel destination) (v3 x 164.0f 0.0f) (v3 110.0f 22.0f 0.0f) 7.5f (destination = scene) world then
                    setScene destination
        if button "NavNextPage" ">" (v3 302.0f 164.0f 0.0f) (v3 28.0f 22.0f 0.0f) 11.0f false world then
            setScene SlugDemoScene.all[nextPage * pageSize]
        let hint = sprintf "LEFT / RIGHT demos   |   page %d / %d   |   buttons select" (page + 1) pageCount
        SlugDemo.slugLeft "NavigationHint" SlugDemo.font hint (v3 -300.0f 144.0f 0.0f) (v3 600.0f 16.0f 0.0f) 7.5f SlugDemo.dim 50.0f TextDirectionLeftToRight None world

    let private drawScene scene world =
        match scene with
        | Emoji -> SlugDemoEmoji.draw world
        | PbrIbl -> SlugDemoPbrIbl.draw world
        | AnimatedGlyphs -> SlugDemoAnimatedGlyphs.draw world
        | LayerEffects -> SlugDemoLayerEffects.draw world
        | Morphing -> SlugDemoMorphing.draw world
        | Projection2d -> SlugDemoProjection2d.draw world
        | Projection3d -> SlugDemoProjection3d.draw world
        | Gradients -> SlugDemoGradients.draw world
        | Masking -> SlugDemoMasking.draw world
        | Hud -> SlugDemoHud.draw world
        | AnimatedHud -> SlugDemoAnimatedHud.draw world
        | ShapesCompositeShapes -> SlugDemoShapesCompositeShapes.draw world
        | MixedScenes -> SlugDemoMixedScenes.draw world
        | AnimatedScenes -> SlugDemoAnimatedScenes.draw world
        | Objects3d -> SlugDemoObjects3d.draw world
        | Svg -> SlugDemoSvg.draw world
        | SlugDemoScene.Text -> SlugDemoText.draw world
        | MixedText -> SlugDemoMixedText.draw world
        | TextEffects -> SlugDemoTextEffects.draw world
        | TextAlongPath -> SlugDemoTextAlongPath.draw world
        | ComputeShaders -> SlugDemoComputeShaders.draw world

    let declare (game : Game) (world : World) =
        let mutable scene = game.GetSlugDemoScene world
        if World.isKeyboardKeyPressed KeyboardKey.Left world then scene <- SlugDemoScene.previous scene
        elif World.isKeyboardKeyPressed KeyboardKey.Right world then scene <- SlugDemoScene.next scene
        let setScene destination = scene <- destination
        World.beginScreen "Showcase" true Vanilla [] world |> ignore
        World.beginGroup "Gallery" [] world
        SlugDemo.backgroundEntity world
        drawNavigation scene setScene world
        drawScene scene world
        World.endGroup world
        World.endScreen world
        game.SetSlugDemoScene scene world

/// The top-level ImSim dispatcher for the showcase.
type SlugShowcaseDispatcher () =
    inherit GameDispatcherImSim ()

    static member Properties =
        let initialScene =
            Environment.GetEnvironmentVariable "SLUG_SHOWCASE_SCENE"
            |> SlugDemoScene.tryFind
            |> Option.defaultValue Emoji
        [define Game.SlugDemoScene initialScene]

    override this.Process (game, world) =
        SlugShowcaseView.declare game world
