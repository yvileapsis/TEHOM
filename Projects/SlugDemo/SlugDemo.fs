// Nu SlugDemo.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace SlugDemo
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
module SlugDemoExtensions =
    type Game with
        member this.GetGameState world : SlugDemoScene = this.Get (nameof Game.GameState) world
        member this.SetGameState (value : SlugDemoScene) world = this.Set (nameof Game.GameState) value world
        member this.GameState = lens (nameof Game.GameState) this this.GetGameState this.SetGameState

    type Screen with
        member this.GetSlugDemoScene world : SlugDemoScene = this.Get (nameof Screen.SlugDemoScene) world
        member this.SetSlugDemoScene (value : SlugDemoScene) world = this.Set (nameof Screen.SlugDemoScene) value world
        member this.SlugDemoScene = lens (nameof Screen.SlugDemoScene) this this.GetSlugDemoScene this.SetSlugDemoScene

[<RequireQualifiedAccess>]
module SlugDemoSceneView =

    let private doSlugButton name args world =
        let init updateResult (entity : Entity) world =
            World.monitor (fun _ world -> updateResult tautology world; Cascade) entity.ClickEvent entity world
        World.doEntityPlus<SlugButtonDispatcher, _> false init name args world

    let private arrowButton name label position world =
        doSlugButton
            name
            [Entity.Position .= position
             Entity.Size .= v3 28.0f 22.0f 0.0f
             Entity.Elevation .= 50.0f
             Entity.SlugFont .= SlugDemo.font
             Entity.FontSizing .= Some 11.0f
             Entity.Text .= label
             Entity.TextColor .= SlugDemo.white
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)]
            world
        |> ignore

    let drawNavigation scene world =
        arrowButton (Simulants.previousButton scene).Name "<" (v3 -302.0f 164.0f 0.0f) world
        SlugDemo.slug
            (Simulants.currentDemo scene).Name
            SlugDemo.font
            (SlugDemoScene.label scene)
            (v3 0.0f 164.0f 0.0f)
            (v3 520.0f 22.0f 0.0f)
            11.0f
            SlugDemo.white
            50.0f
            world
        arrowButton (Simulants.nextButton scene).Name ">" (v3 302.0f 164.0f 0.0f) world

    let drawScene scene world =
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

type SlugDemoSceneDispatcher () =
    inherit ScreenDispatcherImSim ()

    static member Properties =
        [define Screen.SlugDemoScene Emoji]

    override this.Process (_, screen, world) =
        if screen.GetSelected world then
            let scene = screen.GetSlugDemoScene world
            World.beginGroup (Simulants.sceneGroup scene).Name [] world
            SlugDemo.backgroundEntity world
            SlugDemoSceneView.drawNavigation scene world
            SlugDemoSceneView.drawScene scene world
            World.endGroup world

type SlugDemoDispatcher () =
    inherit GameDispatcherImSim ()

    static member Properties =
        let initialScene =
            Environment.GetEnvironmentVariable "SLUG_DEMO_SCENE"
            |> SlugDemoScene.tryFind
            |> Option.defaultValue Emoji
        [define Game.GameState initialScene]

    override this.Process (game, world) =
        let mutable scene = game.GetGameState world
        let previousClicked =
            World.doSubscriptionAny
                "PreviousScene"
                (Simulants.previousButton scene).ClickEvent
                world
        let nextClicked =
            World.doSubscriptionAny
                "NextScene"
                (Simulants.nextButton scene).ClickEvent
                world
        if World.isKeyboardKeyPressed KeyboardKey.Left world || previousClicked then
            scene <- SlugDemoScene.previous scene
        elif World.isKeyboardKeyPressed KeyboardKey.Right world || nextClicked then
            scene <- SlugDemoScene.next scene
        for declaredScene in SlugDemoScene.all do
            let screen = Simulants.screen declaredScene
            World.doScreen<SlugDemoSceneDispatcher>
                screen.Name
                (scene = declaredScene)
                Vanilla
                [Screen.SlugDemoScene .= declaredScene]
                world
            |> ignore
        game.SetGameState scene world
