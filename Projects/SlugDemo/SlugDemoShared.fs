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

/// Every demo currently catalogued by the osgSlug / SlugHorn project, plus the Masking demo shown on its gallery page.
type SlugDemoScene =
    | Emoji
    | PbrIbl
    | AnimatedGlyphs
    | LayerEffects
    | Morphing
    | Projection2d
    | Projection3d
    | Gradients
    | Masking
    | Hud
    | AnimatedHud
    | ShapesCompositeShapes
    | MixedScenes
    | AnimatedScenes
    | Objects3d
    | Svg
    | Text
    | MixedText
    | TextEffects
    | TextAlongPath
    | ComputeShaders

[<RequireQualifiedAccess>]
module SlugDemoScene =

    let all =
        [| Emoji; PbrIbl; AnimatedGlyphs; LayerEffects; Morphing
           Projection2d; Projection3d; Gradients; Masking; Hud
           AnimatedHud; ShapesCompositeShapes; MixedScenes; AnimatedScenes; Objects3d
           Svg; Text; MixedText; TextEffects; TextAlongPath; ComputeShaders |]

    let name scene =
        match scene with
        | Emoji -> "Emoji"
        | PbrIbl -> "PbrIbl"
        | AnimatedGlyphs -> "AnimatedGlyphs"
        | LayerEffects -> "LayerEffects"
        | Morphing -> "Morphing"
        | Projection2d -> "Projection2d"
        | Projection3d -> "Projection3d"
        | Gradients -> "Gradients"
        | Masking -> "Masking"
        | Hud -> "Hud"
        | AnimatedHud -> "AnimatedHud"
        | ShapesCompositeShapes -> "ShapesCompositeShapes"
        | MixedScenes -> "MixedScenes"
        | AnimatedScenes -> "AnimatedScenes"
        | Objects3d -> "Objects3d"
        | Svg -> "Svg"
        | Text -> "Text"
        | MixedText -> "MixedText"
        | TextEffects -> "TextEffects"
        | TextAlongPath -> "TextAlongPath"
        | ComputeShaders -> "ComputeShaders"

    let label scene =
        match scene with
        | Emoji -> "Emoji"
        | PbrIbl -> "PBR / IBL"
        | AnimatedGlyphs -> "Animated Glyphs"
        | LayerEffects -> "Layer Effects"
        | Morphing -> "Morphing"
        | Projection2d -> "2D Projection"
        | Projection3d -> "3D Projection"
        | Gradients -> "Gradients"
        | Masking -> "Masking"
        | Hud -> "HUD"
        | AnimatedHud -> "Animated HUD"
        | ShapesCompositeShapes -> "Shapes / CompositeShapes"
        | MixedScenes -> "Mixed Scenes"
        | AnimatedScenes -> "Animated Scenes"
        | Objects3d -> "3D Objects"
        | Svg -> "SVG"
        | Text -> "Text"
        | MixedText -> "Mixed Text"
        | TextEffects -> "Text Effects"
        | TextAlongPath -> "Text Along Path"
        | ComputeShaders -> "Compute Shaders"


    let index scene = Array.findIndex ((=) scene) all

    let previous scene = all[(index scene + all.Length - 1) % all.Length]

    let next scene = all[(index scene + 1) % all.Length]

    let tryFind value =
        let normalize (text : string) =
            if isNull text then String.Empty
            else text.Replace(" ", String.Empty).Replace("/", String.Empty).Replace("-", String.Empty)
        let normalized = normalize value
        all
        |> Array.tryFind (fun scene ->
            String.Equals (normalize (name scene), normalized, StringComparison.OrdinalIgnoreCase) ||
            String.Equals (normalize (label scene), normalized, StringComparison.OrdinalIgnoreCase))

[<RequireQualifiedAccess>]
module SlugDemo =

    let white = Color (238uy, 242uy, 255uy, 255uy)
    let muted = Color (148uy, 162uy, 190uy, 255uy)
    let dim = Color (98uy, 111uy, 138uy, 255uy)
    let cyan = Color (88uy, 224uy, 255uy, 255uy)
    let magenta = Color (255uy, 112uy, 214uy, 255uy)
    let amber = Color (255uy, 205uy, 106uy, 255uy)
    let green = Color (126uy, 238uy, 177uy, 255uy)
    let coral = Color (255uy, 126uy, 132uy, 255uy)
    let blue = Color (112uy, 166uy, 255uy, 255uy)
    let background = Color (8uy, 12uy, 22uy, 255uy)
    let panelColor = Color (16uy, 25uy, 43uy, 255uy)
    let panelColorLight = Color (27uy, 42uy, 67uy, 255uy)

    let font = Assets.Default.FontSlug
    let fontFilePath = IO.Path.Combine (AppContext.BaseDirectory, "Assets", "Default", "Font.ttf")
    let fontArabic = asset<SlugFont> Assets.Default.PackageName "FontSlugArabic"
    let fontHebrew = asset<SlugFont> Assets.Default.PackageName "FontSlugHebrew"
    let fontDevanagari = asset<SlugFont> Assets.Default.PackageName "FontSlugDevanagari"

    let clockSeconds (world : World) =
        single (world.DateTime.TimeOfDay.TotalSeconds % 10000.0)

    let backgroundEntity world =
        World.doStaticSprite "Background"
            [Entity.Position .= v3 0.0f 0.0f 0.0f
             Entity.Size .= v3 640.0f 360.0f 0.0f
             Entity.Elevation .= -100.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= background] world

    let panel name position size color elevation world =
        World.doStaticSprite name
            [Entity.Position .= position
             Entity.Size .= size
             Entity.Elevation .= elevation
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= color] world

    let slug name slugFont text position size fontSize color elevation world =
        World.doSlugText name
            [Entity.Position .= position
             Entity.Size .= size
             Entity.Elevation .= elevation
             Entity.SlugFont .= slugFont
             Entity.Text .= text
             Entity.FontSizing .= Some fontSize
             Entity.TextColor .= color
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)] world

    let slugLeft name slugFont text (position : Vector3) (size : Vector3) fontSize color elevation direction languageOpt world =
        World.doSlugText name
            [Entity.Position .= position + v3 (size.X * 0.5f) 0.0f 0.0f
             Entity.Size .= size
             Entity.Elevation .= elevation
             Entity.SlugFont .= slugFont
             Entity.Text .= text
             Entity.FontSizing .= Some fontSize
             Entity.TextColor .= color
             Entity.TextDirection .= direction
             Entity.LanguageOpt .= languageOpt
             Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)] world

    let slugDynamic name slugFont text position size fontSize color elevation rotation scale world =
        World.doSlugText name
            [Entity.Position .= position
             Entity.Size .= size
             Entity.Elevation .= elevation
             Entity.SlugFont .= slugFont
             Entity.Text .= text
             Entity.FontSizing .= Some fontSize
             Entity.TextColor .= color
             Entity.Rotation @= rotation
             Entity.Scale @= scale
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)] world

    let mtsdf name text position size fontSize color elevation rotation scale world =
        World.doMsdfText name
            [Entity.Position .= position
             Entity.Size .= size
             Entity.Elevation .= elevation
             Entity.Text .= text
             Entity.FontSizing .= Some fontSize
             Entity.TextColor .= color
             Entity.Rotation @= rotation
             Entity.Scale @= scale
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)] world

