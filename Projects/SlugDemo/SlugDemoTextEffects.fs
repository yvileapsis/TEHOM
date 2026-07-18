namespace SlugDemo
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoTextEffects =

    let private panelColor = Color (9uy, 8uy, 40uy, 255uy)
    let private warm = Color (244uy, 151uy, 24uy, 255uy)
    let private ink = Color (5uy, 4uy, 12uy, 255uy)
    let private paper = Color (250uy, 249uy, 246uy, 255uy)
    let private pigment = Color (63uy, 55uy, 157uy, 255uy)
    let private stroke = Color (242uy, 91uy, 42uy, 255uy)
    let private stoneImage = asset<Image> Assets.Default.PackageName "CobblestoneFloor"


    let private makeEffectComposite fileName text fillSource effectId color =
        let composite = SlugDemoTextSupport.makeTextComposite fileName text
        for layerIndex in 0 .. composite.LayerCount - 1 do
            let state = composite.Layers.Item layerIndex
            composite.SetLayerState
                (layerIndex,
                 { state with
                     Color = color
                     FillSource = fillSource
                     EffectId = effectId
                     EffectParameters =
                         if effectId = 10
                         then v4 0.0f 0.0f (single layerIndex * 1.37f) 0.0f
                         else Vector4.Zero
                     EffectParameters2 = Vector4.Zero })
        composite

    let private warmWord =
        lazy (makeEffectComposite "Lobster-Regular.ttf" "SLUG" SlugFillSource.Solid 9 warm)

    let private scriptShadow =
        lazy (makeEffectComposite "Allura-Regular.ttf" "slughorn" SlugFillSource.Solid 0 ink)

    let private scriptFill =
        lazy (makeEffectComposite "Allura-Regular.ttf" "slughorn" SlugFillSource.Solid 0 paper)

    let private chippedWord =
        lazy (makeEffectComposite "Anton-Regular.ttf" "SLUGHORN" SlugFillSource.Solid 10 pigment)

    let private filledA =
        lazy (makeEffectComposite "LibreBaskerville.ttf" "A" SlugFillSource.Solid 0 paper)

    let private outlineA =
        lazy (makeEffectComposite "LibreBaskerville.ttf" "A" SlugFillSource.Solid 0 stroke)

    let private outlineCutoutA =
        lazy (makeEffectComposite "LibreBaskerville.ttf" "A" SlugFillSource.Solid 0 panelColor)

    let private ringOffsets (radius : single) : Vector2 array =
        Array.init 16 (fun index ->
            let angle = single index * MathF.PI * 2.0f / 16.0f
            v2 (MathF.Cos angle * radius) (MathF.Sin angle * radius))

    let private scriptOutlineOffsets = ringOffsets 2.0f
    let private glyphOutlineOffsets = ringOffsets 1.0f

    let private placeTextComposite (name : string) (composite : SlugCompositeShape) (position : Vector3) (size : Vector3) (rotation : Quaternion) (elevation : single) computeConfigOpt (world : World) =
        let bounds = SlugDemoContours.getCompositeLayerBounds composite
        SlugDemoContours.placeCompositeInBounds
            name composite bounds position size rotation elevation computeConfigOpt world

    let private placeOutline (name : string) (composite : SlugCompositeShape) (position : Vector3) (size : Vector3) (offsets : Vector2 array) (elevation : single) (world : World) =
        for index in 0 .. offsets.Length - 1 do
            let offset = offsets.[index]
            placeTextComposite
                (name + string index)
                composite
                (position + v3 offset.X offset.Y 0.0f)
                size
                Quaternion.Identity
                elevation
                None
                world

    let draw (world : World) =
        let topLeft = v3 -150.0f 55.0f 0.0f
        let topRight = v3 150.0f 55.0f 0.0f
        let bottomLeft = v3 -150.0f -75.0f 0.0f
        let bottomRight = v3 150.0f -75.0f 0.0f
        let panelSize = v3 292.0f 126.0f 0.0f

        SlugDemo.panel "TextEffectsWarmPanel" topLeft panelSize panelColor 0.0f world
        SlugDemo.panel "TextEffectsScriptPanel" topRight panelSize panelColor 0.0f world
        SlugDemo.panel "TextEffectsStrokePanel" bottomRight panelSize panelColor 0.0f world
        World.doStaticSprite
            "TextEffectsStone"
            [Entity.Position .= bottomLeft
             Entity.Size .= panelSize
             Entity.Elevation .= 0.0f
             Entity.StaticImage .= stoneImage
             Entity.Color .= Color (110uy, 110uy, 110uy, 255uy)]
            world

        placeTextComposite
            "TextEffectsWarm"
            warmWord.Value
            (topLeft + v3 0.0f -3.0f 0.0f)
            (v3 180.0f 60.0f 0.0f)
            Quaternion.Identity
            2.0f
            None
            world

        placeOutline
            "TextEffectsScriptOutline"
            scriptShadow.Value
            (topRight + v3 0.0f -2.0f 0.0f)
            (v3 235.0f 54.0f 0.0f)
            scriptOutlineOffsets
            1.0f
            world
        placeTextComposite
            "TextEffectsScript"
            scriptFill.Value
            (topRight + v3 0.0f -2.0f 0.0f)
            (v3 235.0f 54.0f 0.0f)
            Quaternion.Identity
            2.0f
            None
            world

        placeTextComposite
            "TextEffectsChipped"
            chippedWord.Value
            bottomLeft
            (v3 235.0f 32.0f 0.0f)
            (Quaternion.CreateFromAxisAngle (Vector3.UnitZ, -0.16f))
            2.0f
            None
            world

        placeTextComposite
            "TextEffectsFilledA"
            filledA.Value
            (bottomRight + v3 -55.0f -2.0f 0.0f)
            (v3 36.0f 50.0f 0.0f)
            Quaternion.Identity
            2.0f
            None
            world
        placeOutline
            "TextEffectsOutlineA"
            outlineA.Value
            (bottomRight + v3 55.0f -2.0f 0.0f)
            (v3 36.0f 50.0f 0.0f)
            glyphOutlineOffsets
            1.0f
            world
        placeTextComposite
            "TextEffectsOutlineCutoutA"
            outlineCutoutA.Value
            (bottomRight + v3 55.0f -2.0f 0.0f)
            (v3 36.0f 50.0f 0.0f)
            Quaternion.Identity
            2.0f
            None
            world
