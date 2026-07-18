namespace SlugDemo
open System.Numerics
open Nu
open SlugDemoProjectionSupport

[<RequireQualifiedAccess>]
module SlugDemoPbrIbl =

    let private chromeText =
        lazy
            let composite = SlugDemoTextSupport.makeTextComposite "Font.ttf" "osgSlug"
            let struct (minPoint, maxPoint) = SlugDemoContours.getCompositeLayerBounds composite
            let center = (minPoint + maxPoint) * 0.5f
            let span = max (maxPoint.X - minPoint.X) (maxPoint.Y - minPoint.Y)
            let pbrCoordinateScale = 4.0f / max span 1.0e-6f
            for layerIndex in 0 .. composite.LayerCount - 1 do
                let state = composite.Layers.Item layerIndex
                composite.SetLayerState
                    (layerIndex,
                     { state with
                         Color = color 0.90f 0.90f 0.92f 1.0f
                         FillSource = SlugFillSource.PbrMaterial 0
                         EffectParameters2 = v4 center.X center.Y pbrCoordinateScale 0.0f
                         MaterialValues = v4 1.0f 0.08f 0.20f 0.0f })
            composite

    let private renderChromeText (world : World) =
        let composite = chromeText.Value
        SlugDemoContours.placeCompositeInBoundsWithTextures
            "PbrIblText"
            composite
            (SlugDemoContours.getCompositeLayerBounds composite)
            (v3 0.0f -10.0f 0.0f)
            (v3 500.0f 125.0f 0.0f)
            Quaternion.Identity
            2.0f
            None
            pbrEnvironment
            world

    let draw (world : World) =
        SlugDemo.panel
            "PbrIblBackdrop"
            (v3 0.0f -10.0f 0.0f)
            (v3 570.0f 250.0f 0.0f)
            (Color (2uy, 2uy, 3uy, 255uy))
            0.0f
            world
        renderChromeText world
