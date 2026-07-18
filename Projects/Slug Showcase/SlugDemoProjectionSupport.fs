namespace SlugShowcase
open System
open System.Numerics
open Prime
open Nu

module SlugDemoProjectionSupport =

    let zRotation radians =
        Quaternion.CreateFromAxisAngle (Vector3.UnitZ, radians)

    let contour fill stroke scale commands =
        SlugShowcaseContours.makeFilled commands fill NonZero stroke 1.2f scale

    let sprite name position size rotation color elevation world =
        World.doStaticSprite
            name
            [Entity.Position .= position
             Entity.Size .= size
             Entity.Rotation @= rotation
             Entity.Elevation .= elevation
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= color]
            world

    let label name text position size fontSize color elevation world =
        SlugDemo.slugLeft
            name
            SlugDemo.font
            text
            position
            size
            fontSize
            color
            elevation
            TextDirectionLeftToRight
            None
            world

    let pbrLightDot =
        contour
            (color 1.0f 0.79f 0.36f 1.0f)
            (color 1.0f 0.96f 0.68f 1.0f)
            (v2 10.0f 10.0f)
            SlugShowcaseContours.circleCommands


    let projectionCircle =
        contour
            (color 0.055f 0.12f 0.19f 1.0f)
            (color 0.26f 0.75f 0.91f 1.0f)
            (v2 78.0f 78.0f)
            SlugShowcaseContours.circleCommands

    let projectionCircleSmall =
        contour
            (color 0.10f 0.12f 0.22f 1.0f)
            (color 0.98f 0.52f 0.72f 1.0f)
            (v2 48.0f 48.0f)
            SlugShowcaseContours.circleCommands

    let projection2dGridXs = [| -266.0f; -236.0f; -206.0f; -176.0f; -146.0f; -116.0f; -86.0f; -56.0f; -26.0f |]
    let projection2dGridYs = [| 62.0f; 42.0f; 22.0f; 2.0f; -18.0f; -38.0f; -58.0f; -78.0f |]
    let projection3dRays = [| -276.0f; -246.0f; -216.0f; -186.0f; -156.0f; -126.0f; -96.0f; -66.0f; -36.0f; -6.0f |]
    let projection3dRows = [| 31.0f; 20.0f; 9.0f; -2.0f; -13.0f; -24.0f; -35.0f; -46.0f; -57.0f; -68.0f; -79.0f |]
    let projection2dGridXNames = [| "Projection2dGridX0"; "Projection2dGridX1"; "Projection2dGridX2"; "Projection2dGridX3"; "Projection2dGridX4"; "Projection2dGridX5"; "Projection2dGridX6"; "Projection2dGridX7"; "Projection2dGridX8" |]
    let projection2dGridYNames = [| "Projection2dGridY0"; "Projection2dGridY1"; "Projection2dGridY2"; "Projection2dGridY3"; "Projection2dGridY4"; "Projection2dGridY5"; "Projection2dGridY6"; "Projection2dGridY7" |]
    let projection3dRayNames = [| "Projection3dRay0"; "Projection3dRay1"; "Projection3dRay2"; "Projection3dRay3"; "Projection3dRay4"; "Projection3dRay5"; "Projection3dRay6"; "Projection3dRay7"; "Projection3dRay8"; "Projection3dRay9" |]
    let projection3dRowNames = [| "Projection3dRow0"; "Projection3dRow1"; "Projection3dRow2"; "Projection3dRow3"; "Projection3dRow4"; "Projection3dRow5"; "Projection3dRow6"; "Projection3dRow7"; "Projection3dRow8"; "Projection3dRow9"; "Projection3dRow10" |]

    let projectionCircleComposite =
        let source =
            SlugShapeRuntime.fromContourCommands
                SlugShowcaseContours.circleCommands
                SlugFillNonzero
                1.0e-3f
        let data = SlugShapeRuntime.pack [| source |]
        let state =
            { SlugLayerState.defaultState 0 with
                Color = color 0.16f 0.72f 0.88f 1.0f }
        SlugShapeRuntime.createComposite data [| state |]

    let objectGlyphComposites =
        lazy
            use loader = new SlugColorFontLoader (SlugDemo.fontFilePath)
            let glyphs = "SLUGFICSLUG"
            let colors =
                [| SlugDemo.white; SlugDemo.cyan; SlugDemo.white; SlugDemo.cyan
                   SlugDemo.white; SlugDemo.amber; SlugDemo.cyan
                   SlugDemo.white; SlugDemo.magenta; SlugDemo.white; SlugDemo.magenta |]
            glyphs
            |> Seq.mapi (fun index glyph ->
                loader.LoadComposite (loader.GetGlyphId (uint32 glyph), foreground = colors.[index]))
            |> Seq.toArray

    // Dedicated PNG copies keep the six environment faces available in the
    // Render2d asset context without conflicting with the engine's Render3d
    // SkyBox*.tif / cubemap registrations.
    let pbrEnvironment =
        [| asset<Image> Assets.Default.PackageName "SlugPbrRight"
           asset<Image> Assets.Default.PackageName "SlugPbrLeft"
           asset<Image> Assets.Default.PackageName "SlugPbrTop"
           asset<Image> Assets.Default.PackageName "SlugPbrBottom"
           asset<Image> Assets.Default.PackageName "SlugPbrBack"
           asset<Image> Assets.Default.PackageName "SlugPbrFront" |]

    let pbrSlugMaterials =
        lazy
            use loader = new SlugColorFontLoader (SlugDemo.fontFilePath)
            let glyphId = loader.GetGlyphId (uint32 'S')
            [| 0.08f; 0.36f; 0.78f |]
            |> Array.mapi (fun index roughness ->
                let composite = loader.LoadComposite (glyphId, foreground = Color.White)
                let state = composite.Layers.Item 0
                let bounds = composite.Data.Metadata.[state.ShapeIndex].Bounds
                let origin = (bounds.Min + bounds.Max) * 0.5f
                composite.SetLayerState (
                    0,
                    { state with
                        Origin = origin
                        Color = if index = 0 then SlugDemo.cyan elif index = 1 then SlugDemo.magenta else SlugDemo.amber
                        FillSource = SlugFillSource.PbrMaterial 0
                        MaterialValues = v4 0.92f roughness 0.0f 0.0f })
                composite)
