namespace SlugShowcase
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoAnimatedHud =

    // osgSlug's ammo HUD is a single retained CompositeShape.  The shape data is
    // immutable, while the 42 visible / eject layers and the two counter layers
    // are changed in place below.
    let private bulletCount = 20
    let private revealDuration = 5.0f
    let private holdDuration = 1.0f
    let private cycleDuration = revealDuration + holdDuration
    let private fadeDuration = 0.15f
    let private ejectDuration = 0.4f
    let private dimAlpha = 0.25f

    let private outerRadius = 0.40f
    let private innerRadius = 0.32f
    let private seam = 0.06f
    let private ringStroke = 0.005f
    let private arcSegments = 32
    let private bulletWidth = 0.03f
    let private bulletHeight = 0.05f
    let private ejectDistance = 0.25f

    let private circlePoint radius angle =
        v2 (radius * MathF.Cos angle) (radius * MathF.Sin angle)

    let private appendContour (target : ResizeArray<ContourCommand>) (commands : ContourCommand array) =
        for command in commands do target.Add command

    let private annulusCommands startAngle finishAngle outer inner =
        let commands = ResizeArray<ContourCommand> ()
        commands.Add (MoveTo (circlePoint outer startAngle))
        for index in 1 .. arcSegments do
            let amount = single index / single arcSegments
            commands.Add (LineTo (circlePoint outer (startAngle + (finishAngle - startAngle) * amount)))
        commands.Add (LineTo (circlePoint inner finishAngle))
        for index in arcSegments - 1 .. -1 .. 0 do
            let amount = single index / single arcSegments
            commands.Add (LineTo (circlePoint inner (startAngle + (finishAngle - startAngle) * amount)))
        commands.Add CloseContour
        commands.ToArray ()

    let private connectorCommands angle =
        let radial = v2 (MathF.Cos angle) (MathF.Sin angle)
        let normal = v2 (-radial.Y * ringStroke * 0.5f) (radial.X * ringStroke * 0.5f)
        let outer = radial * outerRadius
        let inner = radial * innerRadius
        [| MoveTo (outer + normal)
           LineTo (outer - normal)
           LineTo (inner - normal)
           LineTo (inner + normal)
           CloseContour |]

    let private gaugeStrokeCommands =
        let commands = ResizeArray<ContourCommand> ()
        let upperStart = seam
        let upperFinish = MathF.PI - seam
        appendContour commands (annulusCommands upperStart upperFinish (outerRadius + ringStroke * 0.5f) (outerRadius - ringStroke * 0.5f))
        appendContour commands (annulusCommands upperStart upperFinish (innerRadius + ringStroke * 0.5f) (innerRadius - ringStroke * 0.5f))
        appendContour commands (connectorCommands upperStart)
        appendContour commands (connectorCommands upperFinish)
        commands.ToArray ()

    let private bulletCommands =
        let commands = ResizeArray<ContourCommand> ()
        let gap = bulletHeight / 12.0f
        let pieceHeight = (bulletHeight - 2.0f * gap) / 4.0f
        let yBottom = (outerRadius + innerRadius) * 0.5f - bulletHeight * 0.5f
        let yCaseTop = yBottom + pieceHeight
        let yBodyBottom = yCaseTop + gap
        let yBodyTop = yBodyBottom + pieceHeight * 2.0f
        let yTipBottom = yBodyTop + gap
        let yTip = yTipBottom + pieceHeight
        let addRectangle y height =
            commands.Add (MoveTo (v2 (-bulletWidth * 0.5f) y))
            commands.Add (LineTo (v2 (bulletWidth * 0.5f) y))
            commands.Add (LineTo (v2 (bulletWidth * 0.5f) (y + height)))
            commands.Add (LineTo (v2 (-bulletWidth * 0.5f) (y + height)))
            commands.Add CloseContour
        addRectangle yBottom pieceHeight
        addRectangle yBodyBottom (pieceHeight * 2.0f)
        commands.Add (MoveTo (v2 0.0f yTip))
        commands.Add (LineTo (v2 (-bulletWidth * 0.5f) yTipBottom))
        commands.Add (LineTo (v2 (bulletWidth * 0.5f) yTipBottom))
        commands.Add CloseContour
        commands.ToArray ()

    let private emptyHalfCommands =
        annulusCommands (-seam) (seam - MathF.PI) outerRadius innerRadius

    // Seven-segment digits with chamfered ends are the small, Orbitron-like
    // counter used by the original sample.  They live in the same packed shape
    // data so changing 00..20 only changes ShapeIndex on two layers.
    let private addPolygon (target : ResizeArray<ContourCommand>) (points : Vector2 array) =
        target.Add (MoveTo points.[0])
        for index in 1 .. dec points.Length do target.Add (LineTo points.[index])
        target.Add CloseContour

    let private horizontalSegment centerY =
        let width = 0.14f
        let thickness = 0.026f
        let chamfer = 0.008f
        let left = -width * 0.5f
        let right = width * 0.5f
        let top = centerY + thickness * 0.5f
        let bottom = centerY - thickness * 0.5f
        [| v2 (left + chamfer) top
           v2 (right - chamfer) top
           v2 right (top - chamfer)
           v2 right (bottom + chamfer)
           v2 (right - chamfer) bottom
           v2 (left + chamfer) bottom
           v2 left (bottom + chamfer)
           v2 left (top - chamfer) |]

    let private verticalSegment centerX centerY =
        let height = 0.10f
        let thickness = 0.026f
        let chamfer = 0.008f
        let left = centerX - thickness * 0.5f
        let right = centerX + thickness * 0.5f
        let top = centerY + height * 0.5f
        let bottom = centerY - height * 0.5f
        [| v2 (left + chamfer) top
           v2 (right - chamfer) top
           v2 right (top - chamfer)
           v2 right (bottom + chamfer)
           v2 (right - chamfer) bottom
           v2 (left + chamfer) bottom
           v2 left (bottom + chamfer)
           v2 left (top - chamfer) |]

    let private digitCommands digit =
        let commands = ResizeArray<ContourCommand> ()
        let leftX = -0.057f
        let rightX = 0.057f
        let enabled segment =
            match digit, segment with
            | 0, 0 | 0, 1 | 0, 2 | 0, 3 | 0, 4 | 0, 5 -> true
            | 1, 1 | 1, 2 -> true
            | 2, 0 | 2, 1 | 2, 6 | 2, 4 | 2, 3 -> true
            | 3, 0 | 3, 1 | 3, 2 | 3, 3 | 3, 6 -> true
            | 4, 5 | 4, 6 | 4, 1 | 4, 2 -> true
            | 5, 0 | 5, 5 | 5, 6 | 5, 2 | 5, 3 -> true
            | 6, 0 | 6, 5 | 6, 6 | 6, 4 | 6, 2 | 6, 3 -> true
            | 7, 0 | 7, 1 | 7, 2 -> true
            | 8, _ -> true
            | 9, 0 | 9, 5 | 9, 6 | 9, 1 | 9, 2 | 9, 3 -> true
            | _ -> false
        if enabled 0 then addPolygon commands (horizontalSegment 0.10f)
        if enabled 3 then addPolygon commands (horizontalSegment -0.10f)
        if enabled 6 then addPolygon commands (horizontalSegment 0.0f)
        if enabled 5 then addPolygon commands (verticalSegment leftX 0.05f)
        if enabled 4 then addPolygon commands (verticalSegment leftX -0.05f)
        if enabled 1 then addPolygon commands (verticalSegment rightX 0.05f)
        if enabled 2 then addPolygon commands (verticalSegment rightX -0.05f)
        commands.ToArray ()

    let private makeSource commands =
        SlugShapeRuntime.fromContourCommands commands SlugFillNonzero 1.0e-4f

    let private ammoComposite =
        lazy
            (let strokeSource = makeSource gaugeStrokeCommands
             let bulletSource = makeSource bulletCommands
             let emptySource = makeSource emptyHalfCommands
             let digitSources = Array.init 10 (fun digit -> makeSource (digitCommands digit))
             let sources = Array.append [| strokeSource; bulletSource; emptySource |] digitSources
             let gradientStops =
                 [| { Offset = 0.0f; Color = color 1.0f 0.60f 0.0f 0.90f }
                    { Offset = 0.5f; Color = color 0.90f 0.20f 0.0f 0.60f }
                    { Offset = 1.0f; Color = color 0.55f 0.0f 0.0f 0.25f } |]
             let gradient =
                 SlugGradient
                     (SlugGradientKind.Linear (v2 0.0f (-outerRadius), v2 0.0f outerRadius), gradientStops)
             let data = SlugShapeRuntime.packWithResources sources [| gradient |] gradientStops [||] [||]
             let layers = ResizeArray<SlugLayerState> ()
             layers.Add
                 { SlugLayerState.defaultState 0 with
                     Color = color 1.0f 1.0f 1.0f 1.0f }
             let tStart = -MathF.PI * 0.5f + 0.1f + seam
             let tStep = (MathF.PI - 2.0f * (0.1f + seam)) / single (bulletCount - 1)
             for index in 0 .. bulletCount - 1 do
                 let theta = tStart + single index * tStep
                 layers.Add
                     { SlugLayerState.defaultState 1 with
                         Transform = Matrix4x4.CreateRotationZ theta
                         Color = color 1.0f 1.0f 1.0f 1.0f }
             layers.Add
                 { SlugLayerState.defaultState 2 with
                     FillSource = SlugFillSource.Gradient 0
                     Color = color 1.0f 1.0f 1.0f 0.10f }
             for index in 0 .. bulletCount - 1 do
                 let theta = tStart + single index * tStep
                 layers.Add
                     { SlugLayerState.defaultState 1 with
                         Transform = Matrix4x4.CreateRotationZ theta
                         EffectId = 2
                         EffectParameters = Vector4.Zero
                         Color = color 1.0f 1.0f 1.0f 1.0f }
             // osgSlug authors the counter after restoring the HUD canvas transform:
             // 0.1-unit text centered at (0.5, 0.33). The retained composite still
             // receives the shared -PI/2 root rotation, so apply its inverse here,
             // scale the em-sized segment sources to the canonical text height, and
             // pre-rotate (0.17, x) into the final lower-center placement.
             let counterScale = 0.30f
             let counterY = -0.17f
             let counterRotation = Matrix4x4.CreateRotationZ (MathF.PI / 2.0f)
             for x in [| -0.05f; 0.05f |] do
                 let mutable counterTransform = Matrix4x4.CreateScale counterScale * counterRotation
                 counterTransform.Translation <- Vector3 (-counterY, x, 0.0f)
                 layers.Add
                     { SlugLayerState.defaultState 3 with
                         Transform = counterTransform
                         Color = color 1.0f 1.0f 1.0f 1.0f }
             SlugShapeRuntime.createComposite data (layers.ToArray ()))

    let private layerCounterBase = 2 * bulletCount + 2
    let private layerEjectBase = bulletCount + 2

    let draw (world : World) =
        let seconds = SlugDemo.clockSeconds world
        let t = seconds % cycleDuration
        let composite = ammoComposite.Value
        let tStart = -MathF.PI * 0.5f + 0.1f + seam
        let tStep = (MathF.PI - 2.0f * (0.1f + seam)) / single (bulletCount - 1)
        let mutable ammo = bulletCount
        let mutable continuousAmmo = 0.0f

        for index in 0 .. bulletCount - 1 do
            let reveal = revealDuration * MathF.Sqrt (single index / single (bulletCount - 1))
            let fade = max 0.0f (min 1.0f ((t - reveal) / fadeDuration))
            let bright = 1.0f - (1.0f - dimAlpha) * fade
            if t >= reveal + fadeDuration then ammo <- ammo - 1
            continuousAmmo <- continuousAmmo + 1.0f - fade
            composite.SetLayerColor (index + 1, color 1.0f 1.0f 1.0f bright)

            let ejectT = max 0.0f (min 1.0f ((t - reveal) / ejectDuration))
            let theta = tStart + single index * tStep
            let radial = v2 (MathF.Cos theta) (MathF.Sin theta)
            let mutable ejectTransform = Matrix4x4.CreateRotationZ theta
            ejectTransform.Translation <- Vector3 (radial.X * ejectDistance * ejectT, radial.Y * ejectDistance * ejectT, 0.0f)
            composite.SetLayerTransform (layerEjectBase + index, ejectTransform)
            // Effect 2 is pulse in Nu's shared shader rather than osgSlug's
            // custom displacement hook. Preserve the canonical progress in the
            // layer parameters while applying the exact displacement as a transform.
            composite.SetLayerEffectParam (layerEjectBase + index, v4 0.0f 0.0f 0.0f ejectT)
            composite.SetLayerColor (layerEjectBase + index, color 1.0f 1.0f 1.0f (1.0f - ejectT))

        let danger = 1.0f - continuousAmmo / single bulletCount
        let mutable gradientTransform = Matrix4x4.Identity
        gradientTransform.M11 <- 0.0f
        gradientTransform.M21 <- 1.25f
        gradientTransform.M41 <- 0.5f - 1.5f * danger
        composite.SetLayerGradientTransform (bulletCount + 1, gradientTransform)
        composite.SetLayerColor (bulletCount + 1, color 1.0f 1.0f 1.0f (0.10f + danger * 0.90f))
        composite.SetLayerShapeIndex (layerCounterBase, 3 + ammo / 10)
        composite.SetLayerShapeIndex (layerCounterBase + 1, 3 + ammo % 10)

        // The gallery chrome is intentionally omitted: the canonical centered
        // ammo HUD is the only scene content.
        SlugShowcaseContours.placeComposite
            "AnimatedHudAmmo"
            composite
            (v3 0.0f -20.0f 0.0f)
            (v3 165.0f 165.0f 0.0f)
            (Quaternion.CreateFromAxisAngle (Vector3.UnitZ, -MathF.PI / 2.0f))
            0.0f
            None
            world
