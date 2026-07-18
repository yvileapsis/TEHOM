namespace SlugShowcase

open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoMixedScenes =

    // The reference card is authored in the same 356 x 516 SVG coordinate system as
    // osgslug-compositeshape-mixed.cpp.  Keeping the source coordinates intact makes
    // the card, axolotl, and text anchors easy to compare with the original.
    let private svgPoint x y = v2 (x - 180.0f) (260.0f - y)

    let private polygonCommands (points : Vector2 array) =
        Array.concat
            [| [| MoveTo points.[0] |]
               points.[1 ..] |> Array.map LineTo
               [| CloseContour |] |]

    let private polygonSvg points =
        points |> Array.map (fun (x, y) -> svgPoint x y) |> polygonCommands

    let private roundedRectSvg left top right bottom radius =
        [| MoveTo (svgPoint (left + radius) top)
           LineTo (svgPoint (right - radius) top)
           QuadraticCurveTo (svgPoint right top, svgPoint right (top + radius))
           LineTo (svgPoint right (bottom - radius))
           QuadraticCurveTo (svgPoint right bottom, svgPoint (right - radius) bottom)
           LineTo (svgPoint (left + radius) bottom)
           QuadraticCurveTo (svgPoint left bottom, svgPoint left (bottom - radius))
           LineTo (svgPoint left (top + radius))
           QuadraticCurveTo (svgPoint left top, svgPoint (left + radius) top)
           CloseContour |]

    let private roundedRectSvgReverse left top right bottom radius =
        [| MoveTo (svgPoint (left + radius) top)
           LineTo (svgPoint left (top + radius))
           LineTo (svgPoint left (bottom - radius))
           QuadraticCurveTo (svgPoint left bottom, svgPoint (left + radius) bottom)
           LineTo (svgPoint (right - radius) bottom)
           QuadraticCurveTo (svgPoint right bottom, svgPoint right (bottom - radius))
           LineTo (svgPoint right (top + radius))
           QuadraticCurveTo (svgPoint right top, svgPoint (right - radius) top)
           LineTo (svgPoint (left + radius) top)
           CloseContour |]

    let private cubicSampleCount = 16
    let private strokeMiterLimit = 4.0f

    let private evaluateCubic (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (p3 : Vector2) (t : float32) : Vector2 =
        let u = 1.0f - t
        Vector2.Multiply (p0, u * u * u) +
        Vector2.Multiply (p1, 3.0f * u * u * t) +
        Vector2.Multiply (p2, 3.0f * u * t * t) +
        Vector2.Multiply (p3, t * t * t)

    let private sampleStrokeCenterline (points : Vector2 array) =
        if points.Length = 2 then points
        elif points.Length >= 4 && (points.Length - 1) % 3 = 0 then
            let segmentCount = (points.Length - 1) / 3
            [| for segmentIndex in 0 .. segmentCount - 1 do
                   let controlIndex = segmentIndex * 3
                   let p0 = points.[controlIndex]
                   let p1 = points.[controlIndex + 1]
                   let p2 = points.[controlIndex + 2]
                   let p3 = points.[controlIndex + 3]
                   let firstSample = if segmentIndex = 0 then 0 else 1
                   for sampleIndex in firstSample .. cubicSampleCount do
                       let t = single sampleIndex / single cubicSampleCount
                       yield evaluateCubic p0 p1 p2 p3 t |]
        else invalidArg "points" "A stroke must contain either one line or consecutive cubic Bezier controls."

    // Canvas strokes are expanded into filled outlines before Slug preprocessing.
    // Preserve the source cubic controls instead of treating them as polyline vertices.
    let private capsuleSvg thickness points =
        let centerline =
            points
            |> Array.map (fun (x, y) -> svgPoint x y)
            |> sampleStrokeCenterline
        let halfWidth = thickness * 0.5f
        let segmentNormals =
            Array.init (centerline.Length - 1) (fun index ->
                let delta = centerline.[index + 1] - centerline.[index]
                if delta.LengthSquared () > 1.0e-12f then
                    let tangent = Vector2.Normalize delta
                    v2 (-tangent.Y) tangent.X
                else Vector2.UnitY)
        let pointNormals =
            Array.init centerline.Length (fun index ->
                if index = 0 then segmentNormals.[0]
                elif index = centerline.Length - 1 then segmentNormals.[segmentNormals.Length - 1]
                else
                    let sum = segmentNormals.[index - 1] + segmentNormals.[index]
                    if sum.LengthSquared () <= 1.0e-12f then segmentNormals.[index]
                    else
                        let normal = Vector2.Normalize sum
                        let denominator = Vector2.Dot (normal, segmentNormals.[index])
                        if denominator > 1.0e-3f then normal * min strokeMiterLimit (1.0f / denominator)
                        else segmentNormals.[index])
        let left =
            Array.mapi (fun index point -> point + pointNormals.[index] * halfWidth) centerline
        let right =
            Array.mapi (fun index point -> point - pointNormals.[index] * halfWidth) centerline
        polygonCommands (Array.append left (Array.rev right))

    let private ellipseSvg cx cy rx ry =
        let kappa = 0.55228475f
        [| MoveTo (svgPoint (cx + rx) cy)
           CubicCurveTo
               (svgPoint (cx + rx) (cy + kappa * ry),
                svgPoint (cx + kappa * rx) (cy + ry),
                svgPoint cx (cy + ry))
           CubicCurveTo
               (svgPoint (cx - kappa * rx) (cy + ry),
                svgPoint (cx - rx) (cy + kappa * ry),
                svgPoint (cx - rx) cy)
           CubicCurveTo
               (svgPoint (cx - rx) (cy - kappa * ry),
                svgPoint (cx - kappa * rx) (cy - ry),
                svgPoint cx (cy - ry))
           CubicCurveTo
               (svgPoint (cx + kappa * rx) (cy - ry),
                svgPoint (cx + rx) (cy - kappa * ry),
                svgPoint (cx + rx) cy)
           CloseContour |]

    let private ringSvg outer inner =
        Array.append outer inner
    let private source commands =
        SlugShapeRuntime.fromContourCommands commands SlugFillNonzero 1.0e-3f

    let private solidState shapeIndex color =
        { SlugLayerState.defaultState shapeIndex with Color = color }

    let private makeComposite
        (commandsAndStates : (ContourCommand array * (int -> SlugLayerState)) array)
        (gradients : SlugGradient array)
        (gradientStops : SlugGradientStop array) =
        let sources = commandsAndStates |> Array.map (fun (commands, _) -> source commands)
        let states =
            commandsAndStates
            |> Array.mapi (fun index (_, stateFactory) -> stateFactory index)
        let data = SlugShapeRuntime.packWithResources sources gradients gradientStops [||] [||]
        SlugShapeRuntime.createComposite data states

    // ── CARD BACKGROUND ───────────────────────────────────────────────────────

    let private cardComposite =
        let outer = roundedRectSvg 2.0f 2.0f 358.0f 518.0f 18.0f
        let inner = roundedRectSvgReverse 10.0f 10.0f 350.0f 510.0f 10.0f
        let cardFill = roundedRectSvg 2.0f 2.0f 358.0f 518.0f 18.0f
        let topOverlay = roundedRectSvg 2.0f 2.0f 358.0f 250.0f 18.0f
        let border = ringSvg outer inner
        let divider = capsuleSvg 1.5f [| (20.0f, 250.0f); (340.0f, 250.0f) |]
        let flavorBox = roundedRectSvg 22.0f 330.0f 338.0f 475.0f 8.0f
        let flavorBorder =
            ringSvg
                (roundedRectSvg 22.0f 330.0f 338.0f 475.0f 8.0f)
                (roundedRectSvgReverse 24.0f 332.0f 336.0f 473.0f 6.0f)
        let gradientStops =
            [| { Offset = 0.0f; Color = color 0.08f 0.22f 0.58f 0.70f }
               { Offset = 1.0f; Color = color 0.08f 0.22f 0.58f 0.0f } |]
        let gradient =
            SlugGradient
                (SlugGradientKind.Linear (v2 0.0f 258.0f, v2 0.0f 10.0f), gradientStops)
        makeComposite
            [| cardFill, fun index -> solidState index (color 0.11f 0.10f 0.18f 1.0f)
               topOverlay, fun index ->
                   { SlugLayerState.defaultState index with
                       Color = Color.One
                       FillSource = SlugFillSource.Gradient 0 }
               border, fun index -> solidState index (color 0.85f 0.66f 0.26f 0.70f)
               divider, fun index -> solidState index (color 0.85f 0.66f 0.26f 0.50f)
               flavorBox, fun index -> solidState index (color 0.06f 0.06f 0.12f 0.75f)
               flavorBorder, fun index -> solidState index (color 0.85f 0.66f 0.26f 0.50f) |]
            [| gradient |]
            gradientStops

    // ── AXOLOTL ───────────────────────────────────────────────────────────────

    let private body =
        [| MoveTo (svgPoint 180.0f 128.0f)
           CubicCurveTo (svgPoint 216.0f 122.0f, svgPoint 250.0f 134.0f, svgPoint 252.0f 154.0f)
           CubicCurveTo (svgPoint 254.0f 172.0f, svgPoint 239.0f 192.0f, svgPoint 226.0f 200.0f)
           CubicCurveTo (svgPoint 211.0f 209.0f, svgPoint 196.0f 212.0f, svgPoint 180.0f 212.0f)
           CubicCurveTo (svgPoint 164.0f 212.0f, svgPoint 149.0f 209.0f, svgPoint 134.0f 200.0f)
           CubicCurveTo (svgPoint 121.0f 192.0f, svgPoint 106.0f 172.0f, svgPoint 108.0f 154.0f)
           CubicCurveTo (svgPoint 110.0f 134.0f, svgPoint 144.0f 122.0f, svgPoint 180.0f 128.0f)
           CloseContour |]

    let private face = color 0.72f 0.60f 0.47f 1.0f
    let private gillMain = color 0.85f 0.41f 0.53f 1.0f
    let private gillBranch = color 0.95f 0.63f 0.74f 1.0f
    let private limb = color 0.93f 0.88f 0.81f 1.0f

    // Each tuple is one of the six canonical stalk groups.  Every group carries
    // the same phase on its stalk and three branches, matching osgSlug's layer
    // effect-id grouping while using the existing analytic Slug wave effect.
    let private gillGroups =
        [| 0.0f,
           [| capsuleSvg 2.8f [| (130.0f, 152.0f); (116.0f, 142.0f); (101.0f, 122.0f); (90.0f, 96.0f) |]
              capsuleSvg 1.4f [| (115.0f, 133.0f); (109.0f, 125.0f); (104.0f, 118.0f); (101.0f, 110.0f) |]
              capsuleSvg 1.4f [| (107.0f, 120.0f); (101.0f, 113.0f); (97.0f, 107.0f); (96.0f, 100.0f) |]
              capsuleSvg 1.4f [| (99.0f, 106.0f); (94.0f, 100.0f); (91.0f, 95.0f); (90.0f, 90.0f) |] |]
           0.7f,
           [| capsuleSvg 2.8f [| (140.0f, 141.0f); (132.0f, 127.0f); (127.0f, 106.0f); (124.0f, 78.0f) |]
              capsuleSvg 1.4f [| (135.0f, 124.0f); (128.0f, 117.0f); (124.0f, 111.0f); (122.0f, 105.0f) |]
              capsuleSvg 1.4f [| (130.0f, 109.0f); (124.0f, 103.0f); (121.0f, 97.0f); (120.0f, 91.0f) |]
              capsuleSvg 1.4f [| (125.0f, 93.0f); (120.0f, 87.0f); (118.0f, 82.0f); (117.0f, 77.0f) |] |]
           1.4f,
           [| capsuleSvg 2.8f [| (152.0f, 134.0f); (149.0f, 119.0f); (148.0f, 101.0f); (151.0f, 74.0f) |]
              capsuleSvg 1.4f [| (151.0f, 117.0f); (146.0f, 111.0f); (143.0f, 105.0f); (142.0f, 99.0f) |]
              capsuleSvg 1.4f [| (150.0f, 101.0f); (146.0f, 95.0f); (144.0f, 89.0f); (143.0f, 83.0f) |]
              capsuleSvg 1.4f [| (151.0f, 85.0f); (148.0f, 79.0f); (147.0f, 73.0f); (147.0f, 67.0f) |] |]
           2.1f,
           [| capsuleSvg 2.8f [| (230.0f, 152.0f); (244.0f, 142.0f); (259.0f, 122.0f); (270.0f, 96.0f) |]
              capsuleSvg 1.4f [| (245.0f, 133.0f); (251.0f, 125.0f); (256.0f, 118.0f); (259.0f, 110.0f) |]
              capsuleSvg 1.4f [| (253.0f, 120.0f); (259.0f, 113.0f); (263.0f, 107.0f); (264.0f, 100.0f) |]
              capsuleSvg 1.4f [| (261.0f, 106.0f); (266.0f, 100.0f); (269.0f, 95.0f); (270.0f, 90.0f) |] |]
           2.8f,
           [| capsuleSvg 2.8f [| (220.0f, 141.0f); (228.0f, 127.0f); (233.0f, 106.0f); (236.0f, 78.0f) |]
              capsuleSvg 1.4f [| (225.0f, 124.0f); (232.0f, 117.0f); (236.0f, 111.0f); (238.0f, 105.0f) |]
              capsuleSvg 1.4f [| (230.0f, 109.0f); (236.0f, 103.0f); (239.0f, 97.0f); (240.0f, 91.0f) |]
              capsuleSvg 1.4f [| (235.0f, 93.0f); (240.0f, 87.0f); (242.0f, 82.0f); (243.0f, 77.0f) |] |]
           3.5f,
           [| capsuleSvg 2.8f [| (208.0f, 134.0f); (211.0f, 119.0f); (212.0f, 101.0f); (209.0f, 74.0f) |]
              capsuleSvg 1.4f [| (209.0f, 117.0f); (214.0f, 111.0f); (217.0f, 105.0f); (218.0f, 99.0f) |]
              capsuleSvg 1.4f [| (210.0f, 101.0f); (214.0f, 95.0f); (216.0f, 89.0f); (217.0f, 83.0f) |]
              capsuleSvg 1.4f [| (209.0f, 85.0f); (212.0f, 79.0f); (213.0f, 73.0f); (213.0f, 67.0f) |] |] |]

    let private axoComposite =
        let sourceCommands = ResizeArray<ContourCommand array> ()
        let stateFactories = ResizeArray<int -> SlugLayerState> ()
        let add commands stateFactory =
            sourceCommands.Add commands
            stateFactories.Add stateFactory
        // Invisible full-card bounds keep this atlas-aligned with cardComposite.
        add
            (roundedRectSvg 2.0f 2.0f 358.0f 518.0f 18.0f)
            (fun index -> solidState index Color.Zero)
        add body (fun index -> solidState index (color 0.97f 0.95f 0.91f 1.0f))

        let eyeOuter = color 0.08f 0.16f 0.37f 1.0f
        let eyeIris = color 0.18f 0.35f 0.66f 1.0f
        let eyePupil = color 0.05f 0.12f 0.28f 1.0f
        for cx in [| 156.0f; 204.0f |] do
            add (ellipseSvg cx 150.0f 10.0f 10.0f) (fun index -> solidState index eyeOuter)
            add (ellipseSvg cx 150.0f 7.5f 7.5f) (fun index -> solidState index eyeIris)
            add (ellipseSvg cx 150.0f 4.0f 4.0f) (fun index -> solidState index eyePupil)
        add (ellipseSvg 153.0f 147.0f 2.2f 2.2f) (fun index -> solidState index (color 1.0f 1.0f 1.0f 0.90f))
        add (ellipseSvg 158.0f 153.0f 1.0f 1.0f) (fun index -> solidState index (color 1.0f 1.0f 1.0f 0.40f))
        add (ellipseSvg 201.0f 147.0f 2.2f 2.2f) (fun index -> solidState index (color 1.0f 1.0f 1.0f 0.90f))
        add (ellipseSvg 206.0f 153.0f 1.0f 1.0f) (fun index -> solidState index (color 1.0f 1.0f 1.0f 0.40f))
        add (ellipseSvg 175.0f 160.0f 1.6f 1.6f) (fun index -> solidState index face)
        add (ellipseSvg 185.0f 160.0f 1.6f 1.6f) (fun index -> solidState index face)
        add
            (capsuleSvg 1.8f [| (168.0f, 168.0f); (174.0f, 174.0f); (186.0f, 174.0f); (192.0f, 168.0f) |])
            (fun index -> solidState index face)

        for groupIndex in 0 .. gillGroups.Length - 1 do
            let phase, group = gillGroups.[groupIndex]
            for branchIndex in 0 .. group.Length - 1 do
                let branchColor = if branchIndex = 0 then gillMain else gillBranch
                add
                    group.[branchIndex]
                    (fun index ->
                        { SlugLayerState.defaultState index with
                            Color = branchColor
                            EffectId = 1
                            EffectParameters = v4 3.0f 0.08f 0.0f 0.0f
                            EffectParameters2 = v4 phase 1.2f 0.0f 0.0f })

        add
            (capsuleSvg 9.0f [| (122.0f, 174.0f); (106.0f, 178.0f); (90.0f, 182.0f); (76.0f, 192.0f); (68.0f, 198.0f); (64.0f, 206.0f); (66.0f, 212.0f) |])
            (fun index -> solidState index limb)
        add (capsuleSvg 4.0f [| (66.0f, 212.0f); (60.0f, 219.0f); (55.0f, 224.0f); (51.0f, 227.0f) |]) (fun index -> solidState index limb)
        add (capsuleSvg 4.0f [| (66.0f, 212.0f); (64.0f, 220.0f); (63.0f, 226.0f); (62.0f, 231.0f) |]) (fun index -> solidState index limb)
        add (capsuleSvg 4.0f [| (66.0f, 212.0f); (70.0f, 219.0f); (72.0f, 225.0f); (73.0f, 230.0f) |]) (fun index -> solidState index limb)
        add (capsuleSvg 9.0f [| (238.0f, 174.0f); (254.0f, 178.0f); (270.0f, 182.0f); (284.0f, 192.0f); (292.0f, 198.0f); (296.0f, 206.0f); (294.0f, 212.0f) |]) (fun index -> solidState index limb)
        add (capsuleSvg 4.0f [| (294.0f, 212.0f); (300.0f, 219.0f); (305.0f, 224.0f); (309.0f, 227.0f) |]) (fun index -> solidState index limb)
        add (capsuleSvg 4.0f [| (294.0f, 212.0f); (296.0f, 220.0f); (297.0f, 226.0f); (298.0f, 231.0f) |]) (fun index -> solidState index limb)
        add (capsuleSvg 4.0f [| (294.0f, 212.0f); (290.0f, 219.0f); (288.0f, 225.0f); (287.0f, 230.0f) |]) (fun index -> solidState index limb)
        add (capsuleSvg 7.0f [| (138.0f, 203.0f); (125.0f, 213.0f); (114.0f, 222.0f); (108.0f, 233.0f) |]) (fun index -> solidState index limb)
        add (capsuleSvg 7.0f [| (222.0f, 203.0f); (235.0f, 213.0f); (246.0f, 222.0f); (252.0f, 233.0f) |]) (fun index -> solidState index limb)
        let sourceCommands = sourceCommands.ToArray ()
        let stateFactories = stateFactories.ToArray ()
        let sourcesAndFactories = Array.zip sourceCommands stateFactories
        makeComposite sourcesAndFactories [||] [||]

    // Keep scene animation on SlugDemo's stable showcase clock.  The gallery world
    // deliberately does not advance GameTime, while SlugShape's vertex effects do
    // need a monotonically changing seconds value.
    let private placeComposite
        (name : string)
        (composite : SlugCompositeShape)
        (position : Vector3)
        (size : Vector3)
        (rotation : Quaternion)
        (elevation : single)
        (world : World) =
        let mutable minPoint = Vector2 (Single.PositiveInfinity, Single.PositiveInfinity)
        let mutable maxPoint = Vector2 (Single.NegativeInfinity, Single.NegativeInfinity)
        for metadata in composite.Data.Metadata do
            minPoint <- v2 (min minPoint.X metadata.Bounds.Min.X) (min minPoint.Y metadata.Bounds.Min.Y)
            maxPoint <- v2 (max maxPoint.X metadata.Bounds.Max.X) (max maxPoint.Y metadata.Bounds.Max.Y)
        let extent = v2 (max 1.0e-6f (maxPoint.X - minPoint.X)) (max 1.0e-6f (maxPoint.Y - minPoint.Y))
        let scale = v3 (size.X / extent.X) (size.Y / extent.Y) 1.0f
        let center = (minPoint + maxPoint) * 0.5f
        let rotatedCenter = Vector3.Transform (v3 (center.X * scale.X) (center.Y * scale.Y) 0.0f, rotation)
        let mutable transform =
            Transform.makeIntuitive false (position - rotatedCenter) scale Vector3.Zero size Vector3.Zero elevation
        transform.Rotation <- rotation
        World.renderSlugShape
            { Transform = transform
              ClipOpt = ValueNone
              Composite = composite
              Projective = Matrix4x4.Identity
              Seconds = SlugDemo.clockSeconds world
              Delta = world.GameDelta.SecondsF
              Frame = uint32 world.UpdateTime
              Seed = uint32 (abs (hash name))
              ComputeConfigOpt = None
              TextureSlots = [||] }
            world


    let draw (world : World) =
        // Navigation is supplied by SlugShowcaseView; this scene is the canonical
        // content only, rather than another explanatory ribbon or telemetry card.
        let cardPosition = v3 0.0f -12.0f 0.0f
        let cardSize = v3 180.0f (180.0f * 516.0f / 356.0f) 0.0f
        placeComposite
            "MixedScenesCard"
            cardComposite
            cardPosition
            cardSize
            Quaternion.Identity
            0.0f
            world
        placeComposite
            "MixedScenesAxolotl"
            axoComposite
            cardPosition
            cardSize
            Quaternion.Identity
            1.0f
            world

        // Keep the canonical in-card "AXO" label; gallery navigation identifies
        // the active scene without adding another explanatory banner.
        SlugDemo.slug
            "MixedScenesAxoTitle"
            SlugDemo.font
            "AXO"
            (v3 0.0f -35.0f 0.0f)
            (v3 110.0f 24.0f 0.0f)
            31.0f
            (color 1.0f 1.0f 1.0f 1.0f)
            2.0f
            world
        SlugDemo.slug
            "MixedScenesFlavorOne"
            SlugDemo.font
            "We hacked off every limb."
            (v3 0.0f -70.0f 0.0f)
            (v3 160.0f 16.0f 0.0f)
            8.0f
            (color 0.92f 0.88f 0.78f 1.0f)
            2.0f
            world
        SlugDemo.slug
            "MixedScenesFlavorTwo"
            SlugDemo.font
            "We ran out of swords first."
            (v3 0.0f -88.0f 0.0f)
            (v3 160.0f 16.0f 0.0f)
            8.0f
            (color 0.92f 0.88f 0.78f 1.0f)
            2.0f
            world
