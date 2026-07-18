namespace SlugDemo
open System
open System.Numerics
open Prime
open Nu
open SlugDemoHudComputeSupport

[<RequireQualifiedAccess>]
module SlugDemoHud =

    // The source HUD is authored on a normalized canvas. Keep the same geometry in
    // private contours, then lay the three instruments out in the visible 2 x 2
    // gallery area (the fourth quadrant is intentionally empty).
    let private rectCommands left bottom right top =
        [| MoveTo (v2 left top)
           LineTo (v2 right top)
           LineTo (v2 right bottom)
           LineTo (v2 left bottom)
           CloseContour |]

    let private polygonCommands (points : Vector2 array) =
        Array.append
            [| MoveTo points.[0] |]
            (Array.append
                (points |> Array.skip 1 |> Array.map LineTo)
                [| CloseContour |])

    let private arcStripCommands outer inner startAngle endAngle segmentCount =
        let outerPoints =
            [| for index in 0 .. segmentCount do
                   let t = single index / single segmentCount
                   let angle = startAngle + (endAngle - startAngle) * t
                   yield v2 (MathF.Cos angle * outer) (MathF.Sin angle * outer) |]
        let innerPoints =
            [| for index in 0 .. segmentCount do
                   let reverseIndex = segmentCount - index
                   let t = single reverseIndex / single segmentCount
                   let angle = startAngle + (endAngle - startAngle) * t
                   yield v2 (MathF.Cos angle * inner) (MathF.Sin angle * inner) |]
        polygonCommands (Array.append outerPoints innerPoints)

    let private scaledCircle factor =
        SlugDemoContours.circleCommands
        |> Array.map (function
            | MoveTo point -> MoveTo (v2 (point.X * factor) (point.Y * factor))
            | LineTo point -> LineTo (v2 (point.X * factor) (point.Y * factor))
            | QuadraticCurveTo (control, point) ->
                QuadraticCurveTo
                    (v2 (control.X * factor) (control.Y * factor),
                     v2 (point.X * factor) (point.Y * factor))
            | CubicCurveTo (control1, control2, point) ->
                CubicCurveTo
                    (v2 (control1.X * factor) (control1.Y * factor),
                     v2 (control2.X * factor) (control2.Y * factor),
                     v2 (point.X * factor) (point.Y * factor))
            | CloseContour -> CloseContour)

    let private annulusCommands outer inner =
        Array.append (scaledCircle outer) (scaledCircle inner)

    let private solid commands fillColor winding =
        SlugDemoContours.makeFilled commands fillColor winding Color.Zero 0.0f (v2 1.0f 1.0f)

    let private clockFace =
        SlugDemoContours.makeFilled
            SlugDemoContours.circleCommands
            (color 0.95f 0.92f 0.82f 1.0f)
            NonZero
            (color 0.12f 0.12f 0.18f 1.0f)
            1.0f
            (v2 124.0f 124.0f)

    let private clockTick =
        solid
            (rectCommands -0.5f -0.5f 0.5f 0.5f)
            (color 0.12f 0.12f 0.18f 1.0f)
            NonZero

    let private clockHand =
        solid
            (rectCommands -0.5f 0.0f 0.5f 1.0f)
            (color 0.10f 0.10f 0.16f 1.0f)
            NonZero

    let private clockSecondHand =
        solid
            (rectCommands -0.5f 0.0f 0.5f 1.0f)
            (color 0.82f 0.12f 0.10f 1.0f)
            NonZero

    let private clockHub =
        solid
            SlugDemoContours.circleCommands
            (color 0.10f 0.10f 0.16f 1.0f)
            NonZero

    let private gaugeArcStart = -MathF.PI / 6.0f
    let private gaugeArcEnd = MathF.PI * 7.0f / 6.0f
    let private gaugeSegmentCount = 20
    let private gaugeGap = 0.018f

    let private gaugeSegmentCommands index =
        let step = (gaugeArcEnd - gaugeArcStart) / single gaugeSegmentCount
        let startAngle = gaugeArcStart + single index * step + gaugeGap
        let endAngle = gaugeArcStart + single (index + 1) * step - gaugeGap
        arcStripCommands 0.48f 0.35f startAngle endAngle 5

    let private gaugeComposite =
        let sources =
            Array.init gaugeSegmentCount (fun index ->
                SlugShapeRuntime.fromContourCommands
                    (gaugeSegmentCommands index)
                    SlugFillNonzero
                    1.0e-3f)
        let data = SlugShapeRuntime.pack sources
        let layers =
            sources
            |> Array.mapi (fun index _ ->
                { SlugLayerState.defaultState index with
                    Color =
                        if index >= gaugeSegmentCount - 5 then color 0.95f 0.42f 0.06f 0.98f
                        else color 0.30f 0.27f 0.48f 0.50f })
        SlugShapeRuntime.createComposite data layers

    let private gaugeOutline =
        solid
            (arcStripCommands 0.50f 0.485f (gaugeArcStart - 0.015f) (gaugeArcEnd + 0.015f) 64)
            (color 0.80f 0.82f 0.94f 0.76f)
            NonZero

    let private radarTrail =
        let source =
            SlugShapeRuntime.fromContourCommands
                (annulusCommands 0.50f 0.485f)
                SlugFillEvenOdd
                1.0e-3f
        let stops =
            [| { Offset = 0.00f; Color = color 0.60f 0.85f 1.00f 0.75f }
               { Offset = 0.25f; Color = color 0.60f 0.85f 1.00f 0.35f }
               { Offset = 0.55f; Color = color 0.60f 0.85f 1.00f 0.05f }
               { Offset = 0.85f; Color = color 0.60f 0.85f 1.00f 0.03f }
               { Offset = 1.00f; Color = color 0.60f 0.85f 1.00f 0.75f } |]
        let gradient =
            SlugGradient
                (SlugGradientKind.Sweep (Vector2.Zero, -MathF.PI * 0.5f), stops)
        let data =
            SlugShapeRuntime.packWithResources
                [| source |]
                [| gradient |]
                stops
                [||]
                [||]
        let state =
            { SlugLayerState.defaultState 0 with
                Color = Color.One
                FillSource = SlugFillSource.Gradient 0 }
        AnalyticSlug (SlugShapeRuntime.createComposite data [| state |], None)

    let private radarCrosshair =
        solid
            (rectCommands -0.5f -0.006f 0.5f 0.006f)
            (color 0.60f 0.85f 1.00f 0.12f)
            NonZero

    let private radarTick =
        solid
            (rectCommands -0.5f -0.5f 0.5f 0.5f)
            (color 0.60f 0.85f 1.00f 0.25f)
            NonZero

    let private radarMajorTick =
        solid
            (rectCommands -0.5f -0.5f 0.5f 0.5f)
            (color 0.60f 0.85f 1.00f 0.55f)
            NonZero

    let private radarBrackets =
        let topLeft =
            [| rectCommands -0.50f 0.488f -0.43f 0.50f
               rectCommands -0.50f 0.43f -0.488f 0.50f |]
            |> Array.collect id
        let topRight =
            [| rectCommands 0.43f 0.488f 0.50f 0.50f
               rectCommands 0.488f 0.43f 0.50f 0.50f |]
            |> Array.collect id
        let bottomRight =
            [| rectCommands 0.43f -0.50f 0.50f -0.488f
               rectCommands 0.488f -0.50f 0.50f -0.43f |]
            |> Array.collect id
        let bottomLeft =
            [| rectCommands -0.50f -0.50f -0.43f -0.488f
               rectCommands -0.50f -0.50f -0.488f -0.43f |]
            |> Array.collect id
        [| topLeft; topRight; bottomRight; bottomLeft |]
        |> Array.collect id
        |> fun commands -> solid commands (color 0.60f 0.85f 1.00f 0.30f) NonZero

    let private radarCenterRing =
        solid
            (annulusCommands 0.060f 0.045f)
            (color 0.60f 0.85f 1.00f 0.30f)
            EvenOdd

    let private radarSweepStart = 0.07f
    let private radarSweepEnd = 0.475f

    let private radarSweep =
        solid
            (rectCommands radarSweepStart -0.006f radarSweepEnd 0.006f)
            (color 0.60f 0.85f 1.00f 0.60f)
            NonZero

    let private radarDot =
        solid
            SlugDemoContours.circleCommands
            (color 0.85f 0.95f 1.00f 0.95f)
            NonZero

    let private ringDefinitions =
        [| 0.42f, 0.010f, 0.48f
           0.26f, 0.014f, -1.20f
           0.10f, 0.020f, 2.00f |]

    let private radarRings =
        ringDefinitions
        |> Array.map (fun (radius, width, speed) ->
            let inner = 0.5f - width / (2.0f * radius)
            let gap = MathF.PI / 12.0f
            let shape =
                solid
                    (arcStripCommands 0.5f inner gap (MathF.PI * 2.0f - gap) 64)
                    (color 0.60f 0.85f 1.00f 1.00f)
                    NonZero
            radius, speed, shape)

    let private clockCenter = v3 -154.0f 57.0f 0.0f
    let private gaugeCenter = v3 154.0f 57.0f 0.0f
    let private radarCenter = v3 -154.0f -86.0f 0.0f
    let private radarScale = 0.64f
    let private radarSweepDiameter = 174.0f * radarScale
    let private radarSweepMidpointRadius = (radarSweepStart + radarSweepEnd) * 0.5f * radarSweepDiameter
    let private radarSweepLength = (radarSweepEnd - radarSweepStart) * radarSweepDiameter

    let private drawClock seconds world =
        let secondAngle = seconds % 60.0f / 60.0f * MathF.PI * 2.0f
        let minuteAngle = seconds % 3600.0f / 3600.0f * MathF.PI * 2.0f
        let hourAngle = seconds % 43200.0f / 43200.0f * MathF.PI * 2.0f
        placeContour "HudClockFace" clockFace clockCenter (v3 108.0f 108.0f 0.0f) Quaternion.Identity 0.0f world
        for index in 0 .. 11 do
            let angle = single index * MathF.PI * 2.0f / 12.0f
            let position =
                v3
                    (clockCenter.X + MathF.Sin angle * 44.0f)
                    (clockCenter.Y + MathF.Cos angle * 44.0f)
                    0.0f
            placeContour
                (sprintf "HudClockTick%02d" index)
                clockTick
                position
                (v3 3.5f 11.0f 0.0f)
                (Quaternion.CreateFromAxisAngle (Vector3.UnitZ, -angle))
                1.0f
                world
        for index in 1 .. 12 do
            let angle = single (index % 12) * MathF.PI * 2.0f / 12.0f
            let position =
                v3
                    (clockCenter.X + MathF.Sin angle * 35.0f)
                    (clockCenter.Y + MathF.Cos angle * 35.0f)
                    0.0f
            placeSlug
                (sprintf "HudClockNumber%02d" index)
                (string index)
                position
                (v3 20.0f 13.0f 0.0f)
                9.0f
                (color 0.12f 0.12f 0.18f 1.0f)
                2.0f
                world
        let hourRotation = Quaternion.CreateFromAxisAngle (Vector3.UnitZ, -hourAngle)
        let minuteRotation = Quaternion.CreateFromAxisAngle (Vector3.UnitZ, -minuteAngle)
        let secondRotation = Quaternion.CreateFromAxisAngle (Vector3.UnitZ, -secondAngle)
        let hourPosition = clockCenter + Vector3.Transform (v3 0.0f 16.0f 0.0f, hourRotation)
        let minutePosition = clockCenter + Vector3.Transform (v3 0.0f 18.0f 0.0f, minuteRotation)
        let secondPosition = clockCenter + Vector3.Transform (v3 0.0f 16.0f 0.0f, secondRotation)
        placeContour
            "HudClockHourHand"
            clockHand
            hourPosition
            (v3 4.5f 32.0f 0.0f)
            hourRotation
            3.0f
            world
        placeContour
            "HudClockMinuteHand"
            clockHand
            minutePosition
            (v3 3.5f 42.0f 0.0f)
            minuteRotation
            4.0f
            world
        placeContour
            "HudClockSecondHand"
            clockSecondHand
            secondPosition
            (v3 1.8f 48.0f 0.0f)
            secondRotation
            5.0f
            world
        placeContour "HudClockHub" clockHub clockCenter (v3 8.0f 8.0f 0.0f) Quaternion.Identity 6.0f world

    let private drawGauge world =
        SlugDemoContours.placeComposite
            "HudGaugeSegments"
            gaugeComposite
            gaugeCenter
            (v3 146.0f 116.0f 0.0f)
            Quaternion.Identity
            1.0f
            None
            world
        placeContour
            "HudGaugeOutline"
            gaugeOutline
            gaugeCenter
            (v3 150.0f 120.0f 0.0f)
            Quaternion.Identity
            2.0f
            world
        placeSlug
            "HudGaugeCounter"
            "05"
            gaugeCenter
            (v3 56.0f 28.0f 0.0f)
            22.0f
            (color 0.90f 0.92f 1.00f 1.0f)
            3.0f
            world

    let private drawRadar seconds world =
        placeContour "HudRadarTrail" radarTrail radarCenter (v3 (174.0f * radarScale) (174.0f * radarScale) 0.0f) Quaternion.Identity 0.0f world
        placeContour "HudRadarCrossHorizontal" radarCrosshair radarCenter (v3 (164.0f * radarScale) (2.0f * radarScale) 0.0f) Quaternion.Identity 1.0f world
        placeContour
            "HudRadarCrossVertical"
            radarCrosshair
            radarCenter
            (v3 (164.0f * radarScale) (2.0f * radarScale) 0.0f)
            (Quaternion.CreateFromAxisAngle (Vector3.UnitZ, MathF.PI / 2.0f))
            1.0f
            world
        for index in 0 .. 23 do
            if index % 2 <> 0 then
                let angle = single index * MathF.PI / 12.0f
                let position =
                    v3
                        (radarCenter.X + MathF.Cos angle * 80.0f * radarScale)
                        (radarCenter.Y + MathF.Sin angle * 80.0f * radarScale)
                        0.0f
                placeContour
                    (sprintf "HudRadarMinor%02d" index)
                    radarTick
                    position
                    (v3 (2.0f * radarScale) (8.0f * radarScale) 0.0f)
                    (Quaternion.CreateFromAxisAngle (Vector3.UnitZ, angle - MathF.PI / 2.0f))
                    2.0f
                    world
            else
                let angle = single index * MathF.PI / 12.0f
                let position =
                    v3
                        (radarCenter.X + MathF.Cos angle * 80.0f * radarScale)
                        (radarCenter.Y + MathF.Sin angle * 80.0f * radarScale)
                        0.0f
                placeContour
                    (sprintf "HudRadarMajor%02d" index)
                    radarMajorTick
                    position
                    (v3 (3.0f * radarScale) (13.0f * radarScale) 0.0f)
                    (Quaternion.CreateFromAxisAngle (Vector3.UnitZ, angle - MathF.PI / 2.0f))
                    2.0f
                    world
        placeContour "HudRadarBrackets" radarBrackets radarCenter (v3 (210.0f * radarScale) (148.0f * radarScale) 0.0f) Quaternion.Identity 2.0f world
        placeContour "HudRadarCenterRing" radarCenterRing radarCenter (v3 (22.0f * radarScale) (22.0f * radarScale) 0.0f) Quaternion.Identity 3.0f world
        let sweepRotation = Quaternion.CreateFromAxisAngle (Vector3.UnitZ, -0.65f * seconds)
        let sweepPosition =
            radarCenter +
            Vector3.Transform (v3 radarSweepMidpointRadius 0.0f 0.0f, sweepRotation)
        placeContour
            "HudRadarSweep"
            radarSweep
            sweepPosition
            (v3 radarSweepLength (2.0f * radarScale) 0.0f)
            sweepRotation
            4.0f
            world
        for index = 0 to radarRings.Length - 1 do
            let radius, speed, shape = radarRings.[index]
            let diameter = 170.0f * radarScale * radius / 0.48f
            placeContour
                (sprintf "HudRadarRing%02d" index)
                shape
                radarCenter
                (v3 diameter diameter 0.0f)
                (Quaternion.CreateFromAxisAngle (Vector3.UnitZ, speed * seconds))
                3.0f
                world
        let pulse = 1.0f + 0.18f * MathF.Sin (seconds * 1.8f)
        placeContour
            "HudRadarDot"
            radarDot
            radarCenter
            (v3 (8.0f * radarScale * pulse) (8.0f * radarScale * pulse) 0.0f)
            Quaternion.Identity
            5.0f
            world
        for index in 0 .. 11 do
            let angle = single index * MathF.PI * 2.0f / 12.0f
            let position =
                v3
                    (radarCenter.X + MathF.Sin angle * 93.0f * radarScale)
                    (radarCenter.Y + MathF.Cos angle * 93.0f * radarScale)
                    0.0f
            let degrees = ((3 - index + 12) % 12) * 30
            placeSlug
                (sprintf "HudRadarLabel%02d" index)
                (string degrees)
                position
                (v3 (22.0f * radarScale) (13.0f * radarScale) 0.0f)
                7.5f
                (color 0.60f 0.85f 1.00f 0.50f)
                6.0f
                world

    let draw (world : World) =
        let seconds = SlugDemo.clockSeconds world
        let panelColor = color 0.033f 0.033f 0.133f 1.0f
        SlugDemo.panel
            "HudClockPanel"
            (v3 -154.0f 57.0f 0.0f)
            (v3 304.0f 132.0f 0.0f)
            panelColor
            -20.0f
            world
        SlugDemo.panel
            "HudGaugePanel"
            (v3 154.0f 57.0f 0.0f)
            (v3 304.0f 132.0f 0.0f)
            panelColor
            -20.0f
            world
        SlugDemo.panel
            "HudRadarPanel"
            (v3 -154.0f -86.0f 0.0f)
            (v3 304.0f 132.0f 0.0f)
            panelColor
            -20.0f
            world
        drawClock seconds world
        drawGauge world
        drawRadar seconds world
