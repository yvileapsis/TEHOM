namespace SlugDemo

open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoGradients =

    let private stop offset red green blue alpha =
        { Offset = offset
          Color = color red green blue alpha }

    let private rectCommands left bottom width height =
        let right = left + width
        let top = bottom + height
        [| MoveTo (v2 left top)
           LineTo (v2 right top)
           LineTo (v2 right bottom)
           LineTo (v2 left bottom)
           CloseContour |]

    let private roundedRectCommands left bottom width height radius =
        let right = left + width
        let top = bottom + height
        let k = radius * 0.5522847498f
        [| MoveTo (v2 (left + radius) top)
           CubicCurveTo (v2 (left + radius - k) top, v2 left (top - radius + k), v2 left (top - radius))
           LineTo (v2 left (bottom + radius))
           CubicCurveTo (v2 left (bottom + radius - k), v2 (left + radius - k) bottom, v2 (left + radius) bottom)
           LineTo (v2 (right - radius) bottom)
           CubicCurveTo (v2 (right - radius + k) bottom, v2 right (bottom + radius - k), v2 right (bottom + radius))
           LineTo (v2 right (top - radius))
           CubicCurveTo (v2 right (top - radius + k), v2 (right - radius + k) top, v2 (right - radius) top)
           CloseContour |]

    let private circleCommands centerX centerY radius =
        let k = radius * 0.5522847498f
        [| MoveTo (v2 (centerX + radius) centerY)
           CubicCurveTo (v2 (centerX + radius) (centerY + k), v2 (centerX + k) (centerY + radius), v2 centerX (centerY + radius))
           CubicCurveTo (v2 (centerX - k) (centerY + radius), v2 (centerX - radius) (centerY + k), v2 (centerX - radius) centerY)
           CubicCurveTo (v2 (centerX - radius) (centerY - k), v2 (centerX - k) (centerY - radius), v2 centerX (centerY - radius))
           CubicCurveTo (v2 (centerX + k) (centerY - radius), v2 (centerX + radius) (centerY - k), v2 (centerX + radius) centerY)
           CloseContour |]

    let private linear0Commands =
        rectCommands 0.05f 0.05f 0.9f 0.9f

    let private linear1Commands =
        [| MoveTo (v2 0.5f 0.08f)
           CubicCurveTo (v2 0.5f 0.25f, v2 0.92f 0.35f, v2 0.92f 0.62f)
           CubicCurveTo (v2 0.92f 0.88f, v2 0.65f 0.96f, v2 0.5f 0.78f)
           CubicCurveTo (v2 0.35f 0.96f, v2 0.08f 0.88f, v2 0.08f 0.62f)
           CubicCurveTo (v2 0.08f 0.35f, v2 0.5f 0.25f, v2 0.5f 0.08f)
           CloseContour |]

    let private linear2Commands =
        Array.concat
            [| rectCommands 0.1f 0.25f 0.2f 0.5f
               rectCommands 0.4f 0.25f 0.2f 0.5f
               rectCommands 0.7f 0.25f 0.2f 0.5f |]

    let private radial0Commands =
        circleCommands 0.5f 0.5f 0.45f

    let private radial1Commands =
        circleCommands 0.5f 0.5f 0.45f

    let private sweep0Commands =
        circleCommands 0.5f 0.5f 0.45f

    let private sweep1Commands =
        circleCommands 0.5f 0.5f 0.45f

    // Canvas strokes are centered on their source path, so the 0.02 stroke expands by 0.01 on each side.
    let private strokeOuterCommands =
        roundedRectCommands 0.09f 0.09f 0.92f 0.92f 0.11f

    let private strokeCommands =
        Array.append strokeOuterCommands (roundedRectCommands 0.11f 0.11f 0.88f 0.88f 0.09f)

    let private makeGradientShape fillRule commands kind stops =
        let source =
            SlugShapeRuntime.fromContourCommands commands fillRule 1.0e-3f
        let gradient =
            SlugGradient (kind, stops, Matrix4x4.Identity, SlugGradientPad)
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
        SlugShapeRuntime.createComposite data [| state |]

    // The source canvas examples use pad extension (the default), including the
    // transparent final stop in the gauge range.
    let private strokeShape =
        makeGradientShape
            SlugFillEvenOdd
            strokeCommands
            (SlugGradientKind.Linear (v2 0.1f 0.1f, v2 1.0f 1.0f))
            [| stop 0.0f 0.0f 0.8f 1.0f 1.0f
               stop 1.0f 1.0f 0.0f 0.6f 0.01f |]

    let private linear0Shape =
        makeGradientShape
            SlugFillNonzero
            linear0Commands
            (SlugGradientKind.Linear (v2 0.0f 0.0f, v2 0.5f 0.5f))
            [| stop 0.0f 1.0f 0.5f 0.0f 1.0f
               stop 0.5f 1.0f 1.0f 0.0f 1.0f
               stop 1.0f 1.0f 1.0f 1.0f 1.0f |]

    let private linear1Shape =
        makeGradientShape
            SlugFillNonzero
            linear1Commands
            (SlugGradientKind.Linear (v2 0.5f 0.1f, v2 0.5f 0.95f))
            [| stop 0.0f 0.2f 0.5f 0.1f 1.0f
               stop 0.2f 0.4f 0.9f 0.0f 1.0f
               stop 0.4f 0.6f 0.1f 0.2f 1.0f
               stop 0.9f 0.8f 0.7f 0.3f 1.0f
               stop 1.0f 1.0f 0.2f 0.7f 1.0f |]

    let private linear2Shape =
        makeGradientShape
            SlugFillNonzero
            linear2Commands
            (SlugGradientKind.Linear (v2 0.1f 0.5f, v2 0.9f 0.5f))
            [| stop 0.0f 0.0f 0.8f 1.0f 1.0f
               stop 0.5f 0.6f 0.0f 1.0f 1.0f
               stop 1.0f 1.0f 0.0f 0.8f 1.0f |]

    let private radial0Shape =
        makeGradientShape
            SlugFillNonzero
            radial0Commands
            (SlugGradientKind.Radial (v2 0.5f 0.5f, v2 0.5f 0.5f))
            [| stop 0.0f 0.2f 0.4f 0.6f 1.0f
               stop 0.75f 1.0f 1.0f 1.0f 1.0f
               stop 1.0f 0.2f 0.4f 0.6f 1.0f |]

    let private radial1Shape =
        makeGradientShape
            SlugFillNonzero
            radial1Commands
            (SlugGradientKind.FocalRadial (v2 0.3f 0.3f, 0.2f, v2 0.3f 0.3f, 0.65f))
            [| stop 0.0f 0.0f 1.0f 0.0f 1.0f
               stop 1.0f 0.1f 0.25f 0.5f 1.0f |]

    let private sweep0Shape =
        makeGradientShape
            SlugFillNonzero
            sweep0Commands
            (SlugGradientKind.SweepRange (v2 0.5f 0.5f, -MathF.PI, MathF.PI))
            [| stop 0.0f 1.0f 0.0f 0.0f 1.0f
               stop 0.167f 1.0f 1.0f 0.0f 1.0f
               stop 0.333f 0.0f 1.0f 0.0f 1.0f
               stop 0.5f 0.0f 1.0f 1.0f 1.0f
               stop 0.667f 0.0f 0.0f 1.0f 1.0f
               stop 0.833f 1.0f 0.0f 1.0f 1.0f
               stop 1.0f 1.0f 0.0f 0.0f 1.0f |]

    let private sweep1Shape =
        makeGradientShape
            SlugFillNonzero
            sweep1Commands
            (SlugGradientKind.SweepRange (v2 0.5f 0.5f, -MathF.PI * 0.75f, MathF.PI * 0.75f))
            [| stop 0.0f 1.0f 1.0f 1.0f 1.0f
               stop 1.0f 1.0f 1.0f 1.0f 0.0f |]

    let private setGradientTransform (shape : SlugCompositeShape) transform =
        shape.SetLayerGradientTransform (0, transform)

    let private place name shape x y width height world =
        SlugDemoContours.placeComposite
            name
            shape
            (v3 x y 0.0f)
            (v3 width height 0.0f)
            Quaternion.Identity
            0.0f
            None
            world

    let draw (world : World) =
        let phase = single (SlugDemo.clockSeconds world) * 0.55f
        let gradientRotation =
            Matrix4x4.CreateTranslation (v3 -0.5f -0.5f 0.0f)
            * Matrix4x4.CreateRotationZ phase
            * Matrix4x4.CreateTranslation (v3 0.5f 0.5f 0.0f)
        setGradientTransform sweep0Shape gradientRotation
        place "GradientStroke" strokeShape -225.0f 37.0f 112.0f 82.0f world
        place "GradientLinear0" linear0Shape -75.0f 37.0f 82.0f 82.0f world
        place "GradientLinear1" linear1Shape 75.0f 37.0f 112.0f 82.0f world
        place "GradientLinear2" linear2Shape 225.0f 37.0f 112.0f 82.0f world
        place "GradientRadial0" radial0Shape -225.0f -67.0f 82.0f 82.0f world
        place "GradientRadial1" radial1Shape -75.0f -67.0f 82.0f 82.0f world
        place "GradientSweep0" sweep0Shape 75.0f -67.0f 82.0f 82.0f world
        place "GradientSweep1" sweep1Shape 225.0f -67.0f 82.0f 82.0f world
