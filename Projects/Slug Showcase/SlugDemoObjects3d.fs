namespace SlugShowcase
open System
open System.Numerics
open Prime
open Nu
open SlugDemoProjectionSupport

[<RequireQualifiedAccess>]
module SlugDemoObjects3d =

    let private canonicalStops =
        [| { Offset = 0.0f; Color = color 0.0f 0.8f 1.0f 1.0f }
           { Offset = 0.5f; Color = color 0.6f 0.0f 1.0f 1.0f }
           { Offset = 1.0f; Color = color 1.0f 0.0f 0.8f 1.0f } |]

    let private rectCommands x0 x1 y0 y1 =
        [| MoveTo (v2 x0 y0)
           LineTo (v2 x1 y0)
           LineTo (v2 x1 y1)
           LineTo (v2 x0 y1)
           CloseContour |]

    let private canonicalRectCommands =
        [| (0.1f, 0.3f)
           (0.4f, 0.6f)
           (0.7f, 0.9f) |]
        |> Array.collect (fun (x0, x1) -> rectCommands x0 x1 0.25f 0.75f)

    let private canonicalTriangleCommands =
        [| MoveTo (v2 0.0f 0.0f)
           QuadraticCurveTo (v2 0.5f 0.35f, v2 1.0f 0.0f)
           QuadraticCurveTo (v2 0.75f 0.35f, v2 0.5f 0.7f)
           QuadraticCurveTo (v2 0.25f 0.35f, v2 0.0f 0.0f)
           CloseContour |]

    // Keep the canonical source in one composite so every cube face shows the same three
    // disconnected gradient bars and analytic triangle rather than an untextured guide card.
    let private canonicalContentCommands =
        Array.append canonicalRectCommands canonicalTriangleCommands

    let private makeGradientComposite commands =
        let source = SlugShapeRuntime.fromContourCommands commands SlugFillNonzero 1.0e-3f
        let gradient =
            SlugGradient
                (SlugGradientKind.Linear (v2 0.1f 0.5f, v2 0.9f 0.5f), canonicalStops)
        let data =
            SlugShapeRuntime.packWithResources
                [|source|]
                [|gradient|]
                canonicalStops
                [||]
                [||]
        let state =
            { SlugLayerState.defaultState 0 with
                Color = Color.One
                FillSource = SlugFillSource.Gradient 0 }
        SlugShapeRuntime.createComposite data [|state|]


    let private canonicalComposite = makeGradientComposite canonicalContentCommands
    let private triangleComposite = makeGradientComposite canonicalTriangleCommands
    let private barComposites =
        [| rectCommands 0.1f 0.3f 0.25f 0.75f
           rectCommands 0.4f 0.6f 0.25f 0.75f
           rectCommands 0.7f 0.9f 0.25f 0.75f |]
        |> Array.map makeGradientComposite

    let private makeCubeFaceComposite bodyColor =
        let faceSource =
            SlugShapeRuntime.fromContourCommands
                (rectCommands 0.0f 1.0f 0.0f 0.75f)
                SlugFillNonzero
                1.0e-3f
        let contentSource =
            SlugShapeRuntime.fromContourCommands
                canonicalContentCommands
                SlugFillNonzero
                1.0e-3f
        let gradient =
            SlugGradient
                (SlugGradientKind.Linear (v2 0.1f 0.5f, v2 0.9f 0.5f), canonicalStops)
        let data =
            SlugShapeRuntime.packWithResources
                [|faceSource; contentSource|]
                [|gradient|]
                canonicalStops
                [||]
                [||]
        // Keep the mapped artwork inset from every face edge. The uninterrupted body-color
        // border makes each projected quad read as one solid face instead of detached strips.
        let detailCenter = v3 0.5f 0.375f 0.0f
        let detailTransform =
            Matrix4x4.CreateTranslation (-detailCenter) *
            Matrix4x4.CreateScale (0.625f, 0.625f, 1.0f) *
            Matrix4x4.CreateTranslation detailCenter
        let bodyState =
            { SlugLayerState.defaultState 0 with
                Color = bodyColor }
        let contentState =
            { SlugLayerState.defaultState 1 with
                Transform = detailTransform
                Color = Color.One
                FillSource = SlugFillSource.Gradient 0 }
        SlugShapeRuntime.createComposite data [|bodyState; contentState|]

    let private cubeFrontComposite =
        makeCubeFaceComposite (color 0.10f 0.18f 0.30f 1.0f)

    let private cubeRightComposite =
        makeCubeFaceComposite (color 0.07f 0.12f 0.22f 1.0f)

    let private cubeTopComposite =
        makeCubeFaceComposite (color 0.22f 0.34f 0.52f 1.0f)

    // A colored analytic contour is the body of each object, with canonical bars drawn above it as mapped detail.
    let private sphereComposite = makeGradientComposite SlugShowcaseContours.circleCommands

    let private halfCylinderCommands =
        [| MoveTo (v2 -0.5f -0.5f)
           QuadraticCurveTo (v2 -0.68f 0.0f, v2 -0.5f 0.5f)
           LineTo (v2 0.5f 0.5f)
           QuadraticCurveTo (v2 0.68f 0.0f, v2 0.5f -0.5f)
           LineTo (v2 -0.5f -0.5f)
           CloseContour |]

    let private halfCylinderComposite = makeGradientComposite halfCylinderCommands

    let private waveCommands =
        let sampleTop u = 0.20f + MathF.Sin (u * 5.0f) * 0.10f
        let sampleBottom u = -0.20f + MathF.Sin (u * 5.0f) * 0.10f
        let topPoints = [| for i in 0 .. 16 -> v2 (single i / 16.0f) (sampleTop (single i / 16.0f)) |]
        let bottomPoints = [| for i in 16 .. -1 .. 0 -> v2 (single i / 16.0f) (sampleBottom (single i / 16.0f)) |]
        Array.concat
            [| [| MoveTo topPoints.[0] |]
               topPoints |> Array.skip 1 |> Array.map LineTo
               bottomPoints |> Array.map LineTo
               [| CloseContour |] |]

    let private waveComposite = makeGradientComposite waveCommands

    let private makeProjective pitch roll perspective =
        let pitchSin = MathF.Sin pitch
        let pitchCos = MathF.Cos pitch
        let rollSin = MathF.Sin roll
        let rollCos = MathF.Cos roll
        let mutable raw = Matrix4x4.Identity
        raw.M11 <- rollCos
        raw.M12 <- rollSin
        raw.M21 <- -pitchCos * rollSin
        raw.M22 <- pitchCos * rollCos
        raw.M14 <- 0.0f
        raw.M24 <- perspective * pitchSin
        raw

    let private placeSurface name composite position size rotation elevation projective world =
        SlugShowcaseContours.placeProjectiveComposite
            name
            composite
            position
            size
            rotation
            elevation
            projective
            world

    let private drawSphere center phase world =
        let radius = 55.0f
        let sphereProjective = makeProjective 0.08f 0.0f 0.0008f
        placeSurface
            "Objects3dSphereBody"
            sphereComposite
            center
            (v3 (radius * 2.0f) (radius * 2.0f) 0.0f)
            Quaternion.Identity
            0.0f
            sphereProjective
            world

        // Two curved patches per source bar make the gradient-bearing bars follow the sphere's
        // longitude. The analytic circular body remains visible between the disconnected bars.
        let ranges = [| (0.1f, 0.3f); (0.4f, 0.6f); (0.7f, 0.9f) |]
        let slices = 2
        for barIndex in 0 .. dec ranges.Length do
            let x0, x1 = ranges.[barIndex]
            for slice in 0 .. dec slices do
                let t0 = single slice / single slices
                let t1 = single (slice + 1) / single slices
                let a0 = MathF.Asin (2.0f * (x0 + (x1 - x0) * t0) - 1.0f)
                let a1 = MathF.Asin (2.0f * (x0 + (x1 - x0) * t1) - 1.0f)
                let angle = (a0 + a1) * 0.5f
                let width = max 12.0f ((MathF.Sin a1 - MathF.Sin a0) * radius + 4.0f)
                let height = max 24.0f (MathF.Cos angle * radius * 1.65f)
                let yaw = angle * 0.12f + MathF.Sin (phase * 0.18f) * 0.025f
                placeSurface
                    (sprintf "Objects3dSphereBar%d_%d" barIndex slice)
                    barComposites.[barIndex]
                    (center + v3 (MathF.Sin angle * radius) 0.0f 0.08f)
                    (v3 width height 0.0f)
                    (zRotation yaw)
                    (1.1f + single barIndex * 0.1f)
                    (makeProjective (0.08f + abs angle * 0.10f) (angle * 0.04f) 0.0008f)
                    world

        placeSurface
            "Objects3dSphereTriangle"
            triangleComposite
            (center + v3 0.0f 0.0f 0.18f)
            (v3 46.0f 32.0f 0.0f)
            (zRotation (MathF.Sin phase * 0.05f))
            2.0f
            sphereProjective
            world

    let private makeQuadProjective
        (size : Vector3)
        (bottomLeft : Vector2)
        (bottomRight : Vector2)
        (topRight : Vector2)
        (topLeft : Vector2) =
        // Solve the unit-square homography, then compose it with the centered
        // source rectangle emitted by placeProjectiveComposite.
        let deltaX1 = bottomRight.X - topRight.X
        let deltaX2 = topLeft.X - topRight.X
        let deltaX3 = bottomLeft.X - bottomRight.X + topRight.X - topLeft.X
        let deltaY1 = bottomRight.Y - topRight.Y
        let deltaY2 = topLeft.Y - topRight.Y
        let deltaY3 = bottomLeft.Y - bottomRight.Y + topRight.Y - topLeft.Y
        let denominator = deltaX1 * deltaY2 - deltaX2 * deltaY1
        let projectiveU, projectiveV =
            if abs denominator < 1.0e-6f then 0.0f, 0.0f
            else
                ((deltaX3 * deltaY2 - deltaX2 * deltaY3) / denominator,
                 (deltaX1 * deltaY3 - deltaX3 * deltaY1) / denominator)
        let axisX =
            bottomRight.X - bottomLeft.X + projectiveU * bottomRight.X
        let axisY =
            topLeft.X - bottomLeft.X + projectiveV * topLeft.X
        let verticalAxisX =
            bottomRight.Y - bottomLeft.Y + projectiveU * bottomRight.Y
        let verticalAxisY =
            topLeft.Y - bottomLeft.Y + projectiveV * topLeft.Y
        let normalization = 1.0f + projectiveU * 0.5f + projectiveV * 0.5f
        let mutable result = Matrix4x4.Identity
        result.M11 <- axisX / size.X / normalization
        result.M21 <- axisY / size.Y / normalization
        result.M41 <-
            (axisX * 0.5f + axisY * 0.5f + bottomLeft.X) / normalization
        result.M12 <- verticalAxisX / size.X / normalization
        result.M22 <- verticalAxisY / size.Y / normalization
        result.M42 <-
            (verticalAxisX * 0.5f + verticalAxisY * 0.5f + bottomLeft.Y) / normalization
        result.M14 <- projectiveU / size.X / normalization
        result.M24 <- projectiveV / size.Y / normalization
        result

    let private drawCube center _phase world =
        // These three quads share their projected edge coordinates exactly. Each face is
        // an opaque analytic rectangle with the canonical bars and triangle layered above it.
        let frontSize = v3 100.0f 80.0f 0.0f
        let rightSize = v3 60.0f 80.0f 0.0f
        let topSize = v3 100.0f 50.0f 0.0f
        let frontBottomLeft = v2 -53.0f -44.0f
        let frontBottomRight = v2 44.0f -41.0f
        let frontTopRight = v2 43.0f 35.0f
        let frontTopLeft = v2 -51.0f 35.0f
        let farBottomRight = v2 78.0f -17.0f
        let farTopRight = v2 76.0f 55.0f
        let farTopLeft = v2 -16.0f 54.0f
        let frontProjective =
            makeQuadProjective
                frontSize
                frontBottomLeft
                frontBottomRight
                frontTopRight
                frontTopLeft
        let rightProjective =
            makeQuadProjective
                rightSize
                frontBottomRight
                farBottomRight
                farTopRight
                frontTopRight
        let topProjective =
            makeQuadProjective
                topSize
                frontTopLeft
                frontTopRight
                farTopRight
                farTopLeft

        // Paint the receding faces first and the broad front face last. Exact shared
        // corners plus explicit elevation order prevent gaps and detached edge slats.
        placeSurface
            "Objects3dCubeTop"
            cubeTopComposite
            center
            topSize
            Quaternion.Identity
            2.1f
            topProjective
            world
        placeSurface
            "Objects3dCubeRight"
            cubeRightComposite
            center
            rightSize
            Quaternion.Identity
            2.2f
            rightProjective
            world
        placeSurface
            "Objects3dCubeFront"
            cubeFrontComposite
            center
            frontSize
            Quaternion.Identity
            2.3f
            frontProjective
            world

    let private drawHalfCylinder center phase world =
        let radius = 53.0f
        let halfHeight = 43.0f
        let arc = MathF.PI * 0.75f
        let start = -arc * 0.5f
        let baseProjective = makeProjective 0.14f 0.0f 0.0007f
        placeSurface
            "Objects3dHalfCylinderBody"
            halfCylinderComposite
            center
            (v3 (radius * 2.0f) (halfHeight * 2.0f) 0.0f)
            Quaternion.Identity
            0.0f
            baseProjective
            world

        // The fan of three bars follows the curved half-cylinder rather than leaving only a silhouette.
        let ranges = [| (0.1f, 0.3f); (0.4f, 0.6f); (0.7f, 0.9f) |]
        let slices = 2
        for barIndex in 0 .. dec ranges.Length do
            let x0, x1 = ranges.[barIndex]
            for slice in 0 .. dec slices do
                let t0 = single slice / single slices
                let t1 = single (slice + 1) / single slices
                let angle0 = start + arc * (x0 + (x1 - x0) * t0)
                let angle1 = start + arc * (x0 + (x1 - x0) * t1)
                let angle = (angle0 + angle1) * 0.5f
                let width = max 13.0f ((MathF.Sin angle1 - MathF.Sin angle0) * radius + 5.0f)
                let yaw = angle * 0.35f + MathF.Sin (phase * 0.22f) * 0.025f
                placeSurface
                    (sprintf "Objects3dHalfCylinderBar%d_%d" barIndex slice)
                    barComposites.[barIndex]
                    (center + v3 (MathF.Sin angle * radius) 0.0f 0.08f)
                    (v3 width (halfHeight * 1.85f) 0.0f)
                    (zRotation yaw)
                    (1.0f + single barIndex * 0.1f)
                    (makeProjective (0.16f + abs angle * 0.15f) (angle * 0.10f) 0.0007f)
                    world

        placeSurface
            "Objects3dHalfCylinderTriangle"
            triangleComposite
            (center + v3 0.0f 0.0f 0.16f)
            (v3 44.0f 32.0f 0.0f)
            (zRotation (MathF.Sin phase * 0.04f))
            1.9f
            baseProjective
            world

    let private drawWavedSurface center phase world =
        let width = 252.0f
        let halfHeight = 40.0f
        let baseProjective = makeProjective 0.26f -0.13f 0.0009f
        placeSurface
            "Objects3dWaveBody"
            waveComposite
            (center + v3 0.0f 0.0f -0.02f)
            (v3 width (halfHeight * 2.0f) 0.0f)
            Quaternion.Identity
            0.0f
            baseProjective
            world

        // Repeated mapped bars and a triangle sit on the sampled sinusoidal surface. The broad
        // analytic ribbon underneath keeps the wave unmistakable while preserving UV ordering.
        let ranges = [| (0.1f, 0.3f); (0.4f, 0.6f); (0.7f, 0.9f) |]
        for barIndex in 0 .. dec ranges.Length do
            let x0, x1 = ranges.[barIndex]
            let u = (x0 + x1) * 0.5f
            let wave = MathF.Sin (u * 5.0f + phase * 0.12f) * 13.0f
            let slope = MathF.Cos (u * 5.0f + phase * 0.12f) * 0.32f
            placeSurface
                (sprintf "Objects3dWaveBar%d" barIndex)
                barComposites.[barIndex]
                (center + v3 ((u - 0.5f) * width) wave 0.08f)
                (v3 ((x1 - x0) * width + 6.0f) (halfHeight * 1.45f) 0.0f)
                (zRotation (-slope * 0.30f))
                (1.1f + single barIndex * 0.1f)
                (makeProjective (0.22f + abs slope * 0.20f) (-0.12f + slope * 0.08f) 0.0009f)
                world

        placeSurface
            "Objects3dWaveTriangle"
            triangleComposite
            (center + v3 0.0f (MathF.Sin (2.5f + phase * 0.12f) * 13.0f) 0.16f)
            (v3 56.0f 34.0f 0.0f)
            (zRotation (MathF.Cos (phase * 0.14f) * 0.05f))
            1.9f
            baseProjective
            world

    let draw (world : World) =
        let phase = SlugDemo.clockSeconds world * 0.62f
        drawSphere (v3 -166.0f 36.0f 0.0f) phase world
        drawCube (v3 82.0f 34.0f 0.0f) phase world
        drawHalfCylinder (v3 -166.0f -98.0f 0.0f) phase world
        drawWavedSurface (v3 136.0f -98.0f 0.0f) phase world
