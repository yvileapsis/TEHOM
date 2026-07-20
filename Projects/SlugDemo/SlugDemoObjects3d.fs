namespace SlugDemo
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoObjects3d =

    type private ObjectShape =
        | Sphere
        | Box
        | HalfCylinder
        | Subdivide

    let private selectedShape =
        match Environment.GetEnvironmentVariable "SLUG_DEMO_OBJECT_SHAPE" with
        | null
        | "" -> Sphere
        | value ->
            match value.Trim().ToLowerInvariant() with
            | "sphere" -> Sphere
            | "box" -> Box
            | "half-cylinder" -> HalfCylinder
            | "subdivide" -> Subdivide
            | _ -> invalidArg "SLUG_DEMO_OBJECT_SHAPE" ("Unknown osgSlug 3D shape '" + value + "'.")

    // Nu presents linear colors through an sRGB attachment. These values reproduce
    // osgSlug's visible (1.0, 0.5, 0.0) orange and neutral gray.
    let private orange = color 1.0f 0.214f 0.0f 1.0f
    let private gray = color 0.20f 0.205f 0.23f 1.0f
    let private unitSurfaceBounds = Box2 (v2 -1.0f -1.0f, v2 2.0f 2.0f)

    let private rectCommands x0 x1 y0 y1 =
        [| MoveTo (v2 x0 y0)
           LineTo (v2 x1 y0)
           LineTo (v2 x1 y1)
           LineTo (v2 x0 y1)
           CloseContour |]

    let private polygonCommands (points : Vector2 array) =
        Array.concat
            [| [| MoveTo points[0] |]
               points |> Array.skip 1 |> Array.map LineTo
               [| CloseContour |] |]

    let private triangleEmHeight = 0.7f

    // osgslug-simple-3d maps this same quadratic triangle over the selected mesh.
    let private mappedTriangleBoundary map =
        [| for index in 0 .. 64 do
               let t = single index / 64.0f
               yield map (v2 t (0.7f * t * (1.0f - t)))
           for index in 1 .. 32 do
               let t = single index / 32.0f
               yield map (v2 (1.0f - t * 0.5f) (t * 0.7f))
           for index in 1 .. 32 do
               let t = single index / 32.0f
               yield map (v2 (0.5f * (1.0f - t)) (0.7f * (1.0f - t))) |]
        |> polygonCommands

    let private makeContour commands bounds fillColor =
        let contour =
            SlugDemoContours.makeFilled commands fillColor NonZero Color.Zero 0.0f
        { contour with Bounds = bounds }

    let private sphereBody =
        SlugDemoContours.makeFilled
            SlugDemoContours.circleCommands
            gray
            NonZero
            Color.Zero
            0.0f

    // SubdividedDrawable normalizes the shape's tight em bounds before invoking
    // its position callback. The +Z camera sees normalized u in [0, 0.5].
    let private sphereTriangle =
        let mapSphere (emCoord : Vector2) =
            let u = emCoord.X
            let v = emCoord.Y / triangleEmHeight
            let latitude = MathF.PI * v - MathF.PI * 0.5f
            let longitude = MathF.PI * 2.0f * u
            v2
                (MathF.Cos latitude * MathF.Cos longitude)
                (MathF.Sin latitude)
        let lowerAtSilhouette = 0.7f * 0.5f * 0.5f
        let points =
            [| for index in 0 .. 64 do
                   let u = single index / 64.0f * 0.5f
                   yield mapSphere (v2 u (0.7f * u * (1.0f - u)))
               for index in 1 .. 32 do
                   let t = single index / 32.0f
                   yield mapSphere (v2 0.5f (lowerAtSilhouette + (0.7f - lowerAtSilhouette) * t))
               for index in 1 .. 64 do
                   let u = 0.5f * (1.0f - single index / 64.0f)
                   yield mapSphere (v2 u (1.4f * u)) |]
        makeContour (polygonCommands points) unitSurfaceBounds orange

    let private drawSphere world =
        let center = v3 0.0f -12.0f 0.0f
        let size = v3 230.0f 230.0f 0.0f
        SlugDemoContours.placeContour sphereBody center size Quaternion.Identity 0.0f world
        SlugDemoContours.placeContour sphereTriangle center size Quaternion.Identity 1.0f world

    let private makeCubeFaceComposite bodyColor =
        let bodySource =
            SlugShapeRuntime.fromContourCommands
                (rectCommands 0.0f 1.0f 0.0f 1.0f)
                SlugFillNonzero
                1.0e-3f
        let triangleSource =
            SlugShapeRuntime.fromContourCommands
                [| MoveTo (v2 0.0f 0.0f)
                   QuadraticCurveTo (v2 0.5f 0.5f, v2 1.0f 0.0f)
                   QuadraticCurveTo (v2 0.75f 0.5f, v2 0.5f 1.0f)
                   QuadraticCurveTo (v2 0.25f 0.5f, v2 0.0f 0.0f)
                   CloseContour |]
                SlugFillNonzero
                1.0e-3f
        let data = SlugShapeRuntime.pack [| bodySource; triangleSource |]
        let layers =
            [| { SlugLayerState.defaultState 0 with Color = bodyColor }
               { SlugLayerState.defaultState 1 with Color = orange } |]
        SlugShapeRuntime.createComposite data layers

    let private cubeFront = makeCubeFaceComposite gray
    let private cubeRight = makeCubeFaceComposite (color 0.10f 0.105f 0.12f 1.0f)
    let private cubeTop = makeCubeFaceComposite (color 0.32f 0.33f 0.36f 1.0f)

    let private makeQuadProjective
        (size : Vector3)
        (bottomLeft : Vector2)
        (bottomRight : Vector2)
        (topRight : Vector2)
        (topLeft : Vector2) =
        let deltaX1 = bottomRight.X - topRight.X
        let deltaX2 = topLeft.X - topRight.X
        let deltaX3 = bottomLeft.X - bottomRight.X + topRight.X - topLeft.X
        let deltaY1 = bottomRight.Y - topRight.Y
        let deltaY2 = topLeft.Y - topRight.Y
        let deltaY3 = bottomLeft.Y - bottomRight.Y + topRight.Y - topLeft.Y
        let denominator = deltaX1 * deltaY2 - deltaX2 * deltaY1
        let projectiveU = (deltaX3 * deltaY2 - deltaX2 * deltaY3) / denominator
        let projectiveV = (deltaX1 * deltaY3 - deltaX3 * deltaY1) / denominator
        let axisX = bottomRight.X - bottomLeft.X + projectiveU * bottomRight.X
        let axisY = topLeft.X - bottomLeft.X + projectiveV * topLeft.X
        let verticalAxisX = bottomRight.Y - bottomLeft.Y + projectiveU * bottomRight.Y
        let verticalAxisY = topLeft.Y - bottomLeft.Y + projectiveV * topLeft.Y
        let normalization = 1.0f + projectiveU * 0.5f + projectiveV * 0.5f
        let mutable result = Matrix4x4.Identity
        result.M11 <- axisX / size.X / normalization
        result.M21 <- axisY / size.Y / normalization
        result.M41 <- (axisX * 0.5f + axisY * 0.5f + bottomLeft.X) / normalization
        result.M12 <- verticalAxisX / size.X / normalization
        result.M22 <- verticalAxisY / size.Y / normalization
        result.M42 <- (verticalAxisX * 0.5f + verticalAxisY * 0.5f + bottomLeft.Y) / normalization
        result.M14 <- projectiveU / size.X / normalization
        result.M24 <- projectiveV / size.Y / normalization
        result

    let private drawCube world =
        let center = v3 0.0f -8.0f 0.0f
        let frontSize = v3 170.0f 135.0f 0.0f
        let rightSize = v3 100.0f 135.0f 0.0f
        let topSize = v3 170.0f 90.0f 0.0f
        let frontBottomLeft = v2 -90.0f -72.0f
        let frontBottomRight = v2 74.0f -68.0f
        let frontTopRight = v2 72.0f 60.0f
        let frontTopLeft = v2 -87.0f 60.0f
        let farBottomRight = v2 132.0f -29.0f
        let farTopRight = v2 129.0f 94.0f
        let farTopLeft = v2 -28.0f 92.0f
        let place name composite size elevation projective =
            SlugDemoContours.placeProjectiveComposite
                name composite center size Quaternion.Identity elevation projective world
        place
            "Objects3dCubeTop"
            cubeTop
            topSize
            1.0f
            (makeQuadProjective topSize frontTopLeft frontTopRight farTopRight farTopLeft)
        place
            "Objects3dCubeRight"
            cubeRight
            rightSize
            2.0f
            (makeQuadProjective rightSize frontBottomRight farBottomRight farTopRight frontTopRight)
        place
            "Objects3dCubeFront"
            cubeFront
            frontSize
            3.0f
            (makeQuadProjective frontSize frontBottomLeft frontBottomRight frontTopRight frontTopLeft)

    let private cylinderStops =
        [| { Offset = 0.0f; Color = color 0.10f 0.105f 0.12f 1.0f }
           { Offset = 0.5f; Color = color 0.32f 0.33f 0.36f 1.0f }
           { Offset = 1.0f; Color = color 0.10f 0.105f 0.12f 1.0f } |]

    let private cylinderBody =
        let source =
            SlugShapeRuntime.fromContourCommands
                (rectCommands -1.0f 1.0f -1.0f 1.0f)
                SlugFillNonzero
                1.0e-3f
        let gradient =
            SlugGradient
                (SlugGradientKind.Linear (v2 -1.0f 0.0f, v2 1.0f 0.0f), cylinderStops)
        let data =
            SlugShapeRuntime.packWithResources
                [| source |]
                [| gradient |]
                cylinderStops
                [||]
                [||]
        let state =
            { SlugLayerState.defaultState 0 with
                Color = Color.One
                FillSource = SlugFillSource.Gradient 0 }
        SlugShapeRuntime.createComposite data [| state |]

    let private cylinderTriangle =
        let halfArc = MathF.PI * 0.75f * 0.5f
        let horizontalScale = MathF.Sin halfArc
        let mapCylinder (emCoord : Vector2) =
            let u = emCoord.X
            let v = emCoord.Y / triangleEmHeight
            let angle = (u - 0.5f) * MathF.PI * 0.75f
            v2 (MathF.Sin angle / horizontalScale) (v * 2.0f - 1.0f)
        makeContour
            (mappedTriangleBoundary mapCylinder)
            unitSurfaceBounds
            orange

    let private drawHalfCylinder world =
        let center = v3 0.0f -10.0f 0.0f
        let size = v3 270.0f 180.0f 0.0f
        SlugDemoContours.placeComposite
            "Objects3dHalfCylinderBody"
            cylinderBody
            center
            size
            Quaternion.Identity
            0.0f
            None
            world
        SlugDemoContours.placeContour cylinderTriangle center size Quaternion.Identity 1.0f world

    let private canonicalStops =
        [| { Offset = 0.0f; Color = color 0.0f 0.8f 1.0f 1.0f }
           { Offset = 0.5f; Color = color 0.6f 0.0f 1.0f 1.0f }
           { Offset = 1.0f; Color = color 1.0f 0.0f 0.8f 1.0f } |]

    let private subdividedComposite =
        let mapPoint emX emY =
            let u = (emX - 0.1f) / 0.8f
            let v = (emY - 0.25f) / 0.5f
            v2
                (u * 2.0f - 1.0f)
                ((v * 2.0f - 1.0f) * 0.65f + MathF.Sin (u * 5.0f) * 0.34f)
        let commands =
            [| (0.1f, 0.3f)
               (0.4f, 0.6f)
               (0.7f, 0.9f) |]
            |> Array.collect (fun (x0, x1) ->
                let points =
                    [| for index in 0 .. 24 do
                           let u = x0 + (x1 - x0) * single index / 24.0f
                           yield mapPoint u 0.25f
                       for index in 24 .. -1 .. 0 do
                           let u = x0 + (x1 - x0) * single index / 24.0f
                           yield mapPoint u 0.75f |]
                polygonCommands points)
        let source =
            SlugShapeRuntime.fromContourCommands commands SlugFillNonzero 1.0e-3f
        let gradient =
            SlugGradient
                (SlugGradientKind.Linear (v2 -1.0f 0.0f, v2 1.0f 0.0f), canonicalStops)
        let data =
            SlugShapeRuntime.packWithResources
                [| source |]
                [| gradient |]
                canonicalStops
                [||]
                [||]
        let state =
            { SlugLayerState.defaultState 0 with
                Color = Color.One
                FillSource = SlugFillSource.Gradient 0 }
        SlugShapeRuntime.createComposite data [| state |]

    let private drawSubdivide world =
        SlugDemoContours.placeComposite
            "Objects3dSubdivide"
            subdividedComposite
            (v3 0.0f -8.0f 0.0f)
            (v3 310.0f 170.0f 0.0f)
            Quaternion.Identity
            1.0f
            None
            world

    let draw (world : World) =
        match selectedShape with
        | Sphere -> drawSphere world
        | Box -> drawCube world
        | HalfCylinder -> drawHalfCylinder world
        | Subdivide -> drawSubdivide world
