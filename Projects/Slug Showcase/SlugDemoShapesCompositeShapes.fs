namespace SlugShowcase
open System
open System.Numerics
open Nu
open SlugDemoVectorSupport

[<RequireQualifiedAccess>]
module SlugDemoShapesCompositeShapes =

    let private srgbToLinear channel =
        if channel <= 0.04045f then channel / 12.92f
        else MathF.Pow ((channel + 0.055f) / 1.055f, 2.4f)

    // The reference examples author colors directly in an sRGB framebuffer. Nu's output is
    // sRGB-encoded, so keep the source constants while supplying their linear equivalents.
    let private canonicalColor red green blue =
        color (srgbToLinear red) (srgbToLinear green) (srgbToLinear blue) 1.0f

    let private canonicalNavy = canonicalColor 0.20f 0.20f 0.40f
    let private outlineBlack = canonicalColor 0.07f 0.07f 0.07f
    let private creaturePink = canonicalColor 0.976f 0.627f 0.706f
    let private creatureLightPink = canonicalColor 0.988f 0.800f 0.847f
    let private creatureLegPink = canonicalColor 0.941f 0.439f 0.565f
    let private creatureStalkPink = canonicalColor 0.878f 0.345f 0.471f
    let private creaturePlumePink = canonicalColor 1.0f 0.376f 0.565f
    let private creatureMouth = canonicalColor 0.753f 0.251f 0.376f
    let private facePink = canonicalColor 1.0f 0.60f 0.70f
    let private faceAppendagePink = canonicalColor 0.85f 0.25f 0.35f
    let private faceCheekPink = canonicalColor 1.0f 0.45f 0.55f
    let private faceEyeBlack = canonicalColor 0.05f 0.05f 0.08f
    let private faceMouth = canonicalColor 0.65f 0.15f 0.20f
    let private faceNose = canonicalColor 0.70f 0.30f 0.40f
    let private orange = canonicalColor 1.0f 0.50f 0.0f
    let private green = canonicalColor 0.20f 0.80f 0.40f

    let private ellipseCommands centerX centerY radiusX radiusY rotation =
        let kappa = 0.5522847498f
        let cosine = MathF.Cos rotation
        let sine = MathF.Sin rotation
        let point x y =
            let scaledX = radiusX * x
            let scaledY = radiusY * y
            v2
                (centerX + scaledX * cosine - scaledY * sine)
                (centerY + scaledX * sine + scaledY * cosine)
        [| MoveTo (point 1.0f 0.0f)
           CubicCurveTo (point 1.0f kappa, point kappa 1.0f, point 0.0f 1.0f)
           CubicCurveTo (point (-kappa) 1.0f, point -1.0f kappa, point -1.0f 0.0f)
           CubicCurveTo (point -1.0f (-kappa), point (-kappa) -1.0f, point 0.0f -1.0f)
           CubicCurveTo (point kappa -1.0f, point 1.0f (-kappa), point 1.0f 0.0f)
           CloseContour |]

    let private capsuleCommands x1 y1 x2 y2 radius =
        let startPoint = Vector2 (x1, y1)
        let endPoint = Vector2 (x2, y2)
        let direction = Vector2.Normalize (endPoint - startPoint)
        let normal = Vector2 (-direction.Y, direction.X)
        let kappa = 0.5522847498f
        let point center normalScale directionScale =
            center + normal * (radius * normalScale) + direction * (radius * directionScale)
        [| MoveTo (point startPoint 1.0f 0.0f)
           LineTo (point endPoint 1.0f 0.0f)
           CubicCurveTo
               (point endPoint 1.0f kappa,
                point endPoint kappa 1.0f,
                point endPoint 0.0f 1.0f)
           CubicCurveTo
               (point endPoint (-kappa) 1.0f,
                point endPoint -1.0f kappa,
                point endPoint -1.0f 0.0f)
           LineTo (point startPoint -1.0f 0.0f)
           CubicCurveTo
               (point startPoint -1.0f (-kappa),
                point startPoint (-kappa) -1.0f,
                point startPoint 0.0f -1.0f)
           CubicCurveTo
               (point startPoint kappa -1.0f,
                point startPoint 1.0f (-kappa),
                point startPoint 1.0f 0.0f)
           CloseContour |]

    let private scaleCommands scale commands =
        commands
        |> Array.map (function
            | MoveTo endpoint -> MoveTo (endpoint * scale)
            | LineTo endpoint -> LineTo (endpoint * scale)
            | QuadraticCurveTo (control, endpoint) ->
                QuadraticCurveTo (control * scale, endpoint * scale)
            | CubicCurveTo (control1, control2, endpoint) ->
                CubicCurveTo (control1 * scale, control2 * scale, endpoint * scale)
            | CloseContour -> CloseContour)

    let private analyticCompositeWithScale sourceScale layers =
        let sources =
            layers
            |> Array.map (fun (commands, _) ->
                SlugShapeRuntime.fromContourCommands
                    (scaleCommands sourceScale commands)
                    SlugFillNonzero
                    1.0e-3f)
        let data = SlugShapeRuntime.pack sources
        let states =
            layers
            |> Array.mapi (fun index (_, layerColor) ->
                { SlugLayerState.defaultState index with
                    Color = layerColor })
        AnalyticSlug (SlugShapeRuntime.createComposite data states, None)

    let private analyticComposite layers =
        analyticCompositeWithScale 1.0f layers

    // Cairo-authored axolotl geometry, kept in the reference example's 600-unit canvas.
    let private creatureLegCenters =
        [| (185.0f, 110.0f)
           (255.0f, 98.0f)
           (355.0f, 98.0f)
           (425.0f, 110.0f) |]

    let private creatureOutlineTail =
        [| MoveTo (v2 440.0f 205.0f)
           CubicCurveTo (v2 510.0f 250.0f, v2 530.0f 290.0f, v2 520.0f 320.0f)
           CubicCurveTo (v2 505.0f 340.0f, v2 480.0f 335.0f, v2 465.0f 312.0f)
           CubicCurveTo (v2 450.0f 290.0f, v2 448.0f 255.0f, v2 440.0f 205.0f)
           CloseContour |]

    let private creatureBodyTail =
        [| MoveTo (v2 438.0f 207.0f)
           CubicCurveTo (v2 500.0f 248.0f, v2 522.0f 282.0f, v2 512.0f 308.0f)
           CubicCurveTo (v2 498.0f 328.0f, v2 475.0f 323.0f, v2 461.0f 302.0f)
           CubicCurveTo (v2 448.0f 283.0f, v2 446.0f 258.0f, v2 438.0f 207.0f)
           CloseContour |]

    let private creatureOutlineCommands =
        Array.concat
            [| ellipseCommands 310.0f 195.0f 155.0f 95.0f 0.0f
               ellipseCommands 195.0f 235.0f 80.0f 68.0f 0.0f
               creatureOutlineTail
               yield!
                   creatureLegCenters
                   |> Array.map (fun (x, y) -> ellipseCommands x y 28.0f 44.0f 0.0f) |]

    let private creatureBodyCommands =
        Array.concat
            [| ellipseCommands 310.0f 195.0f 148.0f 88.0f 0.0f
               ellipseCommands 196.0f 236.0f 73.0f 62.0f 0.0f
               creatureBodyTail |]

    let private creatureBellyCommands =
        Array.concat
            [| ellipseCommands 310.0f 188.0f 110.0f 62.0f 0.0f
               ellipseCommands 205.0f 226.0f 50.0f 42.0f 0.0f |]

    let private creatureLegCommands =
        creatureLegCenters
        |> Array.collect (fun (x, y) -> ellipseCommands x y 22.0f 38.0f 0.0f)

    let private creatureStalkCommands =
        [| (155.0f, 280.0f, 120.0f, 350.0f, 9.0f)
           (168.0f, 290.0f, 150.0f, 360.0f, 9.0f)
           (182.0f, 296.0f, 180.0f, 368.0f, 9.0f) |]
        |> Array.collect (fun (x1, y1, x2, y2, radius) ->
            capsuleCommands x1 y1 x2 y2 radius)

    let private creaturePlumeCommands =
        [| (120.0f, 355.0f, 12.0f, 20.0f, -20.0f)
           (108.0f, 365.0f, 9.0f, 17.0f, -40.0f)
           (133.0f, 367.0f, 9.0f, 17.0f, 5.0f)
           (150.0f, 365.0f, 12.0f, 20.0f, -10.0f)
           (138.0f, 375.0f, 9.0f, 17.0f, -30.0f)
           (163.0f, 377.0f, 9.0f, 17.0f, 10.0f)
           (180.0f, 373.0f, 12.0f, 20.0f, 5.0f)
           (168.0f, 381.0f, 9.0f, 17.0f, -15.0f)
           (192.0f, 383.0f, 9.0f, 17.0f, 20.0f) |]
        |> Array.collect (fun (x, y, radiusX, radiusY, angle) ->
            ellipseCommands x y radiusX radiusY (angle * MathF.PI / 180.0f))

    let private creatureEyeBlackCommands =
        ellipseCommands 172.0f 258.0f 12.0f 12.0f 0.0f

    let private creatureEyeWhiteCommands =
        ellipseCommands 175.0f 261.0f 4.0f 4.0f 0.0f

    let private creatureSmileCommands =
        [| MoveTo (v2 181.0f 204.0f)
           CubicCurveTo (v2 190.0f 196.0f, v2 208.0f 196.0f, v2 224.0f 202.0f)
           LineTo (v2 222.0f 208.0f)
           CubicCurveTo (v2 207.0f 203.0f, v2 191.0f 203.0f, v2 183.0f 210.0f)
           CloseContour |]

    let private creature =
        analyticCompositeWithScale (1.0f / 600.0f)
            [| (creatureOutlineCommands, outlineBlack)
               (creatureBodyCommands, creaturePink)
               (creatureBellyCommands, creatureLightPink)
               (creatureLegCommands, creatureLegPink)
               (creatureStalkCommands, creatureStalkPink)
               (creaturePlumeCommands, creaturePlumePink)
               (creatureEyeBlackCommands, outlineBlack)
               (creatureEyeWhiteCommands, canonicalColor 1.0f 1.0f 1.0f)
               (creatureSmileCommands, creatureMouth) |]

    let private facePoint x y = v2 x (-y)

    let private faceAppendageCommands =
        [| MoveTo (facePoint 45.0f 30.0f)
           CubicCurveTo (facePoint 25.0f 10.0f, facePoint 5.0f 5.0f, facePoint 0.0f 15.0f)
           CubicCurveTo (facePoint 5.0f 20.0f, facePoint 20.0f 22.0f, facePoint 45.0f 35.0f)
           CloseContour
           MoveTo (facePoint 40.0f 42.0f)
           CubicCurveTo (facePoint 15.0f 35.0f, facePoint -5.0f 30.0f, facePoint -8.0f 42.0f)
           CubicCurveTo (facePoint -5.0f 50.0f, facePoint 15.0f 48.0f, facePoint 40.0f 47.0f)
           CloseContour
           MoveTo (facePoint 42.0f 55.0f)
           CubicCurveTo (facePoint 20.0f 60.0f, facePoint 5.0f 68.0f, facePoint 5.0f 78.0f)
           CubicCurveTo (facePoint 10.0f 75.0f, facePoint 25.0f 68.0f, facePoint 45.0f 58.0f)
           CloseContour
           MoveTo (facePoint 155.0f 30.0f)
           CubicCurveTo (facePoint 175.0f 10.0f, facePoint 195.0f 5.0f, facePoint 200.0f 15.0f)
           CubicCurveTo (facePoint 195.0f 20.0f, facePoint 180.0f 22.0f, facePoint 155.0f 35.0f)
           CloseContour
           MoveTo (facePoint 160.0f 42.0f)
           CubicCurveTo (facePoint 185.0f 35.0f, facePoint 205.0f 30.0f, facePoint 208.0f 42.0f)
           CubicCurveTo (facePoint 205.0f 50.0f, facePoint 185.0f 48.0f, facePoint 160.0f 47.0f)
           CloseContour
           MoveTo (facePoint 158.0f 55.0f)
           CubicCurveTo (facePoint 180.0f 60.0f, facePoint 195.0f 68.0f, facePoint 195.0f 78.0f)
           CubicCurveTo (facePoint 190.0f 75.0f, facePoint 175.0f 68.0f, facePoint 155.0f 58.0f)
           CloseContour |]

    let private faceHeadCommands =
        [| MoveTo (facePoint 100.0f 15.0f)
           CubicCurveTo (facePoint 55.0f 15.0f, facePoint 25.0f 35.0f, facePoint 30.0f 65.0f)
           CubicCurveTo (facePoint 33.0f 85.0f, facePoint 55.0f 100.0f, facePoint 100.0f 100.0f)
           CubicCurveTo (facePoint 145.0f 100.0f, facePoint 167.0f 85.0f, facePoint 170.0f 65.0f)
           CubicCurveTo (facePoint 175.0f 35.0f, facePoint 145.0f 15.0f, facePoint 100.0f 15.0f)
           CloseContour |]

    let private faceCheekCommands =
        [| MoveTo (facePoint 45.0f 65.0f)
           CubicCurveTo (facePoint 42.0f 58.0f, facePoint 48.0f 52.0f, facePoint 58.0f 55.0f)
           CubicCurveTo (facePoint 65.0f 57.0f, facePoint 68.0f 65.0f, facePoint 63.0f 72.0f)
           CubicCurveTo (facePoint 58.0f 78.0f, facePoint 47.0f 73.0f, facePoint 45.0f 65.0f)
           CloseContour
           MoveTo (facePoint 155.0f 65.0f)
           CubicCurveTo (facePoint 158.0f 58.0f, facePoint 152.0f 52.0f, facePoint 142.0f 55.0f)
           CubicCurveTo (facePoint 135.0f 57.0f, facePoint 132.0f 65.0f, facePoint 137.0f 72.0f)
           CubicCurveTo (facePoint 142.0f 78.0f, facePoint 153.0f 73.0f, facePoint 155.0f 65.0f)
           CloseContour |]

    let private faceEyeCommands =
        [| MoveTo (facePoint 65.0f 45.0f)
           CubicCurveTo (facePoint 60.0f 35.0f, facePoint 70.0f 28.0f, facePoint 82.0f 32.0f)
           CubicCurveTo (facePoint 90.0f 35.0f, facePoint 92.0f 45.0f, facePoint 87.0f 52.0f)
           CubicCurveTo (facePoint 82.0f 58.0f, facePoint 68.0f 55.0f, facePoint 65.0f 45.0f)
           CloseContour
           MoveTo (facePoint 135.0f 45.0f)
           CubicCurveTo (facePoint 140.0f 35.0f, facePoint 130.0f 28.0f, facePoint 118.0f 32.0f)
           CubicCurveTo (facePoint 110.0f 35.0f, facePoint 108.0f 45.0f, facePoint 113.0f 52.0f)
           CubicCurveTo (facePoint 118.0f 58.0f, facePoint 132.0f 55.0f, facePoint 135.0f 45.0f)
           CloseContour |]

    let private faceEyeGlintCommands =
        [| MoveTo (facePoint 73.0f 42.0f)
           CubicCurveTo (facePoint 71.0f 37.0f, facePoint 76.0f 34.0f, facePoint 80.0f 37.0f)
           CubicCurveTo (facePoint 83.0f 39.0f, facePoint 83.0f 44.0f, facePoint 80.0f 47.0f)
           CubicCurveTo (facePoint 77.0f 49.0f, facePoint 74.0f 47.0f, facePoint 73.0f 42.0f)
           CloseContour
           MoveTo (facePoint 127.0f 42.0f)
           CubicCurveTo (facePoint 129.0f 37.0f, facePoint 124.0f 34.0f, facePoint 120.0f 37.0f)
           CubicCurveTo (facePoint 117.0f 39.0f, facePoint 117.0f 44.0f, facePoint 120.0f 47.0f)
           CubicCurveTo (facePoint 123.0f 49.0f, facePoint 126.0f 47.0f, facePoint 127.0f 42.0f)
           CloseContour |]

    let private faceSmileCommands =
        [| MoveTo (facePoint 80.0f 78.0f)
           CubicCurveTo (facePoint 88.0f 88.0f, facePoint 112.0f 88.0f, facePoint 120.0f 78.0f)
           LineTo (facePoint 117.0f 75.0f)
           CubicCurveTo (facePoint 110.0f 83.0f, facePoint 90.0f 83.0f, facePoint 83.0f 75.0f)
           CloseContour |]

    let private faceNoseCommands =
        [| MoveTo (facePoint 90.0f 68.0f)
           CubicCurveTo (facePoint 89.0f 66.0f, facePoint 91.0f 64.0f, facePoint 93.0f 66.0f)
           CubicCurveTo (facePoint 94.0f 68.0f, facePoint 92.0f 70.0f, facePoint 90.0f 68.0f)
           CloseContour
           MoveTo (facePoint 110.0f 68.0f)
           CubicCurveTo (facePoint 111.0f 66.0f, facePoint 109.0f 64.0f, facePoint 107.0f 66.0f)
           CubicCurveTo (facePoint 106.0f 68.0f, facePoint 108.0f 70.0f, facePoint 110.0f 68.0f)
           CloseContour |]

    let private face =
        analyticCompositeWithScale (1.0f / 100.0f)
            [| (faceAppendageCommands, faceAppendagePink)
               (faceHeadCommands, facePink)
               (faceCheekCommands, faceCheekPink)
               (faceEyeCommands, faceEyeBlack)
               (faceEyeGlintCommands, canonicalColor 1.0f 1.0f 1.0f)
               (faceSmileCommands, faceMouth)
               (faceNoseCommands, faceNose) |]

    // The orange border is two clean analytic fills. This preserves the source's jigsaw
    // silhouette without routing sharp/concave joins through Nu's tessellated stroke path.
    let private puzzleOuterCommands =
        [| MoveTo (v2 -0.42f -0.40f)
           LineTo (v2 -0.12f -0.40f)
           CubicCurveTo (v2 -0.12f -0.50f, v2 -0.09f -0.54f, v2 0.0f -0.54f)
           CubicCurveTo (v2 0.09f -0.54f, v2 0.12f -0.50f, v2 0.12f -0.40f)
           LineTo (v2 0.43f -0.36f)
           LineTo (v2 0.43f -0.10f)
           CubicCurveTo (v2 0.31f -0.10f, v2 0.29f -0.05f, v2 0.29f 0.0f)
           CubicCurveTo (v2 0.29f 0.05f, v2 0.31f 0.10f, v2 0.43f 0.10f)
           LineTo (v2 0.40f 0.40f)
           LineTo (v2 0.12f 0.42f)
           CubicCurveTo (v2 0.12f 0.31f, v2 0.08f 0.28f, v2 0.0f 0.28f)
           CubicCurveTo (v2 -0.08f 0.28f, v2 -0.12f 0.31f, v2 -0.12f 0.42f)
           LineTo (v2 -0.36f 0.45f)
           LineTo (v2 -0.36f 0.12f)
           CubicCurveTo (v2 -0.48f 0.12f, v2 -0.52f 0.08f, v2 -0.52f 0.0f)
           CubicCurveTo (v2 -0.52f -0.08f, v2 -0.48f -0.12f, v2 -0.36f -0.12f)
           LineTo (v2 -0.42f -0.40f)
           CloseContour |]

    let private puzzleInnerCommands =
        [| MoveTo (v2 -0.35f -0.34f)
           LineTo (v2 -0.10f -0.34f)
           CubicCurveTo (v2 -0.10f -0.42f, v2 -0.07f -0.46f, v2 0.0f -0.46f)
           CubicCurveTo (v2 0.07f -0.46f, v2 0.10f -0.42f, v2 0.10f -0.34f)
           LineTo (v2 0.36f -0.31f)
           LineTo (v2 0.31f -0.07f)
           CubicCurveTo (v2 0.26f -0.07f, v2 0.21f -0.04f, v2 0.21f 0.0f)
           CubicCurveTo (v2 0.21f 0.04f, v2 0.26f 0.07f, v2 0.31f 0.07f)
           LineTo (v2 0.34f 0.33f)
           LineTo (v2 0.09f 0.30f)
           CubicCurveTo (v2 0.09f 0.25f, v2 0.05f 0.20f, v2 0.0f 0.20f)
           CubicCurveTo (v2 -0.05f 0.20f, v2 -0.09f 0.25f, v2 -0.09f 0.30f)
           LineTo (v2 -0.30f 0.38f)
           LineTo (v2 -0.30f 0.10f)
           CubicCurveTo (v2 -0.40f 0.10f, v2 -0.44f 0.07f, v2 -0.44f 0.0f)
           CubicCurveTo (v2 -0.44f -0.07f, v2 -0.40f -0.10f, v2 -0.30f -0.10f)
           LineTo (v2 -0.35f -0.34f)
           CloseContour |]

    let private puzzle =
        analyticComposite
            [| (puzzleOuterCommands, orange)
               (puzzleInnerCommands, canonicalNavy) |]

    let private ringOuter =
        [| MoveTo (v2 -0.5f 0.5f)
           LineTo (v2 0.5f 0.5f)
           LineTo (v2 0.5f -0.5f)
           LineTo (v2 -0.5f -0.5f)
           CloseContour |]

    // The inner loop remains explicitly opposite-wound so NonZero leaves a real hole.
    let private ringInnerReversed =
        [| MoveTo (v2 -0.25f -0.25f)
           LineTo (v2 0.25f -0.25f)
           LineTo (v2 0.25f 0.25f)
           LineTo (v2 -0.25f 0.25f)
           CloseContour |]

    let private ring =
        analyticComposite
            [| (Array.append ringOuter ringInnerReversed, green) |]

    let draw (world : World) =
        let leftX = -154.0f
        let rightX = 154.0f
        let topY = 57.0f
        let bottomY = -86.0f
        let panelWidth = 304.0f
        let panelHeight = 132.0f
        SlugDemo.panel "ShapesCompositeTopLeft" (v3 leftX topY 0.0f) (v3 panelWidth panelHeight 0.0f) canonicalNavy -10.0f world
        SlugDemo.panel "ShapesCompositeTopRight" (v3 rightX topY 0.0f) (v3 panelWidth panelHeight 0.0f) canonicalNavy -10.0f world
        SlugDemo.panel "ShapesCompositeBottomLeft" (v3 leftX bottomY 0.0f) (v3 panelWidth panelHeight 0.0f) canonicalNavy -10.0f world
        SlugDemo.panel "ShapesCompositeBottomRight" (v3 rightX bottomY 0.0f) (v3 panelWidth panelHeight 0.0f) canonicalNavy -10.0f world

        SlugShowcaseContours.placeContour
            "ShapesCreature"
            creature
            (v3 -144.0f topY 0.0f)
            (v3 158.0f 124.0f 0.0f)
            Quaternion.Identity
            0.0f
            world

        SlugShowcaseContours.placeContour
            "ShapesFace"
            face
            (v3 147.0f 54.0f 0.0f)
            (v3 168.0f 84.0f 0.0f)
            Quaternion.Identity
            0.0f
            world

        SlugShowcaseContours.placeContour
            "ShapesPuzzle"
            puzzle
            (v3 -150.0f -92.0f 0.0f)
            (v3 140.0f 108.0f 0.0f)
            Quaternion.Identity
            0.0f
            world

        SlugShowcaseContours.placeContour
            "ShapesGreenRing"
            ring
            (v3 143.0f bottomY 0.0f)
            (v3 136.0f 112.0f 0.0f)
            (Quaternion.CreateFromAxisAngle (Vector3.UnitZ, -0.08f))
            0.0f
            world
