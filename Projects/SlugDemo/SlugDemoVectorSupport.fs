namespace SlugDemo
open System
open System.Numerics
open Prime
open Nu

module SlugDemoVectorSupport =

    let private contour commands fillColor winding strokeColor strokeThickness =
        SlugDemoContours.makeFilled commands fillColor winding strokeColor strokeThickness

    let panel name x y width height fill elevation world =
        SlugDemo.panel
            name
            (v3 x y 0.0f)
            (v3 width height 0.0f)
            fill
            elevation
            world

    let label name text x y width height fontSize textColor elevation world =
        SlugDemo.slugLeft
            name
            SlugDemo.font
            text
            (v3 x y 0.0f)
            (v3 width height 0.0f)
            fontSize
            textColor
            elevation
            TextDirectionLeftToRight
            None
            world

    let private rectCommands left bottom right top =
        [| MoveTo (v2 left top)
           LineTo (v2 right top)
           LineTo (v2 right bottom)
           LineTo (v2 left bottom)
           CloseContour |]

    let private reverseRectCommands left bottom right top =
        [| MoveTo (v2 left bottom)
           LineTo (v2 right bottom)
           LineTo (v2 right top)
           LineTo (v2 left top)
           CloseContour |]

    let private morphCommands amount =
        let leftBottom = -0.43f - amount * 0.025f
        let rightBottom = 0.43f + amount * 0.035f
        let apex = 0.46f + amount * 0.11f
        [| MoveTo (v2 leftBottom -0.34f)
           QuadraticCurveTo (v2 -0.03f (-0.52f - amount * 0.08f), v2 rightBottom -0.27f)
           QuadraticCurveTo (v2 0.32f (0.02f + amount * 0.10f), v2 0.02f apex)
           QuadraticCurveTo (v2 (-0.30f - amount * 0.08f) (0.03f + amount * 0.10f), v2 leftBottom -0.34f)
           CloseContour |]

    let morphTessellations =
        [| for index in 0 .. 8 do
               let amount = single index / 4.0f - 1.0f
               yield
                   contour
                       (morphCommands amount)
                       (color 0.10f 0.75f 0.95f 0.94f)
                       NonZero
                       (color 0.62f 0.95f 1.0f 1.0f)
                       3.0f
        |]

    let morphDebugTessellation =
        contour
            (morphCommands 0.0f)
            (color 0.10f 0.75f 0.95f 0.75f)
            NonZero
            (color 0.62f 0.95f 1.0f 1.0f)
            2.0f

    let private linearColors =
        [| color 0.28f 0.84f 1.0f 0.95f
           color 0.38f 0.72f 1.0f 0.95f
           color 0.68f 0.44f 1.0f 0.95f
           color 0.98f 0.30f 0.72f 0.95f
           color 1.0f 0.46f 0.36f 0.95f
           color 1.0f 0.76f 0.28f 0.95f
           color 0.42f 0.96f 0.72f 0.95f |]

    let private radialColors =
        [| color 1.0f 0.28f 0.62f 0.96f
           color 1.0f 0.48f 0.26f 0.92f
           color 1.0f 0.78f 0.22f 0.88f
           color 0.28f 0.88f 0.96f 0.82f
           color 0.20f 0.46f 1.0f 0.76f |]


    let private sweepColors =
        [| color 0.96f 0.23f 0.52f 0.88f
           color 0.98f 0.52f 0.24f 0.88f
           color 0.98f 0.82f 0.26f 0.88f
           color 0.31f 0.90f 0.78f 0.88f
           color 0.24f 0.63f 1.0f 0.88f
           color 0.66f 0.36f 1.0f 0.88f |]

    let private makeGradientShape commands kind (colors : Color array) =
        let source = SlugShapeRuntime.fromContourCommands commands SlugFillNonzero 1.0e-3f
        let stops =
            colors
            |> Array.mapi (fun index color ->
                { Offset = single index / single (max 1 (colors.Length - 1))
                  Color = color })
        let gradient = SlugGradient (kind, stops)
        let data = SlugShapeRuntime.packWithResources [|source|] [|gradient|] stops [||] [||]
        let state =
            { SlugLayerState.defaultState 0 with
                Color = Color.One
                FillSource = SlugFillSource.Gradient 0 }
        SlugShapeRuntime.createComposite data [|state|]

    let linearGradientShape =
        makeGradientShape
            (SlugDemoContours.roundedRectCommands 0.08f)
            (SlugGradientKind.Linear (v2 -0.5f 0.0f, v2 0.5f 0.0f))
            linearColors

    let radialGradientShape =
        makeGradientShape
            SlugDemoContours.circleCommands
            (SlugGradientKind.Radial (Vector2.Zero, v2 0.5f 0.5f))
            radialColors

    let sweepGradientShape =
        makeGradientShape
            SlugDemoContours.circleCommands
            (SlugGradientKind.Sweep (Vector2.Zero, -MathF.PI * 0.5f))
            sweepColors

    let private makeMaskedGradientShape invertOpt =
        let content =
            SlugShapeRuntime.fromContourCommands
                (SlugDemoContours.roundedRectCommands 0.08f)
                SlugFillNonzero
                1.0e-3f
        let mask =
            SlugShapeRuntime.fromContourCommands
                SlugDemoContours.circleCommands
                SlugFillNonzero
                1.0e-3f
        let stops =
            [| { Offset = 0.0f; Color = SlugDemo.cyan }
               { Offset = 0.5f; Color = SlugDemo.magenta }
               { Offset = 1.0f; Color = SlugDemo.amber } |]
        let gradient =
            SlugGradient
                (SlugGradientKind.Linear (v2 -0.5f -0.5f, v2 0.5f 0.5f),
                 stops)
        let masks =
            match invertOpt with
            | Some invert ->
                [| { Kind = SlugMaskKind.Shape 1
                     Parameters = v4 1.0f 0.0f 0.0f 0.0f
                     Parameters2 = v4 1.0f 0.0f 0.0f 0.0f
                     Invert = invert } |]
            | None -> [||]
        let data =
            SlugShapeRuntime.packWithResources [|content; mask|] [|gradient|] stops masks [||]
        let state =
            { SlugLayerState.defaultState 0 with
                Color = Color.One
                FillSource = SlugFillSource.Gradient 0
                MaskIndex = if masks.Length = 0 then -1 else 0
                MaterialValues = if masks.Length = 0 then Vector4.Zero else Vector4.UnitX }
        SlugShapeRuntime.createComposite data [|state|]

    let unmaskedGradientShape = makeMaskedGradientShape None
    let normalMaskedGradientShape = makeMaskedGradientShape (Some false)
    let invertedMaskedGradientShape = makeMaskedGradientShape (Some true)

    let shapeTriangle =
        contour
            [| MoveTo (v2 0.0f 0.48f)
               LineTo (v2 0.47f -0.40f)
               LineTo (v2 -0.47f -0.40f)
               CloseContour |]
            (color 0.96f 0.36f 0.42f 0.94f)
            NonZero
            (color 1.0f 0.76f 0.80f 1.0f)
            2.0f

    let shapeCircle =
        contour
            SlugDemoContours.circleCommands
            (color 0.22f 0.76f 1.0f 0.94f)
            NonZero
            (color 0.72f 0.94f 1.0f 1.0f)
            2.0f

    let shapeRoundedRect =
        contour
            (SlugDemoContours.roundedRectCommands 0.13f)
            (color 0.98f 0.72f 0.22f 0.94f)
            NonZero
            (color 1.0f 0.92f 0.60f 1.0f)
            2.0f

    let compositeBody =
        contour
            (SlugDemoContours.roundedRectCommands 0.14f)
            (color 0.16f 0.80f 0.62f 0.95f)
            NonZero
            (color 0.62f 1.0f 0.82f 1.0f)
            2.0f

    let compositeCore =
        contour
            SlugDemoContours.circleCommands
            (color 0.07f 0.20f 0.29f 1.0f)
            NonZero
            (color 0.86f 1.0f 0.95f 1.0f)
            1.5f

    let compositeFin =
        contour
            [| MoveTo (v2 0.0f 0.5f)
               LineTo (v2 0.48f -0.45f)
               LineTo (v2 -0.48f -0.45f)
               CloseContour |]
            (color 0.98f 0.30f 0.54f 0.96f)
            NonZero
            (color 1.0f 0.74f 0.84f 1.0f)
            1.5f

    let private punchOuter =
        rectCommands -0.5f -0.5f 0.5f 0.5f

    let private punchInnerSame =
        rectCommands -0.24f -0.29f 0.24f 0.29f

    let private punchInnerOpposite =
        reverseRectCommands -0.24f -0.29f 0.24f 0.29f

    let punchSameTessellation =
        contour
            (Array.append punchOuter punchInnerSame)
            (color 0.96f 0.36f 0.42f 0.96f)
            NonZero
            (color 1.0f 0.78f 0.82f 1.0f)
            1.8f

    let punchOppositeTessellation =
        contour
            (Array.append punchOuter punchInnerOpposite)
            (color 0.24f 0.62f 1.0f 0.96f)
            NonZero
            (color 0.72f 0.90f 1.0f 1.0f)
            1.8f

