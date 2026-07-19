namespace SlugDemo
open System
open System.Numerics
open Nu
open SlugDemoProjectionSupport

[<RequireQualifiedAccess>]
module SlugDemoProjection2d =

    let private fitScale = 0.5f
    let private navigationHeight = 60.0f
    let private sourceMajorHalfWidth = 2.5f
    // The source grid extent is measured between its outer-line centerlines. Lift the
    // shared frame by the fitted half-width so the bottom stroke remains inside the viewport.
    let private frameCenterY = -navigationHeight * 0.5f + sourceMajorHalfWidth * fitScale
    let private sourceCircleRadius = 40.0f
    let private sourceStrokeWidth = 3.0f
    let private annularPseudostrokeExtent =
        (sourceCircleRadius + sourceStrokeWidth * 0.5f) * 2.0f

    // The reference path is built from two closed quadratic contours. The outer loop winds
    // counter-clockwise and the inner loop clockwise, so non-zero winding leaves only the
    // three-unit annular pseudostroke filled.
    let private annularPseudostrokeCommands =
        let center = v2 50.0f 50.0f
        let outerRadius = sourceCircleRadius + sourceStrokeWidth * 0.5f
        let innerRadius = sourceCircleRadius - sourceStrokeWidth * 0.5f
        let quadraticArcControl = 0.91421354f
        let outerControl = outerRadius * quadraticArcControl
        let innerControl = innerRadius * quadraticArcControl
        [| MoveTo (v2 (center.X + outerRadius) center.Y)
           QuadraticCurveTo (v2 (center.X + outerControl) (center.Y + outerControl), v2 center.X (center.Y + outerRadius))
           QuadraticCurveTo (v2 (center.X - outerControl) (center.Y + outerControl), v2 (center.X - outerRadius) center.Y)
           QuadraticCurveTo (v2 (center.X - outerControl) (center.Y - outerControl), v2 center.X (center.Y - outerRadius))
           QuadraticCurveTo (v2 (center.X + outerControl) (center.Y - outerControl), v2 (center.X + outerRadius) center.Y)
           CloseContour
           MoveTo (v2 (center.X + innerRadius) center.Y)
           QuadraticCurveTo (v2 (center.X + innerControl) (center.Y - innerControl), v2 center.X (center.Y - innerRadius))
           QuadraticCurveTo (v2 (center.X - innerControl) (center.Y - innerControl), v2 (center.X - innerRadius) center.Y)
           QuadraticCurveTo (v2 (center.X - innerControl) (center.Y + innerControl), v2 center.X (center.Y + innerRadius))
           QuadraticCurveTo (v2 (center.X + innerControl) (center.Y + innerControl), v2 (center.X + innerRadius) center.Y)
           CloseContour |]

    let private annularPseudostroke =
        SlugDemoContours.makeFilled
            annularPseudostrokeCommands
            (color 0.20f 0.80f 0.40f 1.0f)
            NonZero
            Color.Zero
            0.0f

    let private drawGrid (world : World) =
        let anchorY = frameCenterY
        let sourceMinorWidth = 0.5f
        let majorWidth = sourceMajorHalfWidth * 2.0f * fitScale
        let minorWidth = sourceMinorWidth * 2.0f * fitScale
        let gridWidth = 800.0f * fitScale
        let gridHeight = 600.0f * fitScale
        let majorColor = color 0.80f 0.80f 0.80f 1.0f
        let minorColor = color 0.65f 0.65f 0.65f 0.8f

        // Emit minor lines first, matching GridDrawable, then put the major lines on top.
        for index in -40 .. 40 do
            if index % 10 <> 0 then
                let x = single index * 10.0f * fitScale
                sprite
                    (sprintf "Projection2dGridMinorVertical%+03d" index)
                    (v3 x anchorY 0.0f)
                    (v3 minorWidth gridHeight 0.0f)
                    Quaternion.Identity
                    minorColor
                    0.0f
                    world

        for index in -30 .. 30 do
            if index % 10 <> 0 then
                let y = anchorY + single index * 10.0f * fitScale
                sprite
                    (sprintf "Projection2dGridMinorHorizontal%+03d" index)
                    (v3 0.0f y 0.0f)
                    (v3 gridWidth minorWidth 0.0f)
                    Quaternion.Identity
                    minorColor
                    0.0f
                    world

        for index in -40 .. 40 do
            if index % 10 = 0 then
                let x = single index * 10.0f * fitScale
                sprite
                    (sprintf "Projection2dGridMajorVertical%+03d" index)
                    (v3 x anchorY 0.0f)
                    (v3 majorWidth gridHeight 0.0f)
                    Quaternion.Identity
                    majorColor
                    0.1f
                    world

        for index in -30 .. 30 do
            if index % 10 = 0 then
                let y = anchorY + single index * 10.0f * fitScale
                sprite
                    (sprintf "Projection2dGridMajorHorizontal%+03d" index)
                    (v3 0.0f y 0.0f)
                    (v3 gridWidth majorWidth 0.0f)
                    Quaternion.Identity
                    majorColor
                    0.1f
                    world

    let draw (world : World) =
        drawGrid world
        // osgSlug's SCALE=2 decomposition and this viewport's 0.5 fit cancel, leaving
        // the source's 83-unit bounds. Its normalized lower-left corner is on the grid
        // origin, so place its center one half-extent above and right of that origin.
        let halfExtent = annularPseudostrokeExtent * 0.5f
        SlugDemoContours.placeContour
            annularPseudostroke
            (v3 halfExtent (frameCenterY + halfExtent) 0.0f)
            (v3 annularPseudostrokeExtent annularPseudostrokeExtent 0.0f)
            Quaternion.Identity
            1.0f
            world
