namespace SlugDemo
open System
open System.Numerics
open Prime
open Nu
open SlugDemoProjectionSupport

[<RequireQualifiedAccess>]
module SlugDemoProjection3d =

    let private gridMin = v2 -200.0f -155.0f
    let private gridMax = v2 200.0f 145.0f
    let private gridSize = gridMax - gridMin
    let private gridCorners =
        [| gridMin
           v2 gridMin.X gridMax.Y
           gridMax
           v2 gridMax.X gridMin.Y |]

    let private sceneCenter = (gridMin + gridMax) * 0.5f
    let private sceneCenter3 = v3 sceneCenter.X sceneCenter.Y 0.0f
    let private ringCenter = v3 25.0f 20.0f 0.0f

    let private projectionPitch = 0.70f
    let private projectionRoll = -0.28f
    let private projectionPerspective = 0.0024f
    let private pitchSin = MathF.Sin projectionPitch
    let private pitchCos = MathF.Cos projectionPitch
    let private rollSin = MathF.Sin projectionRoll
    let private rollCos = MathF.Cos projectionRoll

    let private projectUnframed zoom (point : Vector2) =
        let relative = point - sceneCenter
        let projectedX =
            zoom *
            (relative.X * rollCos -
             relative.Y * pitchCos * rollSin)
        let projectedY =
            zoom *
            (relative.X * rollSin +
             relative.Y * pitchCos * rollCos)
        let denominator = 1.0f + relative.Y * pitchSin * projectionPerspective
        sceneCenter + Vector2 (projectedX / denominator, projectedY / denominator)

    let private projectedGridBounds zoom =
        let mutable minPoint = Vector2 (Single.PositiveInfinity, Single.PositiveInfinity)
        let mutable maxPoint = Vector2 (Single.NegativeInfinity, Single.NegativeInfinity)
        for corner in gridCorners do
            let projected = projectUnframed zoom corner
            minPoint <- Vector2 (min minPoint.X projected.X, min minPoint.Y projected.Y)
            maxPoint <- Vector2 (max maxPoint.X projected.X, max maxPoint.Y projected.Y)
        minPoint, maxPoint

    // Fit the oblique projection into the same source frame as Projection2d.
    // The one uniform scale and the resulting frame translation apply to both
    // the grid sprites and the analytic Slug ring.
    let private unscaledProjectedMin, unscaledProjectedMax = projectedGridBounds 1.0f
    let private unscaledProjectedSize = unscaledProjectedMax - unscaledProjectedMin
    let private projectionZoom =
        min
            (gridSize.X / unscaledProjectedSize.X)
            (gridSize.Y / unscaledProjectedSize.Y)
    let private fittedProjectedMin, fittedProjectedMax = projectedGridBounds projectionZoom
    let private fittedProjectedCenter = (fittedProjectedMin + fittedProjectedMax) * 0.5f
    let private projectionOffset = sceneCenter - fittedProjectedCenter

    let private gridXs =
        [| for index in 0 .. 80 -> gridMin.X + single index * 5.0f |]

    let private gridYs =
        [| for index in 0 .. 60 -> gridMin.Y + single index * 5.0f |]
    let private circleCommands radius clockwise =
        let k = radius * 0.5522847498f
        if clockwise then
            [| MoveTo (v2 radius 0.0f)
               CubicCurveTo (v2 radius -k, v2 k -radius, v2 0.0f -radius)
               CubicCurveTo (v2 -k -radius, v2 -radius -k, v2 -radius 0.0f)
               CubicCurveTo (v2 -radius k, v2 -k radius, v2 0.0f radius)
               CubicCurveTo (v2 k radius, v2 radius k, v2 radius 0.0f)
               CloseContour |]
        else
            [| MoveTo (v2 radius 0.0f)
               CubicCurveTo (v2 radius k, v2 k radius, v2 0.0f radius)
               CubicCurveTo (v2 -k radius, v2 -radius k, v2 -radius 0.0f)
               CubicCurveTo (v2 -radius -k, v2 -k -radius, v2 0.0f -radius)
               CubicCurveTo (v2 k -radius, v2 radius -k, v2 radius 0.0f)
               CloseContour |]

    let private ringComposite =
        let outer = circleCommands 41.5f false
        let inner = circleCommands 38.5f true
        let source =
            SlugShapeRuntime.fromContourCommands
                (Array.append outer inner)
                SlugFillNonzero
                1.0e-3f
        let data = SlugShapeRuntime.pack [| source |]
        let state =
            { SlugLayerState.defaultState 0 with
                Color = color 0.2f 0.8f 0.4f 1.0f }
        SlugShapeRuntime.createComposite data [| state |]

    let private sceneProjective =
        let mutable raw = Matrix4x4.Identity
        raw.M11 <- projectionZoom * rollCos
        raw.M12 <- projectionZoom * rollSin
        raw.M21 <- -projectionZoom * pitchCos * rollSin
        raw.M22 <- projectionZoom * pitchCos * rollCos
        raw.M14 <- 0.0f
        raw.M24 <- projectionPerspective * pitchSin
        let frameCenter3 =
            sceneCenter3 + v3 projectionOffset.X projectionOffset.Y 0.0f
        Matrix4x4.CreateTranslation (-sceneCenter3) *
        raw *
        Matrix4x4.CreateTranslation frameCenter3

    let private ringProjective =
        Matrix4x4.CreateTranslation ringCenter *
        sceneProjective *
        Matrix4x4.CreateTranslation (-ringCenter)

    let private projectPoint point =
        projectUnframed projectionZoom point + projectionOffset

    let private placeProjectedLine name first second width lineColor elevation world =
        let firstProjected = projectPoint first
        let secondProjected = projectPoint second
        let delta = secondProjected - firstProjected
        let length = max 0.001f (delta.Length ())
        let midpoint = (firstProjected + secondProjected) * 0.5f
        let sourceMidpoint = (first + second) * 0.5f
        let sourceDelta = second - first
        let sourceLength = max 0.001f (sourceDelta.Length ())
        let sourceNormal = Vector2 (-sourceDelta.Y / sourceLength, sourceDelta.X / sourceLength)
        let halfWidth = width * 0.5f
        let widthProjected =
            Vector2.Distance (
                projectPoint (sourceMidpoint + sourceNormal * halfWidth),
                projectPoint (sourceMidpoint - sourceNormal * halfWidth))
        sprite
            name
            (v3 midpoint.X midpoint.Y 0.0f)
            (v3 length (max 0.2f widthProjected) 0.0f)
            (zRotation (MathF.Atan2 (delta.Y, delta.X)))
            lineColor
            elevation
            world

    let draw (world : World) =

        for index in 0 .. gridXs.Length - 1 do
            let x = gridXs.[index]
            let major = index % 10 = 0
            placeProjectedLine
                ("Projection3dGridX" + string index)
                (v2 x -155.0f)
                (v2 x 145.0f)
                (if major then 2.5f else 0.5f)
                (if major then color 0.8f 0.8f 0.8f 1.0f else color 0.65f 0.65f 0.65f 0.8f)
                (if major then 1.02f else 1.0f)
                world

        for index in 0 .. gridYs.Length - 1 do
            let y = gridYs.[index]
            let major = index % 10 = 0
            placeProjectedLine
                ("Projection3dGridY" + string index)
                (v2 -200.0f y)
                (v2 200.0f y)
                (if major then 2.5f else 0.5f)
                (if major then color 0.8f 0.8f 0.8f 1.0f else color 0.65f 0.65f 0.65f 0.8f)
                (if major then 1.02f else 1.0f)
                world

        SlugDemoContours.placeProjectiveComposite
            "Projection3dRing"
            ringComposite
            ringCenter
            // The SCALE=2 source extent is 83 units; keep the ring at that full fitted size.
            (v3 83.0f 83.0f 0.0f)
            Quaternion.Identity
            2.0f
            ringProjective
            world
