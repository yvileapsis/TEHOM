// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu

open System
open System.Collections.Generic
open System.Globalization
open System.Numerics
open System.Text.RegularExpressions
open Svg
open Svg.Pathing
open Svg.Transforms

/// A single analytic layer emitted by the SVG loader.  The arrays returned by
/// SlugSvgDocument are copies, so callers cannot mutate the document's state.
[<Struct>]
type SlugSvgLayer =
    { SourceIndex : int
      Transform : Matrix4x4
      Color : Color
      Opacity : single
      FillSource : SlugFillSource
      ClipIndex : int
      MaskIndex : int
      CompositeMode : SlugCompositeMode
      IsVisible : bool
      ElementId : string option }

/// An immutable analytic SVG document.  It contains only curve sources and
/// Slug resources; no renderer, GraphicsPath, tessellation, or raster data is
/// retained.
[<Sealed>]
type SlugSvgDocument internal
    (sources : SlugShapeSource array,
     layers : SlugSvgLayer array,
     gradients : SlugGradient array,
     gradientStops : SlugGradientStop array,
     masks : SlugMaskState array,
     bounds : SlugShapeBounds) =

    let sources = Array.copy sources
    let layers = Array.copy layers
    let gradients = Array.copy gradients
    let gradientStops = Array.copy gradientStops
    let masks = Array.copy masks
    let cloneSource (source : SlugShapeSource) =
        { source with Contours = source.Contours |> Array.map Array.copy }
    let cloneGradient (gradient : SlugGradient) =
        SlugGradient (gradient.Kind, Array.copy gradient.Stops, gradient.Transform, gradient.Extend)

    member _.Sources = sources |> Array.map cloneSource
    member _.Layers = Array.copy layers
    member _.Gradients = gradients |> Array.map cloneGradient
    member _.GradientStops = Array.copy gradientStops
    member _.Masks = Array.copy masks
    member _.Bounds = bounds

    member _.CreateComposite () : SlugCompositeShape =
        let data = SlugShapeRuntime.packWithResources sources gradients gradientStops masks [||]
        let layerStates =
            layers
            |> Array.map (fun layer ->
                let state = SlugLayerState.defaultState layer.SourceIndex
                let maskIndex = if layer.MaskIndex >= 0 then layer.MaskIndex else layer.ClipIndex
                { state with
                    Transform = layer.Transform
                    Color = layer.Color
                    FillSource = layer.FillSource
                    GradientTransform =
                        match layer.FillSource with
                        | SlugFillSource.Gradient gradientIndex ->
                            let mutable inverse = Matrix4x4.Identity
                            if not (Matrix4x4.Invert (gradients[gradientIndex].Transform, &inverse)) then
                                invalidOp "SVG gradient transform is singular."
                            inverse
                        | _ -> Matrix4x4.Identity
                    MaskIndex = maskIndex
                    MaterialValues = if maskIndex >= 0 then Vector4.UnitX else Vector4.Zero
                    CompositeMode = layer.CompositeMode })
        SlugShapeRuntime.createComposite data layerStates

[<RequireQualifiedAccess>]
module SlugSvg =

    let private invariant = CultureInfo.InvariantCulture
    let private pi = MathF.PI
    let private kappa = 0.5522847498307936f
    let private finite x = Single.IsFinite x
    let private epsilon = 1.0e-5f

    type private Style =
        { Fill : string
          FillOpacity : single
          Opacity : single
          FillRule : SlugFillRule
          Stroke : string
          StrokeWidth : single
          Visible : bool
          CompositeMode : SlugCompositeMode }

    let private failf fmt = Printf.ksprintf (fun text -> raise (FormatException text)) fmt

    let private parseSingle name (text : string) =
        let mutable value = 0.0f
        if String.IsNullOrWhiteSpace text || not (Single.TryParse(text.Trim(), NumberStyles.Float, invariant, &value)) || not (finite value) then
            failf "SVG %s must be a finite number; got '%s'." name text
        value

    let private parseOpacity name fallback (text : string option) =
        let value = defaultArg (text |> Option.map (parseSingle name)) fallback
        if value < 0.0f || value > 1.0f then failf "SVG %s must be in [0, 1]; got %g." name value
        value

    let private attr (element : SvgElement) name =
        let mutable value = null
        if element.TryGetAttribute(name, &value) then Some value else None

    let private attrOr name fallback element = defaultArg (attr element name) fallback

    let private parseCssColor (text : string) =
        let text = text.Trim()
        let fromDrawing (drawing : System.Drawing.Color) =
            Color
                (single drawing.R / 255.0f,
                 single drawing.G / 255.0f,
                 single drawing.B / 255.0f,
                 single drawing.A / 255.0f)
        let parseChannel (channel : string) =
            let channel = channel.Trim()
            if channel.EndsWith "%" then parseSingle "color channel" (channel.Substring(0, channel.Length - 1)) / 100.0f
            else parseSingle "color channel" channel / 255.0f
        if text.StartsWith("rgb(", StringComparison.OrdinalIgnoreCase) && text.EndsWith ")" then
            let values = text.Substring(4, text.Length - 5).Split([|','|], StringSplitOptions.RemoveEmptyEntries)
            if values.Length <> 3 then failf "SVG rgb color requires three channels; got '%s'." text
            Color (parseChannel values[0], parseChannel values[1], parseChannel values[2], 1.0f)
        elif text.StartsWith("rgba(", StringComparison.OrdinalIgnoreCase) && text.EndsWith ")" then
            let values = text.Substring(5, text.Length - 6).Split([|','|], StringSplitOptions.RemoveEmptyEntries)
            if values.Length <> 4 then failf "SVG rgba color requires four channels; got '%s'." text
            Color (parseChannel values[0], parseChannel values[1], parseChannel values[2], parseChannel values[3])
        elif text.StartsWith("#", StringComparison.Ordinal) then
            let hex = text.Substring 1
            let expand c = String [| c; c |]
            let hex =
                match hex.Length with
                | 3 -> String.Concat [| expand hex[0]; expand hex[1]; expand hex[2] |]
                | 4 -> String.Concat [| expand hex[0]; expand hex[1]; expand hex[2]; expand hex[3] |]
                | 6
                | 8 -> hex
                | _ -> failf "SVG color has invalid hexadecimal length: '%s'." text
            let component index = Byte.Parse(hex.Substring(index, 2), NumberStyles.HexNumber, invariant)
            let a = if hex.Length = 8 then component 6 else 255uy
            Color (single (component 0) / 255.0f, single (component 2) / 255.0f, single (component 4) / 255.0f, single a / 255.0f)
        else
            let drawing = System.Drawing.Color.FromName text
            if not drawing.IsKnownColor then
                try fromDrawing (System.Drawing.ColorTranslator.FromHtml text)
                with _ -> failf "SVG paint color is not supported: '%s'." text
            else fromDrawing drawing

    let private parseFillRule (text : string) =
        match text.Trim().ToLowerInvariant() with
        | "nonzero" -> SlugFillNonzero
        | "evenodd" -> SlugFillEvenOdd
        | value -> failf "SVG fill-rule '%s' is unsupported; use nonzero or evenodd." value

    let private parseCompositeMode (text : string option) =
        match defaultArg text "normal" |> fun (value : string) -> value.Trim().ToLowerInvariant() with
        | "normal" -> SlugCompositeMode.SlugCompositeSourceOver
        | "clear" -> SlugCompositeMode.SlugCompositeClear
        | "source" -> SlugCompositeMode.SlugCompositeSource
        | "destination" -> SlugCompositeMode.SlugCompositeDestination
        | "source-over" -> SlugCompositeMode.SlugCompositeSourceOver
        | "destination-over" -> SlugCompositeMode.SlugCompositeDestinationOver
        | "source-in" -> SlugCompositeMode.SlugCompositeSourceIn
        | "destination-in" -> SlugCompositeMode.SlugCompositeDestinationIn
        | "source-out" -> SlugCompositeMode.SlugCompositeSourceOut
        | "destination-out" -> SlugCompositeMode.SlugCompositeDestinationOut
        | "source-atop" -> SlugCompositeMode.SlugCompositeSourceAtop
        | "destination-atop" -> SlugCompositeMode.SlugCompositeDestinationAtop
        | "xor" -> SlugCompositeMode.SlugCompositeXor
        | "plus" | "plus-lighter" -> SlugCompositeMode.SlugCompositePlus
        | "screen" -> SlugCompositeMode.SlugCompositeScreen
        | "overlay" -> SlugCompositeMode.SlugCompositeOverlay
        | "darken" -> SlugCompositeMode.SlugCompositeDarken
        | "lighten" -> SlugCompositeMode.SlugCompositeLighten
        | "color-dodge" -> SlugCompositeMode.SlugCompositeColorDodge
        | "color-burn" -> SlugCompositeMode.SlugCompositeColorBurn
        | "hard-light" -> SlugCompositeMode.SlugCompositeHardLight
        | "soft-light" -> SlugCompositeMode.SlugCompositeSoftLight
        | "difference" -> SlugCompositeMode.SlugCompositeDifference
        | "exclusion" -> SlugCompositeMode.SlugCompositeExclusion
        | "multiply" -> SlugCompositeMode.SlugCompositeMultiply
        | "hue" -> SlugCompositeMode.SlugCompositeHslHue
        | "saturation" -> SlugCompositeMode.SlugCompositeHslSaturation
        | "color" -> SlugCompositeMode.SlugCompositeHslColor
        | "luminosity" -> SlugCompositeMode.SlugCompositeHslLuminosity
        | value -> failf "SVG mix-blend-mode '%s' is unsupported." value

    let private readStyle (parent : Style) (element : SvgElement) =
        let fill =
            match attr element "fill" with
            | Some value when value.Trim().Equals("inherit", StringComparison.OrdinalIgnoreCase) -> parent.Fill
            | Some value -> value
            | None -> parent.Fill
        let parseInheritedOpacity (name : string) (inherited : single) (fallback : single) (value : string option) =
            match value with
            | Some text when text.Trim().Equals("inherit", StringComparison.OrdinalIgnoreCase) -> inherited
            | Some text -> parseOpacity name fallback (Some text)
            | None -> fallback
        let fillOpacity = parseInheritedOpacity "fill-opacity" parent.FillOpacity parent.FillOpacity (attr element "fill-opacity")
        let opacity = parent.Opacity * parseInheritedOpacity "opacity" 1.0f 1.0f (attr element "opacity")
        let fillRule =
            match attr element "fill-rule" with
            | Some value when value.Trim().Equals("inherit", StringComparison.OrdinalIgnoreCase) -> parent.FillRule
            | Some value -> parseFillRule value
            | None -> parent.FillRule
        let stroke =
            match attr element "stroke" with
            | Some value when value.Trim().Equals("inherit", StringComparison.OrdinalIgnoreCase) -> parent.Stroke
            | Some value -> value
            | None -> parent.Stroke
        let strokeWidth =
            match attr element "stroke-width" with
            | Some value when value.Trim().Equals("inherit", StringComparison.OrdinalIgnoreCase) -> parent.StrokeWidth
            | Some value -> parseSingle "stroke-width" value
            | None -> parent.StrokeWidth
        let display = defaultArg (attr element "display") ""
        let visibility = defaultArg (attr element "visibility") ""
        let visible = parent.Visible && not (display.Equals("none", StringComparison.OrdinalIgnoreCase) || visibility.Equals("hidden", StringComparison.OrdinalIgnoreCase) || visibility.Equals("collapse", StringComparison.OrdinalIgnoreCase))
        let composite = match attr element "mix-blend-mode" with | Some value -> parseCompositeMode (Some value) | None -> parent.CompositeMode
        { Fill = fill
          FillOpacity = fillOpacity
          Opacity = opacity
          FillRule = fillRule
          Stroke = stroke
          StrokeWidth = strokeWidth
          Visible = visible
          CompositeMode = composite }

    let private unitValue (unit : SvgUnit) =
        if unit.IsNone || unit.IsEmpty then 0.0f
        elif unit.Type = SvgUnitType.Percentage then unit.Value / 100.0f
        else unit.Value

    let private pointOf (x : SvgUnit) (y : SvgUnit) = Vector2 (unitValue x, unitValue y)
    let private drawingPoint (p : System.Drawing.PointF) = Vector2 (p.X, p.Y)

    let private transformOf (transform : SvgTransform) =
        match transform with
        | :? SvgMatrix as matrix ->
            if matrix.Points = null || matrix.Points.Count <> 6 then failf "SVG matrix transform must contain six values."
            let p = matrix.Points |> Seq.map single |> Seq.toArray
            Matrix4x4 (p[0], p[1], 0.0f, 0.0f, p[2], p[3], 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, p[4], p[5], 0.0f, 1.0f)
        | :? SvgTranslate as translate -> Matrix4x4.CreateTranslation (translate.X, translate.Y, 0.0f)
        | :? SvgScale as scale -> Matrix4x4.CreateScale (scale.X, scale.Y, 1.0f)
        | :? SvgRotate as rotate ->
            Matrix4x4.CreateTranslation(-rotate.CenterX, -rotate.CenterY, 0.0f) * Matrix4x4.CreateRotationZ (rotate.Angle * pi / 180.0f) * Matrix4x4.CreateTranslation(rotate.CenterX, rotate.CenterY, 0.0f)
        | :? SvgSkew as skew ->
            let tx = MathF.Tan (skew.AngleX * pi / 180.0f)
            let ty = MathF.Tan (skew.AngleY * pi / 180.0f)
            Matrix4x4 (1.0f, ty, 0.0f, 0.0f, tx, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f)
        | unsupported -> failf "SVG transform '%s' is unsupported." (unsupported.GetType().Name)

    let private localTransform (element : SvgElement) =
        let transformable = element :> ISvgTransformable
        if isNull transformable.Transforms then Matrix4x4.Identity
        else transformable.Transforms |> Seq.fold (fun matrix transform -> matrix * transformOf transform) Matrix4x4.Identity

    let private composeTransform (parent : Matrix4x4) (element : SvgElement) = localTransform element * parent

    let private transformPoint (matrix : Matrix4x4) (point : Vector2) = Vector2.Transform(point, matrix)

    let private mapCommand matrix command =
        match command with
        | MoveTo endpoint -> MoveTo (transformPoint matrix endpoint)
        | LineTo endpoint -> LineTo (transformPoint matrix endpoint)
        | QuadraticCurveTo (control, endpoint) -> QuadraticCurveTo (transformPoint matrix control, transformPoint matrix endpoint)
        | CubicCurveTo (control1, control2, endpoint) -> CubicCurveTo (transformPoint matrix control1, transformPoint matrix control2, transformPoint matrix endpoint)
        | CloseContour -> CloseContour

    /// Convert an SVG elliptical arc to the deterministic cubic sequence from
    /// the SVG implementation notes.  SlugShapeRuntime performs the subsequent
    /// adaptive cubic-to-quadratic conversion.
    let private arcCubics p0 p1 rx ry angle large sweep =
        let rx = abs (double rx)
        let ry = abs (double ry)
        if Vector2.DistanceSquared(p0, p1) <= epsilon * epsilon then []
        elif rx <= double epsilon || ry <= double epsilon then [ CubicCurveTo (p0, (p0 + p1) * 0.5f, p1) ]
        else
            let phi = double angle * Math.PI / 180.0
            let cosPhi = Math.Cos phi
            let sinPhi = Math.Sin phi
            let x1 = double p0.X
            let y1 = double p0.Y
            let x2 = double p1.X
            let y2 = double p1.Y
            let dx = (x1 - x2) / 2.0
            let dy = (y1 - y2) / 2.0
            let x1p = cosPhi * dx + sinPhi * dy
            let y1p = -sinPhi * dx + cosPhi * dy
            let lambda = x1p * x1p / (rx * rx) + y1p * y1p / (ry * ry)
            let rx, ry = if lambda > 1.0 then rx * sqrt lambda, ry * sqrt lambda else rx, ry
            let numerator = max 0.0 ((rx * rx * ry * ry) - (rx * rx * y1p * y1p) - (ry * ry * x1p * x1p))
            let denominator = rx * rx * y1p * y1p + ry * ry * x1p * x1p
            let coefficient = if denominator <= Double.Epsilon then 0.0 else sqrt (numerator / denominator) * (if large = sweep then -1.0 else 1.0)
            let cxp = coefficient * (rx * y1p / ry)
            let cyp = coefficient * (-ry * x1p / rx)
            let cx = cosPhi * cxp - sinPhi * cyp + (x1 + x2) / 2.0
            let cy = sinPhi * cxp + cosPhi * cyp + (y1 + y2) / 2.0
            let ux = (x1p - cxp) / rx
            let uy = (y1p - cyp) / ry
            let vx = (-x1p - cxp) / rx
            let vy = (-y1p - cyp) / ry
            let angleBetween ax ay bx by =
                let cross = ax * by - ay * bx
                let dot = ax * bx + ay * by
                Math.Atan2(cross, dot)
            let theta1 = angleBetween 1.0 0.0 ux uy
            let mutable delta = angleBetween ux uy vx vy
            if not sweep && delta > 0.0 then delta <- delta - 2.0 * Math.PI
            elif sweep && delta < 0.0 then delta <- delta + 2.0 * Math.PI
            let count = max 1 (int (Math.Ceiling (abs delta / (Math.PI / 2.0))))
            let step = delta / double count
            [ for index in 0 .. count - 1 do
                let a0 = theta1 + double index * step
                let a1 = a0 + step
                let alpha = 4.0 / 3.0 * Math.Tan ((a1 - a0) / 4.0)
                let point a =
                    Vector2 (single (cx + rx * cosPhi * Math.Cos a - ry * sinPhi * Math.Sin a), single (cy + rx * sinPhi * Math.Cos a + ry * cosPhi * Math.Sin a))
                let tangent a =
                    Vector2 (single (-rx * cosPhi * Math.Sin a - ry * sinPhi * Math.Cos a), single (-rx * sinPhi * Math.Sin a + ry * cosPhi * Math.Cos a))
                let a = point a0
                let b = point a1
                let c1 = a + tangent a0 * single alpha
                let c2 = b - tangent a1 * single alpha
                yield CubicCurveTo (c1, c2, b) ]

    let private commandsForPath (path : SvgPath) =
        if isNull path.PathData then failf "SVG path has no d/path data."
        let commands = ResizeArray<ContourCommand> ()
        let mutable point = Vector2.Zero
        let mutable start = Vector2.Zero
        let mutable openContour = false
        let mutable previousCubicControl = None
        let mutable previousQuadraticControl = None
        let mutable previousWasCubic = false
        let mutable previousWasQuadratic = false
        let resolve (endpoint : System.Drawing.PointF) =
            if Single.IsNaN endpoint.X || Single.IsNaN endpoint.Y then failf "SVG path has a non-finite endpoint."
            elif endpoint.X = endpoint.X && endpoint.Y = endpoint.Y then endpoint
            else failf "SVG path has a malformed endpoint."
        let resolvePoint (p : System.Drawing.PointF) (relative : bool) (origin : Vector2) =
            let p = drawingPoint (resolve p)
            if relative then origin + p else p
        let beginContour p =
            if openContour then failf "SVG path contains an open subpath before MoveTo."
            commands.Add (MoveTo p)
            point <- p
            start <- p
            openContour <- true
            previousWasCubic <- false
            previousWasQuadratic <- false
        for segment in path.PathData do
            match segment with
            | :? SvgMoveToSegment as move -> beginContour (resolvePoint move.End move.IsRelative point)
            | :? SvgLineSegment as line ->
                if not openContour then failf "SVG line segment appears before MoveTo."
                let endpoint =
                    if Single.IsNaN line.End.X then Vector2 (point.X, (if line.IsRelative then point.Y + line.End.Y else line.End.Y))
                    elif Single.IsNaN line.End.Y then Vector2 ((if line.IsRelative then point.X + line.End.X else line.End.X), point.Y)
                    else resolvePoint line.End line.IsRelative point
                commands.Add (LineTo endpoint)
                point <- endpoint
                previousWasCubic <- false
                previousWasQuadratic <- false
            | :? SvgQuadraticCurveSegment as quadratic ->
                if not openContour then failf "SVG quadratic segment appears before MoveTo."
                let endpoint = resolvePoint quadratic.End quadratic.IsRelative point
                let control =
                    if Single.IsNaN quadratic.ControlPoint.X || Single.IsNaN quadratic.ControlPoint.Y then
                        if previousWasQuadratic then 2.0f * point - defaultArg previousQuadraticControl point else point
                    else resolvePoint quadratic.ControlPoint quadratic.IsRelative point
                commands.Add (QuadraticCurveTo (control, endpoint))
                previousQuadraticControl <- Some control
                previousWasQuadratic <- true
                previousWasCubic <- false
                point <- endpoint
            | :? SvgCubicCurveSegment as cubic ->
                if not openContour then failf "SVG cubic segment appears before MoveTo."
                let endpoint = resolvePoint cubic.End cubic.IsRelative point
                let control2 = resolvePoint cubic.SecondControlPoint cubic.IsRelative point
                let control1 =
                    if Single.IsNaN cubic.FirstControlPoint.X || Single.IsNaN cubic.FirstControlPoint.Y then
                        if previousWasCubic then 2.0f * point - defaultArg previousCubicControl point else point
                    else resolvePoint cubic.FirstControlPoint cubic.IsRelative point
                commands.Add (CubicCurveTo (control1, control2, endpoint))
                previousCubicControl <- Some control2
                previousWasCubic <- true
                previousWasQuadratic <- false
                point <- endpoint
            | :? SvgArcSegment as arc ->
                if not openContour then failf "SVG arc segment appears before MoveTo."
                let endpoint = resolvePoint arc.End arc.IsRelative point
                for command in arcCubics point endpoint arc.RadiusX arc.RadiusY arc.Angle (arc.Size = SvgArcSize.Large) (arc.Sweep = SvgArcSweep.Positive) do commands.Add command
                point <- endpoint
                previousWasCubic <- false
                previousWasQuadratic <- false
            | :? SvgClosePathSegment ->
                if not openContour then failf "SVG close segment appears without an open subpath."
                commands.Add CloseContour
                point <- start
                openContour <- false
                previousWasCubic <- false
                previousWasQuadratic <- false
            | unsupported -> failf "SVG path segment '%s' is unsupported." (unsupported.GetType().Name)
        if openContour then failf "SVG path ends with an open subpath; every subpath must use Z/CloseContour."
        if commands.Count = 0 then failf "SVG path has no commands."
        commands |> Seq.toList


    let private rectangleCommands x y width height rx ry =
        if width <= 0.0f || height <= 0.0f then failf "SVG rect must have positive width and height."
        let rx = max 0.0f (min (abs rx) (width / 2.0f))
        let ry = max 0.0f (min (abs ry) (height / 2.0f))
        if rx = 0.0f || ry = 0.0f then
            [ MoveTo (Vector2 (x, y)); LineTo (Vector2 (x + width, y)); LineTo (Vector2 (x + width, y + height)); LineTo (Vector2 (x, y + height)); CloseContour ]
        else
            let c = kappa
            [ MoveTo (Vector2 (x + rx, y))
              LineTo (Vector2 (x + width - rx, y))
              CubicCurveTo (Vector2 (x + width - rx + c * rx, y), Vector2 (x + width, y + ry - c * ry), Vector2 (x + width, y + ry))
              LineTo (Vector2 (x + width, y + height - ry))
              CubicCurveTo (Vector2 (x + width, y + height - ry + c * ry), Vector2 (x + width - rx + c * rx, y + height), Vector2 (x + width - rx, y + height))
              LineTo (Vector2 (x + rx, y + height))
              CubicCurveTo (Vector2 (x + rx - c * rx, y + height), Vector2 (x, y + height - ry + c * ry), Vector2 (x, y + height - ry))
              LineTo (Vector2 (x, y + ry))
              CubicCurveTo (Vector2 (x, y + ry - c * ry), Vector2 (x + rx - c * rx, y), Vector2 (x + rx, y))
              CloseContour ]

    let private ellipseCommands cx cy rx ry =
        if rx <= 0.0f || ry <= 0.0f then failf "SVG ellipse/circle radii must be positive."
        let c = kappa
        [ MoveTo (Vector2 (cx + rx, cy))
          CubicCurveTo (Vector2 (cx + rx, cy + c * ry), Vector2 (cx + c * rx, cy + ry), Vector2 (cx, cy + ry))
          CubicCurveTo (Vector2 (cx - c * rx, cy + ry), Vector2 (cx - rx, cy + c * ry), Vector2 (cx - rx, cy))
          CubicCurveTo (Vector2 (cx - rx, cy - c * ry), Vector2 (cx - c * rx, cy - ry), Vector2 (cx, cy - ry))
          CubicCurveTo (Vector2 (cx + c * rx, cy - ry), Vector2 (cx + rx, cy - c * ry), Vector2 (cx + rx, cy))
          CloseContour ]

    let private pointsCommands close (points : SvgPointCollection) =
        if isNull points || points.Count < 2 || points.Count % 2 <> 0 then failf "SVG polygon/polyline requires an even, non-empty points list."
        let points = [ for i in 0 .. 2 .. points.Count - 2 -> pointOf points[i] points[i + 1] ]
        if not close then failf "SVG polyline is open and cannot be used as an analytic fill."
        MoveTo points[0] :: (points |> List.skip 1 |> List.map LineTo) @ [ CloseContour ]

    let private geometryCommands (element : SvgElement) =
        match element with
        | :? SvgPath as path -> commandsForPath path
        | :? SvgRectangle as rect -> rectangleCommands (unitValue rect.X) (unitValue rect.Y) (unitValue rect.Width) (unitValue rect.Height) (unitValue rect.CornerRadiusX) (unitValue rect.CornerRadiusY)
        | :? SvgCircle as circle -> ellipseCommands (unitValue circle.CenterX) (unitValue circle.CenterY) (unitValue circle.Radius) (unitValue circle.Radius)
        | :? SvgEllipse as ellipse -> ellipseCommands (unitValue ellipse.CenterX) (unitValue ellipse.CenterY) (unitValue ellipse.RadiusX) (unitValue ellipse.RadiusY)
        | :? SvgPolyline as polyline -> pointsCommands false polyline.Points
        | :? SvgPolygon as polygon -> pointsCommands true polygon.Points
        | _ -> failf "SVG element '%s' has no supported analytic geometry." (element.GetType().Name)

    let private parseUrlReference (text : string) =
        let text = text.Trim()
        let matchValue = Regex.Match(text, "^url\\(\\s*#([^\\)]+)\\s*\\)$", RegexOptions.IgnoreCase)
        if matchValue.Success then Some matchValue.Groups[1].Value else None

    let private gradientUnit (unit : SvgUnit) = if unit.Type = SvgUnitType.Percentage then unit.Value / 100.0f else unit.Value

    let private gradientTransform (gradient : SvgGradientServer) =
        let transformable = gradient :> ISvgTransformable
        if isNull transformable.Transforms then Matrix4x4.Identity
        else transformable.Transforms |> Seq.fold (fun matrix transform -> matrix * transformOf transform) Matrix4x4.Identity
    let private gradientExtend (gradient : SvgGradientServer) =
        match attr gradient "spreadMethod" |> Option.map (fun value -> value.Trim().ToLowerInvariant()) with
        | Some "repeat" -> SlugGradientRepeat
        | Some "reflect" -> SlugGradientReflect
        | Some "pad"
        | None -> SlugGradientPad
        | Some value -> failf "SVG gradient spreadMethod '%s' is unsupported." value

    let private colorFromPaint (text : string option) (fallback : Color) =
        match text with
        | Some value when value.Trim().Equals("none", StringComparison.OrdinalIgnoreCase) -> fallback
        | Some value -> parseCssColor value
        | None -> fallback

    let private transformBounds (matrix : Matrix4x4) (bounds : SlugShapeBounds) =
        let corners =
            [| Vector2 (bounds.Min.X, bounds.Min.Y); Vector2 (bounds.Max.X, bounds.Min.Y); Vector2 (bounds.Max.X, bounds.Max.Y); Vector2 (bounds.Min.X, bounds.Max.Y) |]
            |> Array.map (transformPoint matrix)
        let minX = corners |> Array.minBy (fun p -> p.X) |> fun p -> p.X
        let minY = corners |> Array.minBy (fun p -> p.Y) |> fun p -> p.Y
        let maxX = corners |> Array.maxBy (fun p -> p.X) |> fun p -> p.X
        let maxY = corners |> Array.maxBy (fun p -> p.Y) |> fun p -> p.Y
        { Min = Vector2 (minX, minY); Max = Vector2 (maxX, maxY) }

    let private loadInternal filePath tolerance =
        if String.IsNullOrWhiteSpace filePath then invalidArg (nameof filePath) "SVG file path cannot be empty."
        if not (finite tolerance) || tolerance <= 0.0f then invalidArg (nameof tolerance) "SVG cubic tolerance must be finite and positive."
        // Parsing analytic geometry does not use System.Drawing; only Svg.NET's
        // global capability probe does. Skip that probe so non-Windows hosts can
        // parse the document without pretending that GDI+ is a renderer dependency.
        SvgDocument.SkipGdiPlusCapabilityCheck <- true
        let document = SvgDocument.Open filePath
        let sourceList = ResizeArray<SlugShapeSource> ()
        let layerList = ResizeArray<SlugSvgLayer> ()
        let maskList = ResizeArray<SlugMaskState> ()
        let gradientList = ResizeArray<SlugGradient> ()
        let gradientStopList = ResizeArray<SlugGradientStop> ()
        let gradientIndices = Dictionary<string, int> (StringComparer.Ordinal)
        let gradientObjectBoundingBox = ResizeArray<bool> ()

        let rec allElements (element : SvgElement) = seq {
            yield element
            for child in element.Children do yield! allElements child }

        let addGradient (gradient : SvgGradientServer) (kind : SlugGradientKind) =
            let id = gradient.ID
            if String.IsNullOrWhiteSpace id then failf "SVG gradient server is missing an id."
            if not (gradientIndices.ContainsKey id) then
                let stops =
                    [| for stop in gradient.Stops do
                        let color = colorFromPaint (attr stop "stop-color") (Color (0.0f, 0.0f, 0.0f, 1.0f))
                        let opacity = parseOpacity "stop-opacity" 1.0f (attr stop "stop-opacity")
                        let offset = unitValue stop.Offset
                        if offset < 0.0f || offset > 1.0f then failf "SVG gradient stop offset must be in [0, 1]."
                        yield { Offset = offset; Color = color.MapA (fun alpha -> alpha * opacity) } |]
                if stops.Length = 0 then failf "SVG gradient '%s' has no stops." id
                if stops.Length > 1 then
                    for index in 1 .. stops.Length - 1 do
                        if stops[index].Offset < stops[index - 1].Offset then failf "SVG gradient '%s' has unsorted stops." id
                let gradientIndex = gradientList.Count
                gradientIndices.Add(id, gradientIndex)
                let objectBoundingBox =
                    match attr gradient "gradientUnits" |> Option.map (fun value -> value.Trim().ToLowerInvariant()) with
                    | None
                    | Some "objectboundingbox" -> true
                    | Some "userspaceonuse" -> false
                    | Some value -> failf "SVG gradientUnits '%s' is unsupported." value
                gradientList.Add (SlugGradient (kind, stops, gradientTransform gradient, gradientExtend gradient))
                gradientObjectBoundingBox.Add objectBoundingBox
                gradientStopList.AddRange stops

        for element in allElements document do
            match element with
            | :? SvgLinearGradientServer as linear ->
                addGradient linear (SlugGradientKind.Linear (pointOf linear.X1 linear.Y1, pointOf linear.X2 linear.Y2))
            | :? SvgRadialGradientServer as radial ->
                addGradient radial (SlugGradientKind.Radial (pointOf radial.CenterX radial.CenterY, Vector2 (gradientUnit radial.Radius, gradientUnit radial.Radius)))
            | _ -> ()

        let initialStyle =
            { Fill = "#000000"
              FillOpacity = 1.0f
              Opacity = 1.0f
              FillRule = SlugFillNonzero
              Stroke = "none"
              StrokeWidth = 1.0f
              Visible = true
              CompositeMode = SlugCompositeMode.SlugCompositeSourceOver }

        let makeSource commands fillRule =
            try SlugShapeRuntime.fromContourCommands commands fillRule tolerance
            with ex -> failf "SVG analytic contour conversion failed: %s" ex.Message

        let encodeMaskTransform (matrix : Matrix4x4) =
            let mutable inverse = Matrix4x4.Identity
            if not (Matrix4x4.Invert (matrix, &inverse)) then failf "SVG clip or mask transform is singular."
            { Kind = SlugMaskKind.Shape (sourceList.Count - 1)
              Parameters = Vector4 (inverse.M11, inverse.M21, inverse.M41, inverse.M12)
              Parameters2 = Vector4 (inverse.M22, inverse.M42, 0.0f, 0.0f)
              Invert = false }

        let addMask commands fillRule transform =
            let source = makeSource commands fillRule
            sourceList.Add source
            let index = maskList.Count
            maskList.Add (encodeMaskTransform transform)
            index
        let rec referencedCommands parentTransform (element : SvgElement) =
            let transform = composeTransform parentTransform element
            match element with
            | :? SvgPath
            | :? SvgRectangle
            | :? SvgCircle
            | :? SvgEllipse
            | :? SvgPolyline
            | :? SvgPolygon -> geometryCommands element |> List.map (mapCommand transform)
            | _ -> [ for child in element.Children do yield! referencedCommands transform child ]

        let addReference raw parentTransform fillRule =
            match parseUrlReference raw with
            | None -> -1
            | Some id ->
                let referenced = document.GetElementById id
                if isNull referenced then failf "SVG reference '#%s' was not found." id
                match referenced with
                | :? SvgClipPath as clip ->
                    let commands = [ for child in clip.Children do yield! referencedCommands Matrix4x4.Identity child ]
                    if List.isEmpty commands then failf "SVG clipPath '#%s' has no analytic geometry." id
                    addMask commands fillRule (localTransform clip * parentTransform)
                | :? SvgMask as mask ->
                    let commands = [ for child in mask.Children do yield! referencedCommands Matrix4x4.Identity child ]
                    if List.isEmpty commands then failf "SVG mask '#%s' has no analytic geometry." id
                    addMask commands fillRule (localTransform mask * parentTransform)
                | unsupported -> failf "SVG reference '#%s' targets unsupported element '%s'." id (unsupported.GetType().Name)

        let addVisual element transform style =
            let fillText = style.Fill.Trim()
            let hasStroke = not (String.IsNullOrWhiteSpace style.Stroke) && not (style.Stroke.Trim().Equals("none", StringComparison.OrdinalIgnoreCase)) && style.StrokeWidth > 0.0f
            if hasStroke then failf "SVG element '%s' uses unsupported stroke-only or stroked paint." (element.GetType().Name)
            if attr element "filter" |> Option.exists (fun value -> not (value.Trim().Equals("none", StringComparison.OrdinalIgnoreCase))) then failf "SVG element '%s' uses unsupported filter; analytic SVG does not rasterize filters." (element.GetType().Name)
            if fillText.Equals("none", StringComparison.OrdinalIgnoreCase) then
                if hasStroke then failf "SVG element '%s' has no fill and a stroke; stroke-only SVG is unsupported." (element.GetType().Name)
            elif fillText.StartsWith("url(", StringComparison.OrdinalIgnoreCase) then
                let reference =
                    match parseUrlReference fillText with
                    | Some reference -> reference
                    | None -> failf "SVG fill '%s' is malformed; expected url(#id)." fillText
                let gradientTemplateIndex =
                    match gradientIndices.TryGetValue reference with
                    | true, index -> index
                    | _ -> failf "SVG fill gradient '#%s' was not found or is not a gradient server." reference
                let source = makeSource (geometryCommands element |> List.map (mapCommand Matrix4x4.Identity)) style.FillRule
                let sourceIndex = sourceList.Count
                sourceList.Add source
                let gradientIndex =
                    if gradientObjectBoundingBox[gradientTemplateIndex] then
                        let template = gradientList[gradientTemplateIndex]
                        let extent = source.Bounds.Max - source.Bounds.Min
                        if extent.X <= 0.0f || extent.Y <= 0.0f then
                            failf "SVG objectBoundingBox gradient '#%s' has degenerate geometry bounds." reference
                        let objectBounds =
                            Matrix4x4.CreateScale (extent.X, extent.Y, 1.0f) *
                            Matrix4x4.CreateTranslation (source.Bounds.Min.X, source.Bounds.Min.Y, 0.0f)
                        let instance =
                            SlugGradient
                                (template.Kind,
                                 template.Stops,
                                 template.Transform * objectBounds,
                                 template.Extend)
                        let index = gradientList.Count
                        gradientList.Add instance
                        gradientObjectBoundingBox.Add false
                        gradientStopList.AddRange instance.Stops
                        index
                    else gradientTemplateIndex
                let visibilityAlpha = if style.Visible then 1.0f else 0.0f
                let color = Color.One.MapA (fun alpha -> alpha * style.Opacity * style.FillOpacity * visibilityAlpha)
                let clipIndex = addReference (attrOr "clip-path" "" element) transform style.FillRule
                let maskIndex = addReference (attrOr "mask" "" element) transform style.FillRule
                layerList.Add
                    { SourceIndex = sourceIndex
                      Transform = transform
                      Color = color
                      Opacity = style.Opacity * style.FillOpacity
                      FillSource = SlugFillSource.Gradient gradientIndex
                      ClipIndex = clipIndex
                      MaskIndex = maskIndex
                      CompositeMode = style.CompositeMode
                      IsVisible = style.Visible
                      ElementId = if String.IsNullOrWhiteSpace element.ID then None else Some element.ID }
            else
                let source = makeSource (geometryCommands element |> List.map (mapCommand Matrix4x4.Identity)) style.FillRule
                let sourceIndex = sourceList.Count
                sourceList.Add source
                let baseColor = parseCssColor fillText
                let visibilityAlpha = if style.Visible then 1.0f else 0.0f
                let color = baseColor.MapA (fun alpha -> alpha * style.Opacity * style.FillOpacity * visibilityAlpha)
                let clipIndex = addReference (attrOr "clip-path" "" element) transform style.FillRule
                let maskIndex = addReference (attrOr "mask" "" element) transform style.FillRule
                layerList.Add
                    { SourceIndex = sourceIndex
                      Transform = transform
                      Color = color
                      Opacity = style.Opacity * style.FillOpacity
                      FillSource = SlugFillSource.Solid
                      ClipIndex = clipIndex
                      MaskIndex = maskIndex
                      CompositeMode = style.CompositeMode
                      IsVisible = style.Visible
                      ElementId = if String.IsNullOrWhiteSpace element.ID then None else Some element.ID }

        let rec walk parentTransform parentStyle (element : SvgElement) =
            let style = readStyle parentStyle element
            let transform = composeTransform parentTransform element
            let typeName = element.GetType().Name
            if not style.Visible && (attr element "display" |> Option.exists (fun value -> value.Trim().Equals("none", StringComparison.OrdinalIgnoreCase))) then ()
            elif typeName.Equals("SvgDocument", StringComparison.Ordinal) || typeName.Equals("SvgFragment", StringComparison.Ordinal) || typeName.Equals("SvgGroup", StringComparison.Ordinal) then
                for child in element.Children do walk transform style child
            elif typeName.Equals("SvgDefinitionList", StringComparison.Ordinal) || typeName.Equals("SvgClipPath", StringComparison.Ordinal) || typeName.Equals("SvgMask", StringComparison.Ordinal) || typeName.Equals("SvgLinearGradientServer", StringComparison.Ordinal) || typeName.Equals("SvgRadialGradientServer", StringComparison.Ordinal) || typeName.Equals("SvgGradientStop", StringComparison.Ordinal) then ()
            elif typeName.Equals("SvgText", StringComparison.Ordinal) || typeName.Equals("SvgTspan", StringComparison.Ordinal) || typeName.Equals("SvgForeignObject", StringComparison.Ordinal) then failf "SVG element '%s' is unsupported by analytic Slug SVG loading." typeName
            elif typeName.Equals("SvgImage", StringComparison.Ordinal) || typeName.Equals("SvgPatternServer", StringComparison.Ordinal) then failf "SVG image/pattern content is unsupported by analytic Slug SVG loading."
            elif element :? SvgPath || element :? SvgRectangle || element :? SvgCircle || element :? SvgEllipse || element :? SvgPolygon || element :? SvgPolyline then addVisual element transform style
            elif element.Children.Count > 0 then for child in element.Children do walk transform style child
            else failf "SVG element '%s' is unsupported by analytic Slug SVG loading." typeName

        for child in document.Children do walk Matrix4x4.Identity initialStyle child
        if sourceList.Count = 0 || not (layerList |> Seq.exists (fun layer -> layer.IsVisible)) then failf "SVG document contains no visible analytic filled shapes."
        let mutable minPoint = Vector2 (Single.PositiveInfinity, Single.PositiveInfinity)
        let mutable maxPoint = Vector2 (Single.NegativeInfinity, Single.NegativeInfinity)
        for layer in layerList do
            let layerBounds = transformBounds layer.Transform sourceList[layer.SourceIndex].Bounds
            minPoint <- Vector2 (min minPoint.X layerBounds.Min.X, min minPoint.Y layerBounds.Min.Y)
            maxPoint <- Vector2 (max maxPoint.X layerBounds.Max.X, max maxPoint.Y layerBounds.Max.Y)
        let bounds = { Min = minPoint; Max = maxPoint }
        SlugSvgDocument (sourceList.ToArray (), layerList.ToArray (), gradientList.ToArray (), gradientStopList.ToArray (), maskList.ToArray (), bounds)

    let load filePath tolerance = loadInternal filePath tolerance

    let tryLoad filePath tolerance =
        try Ok (load filePath tolerance)
        with ex -> Error ex.Message

