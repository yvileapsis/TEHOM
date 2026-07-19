// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu

open System
open System.Numerics
open System.Runtime.InteropServices
open System.Threading
open Prime

/// The fill rule used by Slug contour evaluation.
[<Struct>]
type SlugFillRule =
    | SlugFillNonzero
    | SlugFillEvenOdd
/// Porter-Duff and advanced blend modes used by Slug layers and COLR paint groups.
type SlugCompositeMode =
    | SlugCompositeClear = 0
    | SlugCompositeSource = 1
    | SlugCompositeDestination = 2
    | SlugCompositeSourceOver = 3
    | SlugCompositeDestinationOver = 4
    | SlugCompositeSourceIn = 5
    | SlugCompositeDestinationIn = 6
    | SlugCompositeSourceOut = 7
    | SlugCompositeDestinationOut = 8
    | SlugCompositeSourceAtop = 9
    | SlugCompositeDestinationAtop = 10
    | SlugCompositeXor = 11
    | SlugCompositePlus = 12
    | SlugCompositeScreen = 13
    | SlugCompositeOverlay = 14
    | SlugCompositeDarken = 15
    | SlugCompositeLighten = 16
    | SlugCompositeColorDodge = 17
    | SlugCompositeColorBurn = 18
    | SlugCompositeHardLight = 19
    | SlugCompositeSoftLight = 20
    | SlugCompositeDifference = 21
    | SlugCompositeExclusion = 22
    | SlugCompositeMultiply = 23
    | SlugCompositeHslHue = 24
    | SlugCompositeHslSaturation = 25
    | SlugCompositeHslColor = 26
    | SlugCompositeHslLuminosity = 27

/// One quadratic Bezier segment in shape-local em coordinates.
[<Struct; StructLayout (LayoutKind.Sequential)>]
type SlugCurve =
    { P1 : Vector2
      P2 : Vector2
      P3 : Vector2 }

/// Bounds of a Slug shape in shape-local em coordinates.
[<Struct; StructLayout (LayoutKind.Sequential)>]
type SlugShapeBounds =
    { Min : Vector2
      Max : Vector2 }

/// A closed set of quadratic contours before packing into GPU textures.
type [<NoEquality; NoComparison>] SlugShapeSource =
    { Contours : SlugCurve array array
      Bounds : SlugShapeBounds
      FillRule : SlugFillRule
      Advance : single option }

/// One stop in a Slug gradient.
[<Struct; StructLayout (LayoutKind.Sequential)>]
type SlugGradientStop =
    { Offset : single
      Color : Color }
/// Color-line extension outside the first and last gradient stop.
[<Struct>]
type SlugGradientExtend =
    | SlugGradientPad
    | SlugGradientRepeat
    | SlugGradientReflect


/// Coordinates used to evaluate a gradient in layer em space.
[<Struct; RequireQualifiedAccess>]
type SlugGradientKind =
    | Linear of Start : Vector2 * End : Vector2
    | Radial of Center : Vector2 * Radius : Vector2
    | FocalRadial of Center0 : Vector2 * Radius0 : single * Center1 : Vector2 * Radius1 : single
    | Sweep of Center : Vector2 * StartAngle : single
    | SweepRange of Center : Vector2 * StartAngle : single * EndAngle : single

/// A gradient owns one immutable stop array and a transform that may be changed between draws.
[<Sealed>]
type SlugGradient
    (kind : SlugGradientKind,
     stops : SlugGradientStop array,
     ?transform : Matrix4x4,
     ?extend : SlugGradientExtend) =
    let stops =
        if isNull stops then nullArg (nameof stops)
        Array.copy stops
    let mutable transform = defaultArg transform Matrix4x4.Identity
    let extend = defaultArg extend SlugGradientPad

    do
        if stops.Length = 0 then invalidArg (nameof stops) "A Slug gradient needs at least one stop."
        let mutable previous = Single.NegativeInfinity
        for stop in stops do
            if not (Single.IsFinite stop.Offset) || stop.Offset < 0.0f || stop.Offset > 1.0f then
                invalidArg (nameof stops) "Gradient stop offsets must be finite and in [0, 1]."
            if stop.Offset < previous then
                invalidArg (nameof stops) "Gradient stops must be sorted by offset."
            previous <- stop.Offset

    member _.Kind = kind
    member _.Stops = stops
    member _.Extend = extend
    member _.Transform
        with get () = transform
        and set value = transform <- value

/// The source used for a layer's fill operation.
[<Struct; RequireQualifiedAccess>]
type SlugFillSource =
    | Solid
    | Gradient of Index : int
    | Texture of Slot : int
    | Procedural of EffectId : int
    | PbrMaterial of Index : int

/// The analytic mask primitive selected by a layer.
[<RequireQualifiedAccess>]
type SlugMaskKind =
    | None
    | MsdfLayer of LayerIndex : int
    | Shape of ShapeIndex : int
    | Circle
    | Rectangle
    | Capsule
    | Arc
    | ArcBand
    | Hexagon
    | Octagon
    | Star

/// GPU-friendly parameters for one analytic mask.
[<Struct; StructLayout (LayoutKind.Sequential)>]
type SlugMaskState =
    { Kind : SlugMaskKind
      Parameters : Vector4
      Parameters2 : Vector4
      Invert : bool }

/// Static mapped data for a shape that is evaluated over a mesh in 3D.
type [<NoEquality; NoComparison>] SlugMappedMesh =
    { Positions : Vector3 array
      Normals : Vector3 array
      Tangents : Vector4 array
      EmCoordinates : Vector2 array
      Indices : uint32 array }

/// Mutable per-layer state. Matrix and vector fields are deliberately kept in vec4-sized groups.
[<Struct; StructLayout (LayoutKind.Sequential)>]
type SlugLayerState =
    { ShapeIndex : int
      Transform : Matrix4x4
      Origin : Vector2
      Color : Color
      FillSource : SlugFillSource
      EffectId : int
      EffectParameters : Vector4
      EffectParameters2 : Vector4
      GradientTransform : Matrix4x4
      MaskIndex : int
      MaskKind : SlugMaskKind
      MaskParameters : Vector4
      MaskParameters2 : Vector4
      MaskInvert : bool
      CompositeMode : SlugCompositeMode
      MaterialValues : Vector4 }

[<RequireQualifiedAccess>]
module SlugLayerState =
    let defaultState shapeIndex =
        { ShapeIndex = shapeIndex
          Transform = Matrix4x4.Identity
          Origin = Vector2.Zero
          Color = Color.One
          FillSource = SlugFillSource.Solid
          EffectId = 0
          EffectParameters = Vector4.Zero
          EffectParameters2 = Vector4.Zero
          GradientTransform = Matrix4x4.Identity
          MaskIndex = -1
          MaskKind = SlugMaskKind.None
          MaskParameters = Vector4.Zero
          MaskParameters2 = Vector4.Zero
          MaskInvert = false
          CompositeMode = SlugCompositeMode.SlugCompositeSourceOver
          MaterialValues = Vector4.Zero }

/// A sixteen-bit RGBA texel in the Slug band-index image.
[<Struct; StructLayout (LayoutKind.Sequential, Pack = 2)>]
type SlugShapeBandTexel =
    { X : uint16
      Y : uint16
      Z : uint16
      W : uint16 }

/// Addressing and fill metadata for one packed shape.
[<Struct; StructLayout (LayoutKind.Sequential)>]
type SlugShapeMetadata =
    { Bounds : SlugShapeBounds
      CurveLocation : Vector2i
      CurveCount : int
      CurveTexelCount : int
      BandLocation : Vector2i
      BandMax : Vector2i
      BandTransform : Vector4
      Flags : uint32
      FillRule : SlugFillRule
      Advance : single }

/// A dirty range in the mutable layer array.
[<Struct>]
type SlugLayerDirtyRange =
    { Start : int
      Count : int }

/// A read-only view of the layer array. Reads take the composite lock and never publish its backing array.
[<Sealed>]
type SlugLayerCollection internal (gate : obj, layers : SlugLayerState array) =
    member _.Length = lock gate (fun () -> layers.Length)
    member _.Count = lock gate (fun () -> layers.Length)
    member _.Item index =
        lock gate (fun () ->
            if index < 0 || index >= layers.Length then invalidArg (nameof index) "Layer index out of range."
            layers[index])

/// Immutable packed geometry and resources shared by all layers in a composite.
[<Sealed>]
type SlugCompositeShapeData internal
    (curveTextureWidth : int,
     curveTexels : Vector4 array,
     bandTextureWidth : int,
     bandTexels : SlugShapeBandTexel array,
     metadata : SlugShapeMetadata array,
     gradients : SlugGradient array,
     gradientStops : SlugGradientStop array,
     masks : SlugMaskState array,
     mappedMeshes : SlugMappedMesh array,
     id : Guid,
     revision : int64) =

    let curveTexels = if isNull curveTexels then nullArg (nameof curveTexels) else Array.copy curveTexels
    let bandTexels = if isNull bandTexels then nullArg (nameof bandTexels) else Array.copy bandTexels
    let metadata = if isNull metadata then nullArg (nameof metadata) else Array.copy metadata
    let gradients = if isNull gradients then nullArg (nameof gradients) else Array.copy gradients
    let gradientStops = if isNull gradientStops then nullArg (nameof gradientStops) else Array.copy gradientStops
    let masks = if isNull masks then nullArg (nameof masks) else Array.copy masks
    let mappedMeshes = if isNull mappedMeshes then nullArg (nameof mappedMeshes) else Array.copy mappedMeshes

    do
        if curveTextureWidth <= 0 then invalidArg (nameof curveTextureWidth) "Curve texture width must be positive."
        if bandTextureWidth <= 0 then invalidArg (nameof bandTextureWidth) "Band texture width must be positive."
        if id = Guid.Empty then invalidArg (nameof id) "Packed shape data needs a stable identity."
        if revision < 0L then invalidArg (nameof revision) "Packed shape revision cannot be negative."
        if curveTexels.Length > 0 && curveTexels.Length % curveTextureWidth <> 0 then invalidArg (nameof curveTexels) "Curve texels must occupy complete rows."
        if bandTexels.Length > 0 && bandTexels.Length % bandTextureWidth <> 0 then invalidArg (nameof bandTexels) "Band texels must occupy complete rows."

    member _.Id = id
    member _.Revision = revision
    member _.CurveTextureWidth = curveTextureWidth
    member _.CurveTextureHeight = max 1 (curveTexels.Length / curveTextureWidth)
    member _.CurveTexels = curveTexels
    member _.BandTextureWidth = bandTextureWidth
    member _.BandTextureHeight = max 1 (bandTexels.Length / bandTextureWidth)
    member _.BandTexels = bandTexels
    member _.Metadata = metadata
    member _.Gradients = gradients
    member _.GradientStops = gradientStops
    member _.Masks = masks
    member _.MappedMeshes = mappedMeshes
    member _.ShapeCount = metadata.Length

/// Shared immutable geometry with independently mutable, lock-protected layer state.
[<Sealed>]
type SlugCompositeShape (data : SlugCompositeShapeData, initialLayers : SlugLayerState array) =
    let gate = obj ()
    let layers =
        if isNull initialLayers then nullArg (nameof initialLayers)
        Array.copy initialLayers
    let layerCollection = SlugLayerCollection (gate, layers)
    let mutable dirtyStart = 0
    let mutable dirtyCount = layers.Length
    let mutable layerRevision = 0L

    let validateLayerIndex index =
        if index < 0 || index >= layers.Length then invalidArg (nameof index) "Layer index out of range."

    let validateShapeIndex shapeIndex =
        if shapeIndex < 0 || shapeIndex >= data.ShapeCount then invalidArg (nameof shapeIndex) "Shape index out of range."

    let markDirty index =
        if dirtyCount = 0 then dirtyStart <- index; dirtyCount <- 1
        else
            let finish = max (dirtyStart + dirtyCount - 1) index
            let beginIndex = min dirtyStart index
            dirtyStart <- beginIndex
            dirtyCount <- finish - beginIndex + 1
        layerRevision <- layerRevision + 1L

    let setState index state =
        validateLayerIndex index
        validateShapeIndex state.ShapeIndex
        lock gate (fun () ->
            if layers[index] <> state then
                layers[index] <- state
                markDirty index)

    do
        if obj.ReferenceEquals (data, null) then nullArg (nameof data)
        for layer in layers do validateShapeIndex layer.ShapeIndex

    member _.Data = data
    member _.Layers = layerCollection
    member _.LayerCount = layers.Length
    member _.GeometryRevision = data.Revision
    member _.LayerRevision = lock gate (fun () -> layerRevision)
    member _.DirtyLayers =
        lock gate (fun () ->
            if dirtyCount = 0 then None
            else Some { Start = dirtyStart; Count = dirtyCount })

    /// Gives the renderer a complete layer snapshot while the composite lock is held.
    /// Dirty state is cleared only when no mutation occurs during the callback.
    member _.ConsumeLayers (callback : Action<int64, SlugLayerState array>) =
        if isNull callback then nullArg (nameof callback)
        lock gate (fun () ->
            let revisionBefore = layerRevision
            callback.Invoke (revisionBefore, layers)
            if layerRevision = revisionBefore then
                dirtyStart <- 0
                dirtyCount <- 0)

    /// Gives the renderer the backing array only while the composite lock is held.
    /// The range is cleared after the callback returns; an exception leaves it dirty for retry.
    member _.ConsumeDirtyLayers (callback : Action<int, int, SlugLayerState array>) =
        if isNull callback then nullArg (nameof callback)
        lock gate (fun () ->
            if dirtyCount > 0 then
                let start = dirtyStart
                let count = dirtyCount
                let revisionBefore = layerRevision
                callback.Invoke (start, count, layers)
                if layerRevision = revisionBefore then
                    dirtyStart <- 0
                    dirtyCount <- 0)

    member _.ClearDirtyLayers () = lock gate (fun () -> dirtyStart <- 0; dirtyCount <- 0)

    member _.SetLayerState (layerIndex : int, state : SlugLayerState) = setState layerIndex state

    member _.SetLayerColor (layerIndex : int, color : Color) =
        validateLayerIndex layerIndex
        lock gate (fun () ->
            let state = layers[layerIndex]
            if state.Color <> color then
                layers[layerIndex] <- { state with Color = color }
                markDirty layerIndex)

    member _.SetLayerCompositeMode (layerIndex : int, compositeMode : SlugCompositeMode) =
        validateLayerIndex layerIndex
        lock gate (fun () ->
            let state = layers[layerIndex]
            if state.CompositeMode <> compositeMode then
                layers[layerIndex] <- { state with CompositeMode = compositeMode }
                markDirty layerIndex)

    member _.SetLayerTransform (layerIndex : int, transform : Matrix4x4) =
        validateLayerIndex layerIndex
        lock gate (fun () ->
            let state = layers[layerIndex]
            if state.Transform <> transform then
                layers[layerIndex] <- { state with Transform = transform }
                markDirty layerIndex)

    member _.SetLayerEffectId (layerIndex : int, effectId : int) =
        validateLayerIndex layerIndex
        lock gate (fun () ->
            let state = layers[layerIndex]
            if state.EffectId <> effectId then
                layers[layerIndex] <- { state with EffectId = effectId }
                markDirty layerIndex)

    member _.SetLayerEffectParam (layerIndex : int, effectParameters : Vector4) =
        validateLayerIndex layerIndex
        lock gate (fun () ->
            let state = layers[layerIndex]
            if state.EffectParameters <> effectParameters then
                layers[layerIndex] <- { state with EffectParameters = effectParameters }
                markDirty layerIndex)

    member _.SetLayerEffectParam2 (layerIndex : int, effectParameters2 : Vector4) =
        validateLayerIndex layerIndex
        lock gate (fun () ->
            let state = layers[layerIndex]
            if state.EffectParameters2 <> effectParameters2 then
                layers[layerIndex] <- { state with EffectParameters2 = effectParameters2 }
                markDirty layerIndex)

    member this.SetLayerEffectParam (layerIndex : int, parameterIndex : int, value : single) =
        validateLayerIndex layerIndex
        if parameterIndex < 0 || parameterIndex > 3 then invalidArg (nameof parameterIndex) "Effect parameter index must be in [0, 3]."
        lock gate (fun () ->
            let state = layers[layerIndex]
            let current = state.EffectParameters
            let updated =
                match parameterIndex with
                | 0 -> Vector4 (value, current.Y, current.Z, current.W)
                | 1 -> Vector4 (current.X, value, current.Z, current.W)
                | 2 -> Vector4 (current.X, current.Y, value, current.W)
                | _ -> Vector4 (current.X, current.Y, current.Z, value)
            if current <> updated then
                layers[layerIndex] <- { state with EffectParameters = updated }
                markDirty layerIndex)

    member _.SetLayerShapeIndex (layerIndex : int, shapeIndex : int) =
        validateLayerIndex layerIndex
        validateShapeIndex shapeIndex
        lock gate (fun () ->
            let state = layers[layerIndex]
            if state.ShapeIndex <> shapeIndex then
                layers[layerIndex] <- { state with ShapeIndex = shapeIndex }
                markDirty layerIndex)

    member _.SetLayerGradientTransform (layerIndex : int, transform : Matrix4x4) =
        validateLayerIndex layerIndex
        lock gate (fun () ->
            let state = layers[layerIndex]
            if state.GradientTransform <> transform then
                layers[layerIndex] <- { state with GradientTransform = transform }
                markDirty layerIndex)

    member _.SetMask (layerIndex : int, mask : SlugMaskState) =
        validateLayerIndex layerIndex
        match mask.Kind with
        | SlugMaskKind.MsdfLayer index when index < 0 -> invalidArg (nameof mask) "MSDF mask texture index must be non-negative."
        | SlugMaskKind.Shape shapeIndex -> validateShapeIndex shapeIndex
        | _ -> ()
        lock gate (fun () ->
            let state = layers[layerIndex]
            if state.MaskKind <> mask.Kind || state.MaskParameters <> mask.Parameters || state.MaskParameters2 <> mask.Parameters2 || state.MaskInvert <> mask.Invert then
                layers[layerIndex] <- { state with MaskKind = mask.Kind; MaskParameters = mask.Parameters; MaskParameters2 = mask.Parameters2; MaskInvert = mask.Invert }
                markDirty layerIndex)

    member _.SetMask (layerIndex : int, maskIndex : int) =
        validateLayerIndex layerIndex
        if maskIndex < -1 || maskIndex >= data.Masks.Length then invalidArg (nameof maskIndex) "Mask index out of range."
        lock gate (fun () ->
            let state = layers[layerIndex]
            if state.MaskIndex <> maskIndex then
                layers[layerIndex] <- { state with MaskIndex = maskIndex }
                markDirty layerIndex)

    member _.SetMask (layerIndex : int, maskIndex : int, parameters : Vector4, invert : bool) =
        validateLayerIndex layerIndex
        if maskIndex < -1 || maskIndex >= data.Masks.Length then invalidArg (nameof maskIndex) "Mask index out of range."
        lock gate (fun () ->
            let state = layers[layerIndex]
            if state.MaskIndex <> maskIndex || state.MaskParameters <> parameters || state.MaskInvert <> invert then
                layers[layerIndex] <- { state with MaskIndex = maskIndex; MaskParameters = parameters; MaskInvert = invert }
                markDirty layerIndex)

/// Immutable quadratic/cubic authoring segments.
[<Struct; RequireQualifiedAccess>]
type SlugPathSegment =
    | Line of EndPoint : Vector2
    | Quadratic of Control : Vector2 * EndPoint : Vector2
    | Cubic of Control1 : Vector2 * Control2 : Vector2 * EndPoint : Vector2

[<Struct>]
type SlugPathGlyph =
    { LayerIndex : int
      Advance : single }

[<Struct>]
type private SlugPathSample =
    { Distance : single
      Position : Vector2
      Tangent : Vector2 }

/// An immutable path with a deterministic arc-length sampling table.
[<Sealed>]
type SlugPath (startPoint : Vector2, sourceSegments : SlugPathSegment array, ?closed : bool, ?tolerance : single) =
    let closed = defaultArg closed false
    let tolerance = defaultArg tolerance 0.0001f
    let segments = if isNull sourceSegments then nullArg (nameof sourceSegments) else Array.copy sourceSegments

    let finitePoint (point : Vector2) = Single.IsFinite point.X && Single.IsFinite point.Y
    let normalizeOrFallback (vector : Vector2) (fallback : Vector2) =
        let lengthSquared = vector.LengthSquared ()
        if lengthSquared > 1.0e-12f && Single.IsFinite lengthSquared then Vector2.Normalize vector else fallback

    let samples, endPoint =
        if not (finitePoint startPoint) then invalidArg (nameof startPoint) "Path start must be finite."
        if not (Single.IsFinite tolerance) || tolerance <= 0.0f then invalidArg (nameof tolerance) "Path tolerance must be finite and positive."
        for segment in segments do
            match segment with
            | SlugPathSegment.Line p -> if not (finitePoint p) then invalidArg (nameof sourceSegments) "Path points must be finite."
            | SlugPathSegment.Quadratic (c, p) -> if not (finitePoint c && finitePoint p) then invalidArg (nameof sourceSegments) "Path points must be finite."
            | SlugPathSegment.Cubic (c1, c2, p) -> if not (finitePoint c1 && finitePoint c2 && finitePoint p) then invalidArg (nameof sourceSegments) "Path points must be finite."

        let table = ResizeArray<SlugPathSample> ()
        let mutable length = 0.0f
        let mutable current = startPoint
        let mutable fallbackTangent = Vector2.UnitX

        let firstTangent =
            if segments.Length = 0 then Vector2.UnitX
            else
                match segments[0] with
                | SlugPathSegment.Line point ->
                    point - startPoint
                | SlugPathSegment.Quadratic (control, point) ->
                    let tangent = control - startPoint
                    if tangent.LengthSquared () > 1.0e-12f then tangent else point - startPoint
                | SlugPathSegment.Cubic (control1, control2, point) ->
                    let tangent1 = control1 - startPoint
                    if tangent1.LengthSquared () > 1.0e-12f then tangent1
                    else
                        let tangent2 = control2 - startPoint
                        if tangent2.LengthSquared () > 1.0e-12f then tangent2 else point - startPoint

        let addSample point tangent =
            let tangent = normalizeOrFallback tangent fallbackTangent
            if table.Count = 0 then
                table.Add { Distance = 0.0f; Position = point; Tangent = tangent }
            else
                let previous = table[table.Count - 1]
                let delta = Vector2.Distance (point, previous.Position)
                if delta > 1.0e-7f then
                    length <- length + delta
                    table.Add { Distance = length; Position = point; Tangent = tangent }
                else
                    table[table.Count - 1] <- { previous with Position = point; Tangent = tangent }
            fallbackTangent <- tangent

        let evaluateQuadratic (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (t : single) =
            let s = 1.0f - t
            p0 * (s * s) + p1 * (2.0f * s * t) + p2 * (t * t)
        let evaluateCubic (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (p3 : Vector2) (t : single) =
            let s = 1.0f - t
            p0 * (s * s * s) + p1 * (3.0f * s * s * t) + p2 * (3.0f * s * t * t) + p3 * (t * t * t)
        let derivativeQuadratic (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (t : single) =
            (p1 - p0) * (2.0f * (1.0f - t)) + (p2 - p1) * (2.0f * t)
        let derivativeCubic (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (p3 : Vector2) (t : single) =
            (p1 - p0) * (3.0f * (1.0f - t) * (1.0f - t)) +
            (p2 - p1) * (6.0f * (1.0f - t) * t) +
            (p3 - p2) * (3.0f * t * t)

        let appendCurve (evaluate : single -> Vector2) (derivative : single -> Vector2) (p0 : Vector2) (p1 : Vector2) =
            let rec append (t0 : single) (point0 : Vector2) (t1 : single) (point1 : Vector2) depth =
                let tm = (t0 + t1) * 0.5f
                let mid = evaluate tm
                let chordMid = (point0 + point1) * 0.5f
                let flatness = Vector2.Distance (mid, chordMid)
                if flatness <= tolerance || depth >= 24 then addSample point1 (derivative t1)
                else
                    append t0 point0 tm mid (depth + 1)
                    append tm mid t1 point1 (depth + 1)
            append 0.0f p0 1.0f p1 0

        let appendQuadratic (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) =
            appendCurve (evaluateQuadratic p0 p1 p2) (derivativeQuadratic p0 p1 p2) p0 p2

        let appendCubic (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (p3 : Vector2) =
            appendCurve (evaluateCubic p0 p1 p2 p3) (derivativeCubic p0 p1 p2 p3) p0 p3

        addSample startPoint firstTangent
        for segment in segments do
            match segment with
            | SlugPathSegment.Line point ->
                addSample point (point - current)
                current <- point
            | SlugPathSegment.Quadratic (control, point) ->
                appendQuadratic current control point
                current <- point
            | SlugPathSegment.Cubic (control1, control2, point) ->
                appendCubic current control1 control2 point
                current <- point
        if closed && Vector2.DistanceSquared (current, startPoint) > 1.0e-12f then
            addSample startPoint (startPoint - current)
        elif closed && table.Count > 1 then
            table[table.Count - 1] <- { table[table.Count - 1] with Position = startPoint }
        table.ToArray (), current

    do
        if closed && segments.Length = 0 then invalidArg (nameof sourceSegments) "A closed path must contain at least one segment."

    member _.StartPoint = startPoint
    member _.EndPoint = if closed then startPoint else endPoint
    member _.Segments = segments
    member _.Closed = closed
    member _.Length = if samples.Length = 0 then 0.0f else samples[samples.Length - 1].Distance
    member _.Sample (distance : single) =
        if not (Single.IsFinite distance) then invalidArg (nameof distance) "Sample distance must be finite."
        if samples.Length = 0 then startPoint, Vector2.UnitX
        elif samples.Length = 1 then samples[0].Position, samples[0].Tangent
        else
            let target = max 0.0f (min distance (samples[samples.Length - 1].Distance))
            if target <= 0.0f then samples[0].Position, samples[0].Tangent
            elif target >= samples[samples.Length - 1].Distance then
                let final = samples[samples.Length - 1]
                final.Position, final.Tangent
            else
                let mutable low = 0
                let mutable high = samples.Length - 1
                while high - low > 1 do
                    let middle = (low + high) / 2
                    if samples[middle].Distance <= target then low <- middle else high <- middle
                let a = samples[low]
                let b = samples[high]
                let span = b.Distance - a.Distance
                let amount = if span > 0.0f then (target - a.Distance) / span else 0.0f
                let position = Vector2.Lerp (a.Position, b.Position, amount)
                let tangent = normalizeOrFallback (Vector2.Lerp (a.Tangent, b.Tangent, amount)) a.Tangent
                position, tangent

    member this.SampleNormalized (amount : single) = this.Sample (this.Length * max 0.0f (min amount 1.0f))

    /// Places shaped glyph layers along this path using their advances and tangents.
    member this.TextOnPath (composite : SlugCompositeShape, glyphs : SlugPathGlyph array, ?startDistance : single) =
        if obj.ReferenceEquals (composite, null) then nullArg (nameof composite)
        if isNull glyphs then nullArg (nameof glyphs)
        let mutable distance = defaultArg startDistance 0.0f
        for glyph in glyphs do
            if not (Single.IsFinite glyph.Advance) || glyph.Advance < 0.0f then invalidArg (nameof glyphs) "Glyph advances must be finite and non-negative."
            let position, tangent = this.Sample distance
            let state = composite.Layers.Item glyph.LayerIndex
            let angle = MathF.Atan2 (tangent.Y, tangent.X)
            let mutable transform = Matrix4x4.CreateRotationZ angle * state.Transform
            let translation = Vector3 (position.X - state.Origin.X, position.Y - state.Origin.Y, state.Transform.Translation.Z)
            transform.Translation <- translation
            composite.SetLayerTransform (glyph.LayerIndex, transform)
            distance <- distance + glyph.Advance

[<RequireQualifiedAccess>]
module SlugShapeRuntime =
    let private textureWidth = 4096
    let private maxBandCount = 16
    let private bandOverlapEpsilon = 1.0f / 1024.0f
    let private closureEpsilonSquared = 1.0e-10f
    let private maxCubicDepth = 30
    let mutable private nextRevision = 0L

    let private finitePoint (point : Vector2) = Single.IsFinite point.X && Single.IsFinite point.Y
    let private validatePoint name (point : Vector2) = if not (finitePoint point) then invalidArg name "Shape coordinates must be finite."
    let private lerp (a : Vector2) (b : Vector2) = (a + b) * 0.5f

    let private boundsOfContours (contours : SlugCurve array array) =
        let mutable minPoint = Vector2 (Single.PositiveInfinity, Single.PositiveInfinity)
        let mutable maxPoint = Vector2 (Single.NegativeInfinity, Single.NegativeInfinity)
        let mutable hasPoint = false
        for contour in contours do
            for curve in contour do
                for point in [| curve.P1; curve.P2; curve.P3 |] do
                    validatePoint (nameof contours) point
                    minPoint <- Vector2 (min minPoint.X point.X, min minPoint.Y point.Y)
                    maxPoint <- Vector2 (max maxPoint.X point.X, max maxPoint.Y point.Y)
                    hasPoint <- true
        if not hasPoint then invalidArg (nameof contours) "A Slug source needs at least one curve."
        { Min = minPoint; Max = maxPoint }

    let private validateContours (contours : SlugCurve array array) =
        if isNull contours || contours.Length = 0 then invalidArg (nameof contours) "A Slug source needs at least one contour."
        for contour in contours do
            if isNull contour || contour.Length = 0 then invalidArg (nameof contours) "Slug contours cannot be empty."
            let mutable allZero = true
            for index in 0 .. dec contour.Length do
                let curve = contour[index]
                validatePoint (nameof contours) curve.P1
                validatePoint (nameof contours) curve.P2
                validatePoint (nameof contours) curve.P3
                if Vector2.DistanceSquared (curve.P1, curve.P3) > closureEpsilonSquared then allZero <- false
                let next = contour[(index + 1) % contour.Length]
                if Vector2.DistanceSquared (curve.P3, next.P1) > closureEpsilonSquared then invalidArg (nameof contours) "Slug contours must be continuous and closed."
            if allZero then invalidArg (nameof contours) "Slug contours cannot be entirely degenerate."

    let private validateBounds (bounds : SlugShapeBounds) (contours : SlugCurve array array) =
        validatePoint (nameof bounds) bounds.Min
        validatePoint (nameof bounds) bounds.Max
        if bounds.Min.X > bounds.Max.X || bounds.Min.Y > bounds.Max.Y then invalidArg (nameof bounds) "Shape bounds are inverted."
        for contour in contours do
            for curve in contour do
                for point in [| curve.P1; curve.P2; curve.P3 |] do
                    if point.X < bounds.Min.X - 1.0e-4f || point.X > bounds.Max.X + 1.0e-4f || point.Y < bounds.Min.Y - 1.0e-4f || point.Y > bounds.Max.Y + 1.0e-4f then
                        invalidArg (nameof bounds) "Shape bounds must contain every curve control point."

    let private validateRule (fillRule : SlugFillRule) =
        match fillRule with
        | SlugFillNonzero
        | SlugFillEvenOdd -> ()

    let private makeSource (contours : SlugCurve array array) (bounds : SlugShapeBounds) (fillRule : SlugFillRule) (advance : single option) =
        validateContours contours
        validateBounds bounds contours
        validateRule fillRule
        match advance with
        | Some value when not (Single.IsFinite value) || value < 0.0f -> invalidArg (nameof advance) "Shape advance must be finite and non-negative."
        | _ -> ()
        { Contours = contours |> Array.map Array.copy
          Bounds = bounds
          FillRule = fillRule
          Advance = advance }

    let createSource (contours : SlugCurve array array) (fillRule : SlugFillRule) (advance : single option) =
        let contours = if isNull contours then nullArg (nameof contours) else Array.copy contours
        validateContours contours
        makeSource contours (boundsOfContours contours) fillRule advance

    let createSourceWithBounds (contours : SlugCurve array array) (bounds : SlugShapeBounds) (fillRule : SlugFillRule) (advance : single option) =
        let contours = if isNull contours then nullArg (nameof contours) else Array.copy contours
        validateContours contours
        makeSource contours bounds fillRule advance

    let private cubicError (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (p3 : Vector2) (q1 : Vector2) =
        let elevated1 = p0 + (q1 - p0) * (2.0f / 3.0f)
        let elevated2 = p3 + (q1 - p3) * (2.0f / 3.0f)
        max (Vector2.Distance (p1, elevated1)) (Vector2.Distance (p2, elevated2))

    let private appendCubicAsQuadratics
        (tolerance : single)
        (p0 : Vector2)
        (p1 : Vector2)
        (p2 : Vector2)
        (p3 : Vector2)
        (output : ResizeArray<SlugCurve>) =
        let rec subdivide (a : Vector2) (b : Vector2) (c : Vector2) (d : Vector2) depth =
            let midpoint = (a + b * 3.0f + c * 3.0f + d) * 0.125f
            let q1 = midpoint * 2.0f - (a + d) * 0.5f
            let error = cubicError a b c d q1
            if error <= tolerance then output.Add { P1 = a; P2 = q1; P3 = d }
            elif depth >= maxCubicDepth then failwith "Cubic-to-quadratic conversion did not meet the requested tolerance."
            else
                let ab = lerp a b
                let bc = lerp b c
                let cd = lerp c d
                let abc = lerp ab bc
                let bcd = lerp bc cd
                let middle = lerp abc bcd
                subdivide a ab abc middle (depth + 1)
                subdivide middle bcd cd d (depth + 1)
        subdivide p0 p1 p2 p3 0

    /// Convert strict ContourCommand subpaths to closed quadratic Slug contours.
    let fromContourCommands (commands : ContourCommand seq) (fillRule : SlugFillRule) (tolerance : single) =
        if isNull (box commands) then nullArg (nameof commands)
        if not (Single.IsFinite tolerance) || tolerance <= 0.0f then invalidArg (nameof tolerance) "Cubic tolerance must be finite and positive."
        let contours = ResizeArray<SlugCurve array> ()
        let current = ResizeArray<SlugCurve> ()
        let mutable hasSubpath = false
        let mutable isClosed = false
        let mutable point = Vector2.Zero
        let mutable start = Vector2.Zero
        let addLine endpoint =
            current.Add { P1 = point; P2 = (point + endpoint) * 0.5f; P3 = endpoint }
            point <- endpoint
        for command in commands do
            match command with
            | MoveTo endpoint ->
                if hasSubpath && not isClosed then invalidArg (nameof commands) "MoveTo cannot interrupt an open contour."
                validatePoint (nameof commands) endpoint
                current.Clear ()
                point <- endpoint
                start <- endpoint
                hasSubpath <- true
                isClosed <- false
            | LineTo endpoint ->
                if not hasSubpath || isClosed then invalidArg (nameof commands) "LineTo requires an open contour."
                validatePoint (nameof commands) endpoint
                addLine endpoint
            | QuadraticCurveTo (control, endpoint) ->
                if not hasSubpath || isClosed then invalidArg (nameof commands) "QuadraticCurveTo requires an open contour."
                validatePoint (nameof commands) control
                validatePoint (nameof commands) endpoint
                current.Add { P1 = point; P2 = control; P3 = endpoint }
                point <- endpoint
            | CubicCurveTo (control1, control2, endpoint) ->
                if not hasSubpath || isClosed then invalidArg (nameof commands) "CubicCurveTo requires an open contour."
                validatePoint (nameof commands) control1
                validatePoint (nameof commands) control2
                validatePoint (nameof commands) endpoint
                appendCubicAsQuadratics tolerance point control1 control2 endpoint current
                point <- endpoint
            | CloseContour ->
                if not hasSubpath || isClosed then invalidArg (nameof commands) "CloseContour requires one open contour."
                if Vector2.DistanceSquared (point, start) > closureEpsilonSquared then addLine start
                if current.Count = 0 then invalidArg (nameof commands) "A closed contour must contain at least one segment."
                contours.Add (current.ToArray ())
                current.Clear ()
                isClosed <- true
        if hasSubpath && not isClosed then invalidArg (nameof commands) "Every contour must end with CloseContour."
        if contours.Count = 0 then invalidArg (nameof commands) "No closed contours were supplied."
        createSource (contours.ToArray ()) fillRule None
    /// Adapter for callers that already use ContourWinding; Slug intentionally rejects the
    /// tessellator-only Positive, Negative, and AbsGeqTwo modes.
    let fromContourCommandsWithWinding (commands : ContourCommand seq) (winding : ContourWinding) (tolerance : single) =
        match winding with
        | ContourWinding.EvenOdd -> fromContourCommands commands SlugFillEvenOdd tolerance
        | ContourWinding.NonZero -> fromContourCommands commands SlugFillNonzero tolerance

    let private makeBandTexel x y =
        if x < 0 || x > int UInt16.MaxValue || y < 0 || y > int UInt16.MaxValue then failwith "Slug texture address exceeds the 16-bit band format."
        { X = uint16 x; Y = uint16 y; Z = 0us; W = 0us }

    let private chooseBandCount curveCount span =
        if curveCount = 0 || span <= 0.0f then 1
        else max 1 (min maxBandCount (int (sqrt (single curveCount))))

    let private packCurves (sources : SlugShapeSource array) =
        let texels = ResizeArray<Vector4> ()
        let locations = Array.zeroCreate<Vector2i array> sources.Length
        let texelCounts = Array.zeroCreate<int> sources.Length
        let mutable x = 0
        let mutable y = 0
        let ensure index = while texels.Count <= index do texels.Add Vector4.Zero
        let beginBlock count =
            if count > textureWidth then failwith "Slug shape contour exceeds the curve texture row width."
            if x + count > textureWidth then x <- 0; y <- y + 1
            let start = Vector2i (x, y)
            x <- x + count
            start
        for shapeIndex in 0 .. dec sources.Length do
            let source = sources[shapeIndex]
            let shapeLocations = ResizeArray<Vector2i> ()
            let mutable texelCount = 0
            for contour in source.Contours do
                let start = beginBlock (contour.Length + 1)
                texelCount <- texelCount + contour.Length + 1
                for curveIndex in 0 .. dec contour.Length do
                    let curve = contour[curveIndex]
                    let location = Vector2i (start.X + curveIndex, start.Y)
                    shapeLocations.Add location
                    ensure (location.Y * textureWidth + location.X)
                    texels[location.Y * textureWidth + location.X] <- Vector4 (curve.P1.X, curve.P1.Y, curve.P2.X, curve.P2.Y)
                    let nextLocation = Vector2i (location.X + 1, location.Y)
                    ensure (nextLocation.Y * textureWidth + nextLocation.X)
                    let nextP2 = if curveIndex + 1 < contour.Length then contour[curveIndex + 1].P2 else Vector2.Zero
                    texels[nextLocation.Y * textureWidth + nextLocation.X] <- Vector4 (curve.P3.X, curve.P3.Y, nextP2.X, nextP2.Y)
            locations[shapeIndex] <- shapeLocations.ToArray ()
            texelCounts[shapeIndex] <- texelCount
        let height = max 1 (y + 1)
        let result = Array.zeroCreate<Vector4> (height * textureWidth)
        for index in 0 .. dec texels.Count do result[index] <- texels[index]
        result, locations, texelCounts, height

    let private packBands (sources : SlugShapeSource array) (curveLocations : Vector2i array array) =
        let bandTexels = ResizeArray<SlugShapeBandTexel> ()
        let metadata = Array.zeroCreate<SlugShapeMetadata> sources.Length
        let mutable cursor = 0
        let ensure index = while bandTexels.Count <= index do bandTexels.Add Unchecked.defaultof<SlugShapeBandTexel>
        let align span =
            if span > textureWidth then failwith "Slug band span exceeds the band texture row width."
            let x = cursor % textureWidth
            if x + span > textureWidth then cursor <- cursor + textureWidth - x
        for shapeIndex in 0 .. dec sources.Length do
            let source = sources[shapeIndex]
            let curves = source.Contours |> Array.collect id
            let bounds = source.Bounds
            let xSpan = max 0.0001f (bounds.Max.X - bounds.Min.X)
            let ySpan = max 0.0001f (bounds.Max.Y - bounds.Min.Y)
            let makeLists horizontal bandCount =
                Array.init bandCount (fun bandIndex ->
                    let lower = if horizontal then bounds.Min.Y + single bandIndex * ySpan / single bandCount else bounds.Min.X + single bandIndex * xSpan / single bandCount
                    let upper = if horizontal then bounds.Min.Y + single (bandIndex + 1) * ySpan / single bandCount else bounds.Min.X + single (bandIndex + 1) * xSpan / single bandCount
                    [| for curveIndex in 0 .. dec curves.Length do
                           let curve = curves[curveIndex]
                           let minimum = if horizontal then min curve.P1.Y (min curve.P2.Y curve.P3.Y) else min curve.P1.X (min curve.P2.X curve.P3.X)
                           let maximum = if horizontal then max curve.P1.Y (max curve.P2.Y curve.P3.Y) else max curve.P1.X (max curve.P2.X curve.P3.X)
                           if maximum <> minimum && maximum >= lower - bandOverlapEpsilon && minimum <= upper + bandOverlapEpsilon then yield curveIndex |]
                    |> Array.sortByDescending (fun curveIndex -> if horizontal then max curves[curveIndex].P1.X (max curves[curveIndex].P2.X curves[curveIndex].P3.X) else max curves[curveIndex].P1.Y (max curves[curveIndex].P2.Y curves[curveIndex].P3.Y)))
            let mutable bandCount = chooseBandCount curves.Length (max xSpan ySpan)
            let mutable horizontal = makeLists true bandCount
            let mutable vertical = makeLists false bandCount
            let mutable total = Array.sumBy Array.length horizontal + Array.sumBy Array.length vertical
            while total > textureWidth - 2 * bandCount && bandCount > 1 do
                bandCount <- bandCount - 1
                horizontal <- makeLists true bandCount
                vertical <- makeLists false bandCount
                total <- Array.sumBy Array.length horizontal + Array.sumBy Array.length vertical
            let headerCount = bandCount * 2
            align headerCount
            let start = cursor
            let location = Vector2i (start % textureWidth, start / textureWidth)
            ensure (start + headerCount - 1)
            cursor <- cursor + headerCount
            let writeList (list : int array) =
                align list.Length
                let offset = cursor - start
                if offset > int UInt16.MaxValue then failwith "Slug band list offset exceeds the 16-bit texture format."
                for curveIndex in list do
                    let curveLocation = curveLocations[shapeIndex][curveIndex]
                    ensure cursor
                    bandTexels[cursor] <- makeBandTexel curveLocation.X curveLocation.Y
                    cursor <- cursor + 1
                list.Length, offset
            for bandIndex in 0 .. dec bandCount do
                let count, offset = writeList horizontal[bandIndex]
                bandTexels[start + bandIndex] <- makeBandTexel count offset
            for bandIndex in 0 .. dec bandCount do
                let count, offset = writeList vertical[bandIndex]
                bandTexels[start + bandCount + bandIndex] <- makeBandTexel count offset
            let flags = match source.FillRule with | SlugFillNonzero -> 0u | SlugFillEvenOdd -> 1u
            metadata[shapeIndex] <-
                { Bounds = bounds
                  CurveLocation = if curveLocations[shapeIndex].Length > 0 then curveLocations[shapeIndex][0] else Vector2i.Zero
                  CurveCount = curves.Length
                  CurveTexelCount = 0
                  BandLocation = location
                  BandMax = Vector2i (bandCount - 1, bandCount - 1)
                  BandTransform = Vector4 (single bandCount / xSpan, single bandCount / ySpan, -bounds.Min.X * single bandCount / xSpan, -bounds.Min.Y * single bandCount / ySpan)
                  Flags = flags
                  FillRule = source.FillRule
                  Advance = defaultArg source.Advance 0.0f }
        let height = max 1 ((cursor + textureWidth - 1) / textureWidth)
        let result = Array.zeroCreate<SlugShapeBandTexel> (height * textureWidth)
        for index in 0 .. dec bandTexels.Count do result[index] <- bandTexels[index]
        result, metadata, height

    let packWithResources (sources : SlugShapeSource array) (gradients : SlugGradient array) (gradientStops : SlugGradientStop array) (masks : SlugMaskState array) (mappedMeshes : SlugMappedMesh array) =
        if isNull sources then nullArg (nameof sources)
        if isNull gradients then nullArg (nameof gradients)
        if isNull gradientStops then nullArg (nameof gradientStops)
        if isNull masks then nullArg (nameof masks)
        if isNull mappedMeshes then nullArg (nameof mappedMeshes)
        for source in sources do validateContours source.Contours; validateBounds source.Bounds source.Contours; validateRule source.FillRule
        for mask in masks do
            match mask.Kind with
            | SlugMaskKind.MsdfLayer index when index < 0 -> invalidArg (nameof masks) "MSDF mask texture index must be non-negative."
            | SlugMaskKind.Shape shapeIndex when shapeIndex < 0 || shapeIndex >= sources.Length -> invalidArg (nameof masks) "Analytic mask shape index is out of range."
            | _ -> ()
        let curveTexels, curveLocations, curveTexelCounts, _ = packCurves sources
        let bandTexels, metadata, _ = packBands sources curveLocations
        for index in 0 .. dec metadata.Length do metadata[index] <- { metadata[index] with CurveTexelCount = curveTexelCounts[index] }
        let id = Guid.NewGuid ()
        let revision = Interlocked.Increment (&nextRevision)
        SlugCompositeShapeData (textureWidth, curveTexels, textureWidth, bandTexels, metadata, gradients, gradientStops, masks, mappedMeshes, id, revision)

    let pack (sources : SlugShapeSource array) = packWithResources sources [||] [||] [||] [||]

    let createComposite (data : SlugCompositeShapeData) (layers : SlugLayerState array) = SlugCompositeShape (data, layers)
