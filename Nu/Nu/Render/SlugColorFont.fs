// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu

open System
open System.IO
open System.Numerics
open System.Reflection
open System.Runtime.InteropServices
open System.Text
open System.Collections.Generic
open HarfBuzzSharp

/// A color stop copied from HarfBuzz's callback-duration color line.
/// HarfBuzz supplies unpremultiplied colors; PremultipliedColor is retained so a
/// fixed-function consumer can interpolate in the OpenType-required premultiplied space.
[<NoEquality; NoComparison>]
type SlugColorLineStop =
    { Offset : single
      IsForeground : bool
      Color : Color
      PremultipliedColor : Color }

[<RequireQualifiedAccess>]
type SlugColorPaintExtend =
    | Pad
    | Repeat
    | Reflect

[<RequireQualifiedAccess>]
type SlugColorPaintGradientKind =
    | Linear of P0 : Vector2 * P1 : Vector2 * P2 : Vector2
    | Radial of Center0 : Vector2 * Radius0 : single * Center1 : Vector2 * Radius1 : single
    | Sweep of Center : Vector2 * StartAngle : single * EndAngle : single

[<NoEquality; NoComparison>]
type SlugColorPaintGradient =
    { Kind : SlugColorPaintGradientKind
      Stops : SlugColorLineStop array
      Extend : SlugColorPaintExtend }

[<RequireQualifiedAccess>]
type SlugColorPaintFill =
    | Solid of Color : Color * IsForeground : bool
    | Gradient of SlugColorPaintGradient

[<NoEquality; NoComparison>]
type SlugColorPaintClip =
    | Glyph of GlyphId : uint32 * Source : SlugShapeSource option * Transform : Matrix4x4
    | Rectangle of Bounds : Vector4 * Transform : Matrix4x4

[<Sealed>]
type SlugColorPaintLeaf
    (glyphId : uint32,
     source : SlugShapeSource,
     fill : SlugColorPaintFill,
     transform : Matrix4x4,
     fillTransform : Matrix4x4,
     clips : SlugColorPaintClip array,
     compositeMode : SlugCompositeMode) =

    let clips = if isNull clips then nullArg (nameof clips) else Array.copy clips

    member _.GlyphId = glyphId
    member _.Source = source
    member _.Fill = fill
    member _.Transform = transform
    member _.FillTransform = fillTransform
    member _.Clips = clips
    member _.CompositeMode = compositeMode

[<Sealed>]
type SlugColorPaintGroup
    (children : SlugColorPaintNode array,
     compositeMode : SlugCompositeMode) =

    let children = if isNull children then nullArg (nameof children) else Array.copy children

    member _.Children = children
    member _.CompositeMode = compositeMode

and SlugColorPaintNode =
    | Leaf of SlugColorPaintLeaf
    | Group of SlugColorPaintGroup

[<Sealed>]
type SlugColorPaintTree
    (root : SlugColorPaintNode array,
     flattenedLayers : SlugColorPaintLeaf array,
     retainedGroups : SlugColorPaintGroup array) =

    let root = if isNull root then nullArg (nameof root) else Array.copy root
    let flattenedLayers = if isNull flattenedLayers then nullArg (nameof flattenedLayers) else Array.copy flattenedLayers
    let retainedGroups = if isNull retainedGroups then nullArg (nameof retainedGroups) else Array.copy retainedGroups

    member _.Root = root
    /// Leaves which can be submitted directly to Slug's fixed-function layer path.
    member _.FlattenedLayers = flattenedLayers
    /// Groups whose Porter-Duff / advanced blend mode needs an offscreen compositor.
    /// They are deliberately retained instead of being treated as source-over.
    member _.RetainedGroups = retainedGroups

[<Sealed>]
type SlugColorGlyphLayer
    (glyphId : uint32, color : Color, isForeground : bool, source : SlugShapeSource) =
    member _.GlyphId = glyphId
    member _.Color = color
    member _.IsForeground = isForeground
    member _.Source = source

[<RequireQualifiedAccess>]
type SlugColorGlyph =
    | Outline of Source : SlugShapeSource
    | ColrV0 of Layers : SlugColorGlyphLayer array
    | ColrV1 of Paint : SlugColorPaintTree

module private SlugColorFontNative =

    [<Literal>]
    let LibraryName = "libHarfBuzzSharp"

    [<Struct; StructLayout (LayoutKind.Sequential, Pack = 4)>]
    type ColorStop =
        { Offset : single
          IsForeground : int32
          Color : uint32 }

    [<Struct; StructLayout (LayoutKind.Sequential)>]
    type ColorLine =
        { Data : IntPtr
          GetColorStops : IntPtr
          GetColorStopsUserData : IntPtr
          GetExtend : IntPtr
          GetExtendUserData : IntPtr
          Reserved0 : IntPtr
          Reserved1 : IntPtr
          Reserved2 : IntPtr
          Reserved3 : IntPtr
          Reserved5 : IntPtr
          Reserved6 : IntPtr
          Reserved7 : IntPtr
          Reserved8 : IntPtr }

    [<Struct; StructLayout (LayoutKind.Sequential, Pack = 4)>]
    type DrawState =
        { PathOpen : int32
          PathStartX : single
          PathStartY : single
          CurrentX : single
          CurrentY : single
          Reserved1 : uint32
          Reserved2 : uint32
          Reserved3 : uint32
          Reserved4 : uint32
          Reserved5 : uint32
          Reserved6 : uint32
          Reserved7 : uint32 }

    [<Struct; StructLayout (LayoutKind.Sequential, Pack = 4)>]
    type ColorLayer =
        { Glyph : uint32
          ColorIndex : uint32 }

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type DestroyDelegate = delegate of IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type DrawMoveDelegate = delegate of IntPtr * IntPtr * IntPtr * single * single * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type DrawLineDelegate = delegate of IntPtr * IntPtr * IntPtr * single * single * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type DrawQuadraticDelegate = delegate of IntPtr * IntPtr * IntPtr * single * single * single * single * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type DrawCubicDelegate = delegate of IntPtr * IntPtr * IntPtr * single * single * single * single * single * single * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type DrawCloseDelegate = delegate of IntPtr * IntPtr * IntPtr * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintPushTransformDelegate = delegate of IntPtr * IntPtr * single * single * single * single * single * single * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintPopTransformDelegate = delegate of IntPtr * IntPtr * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintColorGlyphDelegate = delegate of IntPtr * IntPtr * uint32 * IntPtr * IntPtr -> int32

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintPushClipGlyphDelegate = delegate of IntPtr * IntPtr * uint32 * IntPtr * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintPushClipRectangleDelegate = delegate of IntPtr * IntPtr * single * single * single * single * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintPopClipDelegate = delegate of IntPtr * IntPtr * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintColorDelegate = delegate of IntPtr * IntPtr * int32 * uint32 * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintLinearGradientDelegate = delegate of IntPtr * IntPtr * IntPtr * single * single * single * single * single * single * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintRadialGradientDelegate = delegate of IntPtr * IntPtr * IntPtr * single * single * single * single * single * single * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintSweepGradientDelegate = delegate of IntPtr * IntPtr * IntPtr * single * single * single * single * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintPushGroupDelegate = delegate of IntPtr * IntPtr * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintPopGroupDelegate = delegate of IntPtr * IntPtr * int32 * IntPtr -> unit

    [<UnmanagedFunctionPointer (CallingConvention.Cdecl)>]
    type PaintCustomPaletteDelegate = delegate of IntPtr * IntPtr * uint32 * IntPtr * IntPtr -> int32


    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_blob_create_from_file")>]
    extern IntPtr BlobCreateFromFile (IntPtr fileName)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_blob_destroy")>]
    extern void BlobDestroy (IntPtr blob)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_face_create")>]
    extern IntPtr FaceCreate (IntPtr blob, uint32 index)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_face_destroy")>]
    extern void FaceDestroy (IntPtr face)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_face_get_upem")>]
    extern uint32 FaceGetUpem (IntPtr face)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_font_create")>]
    extern IntPtr FontCreate (IntPtr face)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_font_destroy")>]
    extern void FontDestroy (IntPtr font)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_font_set_scale")>]
    extern void FontSetScale (IntPtr font, int32 xScale, int32 yScale)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_font_get_nominal_glyph")>]
    extern int32 FontGetNominalGlyph (IntPtr font, uint32 unicode, uint32& glyph)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_ot_font_set_funcs")>]
    extern void OpenTypeFontSetFuncs (IntPtr font)

    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_draw_funcs_create")>]
    extern IntPtr DrawFuncsCreate ()
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_draw_funcs_destroy")>]
    extern void DrawFuncsDestroy (IntPtr funcs)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_draw_funcs_set_move_to_func")>]
    extern void DrawSetMove (IntPtr funcs, DrawMoveDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_draw_funcs_set_line_to_func")>]
    extern void DrawSetLine (IntPtr funcs, DrawLineDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_draw_funcs_set_quadratic_to_func")>]
    extern void DrawSetQuadratic (IntPtr funcs, DrawQuadraticDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_draw_funcs_set_cubic_to_func")>]
    extern void DrawSetCubic (IntPtr funcs, DrawCubicDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_draw_funcs_set_close_path_func")>]
    extern void DrawSetClose (IntPtr funcs, DrawCloseDelegate callback, IntPtr userData, IntPtr destroy)

    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_font_draw_glyph")>]
    extern void FontDrawGlyph (IntPtr font, uint32 glyph, IntPtr funcs, IntPtr drawData)

    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_create")>]
    extern IntPtr PaintFuncsCreate ()
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_destroy")>]
    extern void PaintFuncsDestroy (IntPtr funcs)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_push_transform_func")>]
    extern void PaintSetPushTransform (IntPtr funcs, PaintPushTransformDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_pop_transform_func")>]
    extern void PaintSetPopTransform (IntPtr funcs, PaintPopTransformDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_color_glyph_func")>]
    extern void PaintSetColorGlyph (IntPtr funcs, PaintColorGlyphDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_push_clip_glyph_func")>]
    extern void PaintSetPushClipGlyph (IntPtr funcs, PaintPushClipGlyphDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_push_clip_rectangle_func")>]
    extern void PaintSetPushClipRectangle (IntPtr funcs, PaintPushClipRectangleDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_pop_clip_func")>]
    extern void PaintSetPopClip (IntPtr funcs, PaintPopClipDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_color_func")>]
    extern void PaintSetColor (IntPtr funcs, PaintColorDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_linear_gradient_func")>]
    extern void PaintSetLinearGradient (IntPtr funcs, PaintLinearGradientDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_radial_gradient_func")>]
    extern void PaintSetRadialGradient (IntPtr funcs, PaintRadialGradientDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_sweep_gradient_func")>]
    extern void PaintSetSweepGradient (IntPtr funcs, PaintSweepGradientDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_push_group_func")>]
    extern void PaintSetPushGroup (IntPtr funcs, PaintPushGroupDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_pop_group_func")>]
    extern void PaintSetPopGroup (IntPtr funcs, PaintPopGroupDelegate callback, IntPtr userData, IntPtr destroy)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_paint_funcs_set_custom_palette_color_func")>]
    extern void PaintSetCustomPalette (IntPtr funcs, PaintCustomPaletteDelegate callback, IntPtr userData, IntPtr destroy)

    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_font_paint_glyph")>]
    extern void FontPaintGlyph (IntPtr font, uint32 glyph, IntPtr funcs, IntPtr paintData, uint32 paletteIndex, uint32 foreground)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_ot_color_glyph_has_paint")>]
    extern int32 ColorGlyphHasPaint (IntPtr face, uint32 glyph)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_ot_color_has_layers")>]
    extern int32 ColorHasLayers (IntPtr face)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_ot_color_glyph_get_layers")>]
    extern uint32 ColorGlyphGetLayers (IntPtr face, uint32 glyph, uint32 startOffset, IntPtr layerCount, IntPtr layers)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_ot_color_palette_get_count")>]
    extern uint32 ColorPaletteGetCount (IntPtr face)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_ot_color_palette_get_colors")>]
    extern uint32 ColorPaletteGetColors (IntPtr face, uint32 paletteIndex, uint32 startOffset, IntPtr colorCount, IntPtr colors)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_color_line_get_color_stops")>]
    extern uint32 ColorLineGetStops (IntPtr colorLine, uint32 start, IntPtr count, IntPtr stops)
    [<DllImport (LibraryName, CallingConvention = CallingConvention.Cdecl, EntryPoint = "hb_color_line_get_extend")>]
    extern int32 ColorLineGetExtend (IntPtr colorLine)

    let RequiredExports =
        [| "hb_blob_create_from_file"; "hb_blob_destroy"; "hb_face_create"; "hb_face_destroy"; "hb_face_get_upem"
           "hb_font_create"; "hb_font_destroy"; "hb_font_set_scale"; "hb_font_get_nominal_glyph"; "hb_ot_font_set_funcs"
           "hb_draw_funcs_create"; "hb_draw_funcs_destroy"; "hb_draw_funcs_set_move_to_func"; "hb_draw_funcs_set_line_to_func"
           "hb_draw_funcs_set_quadratic_to_func"; "hb_draw_funcs_set_cubic_to_func"; "hb_draw_funcs_set_close_path_func"; "hb_font_draw_glyph"
           "hb_paint_funcs_create"; "hb_paint_funcs_destroy"; "hb_paint_funcs_set_push_transform_func"; "hb_paint_funcs_set_pop_transform_func"
           "hb_paint_funcs_set_color_glyph_func"; "hb_paint_funcs_set_push_clip_glyph_func"; "hb_paint_funcs_set_push_clip_rectangle_func"
           "hb_paint_funcs_set_pop_clip_func"; "hb_paint_funcs_set_color_func"; "hb_paint_funcs_set_linear_gradient_func"
           "hb_paint_funcs_set_radial_gradient_func"; "hb_paint_funcs_set_sweep_gradient_func"; "hb_paint_funcs_set_push_group_func"
           "hb_paint_funcs_set_pop_group_func"; "hb_paint_funcs_set_custom_palette_color_func"; "hb_font_paint_glyph"; "hb_ot_color_glyph_has_paint"; "hb_ot_color_has_layers"
           "hb_ot_color_glyph_get_layers"; "hb_ot_color_palette_get_count"; "hb_ot_color_palette_get_colors"
           "hb_color_line_get_color_stops"; "hb_color_line_get_extend" |]

    let EnsureExports () =
        let mutable library = IntPtr.Zero
        let baseDirectory = AppContext.BaseDirectory
        let candidates =
            [| LibraryName
               LibraryName + ".dylib"
               LibraryName + ".so"
               "HarfBuzzSharp.dll"
               IO.Path.Combine (baseDirectory, "runtimes", "osx", "native", "libHarfBuzzSharp.dylib")
               IO.Path.Combine (baseDirectory, "runtimes", "linux-x64", "native", "libHarfBuzzSharp.so")
               IO.Path.Combine (baseDirectory, "runtimes", "linux-arm64", "native", "libHarfBuzzSharp.so")
               IO.Path.Combine (baseDirectory, "runtimes", "win-x64", "native", "libHarfBuzzSharp.dll")
               IO.Path.Combine (baseDirectory, "runtimes", "win-arm64", "native", "libHarfBuzzSharp.dll")
               IO.Path.Combine (baseDirectory, "runtimes", "win-x86", "native", "libHarfBuzzSharp.dll") |]
        let mutable index = 0
        while library = IntPtr.Zero && index < candidates.Length do
            NativeLibrary.TryLoad (candidates[index], &library) |> ignore
            index <- index + 1
        if library = IntPtr.Zero then
            raise (PlatformNotSupportedException ("The HarfBuzzSharp native library could not be loaded from any supported platform name."))
        try
            for exportName in RequiredExports do
                let mutable address = IntPtr.Zero
                if not (NativeLibrary.TryGetExport (library, exportName, &address)) then
                    raise (PlatformNotSupportedException ("The HarfBuzzSharp native ABI is missing required export '" + exportName + "'."))
        finally
            NativeLibrary.Free library

module private SlugColorFontImpl =

    open SlugColorFontNative

    [<NoEquality; NoComparison>]
    type private DrawCapture () =
        let commands = ResizeArray<ContourCommand> ()
        let mutable openPath = false
        let mutable start = Vector2.Zero
        let mutable current = Vector2.Zero

        member _.Commands = commands
        member _.MoveTo (x, y) =
            if openPath then
                if Vector2.DistanceSquared (current, start) > 1.0e-10f then commands.Add (LineTo start)
                commands.Add CloseContour
            start <- Vector2 (x, y)
            current <- start
            openPath <- true
            commands.Add (MoveTo start)
        member _.LineTo (x, y) =
            if openPath then
                let p = Vector2 (x, y)
                commands.Add (LineTo p)
                current <- p
        member _.QuadraticTo (cx, cy, x, y) =
            if openPath then
                let p = Vector2 (x, y)
                commands.Add (QuadraticCurveTo (Vector2 (cx, cy), p))
                current <- p
        member _.CubicTo (c1x, c1y, c2x, c2y, x, y) =
            if openPath then
                let p = Vector2 (x, y)
                commands.Add (CubicCurveTo (Vector2 (c1x, c1y), Vector2 (c2x, c2y), p))
                current <- p
        member _.Close () =
            if openPath then
                if Vector2.DistanceSquared (current, start) > 1.0e-10f then commands.Add (LineTo start)
                commands.Add CloseContour
                current <- start
                openPath <- false
        member this.ToSource () =
            if openPath then this.Close ()
            if commands.Count = 0 then None
            else
                try Some (SlugShapeRuntime.fromContourCommands commands SlugFillNonzero 1.0e-3f)
                with _ -> None

    type private PaintGroupBuilder () =
        let children = ResizeArray<SlugColorPaintNode> ()
        member _.Children = children
        member val CompositeMode = SlugCompositeMode.SlugCompositeSourceOver with get, set

    let private compositeMode mode =
        match mode with
        | 0 -> SlugCompositeMode.SlugCompositeClear
        | 1 -> SlugCompositeMode.SlugCompositeSource
        | 2 -> SlugCompositeMode.SlugCompositeDestination
        | 3 -> SlugCompositeMode.SlugCompositeSourceOver
        | 4 -> SlugCompositeMode.SlugCompositeDestinationOver
        | 5 -> SlugCompositeMode.SlugCompositeSourceIn
        | 6 -> SlugCompositeMode.SlugCompositeDestinationIn
        | 7 -> SlugCompositeMode.SlugCompositeSourceOut
        | 8 -> SlugCompositeMode.SlugCompositeDestinationOut
        | 9 -> SlugCompositeMode.SlugCompositeSourceAtop
        | 10 -> SlugCompositeMode.SlugCompositeDestinationAtop
        | 11 -> SlugCompositeMode.SlugCompositeXor
        | 12 -> SlugCompositeMode.SlugCompositePlus
        | 13 -> SlugCompositeMode.SlugCompositeScreen
        | 14 -> SlugCompositeMode.SlugCompositeOverlay
        | 15 -> SlugCompositeMode.SlugCompositeDarken
        | 16 -> SlugCompositeMode.SlugCompositeLighten
        | 17 -> SlugCompositeMode.SlugCompositeColorDodge
        | 18 -> SlugCompositeMode.SlugCompositeColorBurn
        | 19 -> SlugCompositeMode.SlugCompositeHardLight
        | 20 -> SlugCompositeMode.SlugCompositeSoftLight
        | 21 -> SlugCompositeMode.SlugCompositeDifference
        | 22 -> SlugCompositeMode.SlugCompositeExclusion
        | 23 -> SlugCompositeMode.SlugCompositeMultiply
        | 24 -> SlugCompositeMode.SlugCompositeHslHue
        | 25 -> SlugCompositeMode.SlugCompositeHslSaturation
        | 26 -> SlugCompositeMode.SlugCompositeHslColor
        | 27 -> SlugCompositeMode.SlugCompositeHslLuminosity
        | _ -> invalidArg (nameof mode) "Unknown HarfBuzz composite mode."

    type private PaintState (fontHandle : IntPtr, drawFuncs : IntPtr, foreground : Color, paletteIndex : uint32, foregroundBgra : uint32) =
        let root = ResizeArray<SlugColorPaintNode> ()
        let groups = ResizeArray<PaintGroupBuilder> ()
        let transforms = ResizeArray<Matrix4x4> ()
        let clips = ResizeArray<SlugColorPaintClip> ()
        let mutable activeCapture : DrawCapture option = None
        let mutable pendingFill = SlugColorPaintFill.Solid (Color.One, false)
        let mutable userData = IntPtr.Zero
        do transforms.Add Matrix4x4.Identity

        member _.FontHandle = fontHandle
        member _.DrawFuncs = drawFuncs
        member _.Foreground = foreground
        member _.PaletteIndex = paletteIndex
        member _.ForegroundBgra = foregroundBgra
        member _.Root = root
        member _.Groups = groups
        member _.Transforms = transforms
        member _.Clips = clips
        member _.UserData with get () = userData and set value = userData <- value
        member _.ActiveCapture with get () = activeCapture and set value = activeCapture <- value
        member _.PendingFill with get () = pendingFill and set value = pendingFill <- value
        member _.CurrentTransform = transforms[transforms.Count - 1]
        member _.CurrentChildren = if groups.Count = 0 then root else groups[groups.Count - 1].Children

        member _.CaptureGlyph glyph font =
            let capture = DrawCapture ()
            let previous = activeCapture
            activeCapture <- Some capture
            try
                FontDrawGlyph (font, glyph, drawFuncs, userData)
            finally
                activeCapture <- previous
            capture.ToSource ()

        member this.AddLeaf glyph source fill transform clipCount =
            let remainingClips = Array.init clipCount (fun index -> clips[index])
            let leaf = SlugColorPaintLeaf (glyph, source, fill, transform, this.CurrentTransform, remainingClips, SlugCompositeMode.SlugCompositeSourceOver)
            this.CurrentChildren.Add (SlugColorPaintNode.Leaf leaf)

        member this.Paint fill =
            pendingFill <- fill
            if clips.Count > 0 then
                let clipIndex = clips.Count - 1
                match clips[clipIndex] with
                | SlugColorPaintClip.Glyph (glyph, Some source, transform) ->
                    this.AddLeaf glyph source fill transform clipIndex
                | SlugColorPaintClip.Rectangle (bounds, transform) ->
                    let p0 = Vector2 (bounds.X, bounds.Y)
                    let p1 = Vector2 (bounds.Z, bounds.Y)
                    let p2 = Vector2 (bounds.Z, bounds.W)
                    let p3 = Vector2 (bounds.X, bounds.W)
                    let commands = [| MoveTo p0; LineTo p1; LineTo p2; LineTo p3; LineTo p0; CloseContour |]
                    let source = SlugShapeRuntime.fromContourCommands commands SlugFillNonzero 1.0e-3f
                    this.AddLeaf 0u source fill transform clipIndex
                | SlugColorPaintClip.Glyph (_, None, _) -> ()

        member this.PushGroup () = groups.Add (PaintGroupBuilder ())
        member this.PopGroup mode =
            if groups.Count = 0 then invalidOp "HarfBuzz paint group stack underflow."
            let index = groups.Count - 1
            let builder = groups[index]
            groups.RemoveAt index
            builder.CompositeMode <- compositeMode mode
            let node = SlugColorPaintNode.Group (SlugColorPaintGroup (builder.Children.ToArray (), builder.CompositeMode))
            this.CurrentChildren.Add node

        member this.ToTree () =
            if groups.Count <> 0 then invalidOp "HarfBuzz paint group stack was not balanced."
            if transforms.Count <> 1 then invalidOp "HarfBuzz transform stack was not balanced."
            if clips.Count <> 0 then invalidOp "HarfBuzz clip stack was not balanced."
            let flattened = ResizeArray<SlugColorPaintLeaf> ()
            let retained = ResizeArray<SlugColorPaintGroup> ()
            let rec flatten node =
                match node with
                | SlugColorPaintNode.Leaf leaf -> flattened.Add leaf
                | SlugColorPaintNode.Group group when group.CompositeMode = SlugCompositeMode.SlugCompositeSourceOver ->
                    for child in group.Children do flatten child
                | SlugColorPaintNode.Group group -> retained.Add group
            for node in root do flatten node
            SlugColorPaintTree (root.ToArray (), flattened.ToArray (), retained.ToArray ())
    let private stateFrom userData =
        try
            let handle = GCHandle.FromIntPtr userData
            Some (handle.Target :?> PaintState)
        with _ -> None

    let private colorFromBgra (value : uint32) =
        let b = single ((value >>> 24) &&& 0xffu) / 255.0f
        let g = single ((value >>> 16) &&& 0xffu) / 255.0f
        let r = single ((value >>> 8) &&& 0xffu) / 255.0f
        let a = single (value &&& 0xffu) / 255.0f
        Color (r, g, b, a)

    let private colorToBgra (value : Color) =
        let clamp x = uint (Math.Clamp (int (MathF.Round (x * 255.0f)), 0, 255))
        (clamp value.B <<< 24) ||| (clamp value.G <<< 16) ||| (clamp value.R <<< 8) ||| clamp value.A

    let private premultiply (value : Color) = Color (value.R * value.A, value.G * value.A, value.B * value.A, value.A)

    let private copyColorLine colorLine =
        if colorLine = IntPtr.Zero then
            { Kind = SlugColorPaintGradientKind.Linear (Vector2.Zero, Vector2.Zero, Vector2.Zero)
              Stops = [||]
              Extend = SlugColorPaintExtend.Pad }
        else
            let countPtr = Marshal.AllocHGlobal 4
            try
                Marshal.WriteInt32 (countPtr, 0)
                let total = int (ColorLineGetStops (colorLine, 0u, countPtr, IntPtr.Zero))
                let stops = ResizeArray<SlugColorLineStop> ()
                if total > 0 then
                    let maxPage = 32
                    let itemSize = Marshal.SizeOf<ColorStop> ()
                    let pagePtr = Marshal.AllocHGlobal (itemSize * maxPage)
                    try
                        let mutable offset = 0
                        while offset < total do
                            let request = min maxPage (total - offset)
                            Marshal.WriteInt32 (countPtr, request)
                            let received = int (ColorLineGetStops (colorLine, uint32 offset, countPtr, pagePtr))
                            let actual = min request (max 0 (Marshal.ReadInt32 countPtr))
                            for index in 0 .. actual - 1 do
                                let stop = Marshal.PtrToStructure<ColorStop> (IntPtr.Add (pagePtr, itemSize * index))
                                let color = colorFromBgra stop.Color
                                stops.Add { Offset = stop.Offset; IsForeground = stop.IsForeground <> 0; Color = color; PremultipliedColor = premultiply color }
                            if actual = 0 then offset <- total else offset <- offset + actual
                    finally Marshal.FreeHGlobal pagePtr
                { Kind = SlugColorPaintGradientKind.Linear (Vector2.Zero, Vector2.Zero, Vector2.Zero)
                  Stops = stops.ToArray ()
                  Extend = match ColorLineGetExtend colorLine with 1 -> SlugColorPaintExtend.Repeat | 2 -> SlugColorPaintExtend.Reflect | _ -> SlugColorPaintExtend.Pad }
            finally Marshal.FreeHGlobal countPtr

    let private drawMove (_funcs : IntPtr) (drawData : IntPtr) (_state : IntPtr) x y (_userData : IntPtr) =
        match stateFrom drawData with
        | Some state -> match state.ActiveCapture with Some capture -> capture.MoveTo (x, y) | None -> ()
        | None -> ()
    let private drawLine (_funcs : IntPtr) (drawData : IntPtr) (_state : IntPtr) x y (_userData : IntPtr) =
        match stateFrom drawData with
        | Some state -> match state.ActiveCapture with Some capture -> capture.LineTo (x, y) | None -> ()
        | None -> ()
    let private drawQuadratic (_funcs : IntPtr) (drawData : IntPtr) (_state : IntPtr) cx cy x y (_userData : IntPtr) =
        match stateFrom drawData with
        | Some state -> match state.ActiveCapture with Some capture -> capture.QuadraticTo (cx, cy, x, y) | None -> ()
        | None -> ()
    let private drawCubic (_funcs : IntPtr) (drawData : IntPtr) (_state : IntPtr) c1x c1y c2x c2y x y (_userData : IntPtr) =
        match stateFrom drawData with
        | Some state -> match state.ActiveCapture with Some capture -> capture.CubicTo (c1x, c1y, c2x, c2y, x, y) | None -> ()
        | None -> ()
    let private drawClose (_funcs : IntPtr) (drawData : IntPtr) (_state : IntPtr) (_userData : IntPtr) =
        match stateFrom drawData with
        | Some state -> match state.ActiveCapture with Some capture -> capture.Close () | None -> ()
        | None -> ()

    let private paintPushTransform (_funcs : IntPtr) (paintData : IntPtr) xx yx xy yy dx dy (_userData : IntPtr) =
        match stateFrom paintData with
        | Some state ->
            let local = Matrix4x4 (xx, yx, 0.0f, 0.0f, xy, yy, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, dx, dy, 0.0f, 1.0f)
            state.Transforms.Add (state.CurrentTransform * local)
        | None -> ()
    let private paintPopTransform (_funcs : IntPtr) (paintData : IntPtr) (_userData : IntPtr) =
        match stateFrom paintData with
        | Some state when state.Transforms.Count > 1 -> state.Transforms.RemoveAt (state.Transforms.Count - 1)
        | Some _ -> invalidOp "HarfBuzz paint transform stack underflow."
        | None -> ()
    let private paintColorGlyph (_funcs : IntPtr) (_paintData : IntPtr) (_glyph : uint32) (_font : IntPtr) (_userData : IntPtr) =
        // Returning false asks HarfBuzz to traverse the nested COLR paint graph itself.
        0
    let private paintPushClipGlyph (_funcs : IntPtr) (paintData : IntPtr) glyph font (_userData : IntPtr) =
        match stateFrom paintData with
        | Some state ->
            let source = state.CaptureGlyph glyph (if font = IntPtr.Zero then state.FontHandle else font)
            state.Clips.Add (SlugColorPaintClip.Glyph (glyph, source, state.CurrentTransform))
        | None -> ()
    let private paintPushClipRectangle (_funcs : IntPtr) (paintData : IntPtr) xmin ymin xmax ymax (_userData : IntPtr) =
        match stateFrom paintData with
        | Some state -> state.Clips.Add (SlugColorPaintClip.Rectangle (Vector4 (xmin, ymin, xmax, ymax), state.CurrentTransform))
        | None -> ()
    let private paintPopClip (_funcs : IntPtr) (paintData : IntPtr) (_userData : IntPtr) =
        match stateFrom paintData with
        | Some state when state.Clips.Count > 0 -> state.Clips.RemoveAt (state.Clips.Count - 1)
        | Some _ -> invalidOp "HarfBuzz paint clip stack underflow."
        | None -> ()
    let private paintColor (_funcs : IntPtr) (paintData : IntPtr) isForeground color (_userData : IntPtr) =
        match stateFrom paintData with
        | Some state ->
            let foreground = isForeground <> 0
            state.Paint (SlugColorPaintFill.Solid ((if foreground then state.Foreground else colorFromBgra color), foreground))
        | None -> ()
    let private paintLinearGradient (_funcs : IntPtr) (paintData : IntPtr) colorLine x0 y0 x1 y1 x2 y2 (_userData : IntPtr) =
        match stateFrom paintData with
        | Some state ->
            let line = copyColorLine colorLine
            state.Paint (SlugColorPaintFill.Gradient { line with Kind = SlugColorPaintGradientKind.Linear (Vector2 (x0, y0), Vector2 (x1, y1), Vector2 (x2, y2)) })
        | None -> ()
    let private paintRadialGradient (_funcs : IntPtr) (paintData : IntPtr) colorLine x0 y0 r0 x1 y1 r1 (_userData : IntPtr) =
        match stateFrom paintData with
        | Some state ->
            let line = copyColorLine colorLine
            state.Paint (SlugColorPaintFill.Gradient { line with Kind = SlugColorPaintGradientKind.Radial (Vector2 (x0, y0), r0, Vector2 (x1, y1), r1) })
        | None -> ()
    let private paintSweepGradient (_funcs : IntPtr) (paintData : IntPtr) colorLine x0 y0 startAngle endAngle (_userData : IntPtr) =
        match stateFrom paintData with
        | Some state ->
            let line = copyColorLine colorLine
            state.Paint (SlugColorPaintFill.Gradient { line with Kind = SlugColorPaintGradientKind.Sweep (Vector2 (x0, y0), startAngle, endAngle) })
        | None -> ()
    let private paintPushGroup (_funcs : IntPtr) (paintData : IntPtr) (_userData : IntPtr) =
        match stateFrom paintData with Some state -> state.PushGroup () | None -> ()
    let private paintPopGroup (_funcs : IntPtr) (paintData : IntPtr) mode (_userData : IntPtr) =
        match stateFrom paintData with Some state -> state.PopGroup mode | None -> ()
    let private paintCustomPalette (_funcs : IntPtr) (paintData : IntPtr) (_index : uint32) (_color : IntPtr) (_userData : IntPtr) =
        match stateFrom paintData with Some _ -> 0 | None -> 0

    let DrawMove = DrawMoveDelegate drawMove
    let DrawLine = DrawLineDelegate drawLine
    let DrawQuadratic = DrawQuadraticDelegate drawQuadratic
    let DrawCubic = DrawCubicDelegate drawCubic
    let DrawClose = DrawCloseDelegate drawClose
    let PaintPushTransform = PaintPushTransformDelegate paintPushTransform
    let PaintPopTransform = PaintPopTransformDelegate paintPopTransform
    let PaintColorGlyph = PaintColorGlyphDelegate paintColorGlyph
    let PaintPushClipGlyph = PaintPushClipGlyphDelegate paintPushClipGlyph
    let PaintPushClipRectangle = PaintPushClipRectangleDelegate paintPushClipRectangle
    let PaintPopClip = PaintPopClipDelegate paintPopClip
    let PaintColor = PaintColorDelegate paintColor
    let PaintLinearGradient = PaintLinearGradientDelegate paintLinearGradient
    let PaintRadialGradient = PaintRadialGradientDelegate paintRadialGradient
    let PaintSweepGradient = PaintSweepGradientDelegate paintSweepGradient
    let PaintPushGroup = PaintPushGroupDelegate paintPushGroup
    let PaintPopGroup = PaintPopGroupDelegate paintPopGroup
    let PaintCustomPalette = PaintCustomPaletteDelegate paintCustomPalette

    let InstallDrawFunctions funcs userData =
        DrawSetMove (funcs, DrawMove, userData, IntPtr.Zero)
        DrawSetLine (funcs, DrawLine, userData, IntPtr.Zero)
        DrawSetQuadratic (funcs, DrawQuadratic, userData, IntPtr.Zero)
        DrawSetCubic (funcs, DrawCubic, userData, IntPtr.Zero)
        DrawSetClose (funcs, DrawClose, userData, IntPtr.Zero)

    let InstallPaintFunctions funcs userData =
        PaintSetPushTransform (funcs, PaintPushTransform, userData, IntPtr.Zero)
        PaintSetPopTransform (funcs, PaintPopTransform, userData, IntPtr.Zero)
        PaintSetColorGlyph (funcs, PaintColorGlyph, userData, IntPtr.Zero)
        PaintSetPushClipGlyph (funcs, PaintPushClipGlyph, userData, IntPtr.Zero)
        PaintSetPushClipRectangle (funcs, PaintPushClipRectangle, userData, IntPtr.Zero)
        PaintSetPopClip (funcs, PaintPopClip, userData, IntPtr.Zero)
        PaintSetColor (funcs, PaintColor, userData, IntPtr.Zero)
        PaintSetLinearGradient (funcs, PaintLinearGradient, userData, IntPtr.Zero)
        PaintSetRadialGradient (funcs, PaintRadialGradient, userData, IntPtr.Zero)
        PaintSetSweepGradient (funcs, PaintSweepGradient, userData, IntPtr.Zero)
        PaintSetPushGroup (funcs, PaintPushGroup, userData, IntPtr.Zero)
        PaintSetPopGroup (funcs, PaintPopGroup, userData, IntPtr.Zero)
        PaintSetCustomPalette (funcs, PaintCustomPalette, userData, IntPtr.Zero)

    let captureGlyph fontHandle drawFuncs glyph =
        let state = PaintState (fontHandle, drawFuncs, Color.One, 0u, 0xffffffffu)
        let stateHandle = GCHandle.Alloc (state, GCHandleType.Normal)
        state.UserData <- GCHandle.ToIntPtr stateHandle
        try state.CaptureGlyph glyph fontHandle
        finally stateHandle.Free ()

    let queryLayers face glyph =
        let total = int (ColorGlyphGetLayers (face, glyph, 0u, IntPtr.Zero, IntPtr.Zero))
        if total <= 0 then [||]
        else
            let itemSize = Marshal.SizeOf<ColorLayer> ()
            let countPtr = Marshal.AllocHGlobal 4
            try
                let output = ResizeArray<ColorLayer> ()
                let pagePtr = Marshal.AllocHGlobal (itemSize * 32)
                try
                    let mutable offset = 0
                    while offset < total do
                        let request = min 32 (total - offset)
                        Marshal.WriteInt32 (countPtr, request)
                        let returned = ColorGlyphGetLayers (face, glyph, uint32 offset, countPtr, pagePtr)
                        let actual = min request (max 0 (Marshal.ReadInt32 countPtr))
                        for index in 0 .. actual - 1 do output.Add (Marshal.PtrToStructure<ColorLayer> (IntPtr.Add (pagePtr, index * itemSize)))
                        if actual = 0 then offset <- total else offset <- offset + actual
                        if returned = 0u && actual = 0 then offset <- total
                    output.ToArray ()
                finally Marshal.FreeHGlobal pagePtr
            finally Marshal.FreeHGlobal countPtr

    let paletteColor face paletteIndex colorIndex =
        if colorIndex = 0xffffu || paletteIndex >= ColorPaletteGetCount face then Color.One
        else
            let countPtr = Marshal.AllocHGlobal 4
            let colorPtr = Marshal.AllocHGlobal 4
            try
                Marshal.WriteInt32 (countPtr, 1)
                let total = ColorPaletteGetColors (face, paletteIndex, colorIndex, countPtr, colorPtr)
                if total = 0u || Marshal.ReadInt32 countPtr <= 0 then Color.Zero else colorFromBgra (uint32 (Marshal.ReadInt32 colorPtr))
            finally
                Marshal.FreeHGlobal colorPtr
                Marshal.FreeHGlobal countPtr

    let flattenPaint fontHandle drawFuncs paintFuncs glyph paletteIndex foreground =
        let state = PaintState (fontHandle, drawFuncs, foreground, paletteIndex, colorToBgra foreground)
        let stateHandle = GCHandle.Alloc (state, GCHandleType.Normal)
        state.UserData <- GCHandle.ToIntPtr stateHandle
        try
            FontPaintGlyph (fontHandle, glyph, paintFuncs, state.UserData, paletteIndex, colorToBgra foreground)
            Some (state.ToTree ())
        finally stateHandle.Free ()

    let private rectangleSource (bounds : Vector4) =
        let p0 = Vector2 (bounds.X, bounds.Y)
        let p1 = Vector2 (bounds.Z, bounds.Y)
        let p2 = Vector2 (bounds.Z, bounds.W)
        let p3 = Vector2 (bounds.X, bounds.W)
        SlugShapeRuntime.fromContourCommands
            [| MoveTo p0; LineTo p1; LineTo p2; LineTo p3; LineTo p0; CloseContour |]
            SlugFillNonzero
            1.0e-3f

    let private invertTransform name (transform : Matrix4x4) =
        let mutable inverse = Matrix4x4.Identity
        if not (Matrix4x4.Invert (transform, &inverse)) then
            invalidOp (name + " transform is singular.")
        inverse

    let private analyticMask sourceIndex transform =
        let inverse = invertTransform "COLR clip" transform
        { Kind = SlugMaskKind.Shape sourceIndex
          Parameters = Vector4 (inverse.M11, inverse.M21, inverse.M41, inverse.M12)
          Parameters2 = Vector4 (inverse.M22, inverse.M42, 0.0f, 0.0f)
          Invert = false }

    let private linearGradientKind (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) =
        let axis = p2 - p0
        let normal = Vector2 (-axis.Y, axis.X)
        let normalLengthSquared = Vector2.Dot (normal, normal)
        let projection = Vector2.Dot (p1 - p0, normal)
        if normalLengthSquared > 1.0e-12f && abs projection > 1.0e-6f then
            SlugGradientKind.Linear (p0, p0 + normal * (projection / normalLengthSquared))
        else SlugGradientKind.Linear (p0, p1)

    let createComposite (glyph : SlugColorGlyph) foreground =
        let sources = ResizeArray<SlugShapeSource> ()
        let gradients = ResizeArray<SlugGradient> ()
        let gradientStops = ResizeArray<SlugGradientStop> ()
        let masks = ResizeArray<SlugMaskState> ()
        let layers = ResizeArray<SlugLayerState> ()
        let addSource source =
            let index = sources.Count
            sources.Add source
            index
        let addMask (clip : SlugColorPaintClip) =
            let source, transform =
                match clip with
                | SlugColorPaintClip.Glyph (_, Some source, transform) -> source, transform
                | SlugColorPaintClip.Glyph (_, None, _) -> invalidOp "COLRv1 clip glyph has no analytic outline."
                | SlugColorPaintClip.Rectangle (bounds, transform) -> rectangleSource bounds, transform
            let sourceIndex = addSource source
            let maskIndex = masks.Count
            masks.Add (analyticMask sourceIndex transform)
            maskIndex
        let addSolid source transform color compositeMode maskIndex =
            let shapeIndex = addSource source
            let state = SlugLayerState.defaultState shapeIndex
            layers.Add
                { state with
                    Transform = transform
                    Color = color
                    MaskIndex = maskIndex
                    MaterialValues = if maskIndex >= 0 then Vector4.UnitX else Vector4.Zero
                    CompositeMode = compositeMode }
        let addPaintLeaf (leaf : SlugColorPaintLeaf) =
            if leaf.Clips.Length > 1 then
                invalidOp "The Slug renderer currently supports one analytic clip per COLRv1 paint leaf."
            let maskIndex = if leaf.Clips.Length = 0 then -1 else addMask leaf.Clips[0]
            match leaf.Fill with
            | SlugColorPaintFill.Solid (color, _) ->
                addSolid leaf.Source leaf.Transform color leaf.CompositeMode maskIndex
            | SlugColorPaintFill.Gradient gradient ->
                let stops =
                    gradient.Stops
                    |> Array.map (fun stop -> { Offset = stop.Offset; Color = stop.Color })
                let kind =
                    match gradient.Kind with
                    | SlugColorPaintGradientKind.Linear (p0, p1, p2) -> linearGradientKind p0 p1 p2
                    | SlugColorPaintGradientKind.Radial (center0, radius0, center1, radius1) ->
                        SlugGradientKind.FocalRadial (center0, radius0, center1, radius1)
                    | SlugColorPaintGradientKind.Sweep (center, startAngle, endAngle) ->
                        SlugGradientKind.SweepRange (center, startAngle, endAngle)
                let extend =
                    match gradient.Extend with
                    | SlugColorPaintExtend.Pad -> SlugGradientPad
                    | SlugColorPaintExtend.Repeat -> SlugGradientRepeat
                    | SlugColorPaintExtend.Reflect -> SlugGradientReflect
                let gradientIndex = gradients.Count
                gradients.Add (SlugGradient (kind, stops, Matrix4x4.Identity, extend))
                gradientStops.AddRange stops
                let shapeIndex = addSource leaf.Source
                let state = SlugLayerState.defaultState shapeIndex
                let compositeToFill = invertTransform "COLRv1 fill" leaf.FillTransform
                layers.Add
                    { state with
                        Transform = leaf.Transform
                        Color = Color.One
                        FillSource = SlugFillSource.Gradient gradientIndex
                        GradientTransform = leaf.Transform * compositeToFill
                        MaskIndex = maskIndex
                        MaterialValues = if maskIndex >= 0 then Vector4.UnitX else Vector4.Zero
                        CompositeMode = leaf.CompositeMode }
        match glyph with
        | SlugColorGlyph.Outline source ->
            addSolid source Matrix4x4.Identity foreground SlugCompositeMode.SlugCompositeSourceOver -1
        | SlugColorGlyph.ColrV0 colorLayers ->
            for layer in colorLayers do
                addSolid layer.Source Matrix4x4.Identity layer.Color SlugCompositeMode.SlugCompositeSourceOver -1
        | SlugColorGlyph.ColrV1 tree ->
            if tree.RetainedGroups.Length > 0 then
                invalidOp "COLRv1 paint groups using non-source-over compositing require an offscreen compositor."
            for leaf in tree.FlattenedLayers do addPaintLeaf leaf
        if layers.Count = 0 then invalidOp "The color glyph contains no renderable Slug layers."
        let data =
            SlugShapeRuntime.packWithResources
                (sources.ToArray ())
                (gradients.ToArray ())
                (gradientStops.ToArray ())
                (masks.ToArray ())
                [||]
        SlugShapeRuntime.createComposite data (layers.ToArray ())

[<Sealed>]
type SlugColorFontLoader (fontFilePath : string, ?faceIndex : int, ?fontScale : int) =
    let mutable blob = IntPtr.Zero
    let mutable face = IntPtr.Zero
    let mutable font = IntPtr.Zero
    let mutable drawFuncs = IntPtr.Zero
    let mutable disposed = false
    let faceIndex = defaultArg faceIndex 0
    let mutable scale = defaultArg fontScale 0

    do
        if String.IsNullOrWhiteSpace fontFilePath then nullArg (nameof fontFilePath)
        if faceIndex < 0 then invalidArg (nameof faceIndex) "The face index cannot be negative."
        try
            SlugColorFontNative.EnsureExports ()
            let path = Path.GetFullPath fontFilePath
            let pathBytes = Encoding.UTF8.GetBytes (path + "\000")
            let pathPtr = Marshal.AllocHGlobal pathBytes.Length
            try
                Marshal.Copy (pathBytes, 0, pathPtr, pathBytes.Length)
                blob <- SlugColorFontNative.BlobCreateFromFile pathPtr
            finally Marshal.FreeHGlobal pathPtr
            if blob = IntPtr.Zero then raise (FileNotFoundException ("HarfBuzz could not open the font file.", path))
            face <- SlugColorFontNative.FaceCreate (blob, uint32 faceIndex)
            if face = IntPtr.Zero then invalidOp "HarfBuzz could not create the font face."
            font <- SlugColorFontNative.FontCreate face
            if font = IntPtr.Zero then invalidOp "HarfBuzz could not create the HarfBuzz font."
            SlugColorFontNative.OpenTypeFontSetFuncs font
            if scale <= 0 then scale <- int (SlugColorFontNative.FaceGetUpem face)
            if scale <= 0 then scale <- 1
            SlugColorFontNative.FontSetScale (font, scale, scale)
            drawFuncs <- SlugColorFontNative.DrawFuncsCreate ()
            if drawFuncs = IntPtr.Zero then invalidOp "HarfBuzz could not create draw functions."
            SlugColorFontImpl.InstallDrawFunctions drawFuncs IntPtr.Zero
        with exn ->
            if drawFuncs <> IntPtr.Zero then SlugColorFontNative.DrawFuncsDestroy drawFuncs; drawFuncs <- IntPtr.Zero
            if font <> IntPtr.Zero then SlugColorFontNative.FontDestroy font; font <- IntPtr.Zero
            if face <> IntPtr.Zero then SlugColorFontNative.FaceDestroy face; face <- IntPtr.Zero
            if blob <> IntPtr.Zero then SlugColorFontNative.BlobDestroy blob; blob <- IntPtr.Zero
            reraise ()

    member _.FontScale = scale
    member _.UnitsPerEm = scale

    member private _.CheckDisposed () = if disposed then raise (ObjectDisposedException (nameof SlugColorFontLoader))
    member this.TryGetGlyphId (codePoint : uint32) =
        this.CheckDisposed ()
        if codePoint > 0x10ffffu || (codePoint >= 0xd800u && codePoint <= 0xdfffu) then
            invalidArg (nameof codePoint) "The code point must be a Unicode scalar value."
        let mutable glyphId = 0u
        if SlugColorFontNative.FontGetNominalGlyph (font, codePoint, &glyphId) = 0 then None
        else Some glyphId

    member this.GetGlyphId (codePoint : uint32) =
        match this.TryGetGlyphId codePoint with
        | Some glyphId -> glyphId
        | None -> raise (KeyNotFoundException ("The font has no nominal glyph for the requested Unicode scalar value."))


    member this.TryLoadGlyph (glyphId : uint32, ?paletteIndex : uint32, ?foreground : Color) =
        this.CheckDisposed ()
        let paletteIndex = defaultArg paletteIndex 0u
        let foreground = defaultArg foreground Color.One
        if SlugColorFontNative.ColorGlyphHasPaint (face, glyphId) <> 0 then
            let paintFuncs = SlugColorFontNative.PaintFuncsCreate ()
            if paintFuncs = IntPtr.Zero then invalidOp "HarfBuzz could not create paint functions."
            try
                // The paint callbacks are rooted in SlugColorFontImpl for the complete native call.
                let stateUserData = IntPtr.Zero
                SlugColorFontImpl.InstallPaintFunctions paintFuncs stateUserData
                match SlugColorFontImpl.flattenPaint font drawFuncs paintFuncs glyphId paletteIndex foreground with
                | Some paint -> Some (SlugColorGlyph.ColrV1 paint)
                | None -> None
            finally SlugColorFontNative.PaintFuncsDestroy paintFuncs
        elif SlugColorFontNative.ColorHasLayers face <> 0 then
            let layers = SlugColorFontImpl.queryLayers face glyphId
            if layers.Length = 0 then
                match SlugColorFontImpl.captureGlyph font drawFuncs glyphId with Some source -> Some (SlugColorGlyph.Outline source) | None -> None
            else
                let output = ResizeArray<SlugColorGlyphLayer> ()
                for layer in layers do
                    match SlugColorFontImpl.captureGlyph font drawFuncs layer.Glyph with
                    | Some source ->
                        let isForeground = layer.ColorIndex = 0xffffu
                        let color = if isForeground then foreground else SlugColorFontImpl.paletteColor face paletteIndex layer.ColorIndex
                        output.Add (SlugColorGlyphLayer (layer.Glyph, color, isForeground, source))
                    | None -> ()
                Some (SlugColorGlyph.ColrV0 (output.ToArray ()))
        else
            match SlugColorFontImpl.captureGlyph font drawFuncs glyphId with
            | Some source -> Some (SlugColorGlyph.Outline source)
            | None -> None

    member this.LoadGlyph (glyphId : uint32, ?paletteIndex : uint32, ?foreground : Color) =
        match this.TryLoadGlyph (glyphId, ?paletteIndex = paletteIndex, ?foreground = foreground) with
        | Some glyph -> glyph
        | None -> raise (KeyNotFoundException ("The requested glyph has no drawable outline or color paint."))
    member this.LoadComposite (glyphId : uint32, ?paletteIndex : uint32, ?foreground : Color) =
        let foreground = defaultArg foreground Color.One
        let glyph = this.LoadGlyph (glyphId, ?paletteIndex = paletteIndex, foreground = foreground)
        SlugColorFontImpl.createComposite glyph foreground

    member this.Dispose () =
        if not disposed then
            disposed <- true
            if drawFuncs <> IntPtr.Zero then SlugColorFontNative.DrawFuncsDestroy drawFuncs; drawFuncs <- IntPtr.Zero
            if font <> IntPtr.Zero then SlugColorFontNative.FontDestroy font; font <- IntPtr.Zero
            if face <> IntPtr.Zero then SlugColorFontNative.FaceDestroy face; face <- IntPtr.Zero
            if blob <> IntPtr.Zero then SlugColorFontNative.BlobDestroy blob; blob <- IntPtr.Zero

    interface IDisposable with
        member this.Dispose () = this.Dispose ()

