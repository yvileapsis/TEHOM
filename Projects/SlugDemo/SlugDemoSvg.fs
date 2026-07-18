namespace SlugDemo
open System
open System.IO
open System.Numerics
open Nu

[<RequireQualifiedAccess>]
module SlugDemoSvg =

    // osgSlug's NanoSVG demo loads arbitrary external SVG files.  Keep the small
    // SlugHorn-style logo here and load NanoSVG's detailed tiger fixture below.
    let private authoredSvg =
        """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 360 230">
  <defs>
    <linearGradient id="arcGradient" gradientUnits="userSpaceOnUse" x1="15" y1="120" x2="285" y2="100" gradientTransform="rotate(-4 150 110)">
      <stop offset="0" stop-color="#26b890"/>
      <stop offset="0.52" stop-color="#32c5a8"/>
      <stop offset="1" stop-color="#2575c5"/>
    </linearGradient>
    <linearGradient id="arcViolet" gradientUnits="userSpaceOnUse" x1="92" y1="91" x2="230" y2="79">
      <stop offset="0" stop-color="#7b63ef"/>
      <stop offset="0.55" stop-color="#b17cff"/>
      <stop offset="1" stop-color="#55b9dd"/>
    </linearGradient>
    <radialGradient id="shellGradient" gradientUnits="userSpaceOnUse" cx="214" cy="79" r="31">
      <stop offset="0" stop-color="#f7c9ff"/>
      <stop offset="0.35" stop-color="#b77cff"/>
      <stop offset="1" stop-color="#623fd0"/>
    </radialGradient>
    <linearGradient id="slugGradient" gradientUnits="userSpaceOnUse" x1="179" y1="112" x2="218" y2="88">
      <stop offset="0" stop-color="#e8751c"/>
      <stop offset="0.55" stop-color="#f69d26"/>
      <stop offset="1" stop-color="#f3c143"/>
    </linearGradient>
  </defs>

  <g id="slugHornLogo">
    <path id="greenHorn" fill="url(#arcGradient)" d="M16 114 C74 81 137 64 194 65 C238 66 266 83 286 103 L282 111 C255 91 230 80 192 76 C134 72 73 90 21 121 Z"/>
    <path id="violetHorn" fill="url(#arcViolet)" d="M95 91 C132 76 175 73 211 80 C235 84 254 94 268 105 L263 111 C243 99 226 93 207 89 C171 82 132 85 101 98 Z"/>
    <path id="blueHorn" fill="#3b9dd2" opacity="0.8" d="M156 73 C181 67 210 69 232 77 L228 83 C207 76 184 75 160 80 Z"/>

    <path id="antennaLeft" fill="#e2782a" d="M189 80 L186 50 L190 50 L194 80 Z"/>
    <path id="antennaRight" fill="#e2782a" d="M205 77 L207 43 L211 43 L210 78 Z"/>
    <circle id="antennaLeftTip" fill="#54b987" cx="188" cy="47" r="5"/>
    <circle id="antennaRightTip" fill="#e8b450" cx="209" cy="40" r="5"/>
    <ellipse id="slugBody" fill="url(#slugGradient)" cx="192" cy="104" rx="28" ry="21"/>
    <ellipse id="slugShell" fill="url(#shellGradient)" cx="216" cy="82" rx="27" ry="29"/>
    <circle id="slugShellHighlight" fill="#f6d1a5" cx="220" cy="65" r="8"/>
    <circle id="slugEyeLeft" fill="#fbfbfb" cx="185" cy="91" r="6"/>
    <circle id="slugEyeRight" fill="#fbfbfb" cx="202" cy="89" r="6"/>
    <circle id="slugPupilLeft" fill="#172735" cx="185" cy="91" r="2.5"/>
    <circle id="slugPupilRight" fill="#172735" cx="202" cy="89" r="2.5"/>
    <path id="slugSmile" fill="#b85028" d="M185 108 C192 114 199 114 205 108 C200 120 190 120 185 108 Z"/>

    <path id="wordS" fill="#26313a" d="M48 157 H72 C80 157 84 161 84 168 C84 175 80 178 73 179 L60 180 C57 180 56 182 56 185 C56 188 59 190 63 190 H84 V198 H61 C51 198 46 193 46 185 C46 178 51 174 59 173 L71 172 C74 172 75 170 75 168 C75 166 73 165 70 165 H48 Z"/>
    <path id="wordL" fill="#26313a" d="M89 157 H101 V198 H89 Z"/>
    <path id="wordU" fill="#26313a" d="M108 157 H120 V183 C120 188 122 190 126 190 C130 190 132 188 132 183 V157 H144 V198 H133 V194 C130 197 126 199 122 199 C113 199 108 194 108 184 Z"/>
    <path id="wordG" fill="#26313a" fill-rule="evenodd" d="M169 157 C153 157 147 165 147 177 C147 190 154 198 168 198 H177 V205 C177 208 175 209 170 209 H151 V217 H174 C185 217 189 212 189 202 V157 Z M168 166 H177 V189 H168 C162 189 159 185 159 177 C159 170 162 166 168 166 Z"/>
    <path id="wordH" fill="#26313a" d="M195 157 H207 V172 H219 V157 H231 V198 H219 V181 H207 V198 H195 Z"/>
    <path id="wordO" fill="#26313a" fill-rule="evenodd" d="M249 157 C235 157 229 165 229 178 C229 191 235 198 249 198 C263 198 269 191 269 178 C269 165 263 157 249 157 Z M249 166 C255 166 257 170 257 178 C257 186 255 190 249 190 C243 190 241 186 241 178 C241 170 243 166 249 166 Z"/>
    <path id="wordR" fill="#26313a" d="M278 157 H289 V162 C292 158 296 156 302 157 V167 C296 166 292 168 290 172 V198 H278 Z"/>
    <path id="wordN" fill="#26313a" d="M310 157 H321 V161 C325 158 329 157 334 157 C343 157 347 163 347 172 V198 H335 V174 C335 169 333 167 329 167 C325 167 322 169 322 174 V198 H310 Z"/>
  </g>

</svg>"""

    let private authoredSvgPath =
        lazy
            let path = Path.Combine (Path.GetTempPath (), "nu-slugdemo-slughorn-logo.svg")
            File.WriteAllText (path, authoredSvg)
            path

    let private createUprightComposite (document : SlugSvgDocument) =
        let bounds = document.Bounds
        let centerY = (bounds.Min.Y + bounds.Max.Y) * 0.5f
        let reflection =
            Matrix4x4.CreateTranslation (0.0f, -centerY, 0.0f) *
            Matrix4x4.CreateScale (1.0f, -1.0f, 1.0f) *
            Matrix4x4.CreateTranslation (0.0f, centerY, 0.0f)
        let layers = document.Layers
        let reflectPoint (point : Vector2) = Vector2.Transform (point, reflection)
        let sources =
            document.Sources
            |> Array.map (fun source ->
                let contours =
                    source.Contours
                    |> Array.map (Array.map (fun curve ->
                        { P1 = reflectPoint curve.P1
                          P2 = reflectPoint curve.P2
                          P3 = reflectPoint curve.P3 }))
                let corners =
                    [| v2 source.Bounds.Min.X source.Bounds.Min.Y
                       v2 source.Bounds.Max.X source.Bounds.Min.Y
                       v2 source.Bounds.Max.X source.Bounds.Max.Y
                       v2 source.Bounds.Min.X source.Bounds.Max.Y |]
                    |> Array.map reflectPoint
                let minPoint = corners |> Array.reduce (fun a b -> Vector2.Min (a, b))
                let maxPoint = corners |> Array.reduce (fun a b -> Vector2.Max (a, b))
                { source with
                    Contours = contours
                    Bounds = { Min = minPoint; Max = maxPoint } })
        let gradients = document.Gradients
        let data =
            SlugShapeRuntime.packWithResources
                sources
                gradients
                document.GradientStops
                document.Masks
                [||]
        let layerStates =
            layers
            |> Array.map (fun layer ->
                let state = SlugLayerState.defaultState layer.SourceIndex
                let maskIndex = if layer.MaskIndex >= 0 then layer.MaskIndex else layer.ClipIndex
                let gradientTransform =
                    match layer.FillSource with
                    | SlugFillSource.Gradient gradientIndex ->
                        let mutable inverse = Matrix4x4.Identity
                        if not (Matrix4x4.Invert (gradients[gradientIndex].Transform, &inverse)) then
                            invalidOp "SVG gradient transform is singular."
                        reflection * inverse
                    | _ -> Matrix4x4.Identity
                { state with
                    Transform = reflection * layer.Transform * reflection
                    Color = layer.Color
                    FillSource = layer.FillSource
                    GradientTransform = gradientTransform
                    MaskIndex = maskIndex
                    MaterialValues = if maskIndex >= 0 then Vector4.UnitX else Vector4.Zero
                    CompositeMode = layer.CompositeMode })
        SlugShapeRuntime.createComposite data layerStates


    let private logoComposite =
        lazy
            (let path = authoredSvgPath.Value
             try
                 SlugSvg.load path 1.0e-3f
                 |> createUprightComposite
             finally
                 try File.Delete path with _ -> ())

    let private tigerComposite =
        lazy
            (Path.Combine (AppContext.BaseDirectory, "Assets", "Default", "Tiger.svg")
             |> fun path -> SlugSvg.load path 1.0e-3f
             |> createUprightComposite)

    let private placeSvg name composite position size elevation world =
        let bounds = SlugDemoContours.getCompositeLayerBounds composite
        SlugDemoContours.placeCompositeInBounds
            name
            composite
            bounds
            position
            size
            Quaternion.Identity
            elevation
            None
            world

    let draw (world : World) =
        // NanoSVG keeps y-down source geometry and osgSlug applies one outer
        // scale (1, -1, 1).  createUprightComposite bakes that same transform.
        placeSvg
            "SvgSlugHornLogo"
            logoComposite.Value
            (v3 -155.0f 8.0f 0.0f)
            (v3 280.0f 150.0f 0.0f)
            2.0f
            world
        placeSvg
            "SvgTiger"
            tigerComposite.Value
            (v3 160.0f -18.0f 0.0f)
            (v3 230.0f 230.0f 0.0f)
            2.0f
            world
