namespace SlugShowcase
open System
open System.IO
open System.Numerics
open Nu
open SlugDemoVectorSupport

[<RequireQualifiedAccess>]
module SlugDemoSvg =

    // The original osgSlug NanoSVG example accepts arbitrary real-world SVG
    // files. The showcase keeps that same loader path while embedding a small
    // authored SlugHorn-style logo and tiger illustration when no upstream
    // fixture is shipped with the repository.
    let private authoredSvg =
        """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 600 300">
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
    <linearGradient id="tigerFur" gradientUnits="userSpaceOnUse" x1="330" y1="55" x2="550" y2="266" gradientTransform="rotate(-7 440 160)">
      <stop offset="0" stop-color="#f7ad35"/>
      <stop offset="0.48" stop-color="#d96b17"/>
      <stop offset="1" stop-color="#a94013"/>
    </linearGradient>
    <linearGradient id="tigerWhite" gradientUnits="userSpaceOnUse" x1="365" y1="122" x2="505" y2="260">
      <stop offset="0" stop-color="#fffdf4"/>
      <stop offset="0.58" stop-color="#f5f5ee"/>
      <stop offset="1" stop-color="#cfd4db"/>
    </linearGradient>
    <linearGradient id="tigerMouth" gradientUnits="userSpaceOnUse" x1="418" y1="207" x2="454" y2="247">
      <stop offset="0" stop-color="#ff8596"/>
      <stop offset="1" stop-color="#d82f57"/>
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

  <g id="tigerArtwork" transform="translate(7 -3) rotate(-2 445 158)">
    <path id="tigerSilhouette" fill="url(#tigerFur)" d="M321 122 C315 99 318 67 337 40 L371 61 C392 49 416 44 443 48 C469 44 495 50 516 63 L551 40 C567 65 569 97 562 121 C575 150 574 188 557 216 C542 242 516 260 486 273 C467 281 450 278 437 268 C422 280 401 282 382 274 C350 262 328 243 315 216 C302 188 306 149 321 122 Z"/>
    <path id="leftEar" fill="#17161a" d="M327 73 L337 31 L369 57 L359 88 Z"/>
    <path id="leftEarColor" fill="#f2a436" d="M337 51 L340 42 L357 59 L350 67 Z"/>
    <path id="rightEar" fill="#17161a" d="M511 59 L548 32 L560 78 L542 91 Z"/>
    <path id="rightEarColor" fill="#f2a436" d="M530 58 L547 45 L551 66 L541 72 Z"/>

    <path id="foreheadStripe1" fill="#16171b" d="M378 58 C390 49 406 45 418 44 L425 52 C410 57 399 65 390 77 Z"/>
    <path id="foreheadStripe2" fill="#16171b" d="M432 47 C450 45 466 49 477 54 L472 65 C458 59 446 58 436 60 Z"/>
    <path id="foreheadStripe3" fill="#16171b" d="M490 60 C501 65 510 72 519 84 L509 92 C501 81 494 75 484 71 Z"/>
    <path id="leftFaceStripe1" fill="#17171b" d="M335 101 C349 92 365 86 378 88 L373 102 C360 102 350 108 341 117 Z"/>
    <path id="leftFaceStripe2" fill="#17171b" d="M324 125 C343 119 357 120 370 128 L364 140 C350 134 338 136 327 143 Z"/>
    <path id="leftFaceStripe3" fill="#17171b" d="M319 156 C337 151 353 155 363 165 L355 177 C345 168 335 166 320 171 Z"/>
    <path id="rightFaceStripe1" fill="#17171b" d="M535 101 C521 93 506 88 493 90 L498 104 C511 103 521 109 530 117 Z"/>
    <path id="rightFaceStripe2" fill="#17171b" d="M548 126 C531 119 515 121 502 129 L508 141 C522 135 534 137 545 145 Z"/>
    <path id="rightFaceStripe3" fill="#17171b" d="M554 158 C536 151 520 155 509 165 L517 178 C529 169 539 167 554 173 Z"/>

    <path id="leftCheek" fill="url(#tigerWhite)" d="M331 128 C346 116 370 116 391 131 C400 144 400 171 389 193 C379 211 363 220 347 208 C333 196 325 171 331 128 Z"/>
    <path id="rightCheek" fill="url(#tigerWhite)" d="M549 128 C534 116 510 116 489 131 C480 144 480 171 491 193 C501 211 517 220 533 208 C547 196 555 171 549 128 Z"/>
    <path id="leftCheekStripe" fill="#17171b" d="M347 142 C360 137 375 140 384 150 L377 160 C367 153 357 152 347 157 Z"/>
    <path id="rightCheekStripe" fill="#17171b" d="M533 142 C520 137 505 140 496 150 L503 160 C513 153 523 152 533 157 Z"/>
    <path id="leftJawStripe" fill="#17171b" d="M347 193 C359 199 372 198 383 190 L388 203 C375 216 359 217 347 210 Z"/>
    <path id="rightJawStripe" fill="#17171b" d="M533 193 C521 199 508 198 497 190 L492 203 C505 216 521 217 533 210 Z"/>

    <path id="muzzle" fill="url(#tigerWhite)" d="M386 177 C396 158 416 151 437 160 C458 151 478 158 488 177 C495 193 490 216 474 229 C461 241 450 247 437 248 C424 247 413 241 400 229 C384 216 379 193 386 177 Z"/>
    <ellipse id="nose" fill="#17171b" cx="437" cy="184" rx="16" ry="11"/>
    <path id="noseHighlight" fill="#4f4b4b" d="M427 180 C432 175 439 174 445 178 C441 184 434 186 427 180 Z"/>
    <path id="mouthLeft" fill="#17171b" d="M437 193 C428 201 418 202 408 198 C416 210 427 212 437 203 Z"/>
    <path id="mouthRight" fill="#17171b" d="M437 193 C446 201 456 202 466 198 C458 210 447 212 437 203 Z"/>
    <path id="openMouth" fill="#17171b" fill-rule="evenodd" d="M406 204 C418 214 429 217 437 216 C445 217 456 214 468 204 C468 227 456 242 437 247 C418 242 406 227 406 204 Z M416 216 C423 223 430 226 437 226 C444 226 451 223 458 216 C454 233 447 238 437 239 C427 238 420 233 416 216 Z"/>
    <path id="tongue" fill="url(#tigerMouth)" d="M422 223 C429 227 445 227 452 223 C450 239 445 244 437 245 C429 244 424 239 422 223 Z"/>

    <ellipse id="leftEyeWhite" fill="#f9f9f1" cx="389" cy="125" rx="23" ry="16"/>
    <ellipse id="rightEyeWhite" fill="#f9f9f1" cx="485" cy="125" rx="23" ry="16"/>
    <ellipse id="leftEye" fill="#8bd53d" cx="393" cy="126" rx="12" ry="10"/>
    <ellipse id="rightEye" fill="#8bd53d" cx="481" cy="126" rx="12" ry="10"/>
    <ellipse id="leftPupil" fill="#101b16" cx="394" cy="126" rx="4" ry="9"/>
    <ellipse id="rightPupil" fill="#101b16" cx="480" cy="126" rx="4" ry="9"/>
    <circle id="leftEyeGlint" fill="#ffffff" cx="397" cy="122" r="2.5"/>
    <circle id="rightEyeGlint" fill="#ffffff" cx="483" cy="122" r="2.5"/>

    <path id="leftWhiskerTop" fill="#ffffff" d="M387 188 C365 178 342 176 321 180 L321 184 C344 183 366 187 388 194 Z"/>
    <path id="leftWhiskerBottom" fill="#ffffff" d="M388 199 C364 199 341 204 323 213 L325 217 C344 209 366 205 390 205 Z"/>
    <path id="rightWhiskerTop" fill="#ffffff" d="M487 188 C509 178 532 176 553 180 L553 184 C530 183 508 187 486 194 Z"/>
    <path id="rightWhiskerBottom" fill="#ffffff" d="M486 199 C510 199 533 204 551 213 L549 217 C530 209 508 205 484 205 Z"/>
    <path id="chinFur" fill="url(#tigerWhite)" d="M393 235 C406 251 421 263 437 266 C453 263 468 251 481 235 C477 260 463 273 437 278 C411 273 397 260 393 235 Z"/>
  </g>
</svg>"""

    let private authoredSvgPath =
        lazy
            let path = Path.Combine (Path.GetTempPath (), "nu-slug-showcase-slughorn-tiger.svg")
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
        let originalSources = document.Sources
        let isWordmarkSource = Array.zeroCreate originalSources.Length
        let mutable wordmarkMinX = Single.PositiveInfinity
        let mutable wordmarkMaxX = Single.NegativeInfinity
        for layer in layers do
            match layer.ElementId with
            | Some id when id.StartsWith ("word", StringComparison.Ordinal) ->
                isWordmarkSource[layer.SourceIndex] <- true
                let sourceBounds = originalSources[layer.SourceIndex].Bounds
                wordmarkMinX <- min wordmarkMinX sourceBounds.Min.X
                wordmarkMaxX <- max wordmarkMaxX sourceBounds.Max.X
            | _ -> ()
        if not (Single.IsFinite wordmarkMinX && Single.IsFinite wordmarkMaxX) then
            invalidOp "Authored SVG wordmark sources were not found."
        let reflectPoint reflectX (point : Vector2) =
            let point =
                if reflectX then v2 (wordmarkMinX + wordmarkMaxX - point.X) point.Y
                else point
            Vector2.Transform (point, reflection)
        let sources =
            originalSources
            |> Array.mapi (fun sourceIndex source ->
                let reflectX = isWordmarkSource[sourceIndex]
                let contours =
                    source.Contours
                    |> Array.map (fun contour ->
                        if reflectX then
                            // The second reflection restores the wordmark's authored handedness;
                            // retaining contour order preserves its original winding.
                            contour
                            |> Array.map (fun curve ->
                                { P1 = reflectPoint true curve.P1
                                  P2 = reflectPoint true curve.P2
                                  P3 = reflectPoint true curve.P3 })
                        else
                            contour
                            |> Array.rev
                            |> Array.map (fun curve ->
                                { P1 = reflectPoint false curve.P3
                                  P2 = reflectPoint false curve.P2
                                  P3 = reflectPoint false curve.P1 }))
                let reflectedMin =
                    reflectPoint reflectX
                        (v2 (if reflectX then source.Bounds.Max.X else source.Bounds.Min.X) source.Bounds.Max.Y)
                let reflectedMax =
                    reflectPoint reflectX
                        (v2 (if reflectX then source.Bounds.Min.X else source.Bounds.Max.X) source.Bounds.Min.Y)
                { source with
                    Contours = contours
                    Bounds = { Min = reflectedMin; Max = reflectedMax } })
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


    let private authoredSvgShape =
        lazy
            let path = authoredSvgPath.Value
            try
                let document = SlugSvg.load path 1.0e-3f
                AnalyticSlug (createUprightComposite document, None)
            finally
                try File.Delete path with _ -> ()

    let draw (world : World) =
        // SlugSvg preserves SVG's y-down coordinates. The scene bakes osgSlug's
        // scale (1, -1, 1) into the analytic sources above, keeping the culled
        // Slug layer quads front-facing and the full composition centered.
        SlugShowcaseContours.placeContour
            "SvgRealWorldComposition"
            authoredSvgShape.Value
            (v3 0.0f -14.0f 0.0f)
            (v3 290.0f 145.0f 0.0f)
            Quaternion.Identity
            2.0f
            world
