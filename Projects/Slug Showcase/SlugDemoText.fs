namespace SlugShowcase
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SlugDemoText =

    // osgslug-text reads arbitrary UTF-8 input, splits only on LF, and drops the
    // final empty item produced by a trailing newline before appending one LF to
    // each submitted line. Keep that behavior for the showcase's deterministic
    // poem input as well.
    let private sourceText =
        String.concat "\n"
            [| "In Xanadu did Kubla Khan"
               "A stately pleasure-dome decree:"
               "Where Alph, the sacred river, ran"
               "Through caverns measureless to man"
               "Down to a sunless sea."
               "So twice five miles of fertile ground"
               "With walls and towers were girdled round:"
               "And there were gardens bright with sinuous rills,"
               "Where blossomed many an incense-bearing tree;"
               "And here were forests ancient as the hills,"
               "Enfolding sunny spots of greenery."
               "But oh! that deep romantic chasm which slanted"
               "Down the green hill athwart a cedarn cover!"
               "A savage place! as holy and enchanted"
               "As e'er beneath a waning moon was haunted"
               "By woman wailing for her demon-lover!"
               "And from this chasm, with ceaseless turmoil seething,"
               "As if this earth in fast thick pants were breathing,"
               "A mighty fountain momently was forced:"
               "Amid whose swift half-intermitted burst"
               "Huge fragments vaulted like rebounding hail,"
               "Or chiselled flakes beneath the thresher's flail:"
               "And 'mid these dancing rocks at once and ever"
               "It flung up momently the sacred river."
               "Five miles meandering with a mazy motion"
               "Through wood and dale the sacred river ran,"
               "Then reached the caverns measureless to man,"
               "And sank in tumult to a lifeless ocean:"
               "And 'mid this tumult Kubla heard from far"
               "Ancestral voices prophesying war!"
               "The shadow of the dome of pleasure"
               "Floated midway on the waves;"
               "Where was heard the mingled measure"
               "From the fountain and the caves." |] + "\n"
    let private splitLines (text : string) =
        let lines = text.Split ([| '\n' |], StringSplitOptions.None)
        if lines.Length > 0 && String.IsNullOrEmpty lines.[lines.Length - 1] then
            lines.[0 .. lines.Length - 2]
        else
            lines

    let private canonicalLines = splitLines sourceText
    let private canonicalText =
        canonicalLines
        |> Array.map (fun line -> line + "\n")
        |> String.concat ""

    let private sourceCameraSize = v2 800.0f 600.0f
    let private sourceInset = 10.0f
    let private sourceFontSize = 16.0f
    // osgSlug advances a line by the font size plus its line gap and uses 1.2x
    // as the source fallback. Reserve that source-space extent for every line
    // so the complete deterministic input, rather than only its camera, fits.
    let private sourceLineAdvance = sourceFontSize * 1.2f
    let private sourceTextHeight =
        sourceFontSize +
        single (max 0 (canonicalLines.Length - 1)) * sourceLineAdvance
    let private sourceLayoutSize =
        v2
            sourceCameraSize.X
            (max sourceCameraSize.Y (sourceInset * 2.0f + sourceTextHeight))
    let private sourceTextPerimeterSize =
        sourceLayoutSize - v2 (sourceInset * 2.0f) (sourceInset * 2.0f)

    let draw (world : World) =
        let viewportSize = Constants.Render.DisplayVirtualResolution.V2
        let navigationHeight = 60.0f
        let contentSize = v2 viewportSize.X (max 1.0f (viewportSize.Y - navigationHeight))
        let fitScale =
            min
                (contentSize.X / sourceLayoutSize.X)
                (contentSize.Y / sourceLayoutSize.Y)
        let fittedTextPerimeterSize = sourceTextPerimeterSize * fitScale
        let fittedContentCenterY = -navigationHeight * 0.5f
        // Fit osgslug-text's complete source-space layout below navigation.
        // The 800x600 camera remains inside that layout, while the source's
        // ten-pixel inset, sixteen-pixel font, and line advance scale together.
        World.doSlugText
            "Text"
            [Entity.Position .= v3 0.0f fittedContentCenterY 0.0f
             Entity.Size .= fittedTextPerimeterSize.V3
             Entity.Elevation .= 20.0f
             Entity.SlugFont .= SlugDemo.font
             Entity.Text .= canonicalText
             Entity.FontSizing .= Some (sourceFontSize * fitScale)
             Entity.TextColor .= Color.White
             Entity.TextDirection .= TextDirectionLeftToRight
             Entity.LanguageOpt .= None
             Entity.Justification .= Justified (JustifyLeft, JustifyTop)]
            world
