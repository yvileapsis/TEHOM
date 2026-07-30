// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Configuration
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.Json
open HarfBuzzSharp
open ImageMagick
open ImageMagick.Formats
open BCnEncoder.Shared
open BCnEncoder.Encoder
open Prime
open Nu.Vulkan

/// A refinement that can be applied to an asset during the build process.
type Refinement =
    | PsdToPng
    | BlockCompress
    | MtsdfAtlas
    | Slug

    /// Convert a string to a refinement value.
    static member ofString str =
        match str with
        | nameof PsdToPng -> PsdToPng
        | nameof BlockCompress -> BlockCompress
        | nameof MtsdfAtlas -> MtsdfAtlas
        | nameof Slug -> Slug
        | _ -> failwith ("Invalid refinement '" + str + "'.")

/// Describes a game asset, such as an image, sound, or model in detail.
///
/// All assets must belong to an asset Package, which is a unit of asset loading.
///
/// In order for the renderer to render a single texture, that texture, along with all the other assets in the
/// corresponding package, must be loaded. Also, the only way to unload any of those assets is to send a package unload
/// message to the relevent subsystem.
type Asset =
    abstract AssetTag : AssetTag
    abstract FilePath : string
    abstract Refinements : Refinement list
    abstract Associations : string Set

/// Describes a strongly-typed game asset, such as an image, sound, or model, in detail.
///
/// All assets must belong to an asset Package, which is a unit of asset loading.
///
/// In order for the renderer to render a single texture, that texture, along with all the other assets in the
/// corresponding package, must be loaded. Also, the only way to unload any of those assets is to send a package unload
/// message to the relevent subsystem.
type [<ReferenceEquality>] 'a Asset =
    { AssetTag : 'a AssetTag
      FilePath : string
      Refinements : Refinement list
      Associations : string Set }
    interface Asset with
        member this.AssetTag = this.AssetTag
        member this.FilePath = this.FilePath
        member this.Refinements = this.Refinements
        member this.Associations = this.Associations

/// Asset functions.
[<RequireQualifiedAccess>]
module Asset =

    /// Make an asset value.
    let make<'a> assetTag filePath refinements associations : 'a Asset =
        { AssetTag = assetTag
          FilePath = filePath
          Refinements = refinements
          Associations = associations }

/// Tracks assets as well as their originating file paths.
type [<ReferenceEquality>] Package<'a, 's> =
    { Assets : Dictionary<string, DateTimeOffset * Asset * 'a>
      PackageState : 's }

/// A dictionary of asset packages.
type Packages<'a, 's> = Dictionary<string, Package<'a, 's>>

/// Describes assets and how to process and use them.
type AssetDescriptor =
    | Asset of AssetName : string * FilePath : string * Refinements : Refinement list * Associations : string Set
    | Assets of Directory : string * Extensions : string Set * Refinements : Refinement list * Associations : string Set

/// Describes asset packages.
type PackageDescriptor = AssetDescriptor list

[<RequireQualifiedAccess>]
module AssetGraph =

    let [<Literal>] private MtsdfAtlasMaxDimension = 4096
    let [<Literal>] private MtsdfAtlasGlyphsPerShard = 4096

    /// A graph of all the assets used in a game.
    type AssetGraph =
        private
            { FilePathOpt_ : string option
              PackageDescriptors_ : Map<string, PackageDescriptor> }

        member this.PackageDescriptors =
            this.PackageDescriptors_

    let private AssetGraphStr = """
[[Default
 [[Assets Assets/Default [bmp png psd ttf skel json] [PsdToPng] [Render2d]]
  [Asset FontMtsdf "Assets/Default/Font.ttf" [MtsdfAtlas] [Render2d]]
  [Asset FontSlug "Assets/Default/Font.ttf" [Slug] [Render2d]]
  [Assets Assets/Default [jpg jpeg tga tif tiff dds ktx] [BlockCompress] [Render3d]]
  [Assets Assets/Default [cbm fbx gltf glb dae obj mtl raw] [] [Render3d]]
  [Assets Assets/Default [wav ogg mp3] [] [Audio]]
  [Assets Assets/Default [cur] [] [Cursor]]
  [Assets Assets/Default [nueffect nuscript csv] [] [Symbol]]
  [Assets Assets/Default [nuentity nugroup tsx tmx atlas nav nbrd frag vert bin] [] []]]]]"""

    let private getAssetExtension2 blockCompression rawAssetExtension refinement =
        match refinement with
        | PsdToPng -> if rawAssetExtension = ".psd" then ".png" else rawAssetExtension
        | BlockCompress ->
            match blockCompression with
            | BcCompression -> ".dds"
            | AstcCompression -> ".ktx"
        | MtsdfAtlas -> if rawAssetExtension = ".ttf" || rawAssetExtension = ".otf" then ".mtsdffont" else rawAssetExtension
        | Slug -> if rawAssetExtension = ".ttf" || rawAssetExtension = ".otf" then ".slugfont" else rawAssetExtension

    let private getAssetExtension usingRawAssets blockCompression rawAssetExtension refinements =
        if usingRawAssets
        then List.fold (getAssetExtension2 blockCompression) rawAssetExtension refinements
        else rawAssetExtension

    let private makeMtsdfSidecarSubpath intermediateFileSubpath suffix extension =
        let directory = PathF.GetDirectoryName intermediateFileSubpath
        let fileName = PathF.GetFileNameWithoutExtension intermediateFileSubpath + suffix + extension
        if String.IsNullOrEmpty directory then fileName
        else directory + "/" + fileName

    let private tryGetJsonProperty (propertyName : string) (element : JsonElement) =
        let mutable property = Unchecked.defaultof<JsonElement>
        if element.ValueKind = JsonValueKind.Object && element.TryGetProperty (propertyName, &property)
        then Some property
        else None

    let private tryGetJsonInt propertyName element =
        match tryGetJsonProperty propertyName element with
        | Some property when property.ValueKind = JsonValueKind.Number -> Some (property.GetInt32 ())
        | _ -> None

    let private getMtsdfAtlasSidecarPaths directory intermediateFileSubpath =
        let directorySubpath = PathF.GetDirectoryName intermediateFileSubpath
        let sidecarDirectory =
            if String.IsNullOrEmpty directorySubpath then directory
            else directory + "/" + directorySubpath
        let fileName = PathF.GetFileNameWithoutExtension intermediateFileSubpath
        let exactAtlasFilePath = directory + "/" + makeMtsdfSidecarSubpath intermediateFileSubpath ".mtsdfAtlas" ".png"
        let atlasFilePaths = List<string> ()
        if File.Exists exactAtlasFilePath then atlasFilePaths.Add exactAtlasFilePath
        if Directory.Exists sidecarDirectory then
            for filePath in Directory.GetFiles (sidecarDirectory, fileName + ".mtsdfAtlas.*.png") do
                if not (atlasFilePaths.Contains filePath) then atlasFilePaths.Add filePath
        atlasFilePaths.ToArray ()

    let private tryGetMtsdfAtlasGenPath () =
        let environmentPath = Environment.GetEnvironmentVariable "NU_MSDF_ATLAS_GEN"
        if not (String.IsNullOrWhiteSpace environmentPath) then Some environmentPath
        else
            let configuredPath = ConfigurationManager.AppSettings.["MsdfAtlasGenPath"]
            if not (String.IsNullOrWhiteSpace configuredPath) then Some configuredPath
            else
                let localPath =
                    let homePath = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
                    if String.IsNullOrWhiteSpace homePath then ""
                    else homePath + "/.local/bin/msdf-atlas-gen"
                if File.Exists localPath then Some localPath
                else Some "msdf-atlas-gen"

    let private copyFileReplacing inputFilePath outputFilePath =
        Directory.CreateDirectory (PathF.GetDirectoryName outputFilePath) |> ignore
        try if File.Exists outputFilePath then File.SetAttributes (outputFilePath, FileAttributes.None)
            File.Copy (inputFilePath, outputFilePath, true)
            File.SetAttributes (outputFilePath, FileAttributes.ReadOnly) // prevents errors when accidentally altering output compressed image files and the like
        with _ -> Log.info ("Resource lock on '" + outputFilePath + "' has prevented build.")

    let private copyMtsdfAtlasSidecarFiles intermediateFileSubpath intermediateDirectory outputDirectory =
        let directorySubpath = PathF.GetDirectoryName intermediateFileSubpath
        let atlasFilePaths = getMtsdfAtlasSidecarPaths intermediateDirectory intermediateFileSubpath
        if Array.isEmpty atlasFilePaths then
            Log.info ("Could not copy MTSDF atlas sidecar because no atlas files exist for '" + intermediateFileSubpath + "'.")
        for atlasFilePath in atlasFilePaths do
            let atlasFileSubpath =
                if String.IsNullOrEmpty directorySubpath then PathF.GetFileName atlasFilePath
                else directorySubpath + "/" + PathF.GetFileName atlasFilePath
            copyFileReplacing atlasFilePath (outputDirectory + "/" + atlasFileSubpath)

    let private copyMtsdfAtlasSidecars inputFileExtension intermediateFileSubpath intermediateDirectory outputDirectory =
        copyMtsdfAtlasSidecarFiles intermediateFileSubpath intermediateDirectory outputDirectory
        let fontFileSubpath = makeMtsdfSidecarSubpath intermediateFileSubpath ".mtsdfSource" inputFileExtension
        let fontFilePath = intermediateDirectory + "/" + fontFileSubpath
        let fontOutputFilePath = outputDirectory + "/" + fontFileSubpath
        if File.Exists fontFilePath then copyFileReplacing fontFilePath fontOutputFilePath
        else Log.info ("Could not copy MTSDF font sidecar because '" + fontFilePath + "' does not exist.")

    let private makeMtsdfAtlasGenOutput stdout stderr =
        if String.IsNullOrWhiteSpace stderr then
            if String.IsNullOrWhiteSpace stdout then "" else "\n" + stdout
        else "\n" + stderr

    let private runMtsdfAtlasGenProcess atlasGenPath glyphSetFilePathOpt intermediateFilePath refinementFilePath imageFilePath imageFormat =
        let startInfo = ProcessStartInfo ()
        startInfo.FileName <- atlasGenPath
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true
        startInfo.ArgumentList.Add "-font"
        startInfo.ArgumentList.Add intermediateFilePath
        match glyphSetFilePathOpt with
        | Some glyphSetFilePath ->
            startInfo.ArgumentList.Add "-glyphset"
            startInfo.ArgumentList.Add glyphSetFilePath
        | None ->
            startInfo.ArgumentList.Add "-allglyphs"
        startInfo.ArgumentList.Add "-type"
        startInfo.ArgumentList.Add "mtsdf"
        startInfo.ArgumentList.Add "-format"
        startInfo.ArgumentList.Add imageFormat
        startInfo.ArgumentList.Add "-size"
        startInfo.ArgumentList.Add "48"
        startInfo.ArgumentList.Add "-pxrange"
        startInfo.ArgumentList.Add "4"
        startInfo.ArgumentList.Add "-yorigin"
        startInfo.ArgumentList.Add "top"
        startInfo.ArgumentList.Add "-scanline"
        startInfo.ArgumentList.Add "-threads"
        startInfo.ArgumentList.Add "0"
        startInfo.ArgumentList.Add "-imageout"
        startInfo.ArgumentList.Add imageFilePath
        startInfo.ArgumentList.Add "-json"
        startInfo.ArgumentList.Add refinementFilePath
        use proc = new Process ()
        proc.StartInfo <- startInfo
        proc.Start () |> ignore<bool>
        let stdoutTask = proc.StandardOutput.ReadToEndAsync ()
        let stderrTask = proc.StandardError.ReadToEndAsync ()
        proc.WaitForExit ()
        let stdout = stdoutTask.Result
        let stderr = stderrTask.Result
        if proc.ExitCode = 0 && File.Exists refinementFilePath && File.Exists imageFilePath then Right ()
        elif proc.ExitCode = 0 then
            Left
                ("MTSDF atlas generator completed with " + imageFormat + " output, but the expected metadata or atlas file was not produced." +
                 makeMtsdfAtlasGenOutput stdout stderr)
        else Left (makeMtsdfAtlasGenOutput stdout stderr)

    let private convertMtsdfAtlasBmpToPng (bmpFilePath : string) (pngFilePath : string) =
        use image = new MagickImage (bmpFilePath)
        image.Format <- MagickFormat.Png32
        image.Write (pngFilePath)
        try File.Delete bmpFilePath
        with _ -> ()

    let private runMtsdfAtlasGen glyphSetFilePathOpt intermediateFilePath refinementFilePath atlasFilePath =
        match tryGetMtsdfAtlasGenPath () with
        | Some atlasGenPath ->
            try
                match runMtsdfAtlasGenProcess atlasGenPath glyphSetFilePathOpt intermediateFilePath refinementFilePath atlasFilePath "png" with
                | Right () -> ()
                | Left pngOutput ->
                    let bmpAtlasFilePath = PathF.ChangeExtension (atlasFilePath, ".bmp")
                    match runMtsdfAtlasGenProcess atlasGenPath glyphSetFilePathOpt intermediateFilePath refinementFilePath bmpAtlasFilePath "bmp" with
                    | Right () ->
                        convertMtsdfAtlasBmpToPng bmpAtlasFilePath atlasFilePath
                        if not (File.Exists atlasFilePath) then
                            failwith ("Failed to convert fallback MTSDF BMP atlas '" + bmpAtlasFilePath + "' to PNG '" + atlasFilePath + "'.")
                    | Left bmpOutput ->
                        failwith
                            ("Failed to MtsdfAtlas refine asset '" + intermediateFilePath + "' with '" + atlasGenPath + "'." +
                             "\nPNG output failed:" + pngOutput +
                             "\nBMP fallback failed:" + bmpOutput)
            with exn ->
                if exn :? System.ComponentModel.Win32Exception then
                    failwith ("Failed to launch MTSDF atlas generator '" + atlasGenPath + "'. Set NU_MSDF_ATLAS_GEN to a valid msdf-atlas-gen executable for NuPipe builds, or app setting MsdfAtlasGenPath for runtime asset reloads. Error: " + scstring exn)
                else reraise ()
        | None ->
            failwith "MTSDF atlas generation requires NU_MSDF_ATLAS_GEN for NuPipe builds or app setting MsdfAtlasGenPath for runtime asset reloads."

    let private tryGetMtsdfGlyphCount fontFilePath =
        try
            use blob = Blob.FromFile fontFilePath
            use face = new Face (blob, 0u)
            Some (int face.GlyphCount)
        with exn ->
            Log.info ("Could not read glyph count from MTSDF font source '" + fontFilePath + "' due to: " + scstring exn)
            None

    let private validateMtsdfAtlasDimensions metadataFilePath =
        use document = JsonDocument.Parse (File.ReadAllText metadataFilePath)
        let root = document.RootElement
        let atlas = defaultArg (tryGetJsonProperty "atlas" root) root
        let width = defaultArg (tryGetJsonInt "width" atlas) 0
        let height = defaultArg (tryGetJsonInt "height" atlas) 0
        if width > MtsdfAtlasMaxDimension || height > MtsdfAtlasMaxDimension then
            failwith
                ("MTSDF atlas '" + metadataFilePath + "' exceeded the maximum size of " + string MtsdfAtlasMaxDimension + "x" + string MtsdfAtlasMaxDimension +
                 " with generated size " + string width + "x" + string height + ". Reduce the glyph shard size or source font coverage.")

    let private writeMtsdfGlyphSetFile glyphSetFilePath firstGlyphIndex lastGlyphIndex =
        Directory.CreateDirectory (PathF.GetDirectoryName glyphSetFilePath) |> ignore
        File.WriteAllText (glyphSetFilePath, "[" + string firstGlyphIndex + ", " + string lastGlyphIndex + "]")

    let private writeMtsdfSplitManifest (metadataFilePaths : string array) (atlasFilePaths : string array) manifestFilePath =
        Directory.CreateDirectory (PathF.GetDirectoryName manifestFilePath) |> ignore
        use stream = File.Create manifestFilePath
        let options = JsonWriterOptions ()
        use writer = new Utf8JsonWriter (stream, options)
        writer.WriteStartObject ()
        writer.WriteString ("format", "NuMtsdfSplitAtlas")
        writer.WriteNumber ("maxAtlasDimension", MtsdfAtlasMaxDimension)
        writer.WriteStartArray "atlases"
        for i in 0 .. dec metadataFilePaths.Length do
            use document = JsonDocument.Parse (File.ReadAllText metadataFilePaths[i])
            let root = document.RootElement
            let atlas = defaultArg (tryGetJsonProperty "atlas" root) root
            writer.WriteStartObject ()
            writer.WriteNumber ("index", i)
            writer.WriteString ("file", PathF.GetFileName atlasFilePaths[i])
            for property in atlas.EnumerateObject () do property.WriteTo writer
            writer.WriteEndObject ()
        writer.WriteEndArray ()
        if metadataFilePaths.Length > 0 then
            use document = JsonDocument.Parse (File.ReadAllText metadataFilePaths[0])
            let root = document.RootElement
            match tryGetJsonProperty "metrics" root with
            | Some metrics ->
                writer.WritePropertyName "metrics"
                metrics.WriteTo writer
            | None -> ()
        writer.WriteStartArray "glyphs"
        for i in 0 .. dec metadataFilePaths.Length do
            use document = JsonDocument.Parse (File.ReadAllText metadataFilePaths[i])
            let root = document.RootElement
            match tryGetJsonProperty "glyphs" root with
            | Some glyphs when glyphs.ValueKind = JsonValueKind.Array ->
                for glyph in glyphs.EnumerateArray () do
                    writer.WriteStartObject ()
                    writer.WriteNumber ("atlasIndex", i)
                    for property in glyph.EnumerateObject () do property.WriteTo writer
                    writer.WriteEndObject ()
            | _ -> ()
        writer.WriteEndArray ()
        writer.WriteStartArray "kerning"
        writer.WriteEndArray ()
        writer.WriteEndObject ()

    let private runMtsdfAtlasGenAll intermediateFilePath refinementFilePath atlasFilePath =
        runMtsdfAtlasGen None intermediateFilePath refinementFilePath atlasFilePath
        validateMtsdfAtlasDimensions refinementFilePath

    let private runMtsdfAtlasGenSplit intermediateFilePath refinementDirectory refinementFileSubpath refinementFilePath glyphCount =
        let shardCount = max 1 ((glyphCount + dec MtsdfAtlasGlyphsPerShard) / MtsdfAtlasGlyphsPerShard)
        let shardMetadataFilePaths = Array.zeroCreate shardCount
        let shardAtlasFilePaths = Array.zeroCreate shardCount
        for shardIndex in 0 .. dec shardCount do
            let firstGlyphIndex = shardIndex * MtsdfAtlasGlyphsPerShard
            let lastGlyphIndex = min (dec glyphCount) (firstGlyphIndex + dec MtsdfAtlasGlyphsPerShard)
            let glyphSetFileSubpath = makeMtsdfSidecarSubpath refinementFileSubpath (".mtsdfGlyphSet." + shardIndex.ToString "000") ".txt"
            let shardMetadataFileSubpath = makeMtsdfSidecarSubpath refinementFileSubpath (".mtsdfShard." + shardIndex.ToString "000") ".json"
            let shardAtlasFileSubpath = makeMtsdfSidecarSubpath refinementFileSubpath (".mtsdfAtlas." + shardIndex.ToString "000") ".png"
            let glyphSetFilePath = refinementDirectory + "/" + glyphSetFileSubpath
            let shardMetadataFilePath = refinementDirectory + "/" + shardMetadataFileSubpath
            let shardAtlasFilePath = refinementDirectory + "/" + shardAtlasFileSubpath
            writeMtsdfGlyphSetFile glyphSetFilePath firstGlyphIndex lastGlyphIndex
            runMtsdfAtlasGen (Some glyphSetFilePath) intermediateFilePath shardMetadataFilePath shardAtlasFilePath
            validateMtsdfAtlasDimensions shardMetadataFilePath
            shardMetadataFilePaths[shardIndex] <- shardMetadataFilePath
            shardAtlasFilePaths[shardIndex] <- shardAtlasFilePath
        writeMtsdfSplitManifest shardMetadataFilePaths shardAtlasFilePaths refinementFilePath

    /// Apply a single refinement to an asset.
    let private refineAssetOnce (intermediateFileSubpath : string) intermediateDirectory refinementDirectory blockCompression refinement =

        // build the intermediate file path
        let intermediateFileExtension = PathF.GetExtensionMixed intermediateFileSubpath
        let intermediateFilePath = intermediateDirectory + "/" + intermediateFileSubpath

        // build the refinement file path
        let refinementFileExtension = getAssetExtension2 blockCompression intermediateFileExtension refinement
        let refinementFileSubpath = PathF.ChangeExtension (intermediateFileSubpath, refinementFileExtension)
        let refinementFilePath = refinementDirectory + "/" + refinementFileSubpath

        // refine the asset
        Directory.CreateDirectory (PathF.GetDirectoryName refinementFilePath) |> ignore
        match refinement with
        | PsdToPng ->
            if intermediateFileExtension = ".psd" then
                use imageCollection = new MagickImageCollection (intermediateFilePath)
                use image0 = imageCollection[0] // NOTE: we clear out image0 to fix conversion to png somehow.
                image0.ColorFuzz <- Percentage 100.0
                image0.FloodFill (MagickColors.Transparent, 0, 0)
                use image = imageCollection.Flatten MagickColors.Transparent
                use stream = File.OpenWrite refinementFilePath
                image.Write (stream, MagickFormat.Png32)
            else
                if not (File.Exists refinementFilePath) then
                    File.Copy (intermediateFilePath, refinementFilePath)
                elif File.GetLastWriteTime intermediateFilePath > File.GetLastWriteTime refinementFilePath then
                    File.Copy (intermediateFilePath, refinementFilePath, true)

        | BlockCompress ->
            match Hl.inferTextureCompression refinementFilePath with
            | Uncompressed ->
                match blockCompression with
                | BcCompression ->
                    use image = new MagickImage (intermediateFilePath)
                    use stream = File.OpenWrite refinementFilePath
                    let defines = DdsWriteDefines ()
                    defines.FastMipmaps <- false
                    defines.Compression <- DdsCompression.None
                    image.Write (stream, defines)
                | AstcCompression ->
                    use image = new MagickImage (intermediateFilePath)
                    match Hl.tryGenerateUncompressedImage image with
                    | Some (resolution, mipmapHead) ->
                        match Hl.tryGenerateUncompressedMipmaps image with
                        | Some mipmapTail ->
                            let mipmapLevels = inc mipmapTail.Length
                            use stream = File.OpenWrite refinementFilePath
                            use writer = new BinaryWriter (stream)
                            Hl.writeKtxHeader resolution mipmapLevels false writer                      // ktx header
                            writer.Write (uint mipmapHead.Length)                                       // mip head size
                            writer.Write mipmapHead                                                     // mip head data
                            let padding = Array.zeroCreate<byte> ((4 - (mipmapHead.Length % 4)) % 4)    // mip head padding
                            writer.Write padding                                                        //
                            for (_, mipmap) in mipmapTail do                                            // mip tail
                                writer.Write (uint mipmap.Length)                                       // mip tail height
                                writer.Write mipmap                                                     // mip tail data
                                let padding = Array.zeroCreate<byte> ((4 - (mipmap.Length % 4)) % 4)    // mip tail padding
                                writer.Write padding                                                    //
                        | None -> Log.error ("Failed to " + scstring refinement + " refine asset '" + intermediateFilePath + "'.")
                    | None -> Log.error ("Failed to " + scstring refinement + " refine asset '" + intermediateFilePath + "'.")

            | ColorCompression ->
                match blockCompression with
                | BcCompression ->
                    use image = new MagickImage (intermediateFilePath)
                    use stream = File.OpenWrite refinementFilePath
                    let defines = DdsWriteDefines ()
                    defines.FastMipmaps <- false
                    image.Alpha AlphaOption.Set // implicitly directs use of dxt5 compression - https://github.com/ImageMagick/ImageMagick/pull/4914#issuecomment-1060654324
                    image.Write (stream, defines)
                | AstcCompression ->
                    use image = new MagickImage (intermediateFilePath)
                    match Hl.tryCompressImage image with
                    | Some (resolution, mipmapHead) ->
                        match Hl.tryCompressMipmaps image with
                        | Some mipmapTail ->
                            let mipmapLevels = inc mipmapTail.Length
                            use stream = File.OpenWrite refinementFilePath
                            use writer = new BinaryWriter (stream)
                            Hl.writeKtxHeader resolution mipmapLevels true writer                   // mip header
                            writer.Write (uint mipmapHead.Length)                                   // mip head size
                            writer.Write mipmapHead                                                 // mip head data
                            let padding = 3 - (mipmapHead.Length + 3) % 4 |> Array.zeroCreate<byte> // mip head padding
                            writer.Write padding                                                    //
                            for (_, mipmap) in mipmapTail do                                        // mip tail
                                writer.Write (uint mipmap.Length)                                   // mip tail N height
                                writer.Write mipmap                                                 // mip tail N data
                                let padding = 3 - (mipmap.Length + 3) % 4 |> Array.zeroCreate<byte> // mip tail N padding
                                writer.Write padding                                                //
                        | None -> Log.error ("Failed to " + scstring refinement + " refine asset '" + intermediateFilePath + "'.")
                    | None -> Log.error ("Failed to " + scstring refinement + " refine asset '" + intermediateFilePath + "'.")

            | NormalCompression ->
                match blockCompression with
                | BcCompression ->
                    use image = new MagickImage (intermediateFilePath)
                    image.ColorSpace <- ColorSpace.sRGB
                    image.Format <- MagickFormat.Rgba
                    use stream = File.OpenWrite refinementFilePath
                    let encoder = BcEncoder ()
                    encoder.OutputOptions.Quality <- CompressionQuality.BestQuality
                    encoder.OutputOptions.GenerateMipMaps <- true
                    encoder.OutputOptions.FileFormat <- OutputFileFormat.Dds
                    encoder.OutputOptions.Format <- CompressionFormat.Bc5
                    let bytes = image.GetPixels().ToByteArray PixelMapping.RGBA
                    encoder.EncodeToStream (bytes, int image.Width, int image.Height, PixelFormat.Rgba32, stream)
                | AstcCompression ->
                    use image = new MagickImage (intermediateFilePath)
                    match Hl.tryCompressImage image with
                    | Some (resolution, mipmapHead) ->
                        match Hl.tryCompressMipmaps image with
                        | Some mipmapTail ->
                            let mipmapLevels = inc mipmapTail.Length
                            use stream = File.OpenWrite refinementFilePath
                            use writer = new BinaryWriter (stream)
                            Hl.writeKtxHeader resolution mipmapLevels true writer                   // mip header
                            writer.Write (uint mipmapHead.Length)                                   // mip head size
                            writer.Write mipmapHead                                                 // mip head data
                            let padding = 3 - (mipmapHead.Length + 3) % 4 |> Array.zeroCreate<byte> // mip head padding
                            writer.Write padding                                                    //
                            for (_, mipmap) in mipmapTail do                                        // mip tail
                                writer.Write (uint mipmap.Length)                                   // mip tail N height
                                writer.Write mipmap                                                 // mip tail N data
                                let padding = 3 - (mipmap.Length + 3) % 4 |> Array.zeroCreate<byte> // mip tail N padding
                                writer.Write padding                                                //
                        | None -> Log.error ("Failed to " + scstring refinement + " refine asset '" + intermediateFilePath + "'.")
                    | None -> Log.error ("Failed to " + scstring refinement + " refine asset '" + intermediateFilePath + "'.")

        | MtsdfAtlas ->
            if intermediateFileExtension = ".ttf" || intermediateFileExtension = ".otf" then
                let atlasFileSubpath = makeMtsdfSidecarSubpath refinementFileSubpath ".mtsdfAtlas" ".png"
                let atlasFilePath = refinementDirectory + "/" + atlasFileSubpath
                let fontFileSubpath = makeMtsdfSidecarSubpath refinementFileSubpath ".mtsdfSource" intermediateFileExtension
                let fontFilePath = refinementDirectory + "/" + fontFileSubpath
                Directory.CreateDirectory (PathF.GetDirectoryName atlasFilePath) |> ignore
                Directory.CreateDirectory (PathF.GetDirectoryName fontFilePath) |> ignore
                match tryGetMtsdfGlyphCount intermediateFilePath with
                | Some glyphCount when glyphCount > MtsdfAtlasGlyphsPerShard ->
                    runMtsdfAtlasGenSplit intermediateFilePath refinementDirectory refinementFileSubpath refinementFilePath glyphCount
                | Some _ | None ->
                    runMtsdfAtlasGenAll intermediateFilePath refinementFilePath atlasFilePath
                if File.Exists refinementFilePath then copyFileReplacing intermediateFilePath fontFilePath
            else
                Log.error ("MtsdfAtlas refinement requires a .ttf or .otf asset, not '" + intermediateFilePath + "'.")

        | Slug ->
            if intermediateFileExtension = ".ttf" || intermediateFileExtension = ".otf" then
                copyFileReplacing intermediateFilePath refinementFilePath
            else
                Log.error ("Slug refinement requires a .ttf or .otf asset, not '" + intermediateFilePath + "'.")

        // return the latest refinement localities
        (refinementFileSubpath, refinementDirectory)

    /// Apply all refinements to an asset.
    let private refineAsset inputFileSubpath inputDirectory refinementDirectory blockCompression refinements =
        List.fold (fun (intermediateFileSubpath, intermediateDirectory) refinement ->
            refineAssetOnce intermediateFileSubpath intermediateDirectory refinementDirectory blockCompression refinement)
            (inputFileSubpath, inputDirectory)
            refinements

    /// Build all the assets.
    let private buildAssets5 inputDirectory outputDirectory refinementDirectory blockCompression fullBuild (assets : Asset list) =

        // build assets
        for asset in assets do

            // build input file path
            let inputFileSubpath = asset.FilePath
            let inputFileExtension = PathF.GetExtensionMixed inputFileSubpath
            let inputFilePath = inputDirectory + "/" + inputFileSubpath

            // build the output file path
            let outputFileExtension = getAssetExtension true blockCompression inputFileExtension asset.Refinements
            let outputFileSubpath = PathF.ChangeExtension (asset.FilePath, outputFileExtension)
            let outputFilePath = outputDirectory + "/" + outputFileSubpath
            let outputMtsdfAtlasPaths = getMtsdfAtlasSidecarPaths outputDirectory outputFileSubpath
            let outputMtsdfFontPath = outputDirectory + "/" + makeMtsdfSidecarSubpath outputFileSubpath ".mtsdfSource" inputFileExtension
            let hasMtsdfAtlas = List.contains MtsdfAtlas asset.Refinements

            // build the asset if fully building or if it's out of date
            if  fullBuild ||
                not (File.Exists outputFilePath) ||
                File.GetLastWriteTime inputFilePath > File.GetLastWriteTime outputFilePath ||
                hasMtsdfAtlas &&
                (Array.isEmpty outputMtsdfAtlasPaths ||
                 not (File.Exists outputMtsdfFontPath) ||
                 Array.exists (fun (outputMtsdfAtlasPath : string) -> File.GetLastWriteTime inputFilePath > File.GetLastWriteTime outputMtsdfAtlasPath) outputMtsdfAtlasPaths ||
                 File.GetLastWriteTime inputFilePath > File.GetLastWriteTime outputMtsdfFontPath) then

                // refine the asset
                let (intermediateFileSubpath, intermediateDirectory) =
                    if List.isEmpty asset.Refinements then (inputFileSubpath, inputDirectory)
                    else refineAsset inputFileSubpath inputDirectory refinementDirectory blockCompression asset.Refinements

                // attempt to copy the intermediate asset if output file is out of date
                let intermediateFilePath = intermediateDirectory + "/" + intermediateFileSubpath
                let outputFilePath = outputDirectory + "/" + intermediateFileSubpath
                if File.Exists intermediateFilePath then
                    copyFileReplacing intermediateFilePath outputFilePath
                    if hasMtsdfAtlas then copyMtsdfAtlasSidecars inputFileExtension intermediateFileSubpath intermediateDirectory outputDirectory
                else Log.info ("Refined asset '" + intermediateFilePath + "' does not exist for asset '" + scstring asset.AssetTag + "'.")

    /// Collect the associated assets from package descriptor assets value.
    let private collectAssetsFromPackageDescriptorAssets packageName directory extensions associations refinements : Asset list =
        [if Directory.Exists directory then
            let filePaths =
                [for extension in extensions do
                    for filePath in Directory.GetFiles (directory, "*." + extension, SearchOption.AllDirectories) do
                        PathF.Normalize filePath]
            for filePath in filePaths do
                let assetName = PathF.GetFileNameWithoutExtension filePath
                let tag = AssetTag.make<obj> packageName assetName
                yield Asset.make tag filePath refinements associations
         else Log.info ("Invalid directory '" + directory + "'. when looking for assets.")]

    /// Collect the associated assets from a package descriptor.
    let private collectAssetsFromPackageDescriptor packageName packageDescriptor : Asset list =
        [for assetDescriptor in packageDescriptor do
            match assetDescriptor with
            | Asset (assetName, filePath, refinements, associations) ->
                let tag = AssetTag.make<obj> packageName assetName
                yield Asset.make tag filePath refinements associations
            | Assets (directory, extensions, associations, refinements) ->
                yield! collectAssetsFromPackageDescriptorAssets packageName directory extensions refinements associations]

    let private assetCollectionKey (asset : Asset) =
        struct (asset.AssetTag.PackageName, asset.AssetTag.AssetName, asset.FilePath)

    let private mergeCollectedAssets assets =
        assets
        |> List.groupBy assetCollectionKey
        |> List.map snd
        |> List.map (List.reduce (fun asset asset2 ->
            { AssetTag = AssetTag.make asset.AssetTag.PackageName asset.AssetTag.AssetName
              FilePath = asset.FilePath
              Refinements = List.append asset.Refinements asset2.Refinements
              Associations = Set.union asset.Associations asset2.Associations } :> Asset))

    let private refineAssetForUse (asset : Asset) =
        let assetExtension = PathF.GetExtensionMixed asset.FilePath
        let assetExtensionRefined = getAssetExtension true Constants.Render.TextureBlockCompression assetExtension asset.Refinements
        let assetFilePathRefined = PathF.ChangeExtension (asset.FilePath, assetExtensionRefined)
        { AssetTag = AssetTag.make asset.AssetTag.PackageName asset.AssetTag.AssetName
          FilePath = assetFilePathRefined
          Refinements = asset.Refinements
          Associations = asset.Associations } :> Asset

    /// Attempt to collect all the available assets from a package.
    let tryCollectAssetsFromPackage associationOpt packageName assetGraph =
        let mutable packageDescriptor = Unchecked.defaultof<PackageDescriptor>
        match Map.tryGetValue (packageName, assetGraph.PackageDescriptors_, &packageDescriptor) with
        | true ->
            collectAssetsFromPackageDescriptor packageName packageDescriptor
            |> mergeCollectedAssets
            |> List.map refineAssetForUse
            |> List.filter (fun asset -> match associationOpt with Some association -> asset.Associations.Contains association | _ -> true)
            |> Right
        | false -> Left ("Could not find package '" + packageName + "' in asset graph.")

    /// Collect all the available assets from an asset graph document.
    let collectAssets associationOpt assetGraph =
        [for entry in assetGraph.PackageDescriptors_ do
            let packageName = entry.Key
            let packageDescriptor = entry.Value
            yield! collectAssetsFromPackageDescriptor packageName packageDescriptor]
        |> mergeCollectedAssets
        |> List.filter (fun asset -> match associationOpt with Some association -> asset.Associations.Contains association | _ -> true)

    /// Build all the available assets described by an asset graph.
    let buildAssets inputDirectory outputDirectory refinementDirectory blockCompression fullBuild assetGraph =

        // compute the asset graph's tracker file path
        let outputFilePathOpt =
            Option.map (fun (filePath : string) ->
                outputDirectory + "/" + PathF.ChangeExtension (PathF.GetFileName filePath, ".tracker"))
                assetGraph.FilePathOpt_

        // check if the output assetGraph file is newer than the current
        let fullBuild =
            fullBuild ||
            match (assetGraph.FilePathOpt_, outputFilePathOpt) with
            | (Some filePath, Some outputFilePath) -> File.GetLastWriteTime filePath > File.GetLastWriteTime outputFilePath
            | (None, None) -> false
            | (_, _) -> failwithumf ()

        // collect assets
        let currentDirectory = Directory.GetCurrentDirectory ()
        let assets =
            try Directory.SetCurrentDirectory inputDirectory
                collectAssets None assetGraph
            finally
                Directory.SetCurrentDirectory currentDirectory

        // log when image assets are being shared between 2d and 3d renders
        if List.exists (fun (asset : Asset) ->
            let extension = PathF.GetExtensionLower asset.FilePath
            match extension with
            | ImageExtension _ ->
                asset.Associations.Contains Constants.Associations.Render2d &&
                asset.Associations.Contains Constants.Associations.Render3d
            | _ -> false)
            assets then
            Console.WriteLine "Warning: Due to asset graph limitations, associating image assets with both Render2d and Render3d is not fully supported."

        // build assets
        buildAssets5 inputDirectory outputDirectory refinementDirectory blockCompression fullBuild assets

        // output the asset graph tracker file
        match outputFilePathOpt with
        | Some outputFilePath -> File.WriteAllText (outputFilePath, "")
        | None -> ()

    /// The empty asset graph.
    let empty =
        { FilePathOpt_ = None
          PackageDescriptors_ = Map.empty }

    /// Make an asset graph.
    let make filePathOpt packageDescriptors =
        { FilePathOpt_ = filePathOpt
          PackageDescriptors_ = packageDescriptors }

    /// Make an asset graph, attempting to use the file at the given file path.
    let makeFromFileOpt filePath =
        let (filePathOpt, packageDescriptors) =
            if File.Exists filePath then
                try File.ReadAllText filePath
                    |> String.unescape
                    |> scvalue<Map<string, PackageDescriptor>>
                    |> fun packageDescriptors -> (Some filePath, packageDescriptors)
                with exn ->
                    Log.warn ("Could not make asset graph from file '" + filePath + "' due to: " + scstring exn)
                    (None, scvalue AssetGraphStr)
            else (None, scvalue AssetGraphStr)
        make filePathOpt packageDescriptors

/// A graph of all the assets used in a game.
type AssetGraph = AssetGraph.AssetGraph
