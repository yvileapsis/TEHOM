namespace Sudoku
open System
open System.Collections.Generic
open System.IO
open System.Threading
open Prime
open Nu

[<RequireQualifiedAccess>]
module PuzzleCorpus =

    type ImportProgress =
        { Total : int
          Processed : int
          Solved : int
          Failed : int
          CurrentSignatureKey : string
          LastErrorOpt : string option }

    let SchemaVersion = 1
    let ChunkSize = 512
    let SourceName = "Royle17"

    let private writeValue (filePath : String) value =
        let directory = Path.GetDirectoryName filePath
        if not (String.IsNullOrWhiteSpace directory) then
            Directory.CreateDirectory directory |> ignore<DirectoryInfo>
        let filePathTmp = filePath + ".tmp"
        let symbol = valueToSymbol value
        File.WriteAllText (filePathTmp, PrettyPrinter.prettyPrintSymbol symbol PrettyPrinter.defaultPrinter)
        if File.Exists filePath then File.SetAttributes (filePath, FileAttributes.Normal)
        File.Move (filePathTmp, filePath, true)

    let private tryReadValue<'a> filePath =
        if File.Exists filePath then
            try
                File.ReadAllText filePath
                |> scvalue<'a>
                |> Some
            with exn ->
                Log.warn ("Failed to read Sudoku corpus file '" + filePath + "' due to: " + scstring exn)
                None
        else None

    let private normalizePath (path : string) =
        path.Replace (Path.DirectorySeparatorChar, '/')

    let private deterministicHash (text : string) =
        let mutable hash = 2166136261u
        for c in text do
            hash <- hash ^^^ uint32 c
            hash <- hash * 16777619u
        hash

    let private sanitizeSignatureKey (signatureKey : string) =
        let chars =
            signatureKey
            |> Seq.map (fun c ->
                if Char.IsLetterOrDigit c then c
                else '_')
            |> Seq.toArray
        let stemRaw = String chars
        let stem =
            let trimmed = stemRaw.Trim '_'
            if String.IsNullOrWhiteSpace trimmed then "None"
            elif trimmed.Length > 80 then trimmed.Substring (0, 80)
            else trimmed
        let hash = deterministicHash signatureKey
        stem + "_" + (sprintf "%08x" hash)

    let private chunkFilePath outputDirectoryPath signatureKey chunkIndex =
        Path.Combine (outputDirectoryPath, sanitizeSignatureKey signatureKey, "Chunk" + (sprintf "%04i" chunkIndex) + ".nucorpus")
        |> normalizePath

    let private countDifficulties (entries : GeneratedPuzzle list) =
        List.fold (fun counts entry ->
            let difficulty = PuzzleAnalysis.difficultyOfEntry entry
            let count = defaultArg (Map.tryFind difficulty counts) 0
            Map.add difficulty (count + 1) counts)
            Map.empty<Difficulty, int>
            entries

    let private splitIntoChunks (entries : GeneratedPuzzle list) =
        let rec step index (remaining : GeneratedPuzzle list) chunks =
            match remaining with
            | [] -> List.rev chunks
            | _ ->
                let chunk = remaining |> List.truncate ChunkSize
                let rest = remaining |> List.skip (List.length chunk)
                step (index + 1) rest ((index, chunk) :: chunks)
        step 1 entries []

    let private replaceDirectoryWithTemp outputDirectoryPath tempDirectoryPath =
        let backupDirectoryPath = outputDirectoryPath + ".bak"
        if Directory.Exists backupDirectoryPath then Directory.Delete (backupDirectoryPath, true)
        if Directory.Exists outputDirectoryPath then Directory.Move (outputDirectoryPath, backupDirectoryPath)
        Directory.Move (tempDirectoryPath, outputDirectoryPath)
        if Directory.Exists backupDirectoryPath then Directory.Delete (backupDirectoryPath, true)

    let private writeCorpus outputDirectoryPath sourceFilePath totalPuzzles (groups : Dictionary<string, ResizeArray<GeneratedPuzzle>>) (failures : ResizeArray<PuzzleCorpusFailure>) =
        let tempDirectoryPath = outputDirectoryPath + ".tmp"
        if Directory.Exists tempDirectoryPath then Directory.Delete (tempDirectoryPath, true)
        Directory.CreateDirectory tempDirectoryPath |> ignore<DirectoryInfo>
        let mutable chunkInfos = []
        let mutable allEntries : GeneratedPuzzle list = []
        for pair in groups |> Seq.sortBy (fun pair -> pair.Key) do
            let signatureKey = pair.Key
            let entries : GeneratedPuzzle list = pair.Value |> Seq.toList
            allEntries <- entries @ allEntries
            for chunkIndex, chunkEntries in splitIntoChunks entries do
                let finalChunkFilePath = chunkFilePath outputDirectoryPath signatureKey chunkIndex
                let tempChunkFilePath = chunkFilePath tempDirectoryPath signatureKey chunkIndex
                let chunk =
                    { SchemaVersion = SchemaVersion
                      SignatureKey = signatureKey
                      ChunkIndex = chunkIndex
                      Puzzles = chunkEntries }
                writeValue tempChunkFilePath chunk
                chunkInfos <-
                    { SignatureKey = signatureKey
                      ChunkIndex = chunkIndex
                      FilePath = finalChunkFilePath
                      PuzzleCount = List.length chunkEntries
                      DifficultyCounts = countDifficulties chunkEntries } :: chunkInfos
        let allEntries = List.rev allEntries
        let solvedCount = List.length allEntries
        let manifest =
            { SchemaVersion = SchemaVersion
              SourceName = SourceName
              SourceFilePath = sourceFilePath
              OutputDirectoryPath = outputDirectoryPath
              TotalPuzzles = totalPuzzles
              SolvedCount = solvedCount
              FailedCount = failures.Count
              GeneratedAt = DateTimeOffset.UtcNow.ToString "O"
              Chunks = List.rev chunkInfos
              DifficultyCounts = countDifficulties allEntries }
        let failureReport =
            { SchemaVersion = SchemaVersion
              SourceName = SourceName
              SourceFilePath = sourceFilePath
              Failures = failures |> Seq.toList }
        writeValue (Path.Combine (tempDirectoryPath, "Manifest.nucorpus") |> normalizePath) manifest
        writeValue (Path.Combine (tempDirectoryPath, "Unsolved.nucorpus") |> normalizePath) failureReport
        replaceDirectoryWithTemp outputDirectoryPath tempDirectoryPath
        manifest

    let importRoyle sourceFilePath outputDirectoryPath (cancellationToken : CancellationToken) progress =
        if not (File.Exists sourceFilePath) then
            failwith ("Royle17 source file was not found at '" + sourceFilePath + "'.")
        let lines = File.ReadAllLines sourceFilePath
        let groups = Dictionary<string, ResizeArray<GeneratedPuzzle>> ()
        let failures = ResizeArray<PuzzleCorpusFailure> ()
        let mutable solved = 0
        let mutable failed = 0
        let report processed currentSignatureKey lastErrorOpt =
            progress
                { Total = lines.Length
                  Processed = processed
                  Solved = solved
                  Failed = failed
                  CurrentSignatureKey = currentSignatureKey
                  LastErrorOpt = lastErrorOpt }
        report 0 "" None
        lines
        |> Array.iteri (fun index puzzle ->
            cancellationToken.ThrowIfCancellationRequested ()
            let lineNumber = index + 1
            let fail reason =
                failures.Add
                    { LineNumber = lineNumber
                      Puzzle = puzzle
                      Reason = reason }
                failed <- failed + 1
                report lineNumber "" (Some reason)
            match PuzzleAnalysis.solveExact puzzle with
            | PuzzleAnalysis.ExactFailed reason -> fail reason
            | PuzzleAnalysis.ExactSolved solution ->
                match PuzzleAnalysis.solveSimpleFirstFromStrings true solution puzzle with
                | Some profile ->
                    let entry =
                        PuzzleAnalysis.profileToPuzzle
                            lineNumber
                            puzzle
                            solution
                            (PuzzleAnalysis.removedCellCount puzzle)
                            profile.InterestScore
                            None
                            profile
                    let signatureKey = PuzzleAnalysis.techniqueSignatureKey entry.TechniqueCounts
                    let entries =
                        match groups.TryGetValue signatureKey with
                        | true, entries -> entries
                        | false, _ ->
                            let entries = ResizeArray<GeneratedPuzzle> ()
                            groups.Add (signatureKey, entries)
                            entries
                    entries.Add entry
                    solved <- solved + 1
                    if lineNumber % 10 = 0 then report lineNumber signatureKey None
                | None ->
                    fail "Current logical solver did not solve this puzzle.")
        report lines.Length "" None
        writeCorpus outputDirectoryPath sourceFilePath lines.Length groups failures

    let tryReadManifestAt filePath =
        match tryReadValue<PuzzleCorpusManifest> filePath with
        | Some manifest when manifest.SchemaVersion = SchemaVersion -> Some manifest
        | Some _ -> None
        | None -> None

    let tryReadManifest () =
        tryReadManifestAt Assets.Gameplay.Royle17ManifestFilePath

    let readEntries () : GeneratedPuzzle list =
        match tryReadManifest () with
        | Some manifest ->
            manifest.Chunks
            |> List.collect (fun chunkInfo ->
                match tryReadValue<PuzzleCorpusChunk> chunkInfo.FilePath with
                | Some chunk when chunk.SchemaVersion = SchemaVersion -> chunk.Puzzles
                | Some _ -> []
                | None -> [])
        | None -> []

    let entriesByDifficulty difficulty =
        readEntries ()
        |> List.filter (fun entry -> PuzzleAnalysis.difficultyOfEntry entry = difficulty)

    let countsByDifficulty () =
        match tryReadManifest () with
        | Some manifest -> manifest.DifficultyCounts
        | None -> Map.empty<Difficulty, int>
