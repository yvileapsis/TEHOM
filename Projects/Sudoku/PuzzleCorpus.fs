namespace Sudoku
open System
open System.Collections.Generic
open System.IO
open System.Threading
open Prime
open Nu

[<RequireQualifiedAccess>]
module PuzzleCorpus =

    // this describes a chunk of imported corpus puzzles.
    type PuzzleCorpusChunk =
        { SignatureKey : string
          ChunkIndex : int
          Puzzles : SudokuPuzzle list }

    // this describes a chunk file in an imported corpus manifest.
    type PuzzleCorpusChunkInfo =
        { SignatureKey : string
          ChunkIndex : int
          FilePath : string
          PuzzleCount : int
          DifficultyCounts : Map<Difficulty, int> }

    // this describes an imported puzzle corpus.
    type PuzzleCorpusManifest =
        { SourceName : string
          SourceFilePath : string
          OutputDirectoryPath : string
          TotalPuzzles : int
          SolvedCount : int
          FailedCount : int
          GeneratedAt : string
          Chunks : PuzzleCorpusChunkInfo list
          DifficultyCounts : Map<Difficulty, int> }

    // this describes a puzzle that could not be imported into the corpus.
    type PuzzleCorpusFailure =
        { LineNumber : int
          Puzzle : string
          Reason : string }

    // this describes the import failures persisted alongside a corpus manifest.
    type PuzzleCorpusFailureReport =
        { SourceName : string
          SourceFilePath : string
          Failures : PuzzleCorpusFailure list }


    type ImportProgress =
        { Total : int
          Processed : int
          Solved : int
          Failed : int
          CurrentSignatureKey : string
          LastErrorOpt : string option }

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

    let private manifestFilePath outputDirectoryPath =
        Path.Combine (outputDirectoryPath, "Manifest.nucorpus") |> normalizePath

    let private unsolvedFilePath outputDirectoryPath =
        Path.Combine (outputDirectoryPath, "Unsolved.nucorpus") |> normalizePath

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

    let private countDifficulties (entries : SudokuPuzzle list) =
        List.fold (fun counts entry ->
            let difficulty = PuzzleAnalysis.difficultyOfEntry entry
            let count = defaultArg (Map.tryFind difficulty counts) 0
            Map.add difficulty (count + 1) counts)
            Map.empty<Difficulty, int>
            entries

    let private splitIntoChunks (entries : SudokuPuzzle list) =
        let rec step index (remaining : SudokuPuzzle list) chunks =
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

    let private writeCorpus outputDirectoryPath sourceFilePath totalPuzzles (groups : Dictionary<string, ResizeArray<SudokuPuzzle>>) (failures : ResizeArray<PuzzleCorpusFailure>) =
        let tempDirectoryPath = outputDirectoryPath + ".tmp"
        if Directory.Exists tempDirectoryPath then Directory.Delete (tempDirectoryPath, true)
        Directory.CreateDirectory tempDirectoryPath |> ignore<DirectoryInfo>
        let mutable chunkInfos = []
        let mutable allEntries : SudokuPuzzle list = []
        for pair in groups |> Seq.sortBy (fun pair -> pair.Key) do
            let signatureKey = pair.Key
            let entries : SudokuPuzzle list =
                pair.Value
                |> Seq.toList
                |> List.sortBy (fun puzzle -> puzzle.Number)
                |> List.map (fun puzzle -> puzzle.Dehydrate ())
            allEntries <- entries @ allEntries
            for chunkIndex, chunkEntries in splitIntoChunks entries do
                let finalChunkFilePath = chunkFilePath outputDirectoryPath signatureKey chunkIndex
                let tempChunkFilePath = chunkFilePath tempDirectoryPath signatureKey chunkIndex
                let chunk =
                    { SignatureKey = signatureKey
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
            { SourceName = SourceName
              SourceFilePath = sourceFilePath
              OutputDirectoryPath = outputDirectoryPath
              TotalPuzzles = totalPuzzles
              SolvedCount = solvedCount
              FailedCount = failures.Count
              GeneratedAt = DateTimeOffset.UtcNow.ToString "O"
              Chunks = List.rev chunkInfos
              DifficultyCounts = countDifficulties allEntries }
        let failureReport =
            { SourceName = SourceName
              SourceFilePath = sourceFilePath
              Failures = failures |> Seq.toList |> List.sortBy (fun failure -> failure.LineNumber) }
        writeValue (manifestFilePath tempDirectoryPath) manifest
        writeValue (unsolvedFilePath tempDirectoryPath) failureReport
        replaceDirectoryWithTemp outputDirectoryPath tempDirectoryPath
        manifest

    let private readEntriesFromManifest manifest =
        manifest.Chunks
        |> List.collect (fun chunkInfo ->
            match tryReadValue<PuzzleCorpusChunk> chunkInfo.FilePath with
            | Some chunk -> chunk.Puzzles
            | None -> [])

    let private tryReadManifestIn outputDirectoryPath =
        tryReadValue<PuzzleCorpusManifest> (manifestFilePath outputDirectoryPath)

    let private tryReadFailureReportIn outputDirectoryPath =
        tryReadValue<PuzzleCorpusFailureReport> (unsolvedFilePath outputDirectoryPath)

    let private sourceLineMatches (lines : string[]) lineNumber puzzle =
        if lineNumber >= 1 && lineNumber <= lines.Length then
            let sourcePuzzle = SudokuPuzzleInternals.normalizePuzzleString lines[lineNumber - 1]
            let importedPuzzle = SudokuPuzzleInternals.normalizePuzzleString puzzle
            sourcePuzzle = importedPuzzle
        else false

    let importRoyle sourceFilePath outputDirectoryPath (cancellationToken : CancellationToken) progress =
        if not (File.Exists sourceFilePath) then
            failwith ("Royle17 source file was not found at '" + sourceFilePath + "'.")
        let lines = File.ReadAllLines sourceFilePath
        let groups = Dictionary<string, ResizeArray<SudokuPuzzle>> ()
        let failures = ResizeArray<PuzzleCorpusFailure> ()
        let mutable solvedLineNumbers = Set.empty<int>
        let mutable failedLineNumbers = Set.empty<int>
        let addEntry (entry : SudokuPuzzle) =
            let signatureKey = PuzzleAnalysis.techniqueSignatureKey entry.Analysis.TechniqueCounts
            let entries =
                match groups.TryGetValue signatureKey with
                | true, entries -> entries
                | false, _ ->
                    let entries = ResizeArray<SudokuPuzzle> ()
                    groups.Add (signatureKey, entries)
                    entries
            entries.Add (entry.Dehydrate ())
            signatureKey
        match tryReadManifestIn outputDirectoryPath with
        | Some manifest when manifest.SourceName = SourceName && manifest.TotalPuzzles = lines.Length ->
            readEntriesFromManifest manifest
            |> List.sortBy (fun entry -> entry.Number)
            |> List.iter (fun entry ->
                if sourceLineMatches lines entry.Number entry.Puzzle && not (Set.contains entry.Number solvedLineNumbers) then
                    addEntry entry |> ignore
                    solvedLineNumbers <- Set.add entry.Number solvedLineNumbers)
            match tryReadFailureReportIn outputDirectoryPath with
            | Some failureReport ->
                failureReport.Failures
                |> List.sortBy (fun failure -> failure.LineNumber)
                |> List.iter (fun failure ->
                    if sourceLineMatches lines failure.LineNumber failure.Puzzle &&
                       not (Set.contains failure.LineNumber solvedLineNumbers) &&
                       not (Set.contains failure.LineNumber failedLineNumbers) then
                        failures.Add failure
                        failedLineNumbers <- Set.add failure.LineNumber failedLineNumbers)
            | None -> ()
        | _ -> ()
        let mutable processedLineNumbers = Set.union solvedLineNumbers failedLineNumbers
        let mutable solved = Set.count solvedLineNumbers
        let mutable failed = Set.count failedLineNumbers
        let mutable processed = Set.count processedLineNumbers
        let report processed currentSignatureKey lastErrorOpt =
            progress
                { Total = lines.Length
                  Processed = processed
                  Solved = solved
                  Failed = failed
                  CurrentSignatureKey = currentSignatureKey
                  LastErrorOpt = lastErrorOpt }
        let markProcessed lineNumber =
            if not (Set.contains lineNumber processedLineNumbers) then
                processedLineNumbers <- Set.add lineNumber processedLineNumbers
                processed <- processed + 1
        let checkpointCancellation () =
            if cancellationToken.IsCancellationRequested then
                if solved > 0 || failed > 0 then
                    report processed "" (Some "Import canceled; partial corpus saved.")
                    writeCorpus outputDirectoryPath sourceFilePath lines.Length groups failures |> ignore
                cancellationToken.ThrowIfCancellationRequested ()
        report processed "" None
        for index = 0 to lines.Length - 1 do
            checkpointCancellation ()
            let puzzle = lines[index]
            let lineNumber = index + 1
            if not (Set.contains lineNumber processedLineNumbers) then
                let fail reason =
                    failures.Add
                        { LineNumber = lineNumber
                          Puzzle = puzzle
                          Reason = reason }
                    failedLineNumbers <- Set.add lineNumber failedLineNumbers
                    failed <- failed + 1
                    markProcessed lineNumber
                    report processed "" (Some reason)
                match PuzzleAnalysis.solveExact puzzle with
                | PuzzleAnalysis.ExactFailed reason -> fail reason
                | PuzzleAnalysis.ExactSolved solution ->
                    match PuzzleAnalysis.solveSimpleFirstFromStrings solution puzzle with
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
                        let signatureKey = addEntry entry
                        solvedLineNumbers <- Set.add lineNumber solvedLineNumbers
                        solved <- solved + 1
                        markProcessed lineNumber
                        if processed % 10 = 0 then report processed signatureKey None
                    | None ->
                        fail "Current logical solver did not solve this puzzle."
                checkpointCancellation ()
        report lines.Length "" None
        writeCorpus outputDirectoryPath sourceFilePath lines.Length groups failures

    let tryReadManifest () =
        tryReadValue<PuzzleCorpusManifest> Assets.Gameplay.Royle17ManifestFilePath

    let readEntries () : SudokuPuzzle list =
        match tryReadManifest () with
        | Some manifest -> readEntriesFromManifest manifest
        | None -> []

    let entriesByDifficulty difficulty =
        readEntries ()
        |> List.filter (fun entry -> PuzzleAnalysis.difficultyOfEntry entry = difficulty)

    let countsByDifficulty () =
        match tryReadManifest () with
        | Some manifest -> manifest.DifficultyCounts
        | None -> Map.empty<Difficulty, int>