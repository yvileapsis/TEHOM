namespace Sudoku
open System
open System.Collections.Generic
open System.IO
open Prime
open Nu

[<RequireQualifiedAccess>]
module PuzzleBank =

    let SchemaVersion = 6
    let Difficulties = [Trivial; Easy; Normal; Hard]

    let private ioLock = obj ()
    let private queueLock = obj ()
    let private queues = Dictionary<PuzzleSource * Difficulty, GeneratedPuzzle list> ()

    let private emptyData (difficulty : Difficulty) : PuzzleBankData =
        { SchemaVersion = SchemaVersion
          Difficulty = difficulty
          Puzzles = [] }

    let key (puzzle : GeneratedPuzzle) =
        puzzle.PuzzleKey

    let private normalizePuzzleKeys (puzzle : GeneratedPuzzle) =
        { puzzle with
            PuzzleKey = PuzzleAnalysis.canonicalKey puzzle.Puzzle
            SolutionKey = PuzzleAnalysis.canonicalKey puzzle.Solution }

    let private isValidPuzzle (puzzle : GeneratedPuzzle) =
        PuzzleAnalysis.puzzleMatchesSolution puzzle.Puzzle puzzle.Solution &&
        (puzzle.Solution |> PuzzleAnalysis.gridFromPuzzleString |> SudokuGrid.isSolved) &&
        not (String.IsNullOrWhiteSpace puzzle.PuzzleKey) &&
        not (String.IsNullOrWhiteSpace puzzle.SolutionKey)

    let private deduplicate puzzles =
        let (_, puzzles) =
            puzzles
            |> List.fold (fun (keys, puzzles) puzzle ->
                let puzzleKey = key puzzle
                if Set.contains puzzleKey keys then (keys, puzzles)
                else (Set.add puzzleKey keys, puzzle :: puzzles))
                (Set.empty<string>, [])
        List.rev puzzles

    let private numberPuzzles puzzles =
        puzzles
        |> List.mapi (fun i puzzle -> { puzzle with Number = i + 1 })

    let private sanitizeData (difficulty : Difficulty) (data : PuzzleBankData) =
        if data.SchemaVersion = SchemaVersion && data.Difficulty = difficulty then
            { data with
                Puzzles =
                    data.Puzzles
                    |> List.map normalizePuzzleKeys
                    |> List.filter isValidPuzzle
                    |> deduplicate
                    |> numberPuzzles }
        else emptyData difficulty

    let private readDataUnlocked (difficulty : Difficulty) =
        let filePath = Assets.Gameplay.PuzzleBankFilePath difficulty
        if File.Exists filePath then
            try
                File.ReadAllText filePath
                |> scvalue<PuzzleBankData>
                |> sanitizeData difficulty
            with exn ->
                Log.warn ("Failed to read Sudoku puzzle bank '" + filePath + "' due to: " + scstring exn)
                emptyData difficulty
        else emptyData difficulty

    let private writeDataUnlocked (difficulty : Difficulty) (data : PuzzleBankData) =
        let filePath = Assets.Gameplay.PuzzleBankFilePath difficulty
        let directory = Path.GetDirectoryName filePath
        if not (String.IsNullOrWhiteSpace directory) then
            Directory.CreateDirectory directory |> ignore<DirectoryInfo>
        let data = sanitizeData difficulty { data with SchemaVersion = SchemaVersion; Difficulty = difficulty }
        let filePathTmp = filePath + ".tmp"
        let symbol = valueToSymbol data
        File.WriteAllText (filePathTmp, PrettyPrinter.prettyPrintSymbol symbol PrettyPrinter.defaultPrinter)
        if File.Exists filePath then File.SetAttributes (filePath, FileAttributes.Normal)
        File.Move (filePathTmp, filePath, true)
        data

    let read difficulty =
        lock ioLock (fun () -> readDataUnlocked difficulty)

    let write difficulty data =
        lock ioLock (fun () -> writeDataUnlocked difficulty data)

    let count difficulty =
        (read difficulty).Puzzles.Length

    let counts () =
        Difficulties
        |> List.map (fun difficulty -> (difficulty, count difficulty))
        |> Map.ofList

    let classicCount difficulty =
        PuzzleCorpus.countsByDifficulty ()
        |> Map.tryFind difficulty
        |> Option.defaultValue 0

    let classicCounts () =
        Difficulties
        |> List.map (fun difficulty -> (difficulty, classicCount difficulty))
        |> Map.ofList

    let mergeGeneratedBatch (difficulty : Difficulty) (generated : GeneratedPuzzle list) =
        lock ioLock (fun () ->
            let data = readDataUnlocked difficulty
            let data =
                { data with
                    Puzzles =
                        data.Puzzles @ generated
                        |> List.map normalizePuzzleKeys
                        |> List.filter isValidPuzzle
                        |> deduplicate }
            let data = writeDataUnlocked difficulty data
            data.Puzzles.Length)

    let mergeGenerated difficulty generated =
        mergeGeneratedBatch difficulty [generated]

    let private puzzlesForSource source difficulty =
        match source with
        | Generated -> (read difficulty).Puzzles
        | Classic -> PuzzleCorpus.entriesByDifficulty difficulty

    let tryTake source difficulty =
        lock queueLock (fun () ->
            let queueKey = (source, difficulty)
            match queues.TryGetValue queueKey with
            | (true, puzzle :: puzzles) ->
                queues[queueKey] <- puzzles
                Some puzzle
            | (true, []) | (false, _) ->
                let puzzles = puzzlesForSource source difficulty |> SudokuGrid.shuffle
                match puzzles with
                | puzzle :: puzzles ->
                    queues[queueKey] <- puzzles
                    Some puzzle
                | [] ->
                    queues[queueKey] <- []
                    None)
