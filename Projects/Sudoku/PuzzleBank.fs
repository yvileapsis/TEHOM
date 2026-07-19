namespace Sudoku
open System
open System.IO
open Prime
open Nu

// this contains the serialized bank for a single difficulty.
type PuzzleBankData =
    { Difficulty : Difficulty
      Puzzles : SudokuPuzzle list }

    static member empty (difficulty : Difficulty) : PuzzleBankData =
        { Difficulty = difficulty
          Puzzles = [] }

    static member make (difficulty : Difficulty) (puzzles : SudokuPuzzle list) : PuzzleBankData =
        { Difficulty = difficulty
          Puzzles = puzzles }

[<RequireQualifiedAccess>]
module PuzzleBank =

    let Difficulties = [Trivial; Easy; Normal; Hard]

    let private ioLock = obj ()
    let mutable queues = Map.empty<PuzzleSource * Difficulty, SudokuPuzzle list>

    let key (puzzle : SudokuPuzzle) =
        puzzle.PuzzleCanonical

    let private normalizePuzzleCanonicals (puzzle : SudokuPuzzle) =
        puzzle.Dehydrate ()

    let private isValidPuzzle (puzzle : SudokuPuzzle) =
        let solution = SudokuPuzzleInternals.normalizePuzzleString puzzle.Solution
        PuzzleAnalysis.puzzleMatchesSolution puzzle.Puzzle solution &&
        (solution |> SudokuPuzzleInternals.gridFromPuzzleString |> SudokuPuzzleDisplay.isSolved) &&
        not (String.IsNullOrWhiteSpace puzzle.PuzzleCanonical) &&
        not (String.IsNullOrWhiteSpace puzzle.SolutionCanonical)

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

    let private sanitizeData (data : PuzzleBankData) =
        { data with
            Puzzles =
                data.Puzzles
                |> List.map normalizePuzzleCanonicals
                |> List.filter isValidPuzzle
                |> deduplicate
                |> numberPuzzles }

    let private readDataUnlocked (difficulty : Difficulty) =
        let filePath = Assets.Gameplay.PuzzleBankFilePath difficulty.Label
        if File.Exists filePath then
            try
                File.ReadAllText filePath
                |> scvalue<PuzzleBankData>
                |> sanitizeData
            with exn ->
                Log.warn ("Failed to read Sudoku puzzle bank '" + filePath + "' due to: " + scstring exn)
                PuzzleBankData.empty difficulty
        else PuzzleBankData.empty difficulty

    let private writeDataUnlocked (data : PuzzleBankData) =
        let filePath = Assets.Gameplay.PuzzleBankFilePath data.Difficulty.Label
        let directory = Path.GetDirectoryName filePath
        if not (String.IsNullOrWhiteSpace directory) then
            Directory.CreateDirectory directory |> ignore<DirectoryInfo>
        let data = sanitizeData data
        let filePathTmp = filePath + ".tmp"
        let symbol = valueToSymbol data
        File.WriteAllText (filePathTmp, PrettyPrinter.prettyPrintSymbol symbol PrettyPrinter.defaultPrinter)
        if File.Exists filePath then File.SetAttributes (filePath, FileAttributes.Normal)
        File.Move (filePathTmp, filePath, true)
        data

    let read difficulty =
        lock ioLock (fun () -> readDataUnlocked difficulty)

    let write data =
        lock ioLock (fun () -> writeDataUnlocked data)

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

    let mergeGeneratedBatch (difficulty : Difficulty) (generated : SudokuPuzzle list) =
        lock ioLock (fun () ->
            let data = readDataUnlocked difficulty
            let data =
                { data with
                    Puzzles =
                        data.Puzzles @ generated
                        |> List.map normalizePuzzleCanonicals
                        |> List.filter isValidPuzzle
                        |> deduplicate }
            let data = writeDataUnlocked data
            data.Puzzles.Length)

    let mergeGenerated difficulty generated =
        mergeGeneratedBatch difficulty [generated]

    let private puzzlesForSource source difficulty =
        match source with
        | Generated -> (read difficulty).Puzzles
        | Classic -> PuzzleCorpus.entriesByDifficulty difficulty

    let tryTake source difficulty =
        let queueKey = (source, difficulty)
        match Map.tryFind queueKey queues with
        | Some (puzzle :: puzzles) ->
            queues <- Map.add queueKey puzzles queues
            Some (puzzle.Rehydrate ())
        | Some [] | None ->
            match puzzlesForSource source difficulty |> SudokuGrid.shuffle with
            | puzzle :: puzzles ->
                queues <- Map.add queueKey puzzles queues
                Some (puzzle.Rehydrate ())
            | [] ->
                queues <- Map.remove queueKey queues
                None
