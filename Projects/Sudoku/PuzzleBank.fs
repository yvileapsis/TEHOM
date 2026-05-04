namespace Sudoku
open System
open System.Collections.Generic
open System.IO
open Prime
open Nu

[<RequireQualifiedAccess>]
module PuzzleBank =

    let SchemaVersion = 4
    let Difficulties = [Trivial; Easy; Normal; Hard]

    let private ioLock = obj ()
    let private queueLock = obj ()
    let private queues = Dictionary<Difficulty, GeneratedPuzzle list> ()

    let private emptyData (difficulty : Difficulty) : PuzzleBankData =
        { SchemaVersion = SchemaVersion
          Difficulty = difficulty
          Puzzles = [] }

    let private validPuzzleValue value =
        value >= 0 && value <= 9

    let private validSolutionValue value =
        value >= 1 && value <= 9

    let private hasGridShape values =
        List.length values = 81

    let private gridFromFlat (values : int list) =
        let grid = Array2D.zeroCreate<int> 9 9
        values
        |> List.iteri (fun i value ->
            let y = i / 9
            let x = i % 9
            grid[y, x] <- value)
        grid

    let key (puzzle : GeneratedPuzzle) =
        puzzle.Puzzle
        |> List.map string
        |> String.concat ""

    let private isValidPuzzle (puzzle : GeneratedPuzzle) =
        if hasGridShape puzzle.Puzzle && hasGridShape puzzle.Solution && hasGridShape puzzle.Given then
            List.forall validPuzzleValue puzzle.Puzzle &&
            List.forall validSolutionValue puzzle.Solution &&
            List.forall2 (fun value given -> given = (value <> 0)) puzzle.Puzzle puzzle.Given &&
            List.forall2 (fun puzzle solution -> puzzle = 0 || puzzle = solution) puzzle.Puzzle puzzle.Solution &&
            (puzzle.Solution |> gridFromFlat |> SudokuGrid.isSolved)
        else false

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

    let mergeGeneratedBatch (difficulty : Difficulty) (generated : GeneratedPuzzle list) =
        lock ioLock (fun () ->
            let data = readDataUnlocked difficulty
            let data =
                { data with
                    Puzzles =
                        data.Puzzles @ generated
                        |> List.filter isValidPuzzle
                        |> deduplicate }
            let data = writeDataUnlocked difficulty data
            data.Puzzles.Length)

    let mergeGenerated difficulty generated =
        mergeGeneratedBatch difficulty [generated]

    let tryTake difficulty =
        lock queueLock (fun () ->
            match queues.TryGetValue difficulty with
            | (true, puzzle :: puzzles) ->
                queues[difficulty] <- puzzles
                Some puzzle
            | (true, []) | (false, _) ->
                let puzzles = (read difficulty).Puzzles |> SudokuGrid.shuffle
                match puzzles with
                | puzzle :: puzzles ->
                    queues[difficulty] <- puzzles
                    Some puzzle
                | [] ->
                    queues[difficulty] <- []
                    None)