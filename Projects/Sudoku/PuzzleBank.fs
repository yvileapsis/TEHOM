namespace Sudoku
open System
open System.Collections.Generic
open System.IO
open Prime
open Nu

[<RequireQualifiedAccess>]
module PuzzleBank =

    let SchemaVersion = 2
    let Difficulties = [Trivial; Easy; Normal; Hard]

    let private ioLock = obj ()
    let private queueLock = obj ()
    let private queues = Dictionary<Difficulty, PuzzleBankEntry list> ()

    let private emptyData (difficulty : Difficulty) : PuzzleBankData =
        { SchemaVersion = SchemaVersion
          Difficulty = difficulty
          Puzzles = [] }

    let private hasGridShape (values : 'a list) =
        List.length values = 81

    let private validPuzzleValue value =
        value >= 0 && value <= 9

    let private validSolutionValue value =
        value >= 1 && value <= 9

    let private gridFromFlat (values : int list) =
        let grid = Array2D.zeroCreate<int> 9 9
        values
        |> List.iteri (fun i value ->
            let y = i / 9
            let x = i % 9
            grid[y, x] <- value)
        grid

    let private boolGridFromFlat (values : bool list) =
        let grid = Array2D.zeroCreate<bool> 9 9
        values
        |> List.iteri (fun i value ->
            let y = i / 9
            let x = i % 9
            grid[y, x] <- value)
        grid

    let private flattenGrid (grid : int[,]) =
        [for y in 0 .. 8 do
            for x in 0 .. 8 -> grid[y, x]]

    let private flattenBoolGrid (grid : bool[,]) =
        [for y in 0 .. 8 do
            for x in 0 .. 8 -> grid[y, x]]

    let private gridHasShape (grid : 'a[,]) =
        grid.GetLength 0 = 9 && grid.GetLength 1 = 9

    let private entryKey (entry : PuzzleBankEntry) =
        entry.Puzzle
        |> List.map string
        |> String.concat ""

    let private isValidEntry (entry : PuzzleBankEntry) =
        hasGridShape entry.Puzzle &&
        hasGridShape entry.Solution &&
        List.length entry.Given = 81 &&
        List.forall validPuzzleValue entry.Puzzle &&
        List.forall validSolutionValue entry.Solution &&
        List.forall2 (fun value given -> given = (value <> 0)) entry.Puzzle entry.Given &&
        List.forall2 (fun puzzle solution -> puzzle = 0 || puzzle = solution) entry.Puzzle entry.Solution &&
        (entry.Solution |> gridFromFlat |> SudokuGrid.isSolved)

    let private numberEntries (entries : PuzzleBankEntry list) =
        entries
        |> List.mapi (fun i entry -> { entry with Number = i + 1 })

    let private deduplicate (entries : PuzzleBankEntry list) =
        let (_, entries) =
            entries
            |> List.fold (fun (keys, entries) entry ->
                let key = entryKey entry
                if Set.contains key keys then (keys, entries)
                else (Set.add key keys, entry :: entries))
                (Set.empty<string>, [])
        List.rev entries

    let private sanitizeData (difficulty : Difficulty) (data : PuzzleBankData) =
        if data.SchemaVersion = SchemaVersion && data.Difficulty = difficulty then
            { data with
                Puzzles =
                    data.Puzzles
                    |> List.filter isValidEntry
                    |> deduplicate
                    |> numberEntries }
        else emptyData difficulty

    let private readDataUnlocked (difficulty : Difficulty) =
        let filePath = Assets.Gameplay.PuzzleBankFilePath difficulty
        if File.Exists filePath then
            try
                let fileStr = File.ReadAllText filePath
                let data = scvalue<PuzzleBankData> fileStr
                sanitizeData difficulty data
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
        File.WriteAllText (filePathTmp, scstring data)
        if File.Exists filePath then File.SetAttributes (filePath, FileAttributes.Normal)
        File.Move (filePathTmp, filePath, true)
        data

    let private entryFromGenerated (generated : GeneratedPuzzle) : PuzzleBankEntry =
        { Number = Option.defaultValue 0 generated.PuzzleNumberOpt
          Puzzle = flattenGrid generated.Puzzle
          Solution = flattenGrid generated.Solution
          Given = flattenBoolGrid generated.Given
          TechniqueCounts = generated.TechniqueCounts
          GenerationScore = generated.GenerationScore
          MaxEliminationChain = generated.MaxEliminationChain
          Ranking = generated.Ranking }

    let private tryGeneratedFromEntry (entry : PuzzleBankEntry) : GeneratedPuzzle option =
        if isValidEntry entry then
            Some
                { Puzzle = gridFromFlat entry.Puzzle
                  Solution = gridFromFlat entry.Solution
                  Given = boolGridFromFlat entry.Given
                  PuzzleNumberOpt = Some entry.Number
                  TechniqueCounts = entry.TechniqueCounts
                  GenerationScore = entry.GenerationScore
                  MaxEliminationChain = entry.MaxEliminationChain
                  Ranking = entry.Ranking }
        else None

    let read difficulty =
        lock ioLock (fun () -> readDataUnlocked difficulty)

    let write difficulty data =
        lock ioLock (fun () -> writeDataUnlocked difficulty data)

    let key entry =
        entryKey entry

    let toEntry generated =
        entryFromGenerated generated

    let count difficulty =
        (read difficulty).Puzzles.Length

    let counts () =
        Difficulties
        |> List.map (fun difficulty -> (difficulty, count difficulty))
        |> Map.ofList

    let mergeGeneratedBatch (difficulty : Difficulty) (generated : GeneratedPuzzle list) =
        lock ioLock (fun () ->
            let data = readDataUnlocked difficulty
            let incoming =
                generated
                |> List.filter (fun generated ->
                    gridHasShape generated.Puzzle &&
                    gridHasShape generated.Solution &&
                    gridHasShape generated.Given)
                |> List.map entryFromGenerated
            let data =
                { data with
                    Puzzles =
                        data.Puzzles @ incoming
                        |> List.filter isValidEntry
                        |> deduplicate }
            let data = writeDataUnlocked difficulty data
            data.Puzzles.Length)

    let mergeGenerated difficulty generated =
        mergeGeneratedBatch difficulty [generated]

    let tryTake difficulty =
        lock queueLock (fun () ->
            let entries =
                match queues.TryGetValue difficulty with
                | (true, entry :: entries) ->
                    queues[difficulty] <- entries
                    entry :: []
                | (true, []) | (false, _) ->
                    let entries =
                        (read difficulty).Puzzles
                        |> SudokuGrid.shuffle
                    match entries with
                    | entry :: entries ->
                        queues[difficulty] <- entries
                        entry :: []
                    | [] ->
                        queues[difficulty] <- []
                        []
            entries |> List.tryPick tryGeneratedFromEntry)