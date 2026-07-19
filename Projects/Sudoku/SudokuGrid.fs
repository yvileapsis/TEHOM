namespace Sudoku
open System
open System.Collections.Concurrent
open System.Numerics
open Prime
open Nu

// this represents the amount of puzzle information hidden from the player.
type Difficulty =
    | Trivial
    | Easy
    | Normal
    | Hard

    member this.Holes =
        match this with
        | Trivial -> 12
        | Easy -> 42
        | Normal -> 55
        | Hard -> 81

    member this.Label =
        match this with
        | Trivial -> "Trivial"
        | Easy -> "Easy"
        | Normal -> "Normal"
        | Hard -> "Hard"

// this represents the source of playable puzzles.
type PuzzleSource =
    | Generated
    | Classic

    member this.Label =
        match this with
        | Generated -> "Generated"
        | Classic -> "Classic"

// this represents the area a hint is asking the player to inspect.
type HintRegion =
    | HintCell of Vector2i
    | HintCells of Set<Vector2i>
    | HintRow of int
    | HintColumn of int
    | HintBlock of Vector2i

// this represents a simple human solving technique.
type HintTechnique =
    | PencilMarkCorrection
    | FullHouseRow
    | FullHouseColumn
    | FullHouseBlock
    | NakedSingle
    | HiddenSingleRow
    | HiddenSingleColumn
    | HiddenSingleBlock
    | NakedPairRow
    | NakedPairColumn
    | NakedPairBlock
    | HiddenPairRow
    | HiddenPairColumn
    | HiddenPairBlock
    | NakedTripleRow
    | NakedTripleColumn
    | NakedTripleBlock
    | HiddenTripleRow
    | HiddenTripleColumn
    | HiddenTripleBlock
    | PointingRow
    | PointingColumn
    | ClaimingRow
    | ClaimingColumn
    | XWingRow
    | XWingColumn
    | SwordfishRow
    | SwordfishColumn

    member this.Label =
        match this with
        | PencilMarkCorrection -> "Pencil mark correction"
        | FullHouseRow -> "Full house in row"
        | FullHouseColumn -> "Full house in column"
        | FullHouseBlock -> "Full house in block"
        | NakedSingle -> "Naked single"
        | HiddenSingleRow -> "Hidden single in row"
        | HiddenSingleColumn -> "Hidden single in column"
        | HiddenSingleBlock -> "Hidden single in block"
        | NakedPairRow -> "Naked pair in row"
        | NakedPairColumn -> "Naked pair in column"
        | NakedPairBlock -> "Naked pair in block"
        | HiddenPairRow -> "Hidden pair in row"
        | HiddenPairColumn -> "Hidden pair in column"
        | HiddenPairBlock -> "Hidden pair in block"
        | NakedTripleRow -> "Naked triple in row"
        | NakedTripleColumn -> "Naked triple in column"
        | NakedTripleBlock -> "Naked triple in block"
        | HiddenTripleRow -> "Hidden triple in row"
        | HiddenTripleColumn -> "Hidden triple in column"
        | HiddenTripleBlock -> "Hidden triple in block"
        | PointingRow -> "Pointing pair / triple by row"
        | PointingColumn -> "Pointing pair / triple by column"
        | ClaimingRow -> "Claiming pair / triple by row"
        | ClaimingColumn -> "Claiming pair / triple by column"
        | XWingRow -> "X-Wing by rows"
        | XWingColumn -> "X-Wing by columns"
        | SwordfishRow -> "Swordfish by rows"
        | SwordfishColumn -> "Swordfish by columns"

// this represents what a hint will do on its second press.
type HintAction =
    | PlaceNumber of Vector2i * int
    | RemoveMarks of (Vector2i * Set<int>) list
    | CorrectMarks of (Vector2i * Set<int>) list

// this represents a pending hint. The first hint press stores this and highlights its region; the second applies it.
type Hint =
    { Target : Vector2i
      Region : HintRegion
      Technique : HintTechnique
      Action : HintAction }

    member this.Label =
        match this.Action with
        | PlaceNumber (target, number) ->
            this.Technique.Label + ": row " + string (target.Y + 1) + ", column " + string (target.X + 1) + " can be " + string number + "."
        | RemoveMarks removals ->
            let numbers =
                removals
                |> List.collect (fun (_, numbers) -> Set.toList numbers)
                |> Set.ofList
                |> Set.toList
                |> List.map string
                |> String.concat ", "
            this.Technique.Label + ": remove pencil mark" + (if List.length removals = 1 then " " else "s ") + numbers + "."
        | CorrectMarks corrections ->
            this.Technique.Label + ": correct pencil marks in " + string (List.length corrections) + " cell" + (if List.length corrections = 1 then "" else "s") + "."

[<RequireQualifiedAccess>]
module private SudokuPuzzleInternals =

    let PuzzleLength = 81
    let EmptyPuzzle = String.replicate PuzzleLength "."
    let EmptySolution = EmptyPuzzle

    let isPuzzleChar c =
        c = '.' || c >= '1' && c <= '9'

    let normalizePuzzleString (puzzle : string) =
        if isNull puzzle then EmptyPuzzle
        else
            [|for i in 0 .. PuzzleLength - 1 do
                if i < puzzle.Length then
                    let c = puzzle[i]
                    yield if isPuzzleChar c then c else '.'
                else yield '.'|]
            |> String

    let solutionStringForStorage (solution : string) =
        normalizePuzzleString solution + "."

    let gridFromPuzzleString (puzzle : string) =
        let grid = Array2D.zeroCreate<int> 9 9
        if not (isNull puzzle) then
            puzzle
            |> Seq.truncate PuzzleLength
            |> Seq.iteri (fun i c ->
                let y = i / 9
                let x = i % 9
                grid[y, x] <- if c = '.' || c = '0' then 0 else int c - int '0')
        grid

    let givenFromPuzzleString (puzzle : string) =
        let given = Array2D.create 9 9 false
        if not (isNull puzzle) then
            puzzle
            |> Seq.truncate PuzzleLength
            |> Seq.iteri (fun i c ->
                let y = i / 9
                let x = i % 9
                given[y, x] <- c <> '.' && c <> '0')
        given

    let puzzleStringFromGrid (grid : int[,]) =
        [|for y in 0 .. 8 do
            for x in 0 .. 8 do
                let value = grid[y, x]
                yield if value = 0 then '.' else char (int '0' + value)|]
        |> String

    let permutations3 =
        [|[|0; 1; 2|]
          [|0; 2; 1|]
          [|1; 0; 2|]
          [|1; 2; 0|]
          [|2; 0; 1|]
          [|2; 1; 0|]|]

    let makePermutations9 () =
        [|for groupPermutation in permutations3 do
            for inner0 in permutations3 do
                for inner1 in permutations3 do
                    for inner2 in permutations3 do
                        let inners = [|inner0; inner1; inner2|]
                        yield
                            [|for outputGroup in 0 .. 2 do
                                let sourceGroup = groupPermutation[outputGroup]
                                let inner = inners[outputGroup]
                                for outputInner in 0 .. 2 do
                                    yield sourceGroup * 3 + inner[outputInner]|]|]

    let rowPermutations = makePermutations9 ()
    let columnPermutations = rowPermutations
    let canonicalKeyCache = ConcurrentDictionary<string, string> ()

    let computeCanonicalKey (value : string) =
        if isNull value || value.Length <> PuzzleLength then value
        else
            let source = value.ToCharArray ()
            let mutable best : string = null
            for transposed in [false; true] do
                for rowPermutation in rowPermutations do
                    for columnPermutation in columnPermutations do
                        let chars = Array.zeroCreate<char> PuzzleLength
                        let digitMap = Array.zeroCreate<char> 10
                        let mutable nextDigit = int '1'
                        let mutable relation = 0
                        let mutable skipped = false
                        let mutable i = 0
                        while i < PuzzleLength && not skipped do
                            let y = i / 9
                            let x = i % 9
                            let sourceY, sourceX =
                                if transposed then columnPermutation[x], rowPermutation[y]
                                else rowPermutation[y], columnPermutation[x]
                            let raw = source[sourceY * 9 + sourceX]
                            let normalized =
                                if raw = '.' || raw = '0' then '.'
                                else
                                    let index = int raw - int '0'
                                    let mapped = digitMap[index]
                                    if mapped = char 0 then
                                        let mapped = char nextDigit
                                        digitMap[index] <- mapped
                                        nextDigit <- nextDigit + 1
                                        mapped
                                    else mapped
                            if not (isNull best) && relation = 0 then
                                let bestChar = best[i]
                                if normalized > bestChar then skipped <- true
                                elif normalized < bestChar then relation <- -1
                            if not skipped then
                                chars[i] <- normalized
                                i <- i + 1
                        if not skipped then
                            let candidate = String chars
                            if isNull best || String.CompareOrdinal (candidate, best) < 0 then best <- candidate
            best

    let canonicalKey value =
        if isNull value then value
        else
            canonicalKeyCache.GetOrAdd (value, Func<string, string>(computeCanonicalKey))

// this contains one puzzle's solve analysis and generation scoring.
type SudokuPuzzleAnalysis =
    { TechniqueCounts : Map<HintTechnique, int>
      TotalSteps : int
      EliminationSteps : int
      PointingClaimingSteps : int
      NakedSubsetSteps : int
      HiddenSingleSteps : int
      FishSteps : int
      MaxEliminationChain : int
      RemovedCells : int
      GenerationScore : int
      OpportunityScoreOpt : int option }

    static member empty =
        { TechniqueCounts = Map.empty
          TotalSteps = 0
          EliminationSteps = 0
          PointingClaimingSteps = 0
          NakedSubsetSteps = 0
          HiddenSingleSteps = 0
          FishSteps = 0
          MaxEliminationChain = 0
          RemovedCells = 0
          GenerationScore = 0
          OpportunityScoreOpt = None }

// this contains one puzzle's hydrated board data used by gameplay and analysis.
type SudokuPuzzleDisplay =
    { PuzzleGrid : int[,]
      SolutionGrid : int[,]
      Given : bool[,]
      Marks : Set<int>[,] }

    static member allPositions =
        [for y in 0 .. 8 do for x in 0 .. 8 -> v2i x y]

    static member rowPositions (row : int) =
        [for x in 0 .. 8 -> v2i x row]

    static member columnPositions (column : int) =
        [for y in 0 .. 8 -> v2i column y]

    static member blockPositions (block : Vector2i) =
        [for y in block.Y * 3 .. block.Y * 3 + 2 do
            for x in block.X * 3 .. block.X * 3 + 2 -> v2i x y]

    static member blockOfPosition (position : Vector2i) =
        v2i (position.X / 3) (position.Y / 3)

    static member emptyMarks () =
        Array2D.create 9 9 Set.empty<int>

    static member fromStrings puzzle solution =
        let puzzle = SudokuPuzzleInternals.normalizePuzzleString puzzle
        let solution = SudokuPuzzleInternals.normalizePuzzleString solution
        { PuzzleGrid = SudokuPuzzleInternals.gridFromPuzzleString puzzle
          SolutionGrid = SudokuPuzzleInternals.gridFromPuzzleString solution
          Given = SudokuPuzzleInternals.givenFromPuzzleString puzzle
          Marks = SudokuPuzzleDisplay.emptyMarks () }

    static member fromGrids puzzle solution given =
        { PuzzleGrid = Array2D.copy puzzle
          SolutionGrid = Array2D.copy solution
          Given = Array2D.copy given
          Marks = SudokuPuzzleDisplay.emptyMarks () }

    static member combinations (count : int) (items : 'a list) =
        let rec step (count : int) (items : 'a list) =
            match (count, items) with
            | (0, _) -> [[]]
            | (_, []) -> []
            | (count, head :: tail) ->
                let withHead = step (count - 1) tail |> List.map (fun items -> head :: items)
                let withoutHead = step count tail
                withHead @ withoutHead
        step count items

    static member candidates (board : int[,]) (position : Vector2i) =
        if board[position.Y, position.X] <> 0 then Set.empty
        else
            let rowValues = set [for x in 0 .. 8 do if board[position.Y, x] <> 0 then yield board[position.Y, x]]
            let columnValues = set [for y in 0 .. 8 do if board[y, position.X] <> 0 then yield board[y, position.X]]
            let blockMinX = position.X / 3 * 3
            let blockMinY = position.Y / 3 * 3
            let blockValues =
                set [for y in blockMinY .. blockMinY + 2 do
                        for x in blockMinX .. blockMinX + 2 do
                            if board[y, x] <> 0 then yield board[y, x]]
            Set.difference (set [1 .. 9]) (Set.unionMany [rowValues; columnValues; blockValues])

    static member workingCandidates (display : SudokuPuzzleDisplay) (position : Vector2i) =
        let grid = display.PuzzleGrid
        if grid[position.Y, position.X] <> 0 then Set.empty
        else
            let marks = display.Marks[position.Y, position.X]
            if Set.notEmpty marks then marks
            else SudokuPuzzleDisplay.candidates grid position

    static member fillLegalMarks (display : SudokuPuzzleDisplay) =
        let grid = display.PuzzleGrid
        let given = display.Given
        let marks = Array2D.copy display.Marks
        for position in SudokuPuzzleDisplay.allPositions do
            if grid[position.Y, position.X] = 0 &&
               not given[position.Y, position.X] &&
               Set.isEmpty marks[position.Y, position.X] then
                marks[position.Y, position.X] <- SudokuPuzzleDisplay.candidates grid position
        { display with Marks = marks }

    static member hasOpenCellsWithoutMarks (display : SudokuPuzzleDisplay) =
        let grid = display.PuzzleGrid
        let given = display.Given
        let marks = display.Marks
        SudokuPuzzleDisplay.allPositions
        |> List.exists (fun position ->
            grid[position.Y, position.X] = 0 &&
            not given[position.Y, position.X] &&
            Set.isEmpty marks[position.Y, position.X])

    static member correctedMarkSets (display : SudokuPuzzleDisplay) =
        let grid = display.PuzzleGrid
        let solution = display.SolutionGrid
        let given = display.Given
        let marksGrid = display.Marks
        SudokuPuzzleDisplay.allPositions
        |> List.choose (fun position ->
            let marks = marksGrid[position.Y, position.X]
            if grid[position.Y, position.X] = 0 &&
               not given[position.Y, position.X] &&
               Set.notEmpty marks then
                let legal = SudokuPuzzleDisplay.candidates grid position
                let solution = solution[position.Y, position.X]
                if Set.contains solution legal then
                    let corrected = Set.add solution (Set.intersect marks legal)
                    if corrected <> marks then Some (position, corrected)
                    else None
                else None
            else None)

    static member peerPositions (position : Vector2i) =
        let block = SudokuPuzzleDisplay.blockOfPosition position
        [SudokuPuzzleDisplay.rowPositions position.Y
         SudokuPuzzleDisplay.columnPositions position.X
         SudokuPuzzleDisplay.blockPositions block]
        |> List.concat
        |> List.filter (fun peer -> peer <> position)
        |> Set.ofList
        |> Set.toList

    static member prunePeerMarks (position : Vector2i) (number : int) (marks : Set<int>[,]) =
        if number <> 0 then
            for peer in SudokuPuzzleDisplay.peerPositions position do
                marks[peer.Y, peer.X] <- Set.remove number marks[peer.Y, peer.X]

    static member applyHint (hint : Hint) (display : SudokuPuzzleDisplay) =
        match hint.Action with
        | PlaceNumber (target, number) ->
            let grid = Array2D.copy display.PuzzleGrid
            let marks = Array2D.copy display.Marks
            grid[target.Y, target.X] <- number
            marks[target.Y, target.X] <- Set.empty
            SudokuPuzzleDisplay.prunePeerMarks target number marks
            { display with PuzzleGrid = grid; Marks = marks }
        | RemoveMarks removals ->
            let marks = Array2D.copy display.Marks
            for (position, removed) in removals do
                marks[position.Y, position.X] <- Set.difference marks[position.Y, position.X] removed
            { display with Marks = marks }
        | CorrectMarks corrections ->
            let marks = Array2D.copy display.Marks
            for (position, corrected) in corrections do
                marks[position.Y, position.X] <- corrected
            { display with Marks = marks }

    static member isSolved (board : int[,]) =
        let row y =
            [for x in 0 .. 8 -> board[y, x]]
        let column x =
            [for y in 0 .. 8 -> board[y, x]]
        let block blockX blockY =
            [for y in blockY * 3 .. blockY * 3 + 2 do
             for x in blockX * 3 .. blockX * 3 + 2 -> board[y, x]]

        let hasNoRepeats values = List.distinct values = values
        let hasAllNumbers values = List.length values = 9 // List.sort values = numbers

        [0 .. 8] |> List.forall (row >> hasNoRepeats)
        && [0 .. 8] |> List.forall (column >> hasNoRepeats)
        && [0 .. 8] |> List.forall (fun i -> hasNoRepeats (block (i % 3) (i / 3)))
        && [0 .. 8] |> List.forall (fun i -> hasAllNumbers (block (i % 3) (i / 3)))

    static member hasConflict (board : int[,]) (position : Vector2i) (value : int) =
        if value = 0 then false
        else
            let row = position.Y
            let column = position.X
            let rowConflict =
                [0 .. 8]
                |> List.exists (fun x -> x <> column && board[row, x] = value)
            let columnConflict =
                [0 .. 8]
                |> List.exists (fun y -> y <> row && board[y, column] = value)
            let blockConflict =
                SudokuPuzzleDisplay.blockPositions (SudokuPuzzleDisplay.blockOfPosition position)
                |> List.exists (fun position2 -> position2 <> position && board[position2.Y, position2.X] = value)
            rowConflict || columnConflict || blockConflict

    static member isPuzzleSolvedAgainstSolution (solution : int[,]) (puzzle : int[,]) =
        SudokuPuzzleDisplay.allPositions
        |> List.forall (fun position -> puzzle[position.Y, position.X] = solution[position.Y, position.X])

    static member givenFromPuzzle (puzzle : int[,]) =
        let given = Array2D.create 9 9 false
        for position in SudokuPuzzleDisplay.allPositions do
            given[position.Y, position.X] <- puzzle[position.Y, position.X] <> 0
        given

// this contains a puzzle's storage identity plus optional analysis and hydrated board data.
type SudokuPuzzle =
    { SolutionCanonical : string
      PuzzleCanonical : string
      Number : int
      Solution : string
      Puzzle : string
      AnalysisOpt : SudokuPuzzleAnalysis option
      DisplayOpt : SudokuPuzzleDisplay option }

    member this.Analysis =
        defaultArg this.AnalysisOpt SudokuPuzzleAnalysis.empty

    member this.Display =
        match this.DisplayOpt with
        | Some display -> display
        | None -> failwith "SudokuPuzzle display data is not hydrated."

    member this.Dehydrate () =
        let puzzle =
            match this.DisplayOpt with
            | Some display -> SudokuPuzzleInternals.puzzleStringFromGrid display.PuzzleGrid
            | None -> SudokuPuzzleInternals.normalizePuzzleString this.Puzzle
        let solution =
            match this.DisplayOpt with
            | Some display -> SudokuPuzzleInternals.puzzleStringFromGrid display.SolutionGrid
            | None -> SudokuPuzzleInternals.normalizePuzzleString this.Solution
        let puzzleCanonical = SudokuPuzzleInternals.canonicalKey puzzle
        let solutionCanonical = SudokuPuzzleInternals.canonicalKey solution
        { this with
            Puzzle = puzzle
            Solution = SudokuPuzzleInternals.solutionStringForStorage solution
            PuzzleCanonical = puzzleCanonical
            SolutionCanonical = SudokuPuzzleInternals.solutionStringForStorage solutionCanonical
            DisplayOpt = None }

    member this.Rehydrate () =
        let puzzle = SudokuPuzzleInternals.normalizePuzzleString this.Puzzle
        let solution = SudokuPuzzleInternals.normalizePuzzleString this.Solution
        let display = Option.defaultWith (fun () -> SudokuPuzzleDisplay.fromStrings puzzle solution) this.DisplayOpt
        { this with
            Puzzle = puzzle
            Solution = solution
            PuzzleCanonical = SudokuPuzzleInternals.canonicalKey puzzle
            SolutionCanonical = SudokuPuzzleInternals.canonicalKey solution
            DisplayOpt = Some display }

    static member empty =
        { SolutionCanonical = SudokuPuzzleInternals.EmptySolution
          PuzzleCanonical = SudokuPuzzleInternals.EmptyPuzzle
          Number = 0
          Solution = SudokuPuzzleInternals.EmptySolution
          Puzzle = SudokuPuzzleInternals.EmptyPuzzle
          AnalysisOpt = None
          DisplayOpt = None }

    static member fromGrids number puzzle solution given =
        let puzzleString = SudokuPuzzleInternals.puzzleStringFromGrid puzzle
        let solutionString = SudokuPuzzleInternals.puzzleStringFromGrid solution
        { SudokuPuzzle.empty with
            SolutionCanonical = ""
            PuzzleCanonical = ""
            Number = number
            Solution = solutionString
            Puzzle = puzzleString
            DisplayOpt = Some (SudokuPuzzleDisplay.fromGrids puzzle solution given) }

[<RequireQualifiedAccess>]
module SudokuGrid =

    let shuffle (items : 'a list) =
        items |> List.sortBy (fun _ -> Gen.random1 Int32.MaxValue)

    let makeSolvedBoard () =
        let board = Array2D.zeroCreate<int> 9 9
        let digits = shuffle [1 .. 9] |> List.toArray
        let rowBands = shuffle [0 .. 2]
        let columnBands = shuffle [0 .. 2]
        let rows =
            [for band in rowBands do
                for row in shuffle [0 .. 2] -> band * 3 + row]
        let columns =
            [for band in columnBands do
                for column in shuffle [0 .. 2] -> band * 3 + column]
        for y in 0 .. 8 do
            for x in 0 .. 8 do
                let pattern = (rows[y] * 3 + rows[y] / 3 + columns[x]) % 9
                board[y, x] <- digits[pattern]
        board
