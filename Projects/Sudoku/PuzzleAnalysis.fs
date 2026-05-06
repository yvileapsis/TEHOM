namespace Sudoku
open System
open System.Collections.Generic
open System.Numerics
open Prime

[<RequireQualifiedAccess>]
module PuzzleAnalysis =

    type SolveProfile =
        { TechniqueCounts : Map<HintTechnique, int>
          TotalSteps : int
          EliminationSteps : int
          PointingClaimingSteps : int
          NakedSubsetSteps : int
          HiddenSingleSteps : int
          FishSteps : int
          MaxEliminationChain : int
          CurrentEliminationChain : int
          InterestScore : int
          TraceReversed : PuzzleSolveStep list }

    type HintSelection =
        { Hint : Hint
          Score : int }

    type ExactSolveResult =
        | ExactSolved of string
        | ExactFailed of string

    let PuzzleLength = 81
    let EmptyPuzzle = String.replicate PuzzleLength "."
    let EmptySolution = EmptyPuzzle

    let emptySolveProfile =
        { TechniqueCounts = Map.empty<HintTechnique, int>
          TotalSteps = 0
          EliminationSteps = 0
          PointingClaimingSteps = 0
          NakedSubsetSteps = 0
          HiddenSingleSteps = 0
          FishSteps = 0
          MaxEliminationChain = 0
          CurrentEliminationChain = 0
          InterestScore = 0
          TraceReversed = [] }

    let private isPuzzleChar c =
        c = '.' || c >= '1' && c <= '9'

    let private isSolutionChar c =
        c >= '1' && c <= '9'

    let isValidPuzzleString (puzzle : string) =
        not (isNull puzzle) &&
        puzzle.Length = PuzzleLength &&
        puzzle |> Seq.forall isPuzzleChar

    let isValidSolutionString (solution : string) =
        not (isNull solution) &&
        solution.Length = PuzzleLength &&
        solution |> Seq.forall isSolutionChar

    let givenCount (puzzle : string) =
        puzzle |> Seq.filter (fun c -> c <> '.') |> Seq.length

    let removedCellCount (puzzle : string) =
        puzzle |> Seq.filter ((=) '.') |> Seq.length

    let gridFromPuzzleString (puzzle : string) =
        let grid = Array2D.zeroCreate<int> 9 9
        if not (isNull puzzle) then
            puzzle
            |> Seq.truncate PuzzleLength
            |> Seq.iteri (fun i c ->
                let y = i / 9
                let x = i % 9
                grid[y, x] <- if c = '.' then 0 else int c - int '0')
        grid

    let givenFromPuzzleString (puzzle : string) =
        let given = Array2D.create 9 9 false
        if not (isNull puzzle) then
            puzzle
            |> Seq.truncate PuzzleLength
            |> Seq.iteri (fun i c ->
                let y = i / 9
                let x = i % 9
                given[y, x] <- c <> '.')
        given

    let puzzleStringFromGrid (grid : int[,]) =
        [|for y in 0 .. 8 do
            for x in 0 .. 8 do
                let value = grid[y, x]
                yield if value = 0 then '.' else char (int '0' + value)|]
        |> String

    let solutionStringFromGrid (grid : int[,]) =
        [|for y in 0 .. 8 do
            for x in 0 .. 8 do
                let value = grid[y, x]
                yield if value = 0 then '.' else char (int '0' + value)|]
        |> String

    let puzzleMatchesSolution (puzzle : string) (solution : string) =
        isValidPuzzleString puzzle &&
        isValidSolutionString solution &&
        Seq.forall2 (fun puzzleChar solutionChar -> puzzleChar = '.' || puzzleChar = solutionChar) puzzle solution

    let private permutations3 =
        [|[|0; 1; 2|]
          [|0; 2; 1|]
          [|1; 0; 2|]
          [|1; 2; 0|]
          [|2; 0; 1|]
          [|2; 1; 0|]|]

    let private makePermutations9 () =
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

    let private rowPermutations = makePermutations9 ()
    let private columnPermutations = rowPermutations
    let private canonicalKeyLock = obj ()
    let private canonicalKeyCache = Dictionary<string, string> ()

    let private computeCanonicalKey (value : string) =
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
            lock canonicalKeyLock (fun () ->
                match canonicalKeyCache.TryGetValue value with
                | true, key -> Some key
                | false, _ -> None)
            |> function
                | Some key -> key
                | None ->
                    let key = computeCanonicalKey value
                    lock canonicalKeyLock (fun () ->
                        if not (canonicalKeyCache.ContainsKey value) then
                            canonicalKeyCache.Add (value, key))
                    key

    let techniqueRank technique =
        match technique with
        | PencilMarkCorrection -> -1
        | FullHouseRow | FullHouseColumn | FullHouseBlock -> 0
        | NakedSingle -> 1
        | HiddenSingleRow | HiddenSingleColumn | HiddenSingleBlock -> 2
        | NakedPairRow | NakedPairColumn | NakedPairBlock -> 3
        | HiddenPairRow | HiddenPairColumn | HiddenPairBlock -> 4
        | NakedTripleRow | NakedTripleColumn | NakedTripleBlock -> 5
        | HiddenTripleRow | HiddenTripleColumn | HiddenTripleBlock -> 6
        | PointingRow | PointingColumn | ClaimingRow | ClaimingColumn -> 7
        | XWingRow | XWingColumn | SwordfishRow | SwordfishColumn -> 8

    let isPointingOrClaiming technique =
        match technique with
        | PointingRow | PointingColumn | ClaimingRow | ClaimingColumn -> true
        | _ -> false

    let isNakedSubset technique =
        match technique with
        | NakedPairRow | NakedPairColumn | NakedPairBlock
        | HiddenPairRow | HiddenPairColumn | HiddenPairBlock
        | NakedTripleRow | NakedTripleColumn | NakedTripleBlock
        | HiddenTripleRow | HiddenTripleColumn | HiddenTripleBlock -> true
        | _ -> false

    let isHiddenSingle technique =
        match technique with
        | HiddenSingleRow | HiddenSingleColumn | HiddenSingleBlock -> true
        | _ -> false

    let isFish technique =
        match technique with
        | XWingRow | XWingColumn | SwordfishRow | SwordfishColumn -> true
        | _ -> false

    let removedMarkCount hint =
        match hint.Action with
        | RemoveMarks removals -> removals |> List.sumBy (fun (_, marks) -> Set.count marks)
        | PlaceNumber _ -> 0
        | CorrectMarks _ -> 0

    let private incrementTechniqueCount technique counts =
        let count =
            match Map.tryFind technique counts with
            | Some count -> count
            | None -> 0
        Map.add technique (count + 1) counts

    let private removalGoals hint =
        match hint.Action with
        | RemoveMarks removals ->
            [for (position, numbers) in removals do
                for number in numbers -> (position, number)]
            |> Set.ofList
        | PlaceNumber _ -> Set.empty<(Vector2i * int)>
        | CorrectMarks _ -> Set.empty<(Vector2i * int)>

    let private placementGoal hint =
        match hint.Action with
        | PlaceNumber (position, number) -> Some (position, number)
        | RemoveMarks _ -> None
        | CorrectMarks _ -> None

    let private hasSimplerPlacementGoal target number simplerHints =
        simplerHints
        |> List.exists (fun hint ->
            match placementGoal hint with
            | Some (position, number2) -> position = target && number2 = number
            | None -> false)

    let private simplerRemovalGoals hint allHints =
        let rank = techniqueRank hint.Technique
        allHints
        |> List.filter (fun hint2 -> techniqueRank hint2.Technique < rank)
        |> List.fold (fun goals hint2 -> Set.union goals (removalGoals hint2)) (Set.empty<(Vector2i * int)>)

    let nextEliminationChain (hint : Hint) (profile : SolveProfile) =
        match hint.Action with
        | RemoveMarks _ -> profile.CurrentEliminationChain + 1
        | PlaceNumber _ -> 0
        | CorrectMarks _ -> 0

    let scoreSimpleHint (hint : Hint) (nextEliminationChain : int) =
        match hint.Action with
        | CorrectMarks _ -> 0
        | RemoveMarks _ when isPointingOrClaiming hint.Technique ->
            90 + removedMarkCount hint * 6 + nextEliminationChain * 16
        | RemoveMarks _ when isNakedSubset hint.Technique ->
            45 + removedMarkCount hint * 4 + nextEliminationChain * 10
        | RemoveMarks _ when isFish hint.Technique ->
            -250
        | RemoveMarks _ ->
            12 + removedMarkCount hint * 2 + nextEliminationChain * 4
        | PlaceNumber _ when isHiddenSingle hint.Technique ->
            -8
        | PlaceNumber _ ->
            1

    let scoreGenerationHint (profile : SolveProfile) (allHints : Hint list) (hint : Hint) =
        let nextChain = nextEliminationChain hint profile
        match hint.Action with
        | CorrectMarks _ -> 0
        | RemoveMarks _ ->
            let novelGoals = Set.difference (removalGoals hint) (simplerRemovalGoals hint allHints)
            let novelCount = Set.count novelGoals
            if novelCount = 0 then -100000
            elif isPointingOrClaiming hint.Technique then
                2000 + novelCount * 90 + nextChain * 120
            elif isNakedSubset hint.Technique then
                700 + novelCount * 50 + nextChain * 60
            elif isFish hint.Technique then
                -10000
            else
                scoreSimpleHint hint nextChain
        | PlaceNumber (target, number) ->
            let rank = techniqueRank hint.Technique
            let simplerHints =
                allHints
                |> List.filter (fun hint2 -> techniqueRank hint2.Technique < rank)
            if hasSimplerPlacementGoal target number simplerHints then -100000
            elif isHiddenSingle hint.Technique then -45
            elif hint.Technique = NakedSingle then -20
            else -12

    let selectSimpleFirst allowFish (profile : SolveProfile) (state : SudokuBoardState) =
        match SudokuHints.findFirst allowFish state with
        | Some hint ->
            Some
                { Hint = hint
                  Score = scoreSimpleHint hint (nextEliminationChain hint profile) }
        | None -> None

    let selectHardFirst allowFish (profile : SolveProfile) (state : SudokuBoardState) =
        let allHints = SudokuHints.findAll allowFish state
        let selections =
            allHints
            |> List.map (fun hint ->
                { Hint = hint
                  Score = scoreGenerationHint profile allHints hint })
            |> List.filter (fun selection -> selection.Score > -100000)
        selections
        |> List.sortByDescending (fun selection ->
            (selection.Score, techniqueRank selection.Hint.Technique, removedMarkCount selection.Hint))
        |> List.tryHead

    let updateSolveProfile (selection : HintSelection) (profile : SolveProfile) =
        let hint = selection.Hint
        let isElimination =
            match hint.Action with
            | RemoveMarks _ -> true
            | PlaceNumber _ -> false
            | CorrectMarks _ -> false
        let currentEliminationChain =
            if isElimination then profile.CurrentEliminationChain + 1
            else 0
        let interestScore = profile.InterestScore + selection.Score
        let solveStep =
            { StepIndex = profile.TotalSteps + 1
              Hint = hint
              Score = selection.Score
              CumulativeScore = interestScore
              EliminationChain = currentEliminationChain
              RemovedMarkCount = removedMarkCount hint }
        { profile with
            TechniqueCounts = incrementTechniqueCount hint.Technique profile.TechniqueCounts
            TotalSteps = profile.TotalSteps + 1
            EliminationSteps = profile.EliminationSteps + (if isElimination then 1 else 0)
            PointingClaimingSteps = profile.PointingClaimingSteps + (if isPointingOrClaiming hint.Technique then 1 else 0)
            NakedSubsetSteps = profile.NakedSubsetSteps + (if isNakedSubset hint.Technique then 1 else 0)
            HiddenSingleSteps = profile.HiddenSingleSteps + (if isHiddenSingle hint.Technique then 1 else 0)
            FishSteps = profile.FishSteps + (if isFish hint.Technique then 1 else 0)
            MaxEliminationChain = max profile.MaxEliminationChain currentEliminationChain
            CurrentEliminationChain = currentEliminationChain
            InterestScore = interestScore
            TraceReversed = solveStep :: profile.TraceReversed }

    let solveWithSelector
        (selector : SolveProfile -> SudokuBoardState -> HintSelection option)
        (solution : int[,])
        (puzzle : int[,])
        (given : bool[,]) =
        let state : SudokuBoardState =
            { Puzzle = Array2D.copy puzzle
              Solution = solution
              Given = Array2D.copy given
              Marks = SudokuGrid.makeMarks () }
            |> SudokuGrid.fillLegalMarks
        let rec solve (stepsRemaining : int) (profile : SolveProfile) (state : SudokuBoardState) =
            if SudokuGrid.isPuzzleSolvedAgainstSolution solution state.Puzzle then Some profile
            elif stepsRemaining <= 0 then None
            else
                match selector profile state with
                | Some selection ->
                    let state = SudokuGrid.applyHint selection.Hint state
                    let profile = updateSolveProfile selection profile
                    solve (stepsRemaining - 1) profile state
                | None ->
                    if SudokuGrid.hasOpenCellsWithoutMarks state then
                        solve (stepsRemaining - 1) profile (SudokuGrid.fillLegalMarks state)
                    else None
        solve 1000 emptySolveProfile state

    let solveSimpleFirst allowFish (solution : int[,]) (puzzle : int[,]) (given : bool[,]) =
        solveWithSelector (selectSimpleFirst allowFish) solution puzzle given

    let solveHardFirst allowFish (solution : int[,]) (puzzle : int[,]) (given : bool[,]) =
        solveWithSelector (selectHardFirst allowFish) solution puzzle given

    let solveSimpleFirstFromStrings allowFish (solution : string) (puzzle : string) =
        let solutionGrid = gridFromPuzzleString solution
        let puzzleGrid = gridFromPuzzleString puzzle
        let given = givenFromPuzzleString puzzle
        solveSimpleFirst allowFish solutionGrid puzzleGrid given

    let opportunityScore (profileOpt : SolveProfile option) =
        match profileOpt with
        | Some profile ->
            profile.PointingClaimingSteps * 50 +
            profile.NakedSubsetSteps * 30 +
            profile.MaxEliminationChain * 20 +
            profile.EliminationSteps * 10
        | None -> -1000

    let profileToPuzzle
        (number : int)
        (puzzle : string)
        (solution : string)
        (removedCells : int)
        (generationScore : int)
        (opportunityScoreOpt : int option)
        (profile : SolveProfile) =
        { Number = number
          Puzzle = puzzle
          Solution = solution
          PuzzleKey = canonicalKey puzzle
          SolutionKey = canonicalKey solution
          TechniqueCounts = profile.TechniqueCounts
          TotalSteps = profile.TotalSteps
          EliminationSteps = profile.EliminationSteps
          PointingClaimingSteps = profile.PointingClaimingSteps
          NakedSubsetSteps = profile.NakedSubsetSteps
          HiddenSingleSteps = profile.HiddenSingleSteps
          FishSteps = profile.FishSteps
          MaxEliminationChain = profile.MaxEliminationChain
          RemovedCells = removedCells
          GenerationScore = generationScore
          OpportunityScoreOpt = opportunityScoreOpt }

    let techniqueSignatureKey (counts : Map<HintTechnique, int>) =
        let techniques =
            counts
            |> Map.toList
            |> List.filter (fun (_, count) -> count > 0)
            |> List.sortBy (fun (technique, _) -> techniqueRank technique, string technique)
            |> List.map (fun (technique, _) -> string technique)
        match techniques with
        | [] -> "None"
        | _ -> String.concat "+" techniques

    let difficultyOfEntry (entry : GeneratedPuzzle) =
        if entry.FishSteps > 0 || entry.MaxEliminationChain >= 4 || entry.EliminationSteps >= 10 then Hard
        elif entry.EliminationSteps > 0 then Normal
        elif entry.HiddenSingleSteps > 0 then Easy
        else Trivial

    let private blockIndex i =
        (i / 27) * 3 + (i % 9) / 3

    let solveExact (puzzle : string) =
        if not (isValidPuzzleString puzzle) then ExactFailed "Invalid puzzle string."
        else
            let grid =
                puzzle.ToCharArray ()
                |> Array.map (fun c -> if c = '.' then 0 else int c - int '0')
            let rows = Array.zeroCreate<int> 9
            let columns = Array.zeroCreate<int> 9
            let blocks = Array.zeroCreate<int> 9
            let mutable conflict = false
            for i in 0 .. PuzzleLength - 1 do
                let value = grid[i]
                if value <> 0 then
                    let bit = 1 <<< (value - 1)
                    let row = i / 9
                    let column = i % 9
                    let block = blockIndex i
                    if ((rows[row] ||| columns[column] ||| blocks[block]) &&& bit) <> 0 then conflict <- true
                    rows[row] <- rows[row] ||| bit
                    columns[column] <- columns[column] ||| bit
                    blocks[block] <- blocks[block] ||| bit
            if conflict then ExactFailed "Puzzle has conflicting givens."
            else
                let allMask = (1 <<< 9) - 1
                let mutable solutionCount = 0
                let mutable solutionOpt : int[] option = None
                let rec search () =
                    if solutionCount < 2 then
                        let mutable best = -1
                        let mutable bestMask = 0
                        let mutable bestCount = 10
                        let mutable impossible = false
                        for i in 0 .. PuzzleLength - 1 do
                            if grid[i] = 0 then
                                let row = i / 9
                                let column = i % 9
                                let block = blockIndex i
                                let mask = allMask &&& ~~~(rows[row] ||| columns[column] ||| blocks[block])
                                let count = int (BitOperations.PopCount (uint32 mask))
                                if count = 0 then impossible <- true
                                elif count < bestCount then
                                    best <- i
                                    bestMask <- mask
                                    bestCount <- count
                        if not impossible then
                            if best = -1 then
                                solutionCount <- solutionCount + 1
                                match solutionOpt with
                                | None -> solutionOpt <- Some (Array.copy grid)
                                | Some _ -> ()
                            else
                                let row = best / 9
                                let column = best % 9
                                let block = blockIndex best
                                for value in 1 .. 9 do
                                    let bit = 1 <<< (value - 1)
                                    if solutionCount < 2 && (bestMask &&& bit) <> 0 then
                                        grid[best] <- value
                                        rows[row] <- rows[row] ||| bit
                                        columns[column] <- columns[column] ||| bit
                                        blocks[block] <- blocks[block] ||| bit
                                        search ()
                                        blocks[block] <- blocks[block] &&& ~~~bit
                                        columns[column] <- columns[column] &&& ~~~bit
                                        rows[row] <- rows[row] &&& ~~~bit
                                        grid[best] <- 0
                search ()
                match solutionCount, solutionOpt with
                | 1, Some solution ->
                    let solution =
                        solution
                        |> Array.map (fun value -> char (int '0' + value))
                        |> String
                    ExactSolved solution
                | 0, _ -> ExactFailed "Puzzle has no solution."
                | _ -> ExactFailed "Puzzle has multiple solutions."
