namespace Sudoku
open System.Numerics
open Prime

[<RequireQualifiedAccess>]
module PuzzleGeneration =

    type private SolveProfile =
        { TechniqueCounts : Map<HintTechnique, int>
          TotalSteps : int
          EliminationSteps : int
          PointingClaimingSteps : int
          NakedSubsetSteps : int
          HiddenSingleSteps : int
          FishSteps : int
          MaxEliminationChain : int
          CurrentEliminationChain : int
          InterestScore : int }

    type private HintSelection =
        { Hint : Hint
          Score : int }

    let private emptySolveProfile =
        { TechniqueCounts = Map.empty<HintTechnique, int>
          TotalSteps = 0
          EliminationSteps = 0
          PointingClaimingSteps = 0
          NakedSubsetSteps = 0
          HiddenSingleSteps = 0
          FishSteps = 0
          MaxEliminationChain = 0
          CurrentEliminationChain = 0
          InterestScore = 0 }

    let private incrementTechniqueCount (technique : HintTechnique) (counts : Map<HintTechnique, int>) =
        let count =
            match Map.tryFind technique counts with
            | Some count -> count
            | None -> 0
        Map.add technique (count + 1) counts

    let private isPointingOrClaiming (technique : HintTechnique) =
        match technique with
        | PointingRow | PointingColumn | ClaimingRow | ClaimingColumn -> true
        | _ -> false

    let private isNakedSubset (technique : HintTechnique) =
        match technique with
        | NakedPairRow | NakedPairColumn | NakedPairBlock
        | NakedTripleRow | NakedTripleColumn | NakedTripleBlock -> true
        | _ -> false

    let private isHiddenSingle (technique : HintTechnique) =
        match technique with
        | HiddenSingleRow | HiddenSingleColumn | HiddenSingleBlock -> true
        | _ -> false

    let private isFish (technique : HintTechnique) =
        match technique with
        | XWingRow | XWingColumn | SwordfishRow | SwordfishColumn -> true
        | _ -> false

    let private techniqueRank (technique : HintTechnique) =
        match technique with
        | FullHouseRow | FullHouseColumn | FullHouseBlock -> 0
        | NakedSingle -> 1
        | HiddenSingleRow | HiddenSingleColumn | HiddenSingleBlock -> 2
        | NakedPairRow | NakedPairColumn | NakedPairBlock -> 3
        | NakedTripleRow | NakedTripleColumn | NakedTripleBlock -> 4
        | PointingRow | PointingColumn | ClaimingRow | ClaimingColumn -> 5
        | XWingRow | XWingColumn | SwordfishRow | SwordfishColumn -> 6

    let private removedMarkCount (hint : Hint) =
        match hint.Action with
        | RemoveMarks removals -> removals |> List.sumBy (fun (_, marks) -> Set.count marks)
        | PlaceNumber _ -> 0

    let private removalGoals (hint : Hint) =
        match hint.Action with
        | RemoveMarks removals ->
            [for (position, numbers) in removals do
                for number in numbers -> (position, number)]
            |> Set.ofList
        | PlaceNumber _ -> Set.empty<(Vector2i * int)>

    let private placementGoal (hint : Hint) =
        match hint.Action with
        | PlaceNumber (position, number) -> Some (position, number)
        | RemoveMarks _ -> None

    let private hasSimplerPlacementGoal (target : Vector2i) (number : int) (simplerHints : Hint list) =
        simplerHints
        |> List.exists (fun hint ->
            match placementGoal hint with
            | Some (position, number2) -> position = target && number2 = number
            | None -> false)

    let private simplerRemovalGoals (hint : Hint) (allHints : Hint list) =
        let rank = techniqueRank hint.Technique
        allHints
        |> List.filter (fun hint2 -> techniqueRank hint2.Technique < rank)
        |> List.fold (fun goals hint2 -> Set.union goals (removalGoals hint2)) (Set.empty<(Vector2i * int)>)

    let private nextEliminationChain (hint : Hint) (profile : SolveProfile) =
        match hint.Action with
        | RemoveMarks _ -> profile.CurrentEliminationChain + 1
        | PlaceNumber _ -> 0

    let private scoreSimpleHint (hint : Hint) (nextEliminationChain : int) =
        match hint.Action with
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

    let private scoreGenerationHint (profile : SolveProfile) (allHints : Hint list) (hint : Hint) =
        let nextChain = nextEliminationChain hint profile
        match hint.Action with
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

    let private selectSimpleFirstNoFish (profile : SolveProfile) (state : SudokuBoardState) =
        match SudokuHints.findFirst false state with
        | Some hint ->
            Some
                { Hint = hint
                  Score = scoreSimpleHint hint (nextEliminationChain hint profile) }
        | None -> None

    let private selectHardFirstNoFish (profile : SolveProfile) (state : SudokuBoardState) =
        let allHints = SudokuHints.findAll false state
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

    let private updateSolveProfile (selection : HintSelection) (profile : SolveProfile) =
        let hint = selection.Hint
        let isElimination =
            match hint.Action with
            | RemoveMarks _ -> true
            | PlaceNumber _ -> false
        let currentEliminationChain =
            if isElimination then profile.CurrentEliminationChain + 1
            else 0
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
            InterestScore = profile.InterestScore + selection.Score }

    let private solveWithSelector (selector : SolveProfile -> SudokuBoardState -> HintSelection option) (solution : int[,]) (puzzle : int[,]) (given : bool[,]) =
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

    let private solveSimpleFirstNoFish (solution : int[,]) (puzzle : int[,]) (given : bool[,]) =
        solveWithSelector selectSimpleFirstNoFish solution puzzle given

    let private solveHardFirstNoFish (solution : int[,]) (puzzle : int[,]) (given : bool[,]) =
        solveWithSelector selectHardFirstNoFish solution puzzle given

    let private buildPuzzleFromSolution (difficulty : Difficulty) (solution : int[,]) =
        let puzzle = Array2D.copy solution
        let given = Array2D.create 9 9 true
        let holes = SudokuGrid.shuffle SudokuGrid.allPositions
        let targetReached (removed : int) =
            match difficulty.TargetHoles with
            | Some holes -> removed >= holes
            | None -> false
        let rec removeCells (removed : int) (profile : SolveProfile) (positions : Vector2i list) =
            if targetReached removed then (removed, Array2D.copy puzzle, Array2D.copy given, profile)
            else
                match positions with
                | [] -> (removed, Array2D.copy puzzle, Array2D.copy given, profile)
                | position :: remaining ->
                    let value = puzzle[position.Y, position.X]
                    puzzle[position.Y, position.X] <- 0
                    given[position.Y, position.X] <- false
                    match solveHardFirstNoFish solution puzzle given with
                    | Some profile -> removeCells (removed + 1) profile remaining
                    | None ->
                        puzzle[position.Y, position.X] <- value
                        given[position.Y, position.X] <- true
                        removeCells removed profile remaining
        removeCells 0 emptySolveProfile holes

    let private withSimpleFirstPenalty (solution : int[,]) (puzzle : int[,]) (given : bool[,]) (profile : SolveProfile) =
        match solveSimpleFirstNoFish solution puzzle given with
        | Some simpleProfile ->
            let placementSteps = simpleProfile.TotalSteps - simpleProfile.EliminationSteps
            let noPointingPenalty = if simpleProfile.PointingClaimingSteps = 0 then 500 else 0
            let shortChainPenalty = if simpleProfile.MaxEliminationChain < 2 then 250 else 0
            let penalty = simpleProfile.HiddenSingleSteps * 18 + placementSteps * 3 + noPointingPenalty + shortChainPenalty
            { profile with InterestScore = profile.InterestScore - penalty }
        | None -> profile

    let private generationSampleCount (difficulty : Difficulty) =
        match difficulty with
        | Trivial -> 1
        | Easy -> 4
        | Normal -> 8
        | Hard -> 6

    let private hasReachedHoleTarget (difficulty : Difficulty) (removed : int) =
        match difficulty.TargetHoles with
        | Some holes -> removed >= holes
        | None -> false

    let private isBetterPuzzleCandidate (difficulty : Difficulty) (candidate : int * int[,] * int[,] * bool[,] * SolveProfile) (current : int * int[,] * int[,] * bool[,] * SolveProfile) =
        let (candidateRemoved, _, _, _, candidateProfile) = candidate
        let (currentRemoved, _, _, _, currentProfile) = current
        let candidateComplete = hasReachedHoleTarget difficulty candidateRemoved
        let currentComplete = hasReachedHoleTarget difficulty currentRemoved
        if candidateComplete <> currentComplete then candidateComplete
        elif candidateProfile.InterestScore <> currentProfile.InterestScore then candidateProfile.InterestScore > currentProfile.InterestScore
        elif candidateProfile.PointingClaimingSteps <> currentProfile.PointingClaimingSteps then candidateProfile.PointingClaimingSteps > currentProfile.PointingClaimingSteps
        elif candidateProfile.MaxEliminationChain <> currentProfile.MaxEliminationChain then candidateProfile.MaxEliminationChain > currentProfile.MaxEliminationChain
        elif candidateRemoved <> currentRemoved then candidateRemoved > currentRemoved
        else candidateProfile.HiddenSingleSteps < currentProfile.HiddenSingleSteps

    let private toGeneratedPuzzle (solution : int[,]) (puzzle : int[,]) (given : bool[,]) (profile : SolveProfile) =
        { Solution = solution
          Puzzle = puzzle
          Given = given
          TechniqueCounts = profile.TechniqueCounts
          GenerationScore = profile.InterestScore
          MaxEliminationChain = profile.MaxEliminationChain }

    let make (difficulty : Difficulty) =
        let rec tryMake (samplesRemaining : int) (bestOpt : (int * int[,] * int[,] * bool[,] * SolveProfile) option) =
            let solution = SudokuGrid.makeSolvedBoard ()
            let (removed, puzzle, given, profile) = buildPuzzleFromSolution difficulty solution
            let profile = withSimpleFirstPenalty solution puzzle given profile
            let candidate = (removed, solution, puzzle, given, profile)
            let bestOpt =
                match bestOpt with
                | Some best when isBetterPuzzleCandidate difficulty candidate best -> Some candidate
                | Some _ -> bestOpt
                | None -> Some candidate
            if samplesRemaining > 1 then tryMake (samplesRemaining - 1) bestOpt
            else
                match bestOpt with
                | Some (_, solution, puzzle, given, profile) -> toGeneratedPuzzle solution puzzle given profile
                | None ->
                    let solution = SudokuGrid.makeSolvedBoard ()
                    let puzzle = Array2D.copy solution
                    let given = SudokuGrid.givenFromPuzzle puzzle
                    toGeneratedPuzzle solution puzzle given emptySolveProfile
        tryMake (generationSampleCount difficulty) None
