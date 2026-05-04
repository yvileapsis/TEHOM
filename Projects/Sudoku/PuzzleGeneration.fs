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
          InterestScore : int
          TraceReversed : PuzzleSolveStep list }

    type private HintSelection =
        { Hint : Hint
          Score : int }

    type private PuzzleCandidate =
        { Removed : int
          Solution : int[,]
          Puzzle : int[,]
          Given : bool[,]
          Profile : SolveProfile
          OpportunityProfile : SolveProfile option }

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
          InterestScore = 0
          TraceReversed = [] }

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
        | HiddenPairRow | HiddenPairColumn | HiddenPairBlock
        | NakedTripleRow | NakedTripleColumn | NakedTripleBlock
        | HiddenTripleRow | HiddenTripleColumn | HiddenTripleBlock -> true
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

    let private removedMarkCount (hint : Hint) =
        match hint.Action with
        | RemoveMarks removals -> removals |> List.sumBy (fun (_, marks) -> Set.count marks)
        | PlaceNumber _ -> 0
        | CorrectMarks _ -> 0

    let private removalGoals (hint : Hint) =
        match hint.Action with
        | RemoveMarks removals ->
            [for (position, numbers) in removals do
                for number in numbers -> (position, number)]
            |> Set.ofList
        | PlaceNumber _ -> Set.empty<(Vector2i * int)>
        | CorrectMarks _ -> Set.empty<(Vector2i * int)>

    let private placementGoal (hint : Hint) =
        match hint.Action with
        | PlaceNumber (position, number) -> Some (position, number)
        | RemoveMarks _ -> None
        | CorrectMarks _ -> None

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
        | CorrectMarks _ -> 0

    let private scoreSimpleHint (hint : Hint) (nextEliminationChain : int) =
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

    let private scoreGenerationHint (profile : SolveProfile) (allHints : Hint list) (hint : Hint) =
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
        let targetReached (removed : int) =
            match difficulty.TargetHoles with
            | Some holes -> removed >= holes
            | None -> false

        let rec removeCellsInPass (removed : int) (profile : SolveProfile) (acceptedInPass : int) (positions : Vector2i list) =
            if targetReached removed then (removed, profile, acceptedInPass)
            else
                match positions with
                | [] -> (removed, profile, acceptedInPass)
                | position :: remaining ->
                    if given[position.Y, position.X] then
                        let value = puzzle[position.Y, position.X]
                        puzzle[position.Y, position.X] <- 0
                        given[position.Y, position.X] <- false
                        match solveSimpleFirstNoFish solution puzzle given with
                        | Some profile -> removeCellsInPass (removed + 1) profile (acceptedInPass + 1) remaining
                        | None ->
                            puzzle[position.Y, position.X] <- value
                            given[position.Y, position.X] <- true
                            removeCellsInPass removed profile acceptedInPass remaining
                    else removeCellsInPass removed profile acceptedInPass remaining

        let rec removeUntilDone (removed : int) (profile : SolveProfile) =
            let positions = SudokuGrid.shuffle SudokuGrid.allPositions
            let (removed, profile, acceptedInPass) = removeCellsInPass removed profile 0 positions
            match difficulty.TargetHoles with
            | Some _ ->
                if targetReached removed || acceptedInPass = 0 then (removed, Array2D.copy puzzle, Array2D.copy given, profile)
                else removeUntilDone removed profile
            | None ->
                if acceptedInPass > 0 then removeUntilDone removed profile
                else (removed, Array2D.copy puzzle, Array2D.copy given, profile)

        removeUntilDone 0 emptySolveProfile

    let private placementStepCount (profile : SolveProfile) =
        profile.TotalSteps - profile.EliminationSteps

    let private liveComplexityTier (profile : SolveProfile) =
        if profile.PointingClaimingSteps > 0 then 2
        elif profile.NakedSubsetSteps > 0 || profile.MaxEliminationChain >= 2 then 1
        else 0

    let private scoreLiveProfile (difficulty : Difficulty) (removed : int) (profile : SolveProfile) =
        let placementSteps = placementStepCount profile
        let complexityScore =
            profile.PointingClaimingSteps * 1400 +
            profile.NakedSubsetSteps * 650 +
            profile.MaxEliminationChain * 300 +
            profile.EliminationSteps * 80
        let hiddenSinglePenalty =
            match difficulty with
            | Hard -> profile.HiddenSingleSteps * 42
            | _ -> profile.HiddenSingleSteps * 18
        let placementPenalty =
            match difficulty with
            | Hard -> placementSteps * 8
            | _ -> placementSteps * 3
        let simplePenalty = hiddenSinglePenalty + placementPenalty
        let emptyEliminationPenalty =
            if profile.EliminationSteps = 0 then
                match difficulty with
                | Hard -> 1500
                | _ -> 250
            else 0
        let noComplexityPenalty =
            match difficulty with
            | Hard when liveComplexityTier profile = 0 -> 1800
            | _ -> 0
        let shortChainPenalty =
            match difficulty with
            | Hard when profile.MaxEliminationChain < 2 -> 700
            | _ -> 0
        let removedScore =
            match difficulty with
            | Hard -> removed * 2
            | _ -> removed * 20
        complexityScore + removedScore - simplePenalty - emptyEliminationPenalty - noComplexityPenalty - shortChainPenalty

    let private withLiveGenerationScore (difficulty : Difficulty) (removed : int) (profile : SolveProfile) =
        { profile with InterestScore = scoreLiveProfile difficulty removed profile }

    let private makeOpportunityProfile (difficulty : Difficulty) (solution : int[,]) (puzzle : int[,]) (given : bool[,]) =
        match difficulty with
        | Hard -> solveHardFirstNoFish solution puzzle given
        | _ -> None

    let private opportunityScore (profileOpt : SolveProfile option) =
        match profileOpt with
        | Some profile ->
            profile.PointingClaimingSteps * 50 +
            profile.NakedSubsetSteps * 30 +
            profile.MaxEliminationChain * 20 +
            profile.EliminationSteps * 10
        | None -> -1000

    let private generationSampleCount (difficulty : Difficulty) =
        match difficulty with
        | Trivial -> 1
        | Easy -> 4
        | Normal -> 8
        | Hard -> 3

    let private hasReachedHoleTarget (difficulty : Difficulty) (removed : int) =
        match difficulty.TargetHoles with
        | Some holes -> removed >= holes
        | None -> false

    let private isBetterPuzzleCandidate (difficulty : Difficulty) (candidate : PuzzleCandidate) (current : PuzzleCandidate) =
        let candidateComplete = hasReachedHoleTarget difficulty candidate.Removed
        let currentComplete = hasReachedHoleTarget difficulty current.Removed
        if candidateComplete <> currentComplete then candidateComplete
        else
            match difficulty with
            | Hard ->
                let candidateTier = liveComplexityTier candidate.Profile
                let currentTier = liveComplexityTier current.Profile
                let candidateOpportunityScore = opportunityScore candidate.OpportunityProfile
                let currentOpportunityScore = opportunityScore current.OpportunityProfile
                if candidateTier <> currentTier then candidateTier > currentTier
                elif candidate.Profile.InterestScore <> current.Profile.InterestScore then candidate.Profile.InterestScore > current.Profile.InterestScore
                elif candidate.Profile.PointingClaimingSteps <> current.Profile.PointingClaimingSteps then candidate.Profile.PointingClaimingSteps > current.Profile.PointingClaimingSteps
                elif candidate.Profile.NakedSubsetSteps <> current.Profile.NakedSubsetSteps then candidate.Profile.NakedSubsetSteps > current.Profile.NakedSubsetSteps
                elif candidate.Profile.MaxEliminationChain <> current.Profile.MaxEliminationChain then candidate.Profile.MaxEliminationChain > current.Profile.MaxEliminationChain
                elif candidate.Profile.EliminationSteps <> current.Profile.EliminationSteps then candidate.Profile.EliminationSteps > current.Profile.EliminationSteps
                elif candidate.Profile.HiddenSingleSteps <> current.Profile.HiddenSingleSteps then candidate.Profile.HiddenSingleSteps < current.Profile.HiddenSingleSteps
                elif candidateOpportunityScore <> currentOpportunityScore then candidateOpportunityScore > currentOpportunityScore
                else candidate.Removed > current.Removed
            | _ ->
                if candidate.Profile.InterestScore <> current.Profile.InterestScore then candidate.Profile.InterestScore > current.Profile.InterestScore
                elif candidate.Profile.PointingClaimingSteps <> current.Profile.PointingClaimingSteps then candidate.Profile.PointingClaimingSteps > current.Profile.PointingClaimingSteps
                elif candidate.Profile.MaxEliminationChain <> current.Profile.MaxEliminationChain then candidate.Profile.MaxEliminationChain > current.Profile.MaxEliminationChain
                elif candidate.Removed <> current.Removed then candidate.Removed > current.Removed
                else candidate.Profile.HiddenSingleSteps < current.Profile.HiddenSingleSteps

    let private toPuzzleRanking (candidate : PuzzleCandidate) =
        { RemovedCells = candidate.Removed
          TechniqueCounts = candidate.Profile.TechniqueCounts
          TotalSteps = candidate.Profile.TotalSteps
          EliminationSteps = candidate.Profile.EliminationSteps
          PointingClaimingSteps = candidate.Profile.PointingClaimingSteps
          NakedSubsetSteps = candidate.Profile.NakedSubsetSteps
          HiddenSingleSteps = candidate.Profile.HiddenSingleSteps
          FishSteps = candidate.Profile.FishSteps
          MaxEliminationChain = candidate.Profile.MaxEliminationChain
          GenerationScore = candidate.Profile.InterestScore
          OpportunityScoreOpt = candidate.OpportunityProfile |> Option.map (fun profile -> opportunityScore (Some profile))
          SolveTrace = List.rev candidate.Profile.TraceReversed }

    let private toGeneratedPuzzle (candidate : PuzzleCandidate) : GeneratedPuzzle =
        let ranking = toPuzzleRanking candidate
        { Solution = candidate.Solution
          Puzzle = candidate.Puzzle
          Given = candidate.Given
          PuzzleNumberOpt = None
          TechniqueCounts = ranking.TechniqueCounts
          GenerationScore = ranking.GenerationScore
          MaxEliminationChain = ranking.MaxEliminationChain
          Ranking = ranking }

    let make (difficulty : Difficulty) =
        let rec tryMake (samplesRemaining : int) (bestOpt : PuzzleCandidate option) =
            let solution = SudokuGrid.makeSolvedBoard ()
            let (removed, puzzle, given, profile) = buildPuzzleFromSolution difficulty solution
            let profile = withLiveGenerationScore difficulty removed profile
            let candidate =
                { Removed = removed
                  Solution = solution
                  Puzzle = puzzle
                  Given = given
                  Profile = profile
                  OpportunityProfile = makeOpportunityProfile difficulty solution puzzle given }
            let bestOpt =
                match bestOpt with
                | Some best when isBetterPuzzleCandidate difficulty candidate best -> Some candidate
                | Some _ -> bestOpt
                | None -> Some candidate
            if samplesRemaining > 1 then tryMake (samplesRemaining - 1) bestOpt
            else
                match bestOpt with
                | Some candidate -> toGeneratedPuzzle candidate
                | None ->
                    let solution = SudokuGrid.makeSolvedBoard ()
                    let puzzle = Array2D.copy solution
                    let given = SudokuGrid.givenFromPuzzle puzzle
                    { Removed = 0
                      Solution = solution
                      Puzzle = puzzle
                      Given = given
                      Profile = emptySolveProfile
                      OpportunityProfile = None }
                    |> toGeneratedPuzzle
        tryMake (generationSampleCount difficulty) None
