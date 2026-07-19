namespace Sudoku
open System.Numerics

[<RequireQualifiedAccess>]
module PuzzleGeneration =

    type private PuzzleCandidate =
        { Removed : int
          Solution : int[,]
          Puzzle : int[,]
          Given : bool[,]
          Profile : PuzzleAnalysis.SolveProfile
          OpportunityProfile : PuzzleAnalysis.SolveProfile option }

    let private buildPuzzleFromSolution (difficulty : Difficulty) (solution : int[,]) =
        let puzzle = Array2D.copy solution
        let given = Array2D.create 9 9 true
        let targetReached (removed : int) = removed >= difficulty.Holes

        let rec removeCellsInPass
            (removed : int)
            (profile : PuzzleAnalysis.SolveProfile)
            (acceptedInPass : int)
            (positions : Vector2i list) =
            if targetReached removed then (removed, profile, acceptedInPass)
            else
                match positions with
                | [] -> (removed, profile, acceptedInPass)
                | position :: remaining ->
                    if given[position.Y, position.X] then
                        let value = puzzle[position.Y, position.X]
                        puzzle[position.Y, position.X] <- 0
                        given[position.Y, position.X] <- false
                        match PuzzleAnalysis.solveSimpleFirst solution puzzle given with
                        | Some profile -> removeCellsInPass (removed + 1) profile (acceptedInPass + 1) remaining
                        | None ->
                            puzzle[position.Y, position.X] <- value
                            given[position.Y, position.X] <- true
                            removeCellsInPass removed profile acceptedInPass remaining
                    else removeCellsInPass removed profile acceptedInPass remaining

        let rec removeUntilDone (removed : int) (profile : PuzzleAnalysis.SolveProfile) =
            let positions = SudokuGrid.shuffle SudokuPuzzleDisplay.allPositions
            let removed, profile, acceptedInPass = removeCellsInPass removed profile 0 positions
            if targetReached removed || acceptedInPass = 0 then (removed, Array2D.copy puzzle, Array2D.copy given, profile)
            else removeUntilDone removed profile


        removeUntilDone 0 PuzzleAnalysis.emptySolveProfile

    let private placementStepCount (profile : PuzzleAnalysis.SolveProfile) =
        profile.TotalSteps - profile.EliminationSteps

    let private liveComplexityTier (profile : PuzzleAnalysis.SolveProfile) =
        if profile.PointingClaimingSteps > 0 then 2
        elif profile.NakedSubsetSteps > 0 || profile.MaxEliminationChain >= 2 then 1
        else 0

    let private scoreLiveProfile (difficulty : Difficulty) (removed : int) (profile : PuzzleAnalysis.SolveProfile) =
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

    let private withLiveGenerationScore (difficulty : Difficulty) (removed : int) (profile : PuzzleAnalysis.SolveProfile) =
        { profile with InterestScore = scoreLiveProfile difficulty removed profile }

    let private makeOpportunityProfile (difficulty : Difficulty) (solution : int[,]) (puzzle : int[,]) (given : bool[,]) =
        match difficulty with
        | Hard -> PuzzleAnalysis.solveHardFirst solution puzzle given
        | _ -> None

    let private generationSampleCount (difficulty : Difficulty) =
        match difficulty with
        | Trivial -> 1
        | Easy -> 4
        | Normal -> 8
        | Hard -> 3

    let private hasReachedHoleTarget (difficulty : Difficulty) (removed : int) =
        removed >= difficulty.Holes

    let private isBetterPuzzleCandidate (difficulty : Difficulty) (candidate : PuzzleCandidate) (current : PuzzleCandidate) =
        let candidateComplete = hasReachedHoleTarget difficulty candidate.Removed
        let currentComplete = hasReachedHoleTarget difficulty current.Removed
        if candidateComplete <> currentComplete then candidateComplete
        else
            match difficulty with
            | Hard ->
                let candidateTier = liveComplexityTier candidate.Profile
                let currentTier = liveComplexityTier current.Profile
                let candidateOpportunityScore = PuzzleAnalysis.opportunityScore candidate.OpportunityProfile
                let currentOpportunityScore = PuzzleAnalysis.opportunityScore current.OpportunityProfile
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

    let private toSudokuPuzzle (candidate : PuzzleCandidate) =
        let puzzle = SudokuPuzzleInternals.puzzleStringFromGrid candidate.Puzzle
        let solution = SudokuPuzzleInternals.puzzleStringFromGrid candidate.Solution
        let opportunityScoreOpt = candidate.OpportunityProfile |> Option.map (fun profile -> PuzzleAnalysis.opportunityScore (Some profile))
        PuzzleAnalysis.profileToPuzzle 0 puzzle solution candidate.Removed candidate.Profile.InterestScore opportunityScoreOpt candidate.Profile

    let make (difficulty : Difficulty) =
        let rec tryMake samplesRemaining bestOpt =
            let solution = SudokuGrid.makeSolvedBoard ()
            let removed, puzzle, given, profile = buildPuzzleFromSolution difficulty solution
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
                | Some candidate -> toSudokuPuzzle candidate
                | None ->
                    let solution = SudokuGrid.makeSolvedBoard ()
                    let puzzle = Array2D.copy solution
                    let given = SudokuPuzzleDisplay.givenFromPuzzle puzzle
                    { Removed = 0
                      Solution = solution
                      Puzzle = puzzle
                      Given = given
                      Profile = PuzzleAnalysis.emptySolveProfile
                      OpportunityProfile = None }
                    |> toSudokuPuzzle
        tryMake (generationSampleCount difficulty) None