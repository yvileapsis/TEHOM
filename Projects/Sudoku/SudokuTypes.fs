namespace Sudoku
open System.Numerics
open Prime

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

    member this.TargetHoles =
        match this with
        | Hard -> None
        | _ -> Some (this.Holes)

    member this.Label =
        match this with
        | Trivial -> "Trivial"
        | Easy -> "Easy"
        | Normal -> "Normal"
        | Hard -> "Hard"

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

// this contains the board data needed by hint and generation code.
type SudokuBoardState =
    { Puzzle : int[,]
      Solution : int[,]
      Given : bool[,]
      Marks : Set<int>[,] }

// this contains one selected solving step from a ranked solve pass.
type PuzzleSolveStep =
    { StepIndex : int
      Hint : Hint
      Score : int
      CumulativeScore : int
      EliminationChain : int
      RemovedMarkCount : int }

// this contains the aggregate ranking information produced while generating a puzzle.
type PuzzleRanking =
    { RemovedCells : int
      TechniqueCounts : Map<HintTechnique, int>
      TotalSteps : int
      EliminationSteps : int
      PointingClaimingSteps : int
      NakedSubsetSteps : int
      HiddenSingleSteps : int
      FishSteps : int
      MaxEliminationChain : int
      GenerationScore : int
      OpportunityScoreOpt : int option
      SolveTrace : PuzzleSolveStep list }

// this contains a generated puzzle and the solver profile selected for it.
type GeneratedPuzzle =
    { Solution : int[,]
      Puzzle : int[,]
      Given : bool[,]
      PuzzleNumberOpt : int option
      TechniqueCounts : Map<HintTechnique, int>
      GenerationScore : int
      MaxEliminationChain : int
      Ranking : PuzzleRanking }

// this contains one serialized puzzle bank entry. Grids are flattened row-major to keep the bank text simple.
type PuzzleBankEntry =
    { Number : int
      Puzzle : int list
      Solution : int list
      Given : bool list
      TechniqueCounts : Map<HintTechnique, int>
      GenerationScore : int
      MaxEliminationChain : int
      Ranking : PuzzleRanking }

// this contains the serialized bank for a single difficulty.
type PuzzleBankData =
    { SchemaVersion : int
      Difficulty : Difficulty
      Puzzles : PuzzleBankEntry list }
