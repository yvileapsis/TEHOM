namespace Sudoku
open System
open System.Numerics
open Prime
open Nu
open Sudoku

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Won
    | Quit

// this represents the amount of puzzle information hidden from the player.
type Difficulty =
    | Trivial
    | Easy
    | Normal
    | Hard

    member this.Holes =
        match this with
        | Trivial -> 3
        | Easy -> 30
        | Normal -> 45
        | Hard -> 55

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
    | NakedTripleRow
    | NakedTripleColumn
    | NakedTripleBlock
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
        | NakedTripleRow -> "Naked triple in row"
        | NakedTripleColumn -> "Naked triple in column"
        | NakedTripleBlock -> "Naked triple in block"
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

// this is our MMCC model type representing gameplay.
type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      Puzzle : int[,]
      Solution : int[,]
      Given : bool[,]
      Marks : Set<int>[,]
      SelectedCellOpt : Vector2i option
      Difficulty : Difficulty
      PencilMode : bool
      HintOpt : Hint option
      HintStatusOpt : string option
      Score : int }

    member this.BoardSize = v2iDup 9

    member this.IsSolved =
        let board = this.Puzzle
        let setFull values = Set.ofSeq values = set [1 .. 9]
        let rowsValid =
            [0 .. 8]
            |> List.forall (fun y -> setFull [for x in 0 .. 8 -> board[y, x]])
        let columnsValid =
            [0 .. 8]
            |> List.forall (fun x -> setFull [for y in 0 .. 8 -> board[y, x]])
        let blocksValid =
            [for by in 0 .. 2 do
                for bx in 0 .. 2 do
                    yield [for y in by * 3 .. by * 3 + 2 do
                            for x in bx * 3 .. bx * 3 + 2 ->
                                board[y, x]]]
            |> List.forall setFull
        rowsValid && columnsValid && blocksValid

    member this.HasConflict (position : Vector2i) (value : int) =
        if value = 0 then false
        else
            let board = this.Puzzle
            let row = position.Y
            let column = position.X
            let rowConflict =
                [0 .. 8]
                |> List.exists (fun x -> x <> column && board[row, x] = value)
            let columnConflict =
                [0 .. 8]
                |> List.exists (fun y -> y <> row && board[y, column] = value)
            let blockMinX = column / 3 * 3
            let blockMinY = row / 3 * 3
            let blockConflict =
                [for y in blockMinY .. blockMinY + 2 do
                    for x in blockMinX .. blockMinX + 2 -> v2i x y]
                |> List.exists (fun position2 -> position2 <> position && board[position2.Y, position2.X] = value)
            rowConflict || columnConflict || blockConflict

    static member private shuffle (list : 'a list) =
        list |> List.sortBy (fun _ -> Gen.random1 Int32.MaxValue)

    static member public makeSolvedBoard () =
        let board = Array2D.zeroCreate<int> 9 9
        let digits = Gameplay.shuffle [1 .. 9] |> List.toArray
        let rowBands = Gameplay.shuffle [0 .. 2]
        let columnBands = Gameplay.shuffle [0 .. 2]
        let rows =
            [for band in rowBands do
                for row in Gameplay.shuffle [0 .. 2] -> band * 3 + row]
        let columns =
            [for band in columnBands do
                for column in Gameplay.shuffle [0 .. 2] -> band * 3 + column]
        for y in 0 .. 8 do
            for x in 0 .. 8 do
                let pattern = (rows[y] * 3 + rows[y] / 3 + columns[x]) % 9
                board[y, x] <- digits[pattern]
        board

    static member public makePuzzle (difficulty : Difficulty) (solution : int[,]) =
        let puzzle = Array2D.copy solution
        let given = Array2D.create 9 9 true
        let holes = Gameplay.shuffle [for y in 0 .. 8 do for x in 0 .. 8 -> v2i x y] |> List.take difficulty.Holes
        for hole in holes do
            puzzle[hole.Y, hole.X] <- 0
            given[hole.Y, hole.X] <- false
        (puzzle, given)

    static member private makeMarks () =
        Array2D.create 9 9 Set.empty<int>

    static member private allPositions =
        [for y in 0 .. 8 do for x in 0 .. 8 -> v2i x y]

    static member private rowPositions (row : int) =
        [for x in 0 .. 8 -> v2i x row]

    static member private columnPositions (column : int) =
        [for y in 0 .. 8 -> v2i column y]

    static member public blockPositions (block : Vector2i) =
        [for y in block.Y * 3 .. block.Y * 3 + 2 do
            for x in block.X * 3 .. block.X * 3 + 2 -> v2i x y]

    static member public blockOfPosition (position : Vector2i) =
        v2i (position.X / 3) (position.Y / 3)

    static member private combinations (count : int) (items : 'a list) =
        let rec step count items =
            match (count, items) with
            | (0, _) -> [[]]
            | (_, []) -> []
            | (count, head :: tail) ->
                let withHead = step (count - 1) tail |> List.map (fun items -> head :: items)
                let withoutHead = step count tail
                withHead @ withoutHead
        step count items

    static member private candidates (board : int[,]) (position : Vector2i) =
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

    static member private workingCandidates (gameplay : Gameplay) (position : Vector2i) =
        if gameplay.Puzzle[position.Y, position.X] <> 0 then Set.empty
        else
            let marks = gameplay.Marks[position.Y, position.X]
            if Set.notEmpty marks then marks
            else Gameplay.candidates gameplay.Puzzle position

    static member private fillLegalMarks (gameplay : Gameplay) =
        let marks = Array2D.copy gameplay.Marks
        for position in Gameplay.allPositions do
            if gameplay.Puzzle[position.Y, position.X] = 0 &&
               not gameplay.Given[position.Y, position.X] &&
               Set.isEmpty marks[position.Y, position.X] then
                marks[position.Y, position.X] <- Gameplay.candidates gameplay.Puzzle position
        { gameplay with
            Marks = marks
            PencilMode = true
            HintOpt = None
            HintStatusOpt = Some "No immediate hint found; filled legal pencil marks." }

    static member private hasOpenCellsWithoutMarks (gameplay : Gameplay) =
        Gameplay.allPositions
        |> List.exists (fun position ->
            gameplay.Puzzle[position.Y, position.X] = 0 &&
            not gameplay.Given[position.Y, position.X] &&
            Set.isEmpty gameplay.Marks[position.Y, position.X])

    static member private tryMakeHint (technique : HintTechnique) (region : HintRegion) (position : Vector2i) (number : int) (gameplay : Gameplay) =
        if gameplay.Solution[position.Y, position.X] = number
        then Some { Target = position; Region = region; Technique = technique; Action = PlaceNumber (position, number) }
        else None

    static member private tryMakeEliminationHint (technique : HintTechnique) (region : HintRegion) (eliminations : (Vector2i * Set<int>) list) (gameplay : Gameplay) =
        let removals =
            eliminations
            |> List.choose (fun (position, eliminated) ->
                let marks = gameplay.Marks[position.Y, position.X]
                let safeEliminated = Set.remove gameplay.Solution[position.Y, position.X] eliminated
                let removed = Set.intersect marks safeEliminated
                if Set.notEmpty removed then Some (position, removed)
                else None)
        match removals with
        | (target, _) :: _ ->
            let removalPositions = removals |> List.map fst |> Set.ofList
            let region =
                match region with
                | HintCells positions -> HintCells (Set.union positions removalPositions)
                | _ -> region
            Some { Target = target; Region = region; Technique = technique; Action = RemoveMarks removals }
        | [] -> None

    static member private tryFindFullHouse (technique : HintTechnique) (region : HintRegion) (positions : Vector2i list) (gameplay : Gameplay) =
        let emptyPositions = positions |> List.filter (fun position -> gameplay.Puzzle[position.Y, position.X] = 0)
        match emptyPositions with
        | [position] ->
            let existing = set [for position in positions do if gameplay.Puzzle[position.Y, position.X] <> 0 then yield gameplay.Puzzle[position.Y, position.X]]
            let missing = Set.difference (set [1 .. 9]) existing
            if Set.count missing = 1 then Gameplay.tryMakeHint technique region position (Set.minElement missing) gameplay
            else None
        | _ -> None

    static member private tryFindFullHouseInRow (gameplay : Gameplay) =
        [0 .. 8]
        |> List.tryPick (fun row ->
            Gameplay.tryFindFullHouse FullHouseRow (HintRow row) (Gameplay.rowPositions row) gameplay)

    static member private tryFindFullHouseInColumn (gameplay : Gameplay) =
        [0 .. 8]
        |> List.tryPick (fun column ->
            Gameplay.tryFindFullHouse FullHouseColumn (HintColumn column) (Gameplay.columnPositions column) gameplay)

    static member private tryFindFullHouseInBlock (gameplay : Gameplay) =
        [for y in 0 .. 2 do for x in 0 .. 2 -> v2i x y]
        |> List.tryPick (fun block ->
            Gameplay.tryFindFullHouse FullHouseBlock (HintBlock block) (Gameplay.blockPositions block) gameplay)

    static member private tryFindNakedSingle (gameplay : Gameplay) =
        Gameplay.allPositions
        |> List.tryPick (fun position ->
            let candidates = Gameplay.workingCandidates gameplay position
            if Set.count candidates = 1 then
                Gameplay.tryMakeHint NakedSingle (HintCell position) position (Set.minElement candidates) gameplay
            else None)

    static member private tryFindHiddenSingleInRow (gameplay : Gameplay) =
        [for row in 0 .. 8 do
            for number in 1 .. 9 do
                let positions =
                    [for x in 0 .. 8 do
                        let position = v2i x row
                        if Set.contains number (Gameplay.workingCandidates gameplay position) then yield position]
                match positions with
                | [position] -> yield (row, number, position)
                | _ -> ()]
        |> List.tryPick (fun (row : int, number : int, position : Vector2i) ->
            Gameplay.tryMakeHint HiddenSingleRow (HintRow row) position number gameplay)

    static member private tryFindHiddenSingleInColumn (gameplay : Gameplay) =
        [for column in 0 .. 8 do
            for number in 1 .. 9 do
                let positions =
                    [for y in 0 .. 8 do
                        let position = v2i column y
                        if Set.contains number (Gameplay.workingCandidates gameplay position) then yield position]
                match positions with
                | [position] -> yield (column, number, position)
                | _ -> ()]
        |> List.tryPick (fun (column : int, number : int, position : Vector2i) ->
            Gameplay.tryMakeHint HiddenSingleColumn (HintColumn column) position number gameplay)

    static member private tryFindHiddenSingleInBlock (gameplay : Gameplay) =
        [for blockY in 0 .. 2 do
            for blockX in 0 .. 2 do
                let block = v2i blockX blockY
                for number in 1 .. 9 do
                    let positions =
                        [for y in blockY * 3 .. blockY * 3 + 2 do
                            for x in blockX * 3 .. blockX * 3 + 2 do
                                let position = v2i x y
                                if Set.contains number (Gameplay.workingCandidates gameplay position) then yield position]
                    match positions with
                    | [position] -> yield (block, number, position)
                    | _ -> ()]
        |> List.tryPick (fun (block : Vector2i, number : int, position : Vector2i) ->
            Gameplay.tryMakeHint HiddenSingleBlock (HintBlock block) position number gameplay)

    static member private tryFindNakedSubsetInUnit (technique : HintTechnique) (region : HintRegion) (size : int) (positions : Vector2i list) (gameplay : Gameplay) =
        let candidatesByPosition =
            positions
            |> List.choose (fun position ->
                let candidates = Gameplay.workingCandidates gameplay position
                if Set.count candidates >= 2 && Set.count candidates <= size then Some (position, candidates)
                else None)
        Gameplay.combinations size candidatesByPosition
        |> List.tryPick (fun subset ->
            let subsetCandidates = subset |> List.map snd |> Set.unionMany
            if Set.count subsetCandidates = size then
                let subsetPositions = subset |> List.map fst |> Set.ofList
                let eliminations =
                    [for position in positions do
                        if not (Set.contains position subsetPositions) && gameplay.Puzzle[position.Y, position.X] = 0 then
                            yield (position, subsetCandidates)]
                Gameplay.tryMakeEliminationHint technique region eliminations gameplay
            else None)

    static member private tryFindNakedPairsInRows (gameplay : Gameplay) =
        [0 .. 8]
        |> List.tryPick (fun row ->
            Gameplay.tryFindNakedSubsetInUnit NakedPairRow (HintRow row) 2 (Gameplay.rowPositions row) gameplay)

    static member private tryFindNakedPairsInColumns (gameplay : Gameplay) =
        [0 .. 8]
        |> List.tryPick (fun column ->
            Gameplay.tryFindNakedSubsetInUnit NakedPairColumn (HintColumn column) 2 (Gameplay.columnPositions column) gameplay)

    static member private tryFindNakedPairsInBlocks (gameplay : Gameplay) =
        [for y in 0 .. 2 do for x in 0 .. 2 -> v2i x y]
        |> List.tryPick (fun block ->
            Gameplay.tryFindNakedSubsetInUnit NakedPairBlock (HintBlock block) 2 (Gameplay.blockPositions block) gameplay)

    static member private tryFindNakedTriplesInRows (gameplay : Gameplay) =
        [0 .. 8]
        |> List.tryPick (fun row ->
            Gameplay.tryFindNakedSubsetInUnit NakedTripleRow (HintRow row) 3 (Gameplay.rowPositions row) gameplay)

    static member private tryFindNakedTriplesInColumns (gameplay : Gameplay) =
        [0 .. 8]
        |> List.tryPick (fun column ->
            Gameplay.tryFindNakedSubsetInUnit NakedTripleColumn (HintColumn column) 3 (Gameplay.columnPositions column) gameplay)

    static member private tryFindNakedTriplesInBlocks (gameplay : Gameplay) =
        [for y in 0 .. 2 do for x in 0 .. 2 -> v2i x y]
        |> List.tryPick (fun block ->
            Gameplay.tryFindNakedSubsetInUnit NakedTripleBlock (HintBlock block) 3 (Gameplay.blockPositions block) gameplay)

    static member private tryFindPointingRowOrColumn (gameplay : Gameplay) =
        [for blockY in 0 .. 2 do
            for blockX in 0 .. 2 do
                let block = v2i blockX blockY
                let blockPositions = Gameplay.blockPositions block
                for number in 1 .. 9 do
                    let positions =
                        blockPositions
                        |> List.filter (fun position -> Set.contains number (Gameplay.workingCandidates gameplay position))
                    if List.length positions >= 2 then
                        let rows = positions |> List.map (fun position -> position.Y) |> Set.ofList
                        let columns = positions |> List.map (fun position -> position.X) |> Set.ofList
                        if Set.count rows = 1 then
                            let row = Set.minElement rows
                            let eliminations =
                                [for position in Gameplay.rowPositions row do
                                    if Gameplay.blockOfPosition position <> block && Set.contains number (Gameplay.workingCandidates gameplay position) then
                                        yield (position, set [number])]
                            let region = HintCells (Set.ofList positions)
                            yield (PointingRow, region, eliminations)
                        if Set.count columns = 1 then
                            let column = Set.minElement columns
                            let eliminations =
                                [for position in Gameplay.columnPositions column do
                                    if Gameplay.blockOfPosition position <> block && Set.contains number (Gameplay.workingCandidates gameplay position) then
                                        yield (position, set [number])]
                            let region = HintCells (Set.ofList positions)
                            yield (PointingColumn, region, eliminations)]
        |> List.tryPick (fun (technique : HintTechnique, region : HintRegion, eliminations : (Vector2i * Set<int>) list) ->
            Gameplay.tryMakeEliminationHint technique region eliminations gameplay)

    static member private tryFindClaimingRowOrColumn (gameplay : Gameplay) =
        let rowClaims =
            [for row in 0 .. 8 do
                let rowPositions = Gameplay.rowPositions row
                for number in 1 .. 9 do
                    let positions =
                        rowPositions
                        |> List.filter (fun position -> Set.contains number (Gameplay.workingCandidates gameplay position))
                    if List.length positions >= 2 then
                        let blocks = positions |> List.map Gameplay.blockOfPosition |> Set.ofList
                        if Set.count blocks = 1 then
                            let block = Set.minElement blocks
                            let eliminations =
                                [for position in Gameplay.blockPositions block do
                                    if position.Y <> row && Set.contains number (Gameplay.workingCandidates gameplay position) then
                                        yield (position, set [number])]
                            let region = HintCells (Set.ofList positions)
                            yield (ClaimingRow, region, eliminations)]
        let columnClaims =
            [for column in 0 .. 8 do
                let columnPositions = Gameplay.columnPositions column
                for number in 1 .. 9 do
                    let positions =
                        columnPositions
                        |> List.filter (fun position -> Set.contains number (Gameplay.workingCandidates gameplay position))
                    if List.length positions >= 2 then
                        let blocks = positions |> List.map Gameplay.blockOfPosition |> Set.ofList
                        if Set.count blocks = 1 then
                            let block = Set.minElement blocks
                            let eliminations =
                                [for position in Gameplay.blockPositions block do
                                    if position.X <> column && Set.contains number (Gameplay.workingCandidates gameplay position) then
                                        yield (position, set [number])]
                            let region = HintCells (Set.ofList positions)
                            yield (ClaimingColumn, region, eliminations)]
        rowClaims @ columnClaims
        |> List.tryPick (fun (technique : HintTechnique, region : HintRegion, eliminations : (Vector2i * Set<int>) list) ->
            Gameplay.tryMakeEliminationHint technique region eliminations gameplay)

    static member private tryFindFishRows (size : int) (technique : HintTechnique) (gameplay : Gameplay) =
        [for number in 1 .. 9 do
            let rows =
                [for row in 0 .. 8 do
                    let columns =
                        [for position in Gameplay.rowPositions row do
                            if Set.contains number (Gameplay.workingCandidates gameplay position) then yield position.X]
                    if List.length columns >= 2 && List.length columns <= size then yield (row, Set.ofList columns)]
            for rowSet in Gameplay.combinations size rows do
                let columns = rowSet |> List.map snd |> Set.unionMany
                if Set.count columns = size then
                    let selectedRows = rowSet |> List.map fst |> Set.ofList
                    let eliminations =
                        [for column in columns do
                            for row in 0 .. 8 do
                                if not (Set.contains row selectedRows) then
                                    let position = v2i column row
                                    if Set.contains number (Gameplay.workingCandidates gameplay position) then
                                        yield (position, set [number])]
                    let regionPositions =
                        [for row in selectedRows do
                            for column in columns -> v2i column row]
                        |> Set.ofList
                    yield (technique, HintCells regionPositions, eliminations)]
        |> List.tryPick (fun (technique : HintTechnique, region : HintRegion, eliminations : (Vector2i * Set<int>) list) ->
            Gameplay.tryMakeEliminationHint technique region eliminations gameplay)

    static member private tryFindFishColumns (size : int) (technique : HintTechnique) (gameplay : Gameplay) =
        [for number in 1 .. 9 do
            let columns =
                [for column in 0 .. 8 do
                    let rows =
                        [for position in Gameplay.columnPositions column do
                            if Set.contains number (Gameplay.workingCandidates gameplay position) then yield position.Y]
                    if List.length rows >= 2 && List.length rows <= size then yield (column, Set.ofList rows)]
            for columnSet in Gameplay.combinations size columns do
                let rows = columnSet |> List.map snd |> Set.unionMany
                if Set.count rows = size then
                    let selectedColumns = columnSet |> List.map fst |> Set.ofList
                    let eliminations =
                        [for row in rows do
                            for column in 0 .. 8 do
                                if not (Set.contains column selectedColumns) then
                                    let position = v2i column row
                                    if Set.contains number (Gameplay.workingCandidates gameplay position) then
                                        yield (position, set [number])]
                    let regionPositions =
                        [for column in selectedColumns do
                            for row in rows -> v2i column row]
                        |> Set.ofList
                    yield (technique, HintCells regionPositions, eliminations)]
        |> List.tryPick (fun (technique : HintTechnique, region : HintRegion, eliminations : (Vector2i * Set<int>) list) ->
            Gameplay.tryMakeEliminationHint technique region eliminations gameplay)

    static member private tryFindHint (gameplay : Gameplay) =
        let finders : (Gameplay -> Hint option) list =
            [fun gameplay -> Gameplay.tryFindFullHouseInRow gameplay
             fun gameplay -> Gameplay.tryFindFullHouseInColumn gameplay
             fun gameplay -> Gameplay.tryFindFullHouseInBlock gameplay
             fun gameplay -> Gameplay.tryFindNakedSingle gameplay
             fun gameplay -> Gameplay.tryFindHiddenSingleInRow gameplay
             fun gameplay -> Gameplay.tryFindHiddenSingleInColumn gameplay
             fun gameplay -> Gameplay.tryFindHiddenSingleInBlock gameplay
             fun gameplay -> Gameplay.tryFindNakedPairsInRows gameplay
             fun gameplay -> Gameplay.tryFindNakedPairsInColumns gameplay
             fun gameplay -> Gameplay.tryFindNakedPairsInBlocks gameplay
             fun gameplay -> Gameplay.tryFindNakedTriplesInRows gameplay
             fun gameplay -> Gameplay.tryFindNakedTriplesInColumns gameplay
             fun gameplay -> Gameplay.tryFindNakedTriplesInBlocks gameplay
             fun gameplay -> Gameplay.tryFindPointingRowOrColumn gameplay
             fun gameplay -> Gameplay.tryFindClaimingRowOrColumn gameplay
             fun gameplay -> Gameplay.tryFindFishRows 2 XWingRow gameplay
             fun gameplay -> Gameplay.tryFindFishColumns 2 XWingColumn gameplay
             fun gameplay -> Gameplay.tryFindFishRows 3 SwordfishRow gameplay
             fun gameplay -> Gameplay.tryFindFishColumns 3 SwordfishColumn gameplay]
        finders
        |> List.tryPick (fun find -> find gameplay)

    static member private withNumberAt (position : Vector2i) (number : int) (gameplay : Gameplay) =
        if gameplay.GameplayState = Playing && not gameplay.Given[position.Y, position.X] then
            let puzzle = Array2D.copy gameplay.Puzzle
            let marks = Array2D.copy gameplay.Marks
            puzzle[position.Y, position.X] <- number
            marks[position.Y, position.X] <- Set.empty
            let gameplay =
                { gameplay with
                    Puzzle = puzzle
                    Marks = marks
                    HintOpt = None
                    HintStatusOpt = None }
            if gameplay.IsSolved then { gameplay with GameplayState = Won; Score = inc gameplay.Score }
            else gameplay
        else gameplay

    static member private removeMarks (removals : (Vector2i * Set<int>) list) (gameplay : Gameplay) =
        let marks = Array2D.copy gameplay.Marks
        for (position, removed) in removals do
            marks[position.Y, position.X] <- Set.difference marks[position.Y, position.X] removed
        { gameplay with
            Marks = marks
            HintOpt = None
            HintStatusOpt = None }

    static member private toggleMarkAt (position : Vector2i) (number : int) (gameplay : Gameplay) =
        if gameplay.GameplayState = Playing && not gameplay.Given[position.Y, position.X] && gameplay.Puzzle[position.Y, position.X] = 0 then
            let marks = Array2D.copy gameplay.Marks
            marks[position.Y, position.X] <-
                if Set.contains number marks[position.Y, position.X]
                then Set.remove number marks[position.Y, position.X]
                else Set.add number marks[position.Y, position.X]
            { gameplay with
                Marks = marks
                HintOpt = None
                HintStatusOpt = None }
        else gameplay

    static member public withNumber (number : int) (gameplay : Gameplay) =
        match gameplay.SelectedCellOpt with
        | Some position when gameplay.GameplayState = Playing && not gameplay.Given[position.Y, position.X] && gameplay.PencilMode && number <> 0 ->
            Gameplay.toggleMarkAt position number gameplay
        | Some position when gameplay.GameplayState = Playing && not gameplay.Given[position.Y, position.X] ->
            Gameplay.withNumberAt position number gameplay
        | _ -> gameplay

    static member public clearSelected (gameplay : Gameplay) =
        match gameplay.SelectedCellOpt with
        | Some position when gameplay.GameplayState = Playing && not gameplay.Given[position.Y, position.X] ->
            Gameplay.withNumberAt position 0 gameplay
        | _ -> gameplay

    static member public moveSelection (delta : Vector2i) (gameplay : Gameplay) =
        match gameplay.SelectedCellOpt with
        | Some selected ->
            let selected = v2i ((selected.X + delta.X + 9) % 9) ((selected.Y + delta.Y + 9) % 9)
            { gameplay with SelectedCellOpt = Some selected }
        | None -> { gameplay with SelectedCellOpt = Some (v2i 0 0) }

    static member public withHint (gameplay : Gameplay) =
        if gameplay.GameplayState = Playing then
            match gameplay.HintOpt with
            | Some hint ->
                match hint.Action with
                | PlaceNumber (target, number) ->
                    let gameplay = Gameplay.withNumberAt target number ({ gameplay with SelectedCellOpt = Some target })
                    { gameplay with HintStatusOpt = Some ("Placed " + string number + " by " + hint.Technique.Label + ".") }
                | RemoveMarks removals ->
                    let removedCount = removals |> List.sumBy (fun (_, numbers) -> Set.count numbers)
                    let gameplay = Gameplay.removeMarks removals gameplay
                    { gameplay with HintStatusOpt = Some ("Removed " + string removedCount + " pencil mark" + (if removedCount = 1 then "" else "s") + " by " + hint.Technique.Label + ".") }
            | None ->
                match Gameplay.tryFindHint gameplay with
                | Some hint ->
                    let applyText =
                        match hint.Action with
                        | PlaceNumber _ -> " Press Hint again to place it."
                        | RemoveMarks _ -> " Press Hint again to remove the marks."
                    { gameplay with
                        HintOpt = Some hint
                        HintStatusOpt = Some (hint.Label + applyText)
                        SelectedCellOpt = Some hint.Target }
                | None ->
                    if Gameplay.hasOpenCellsWithoutMarks gameplay
                    then Gameplay.fillLegalMarks gameplay
                    else
                        { gameplay with
                            HintOpt = None
                            HintStatusOpt = Some "No hint available from the current pencil marks." }
        else gameplay

    static member make (difficulty : Difficulty) (score : int) =
        let solution = Gameplay.makeSolvedBoard ()
        let (puzzle, given) = Gameplay.makePuzzle difficulty solution
        { GameplayTime = 0L
          GameplayState = Playing
          Puzzle = puzzle
          Solution = solution
          Given = given
          Marks = Gameplay.makeMarks ()
          SelectedCellOpt = Some (v2i 0 0)
          Difficulty = difficulty
          PencilMode = false
          HintOpt = None
          HintStatusOpt = None
          Score = score }

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { Gameplay.make Normal 0 with
            GameplayState = Quit
            SelectedCellOpt = None }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = Gameplay.make Normal 0

// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | TimeUpdate
    | SelectCellAtMouse
    | MoveSelection of Vector2i
    | EnterNumber of int
    | ClearCell
    | TogglePencilMode
    | SetDifficulty of Difficulty
    | RequestHint
    | Restart
    | Nil
    interface Message

// this is our gameplay MMCC command type.
type GameplayCommand =
    | StartQuitting
    interface Command

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.QuitEvent = Events.QuitEvent --> this

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

    static let boardCenter = v2 -52.0f -10.0f
    static let cellSize = 34.0f
    static let boardSize = cellSize * 9.0f
    static let boardMin = boardCenter - v2Dup (boardSize * 0.5f)

    static let tryKeyboardNumber (key : KeyboardKey) =
        match key with
        | KeyboardKey.Num1 | KeyboardKey.Kp1 -> Some 1
        | KeyboardKey.Num2 | KeyboardKey.Kp2 -> Some 2
        | KeyboardKey.Num3 | KeyboardKey.Kp3 -> Some 3
        | KeyboardKey.Num4 | KeyboardKey.Kp4 -> Some 4
        | KeyboardKey.Num5 | KeyboardKey.Kp5 -> Some 5
        | KeyboardKey.Num6 | KeyboardKey.Kp6 -> Some 6
        | KeyboardKey.Num7 | KeyboardKey.Kp7 -> Some 7
        | KeyboardKey.Num8 | KeyboardKey.Kp8 -> Some 8
        | KeyboardKey.Num9 | KeyboardKey.Kp9 -> Some 9
        | _ -> None

    static let tryMouseCell (world : World) =
        let mouse = World.getMousePosition2dWorld false world
        let local = mouse - boardMin
        if local.X >= 0.0f && local.Y >= 0.0f && local.X < boardSize && local.Y < boardSize then
            let column = int (local.X / cellSize)
            let row = 8 - int (local.Y / cellSize)
            Some (v2i column row)
        else None

    static let cellPosition (x : int) (y : int) =
        v3 (boardMin.X + (single x + 0.5f) * cellSize) (boardMin.Y + (single (8 - y) + 0.5f) * cellSize) 0.0f

    static let numberStatusPosition (number : int) =
        v3 (boardMin.X - 38.0f) (boardMin.Y + (single (9 - number) + 0.5f) * cellSize) 0.0f

    static let markPositionLocal (number : int) =
        let index = number - 1
        let column = index % 3
        let row = index / 3
        v3 ((single column - 1.0f) * 9.0f) ((1.0f - single row) * 9.0f) 0.0f

    static let positionInHintRegion (hint : Hint) (position : Vector2i) =
        match hint.Region with
        | HintCell cell -> position = cell
        | HintCells cells -> Set.contains position cells
        | HintRow row -> position.Y = row
        | HintColumn column -> position.X = column
        | HintBlock block -> position.X / 3 = block.X && position.Y / 3 = block.Y

    static let cellColor (gameplay : Gameplay) (position : Vector2i) (value : int) =
        match gameplay.HintOpt with
        | Some hint when position = hint.Target -> color 0.78f 0.58f 0.14f 1.0f
        | Some hint when positionInHintRegion hint position -> color 0.18f 0.40f 0.26f 1.0f
        | _ ->
            if gameplay.SelectedCellOpt = Some position then color 0.30f 0.48f 0.72f 1.0f
            elif gameplay.Given[position.Y, position.X] then color 0.18f 0.22f 0.27f 1.0f
            elif gameplay.HasConflict position value then color 0.58f 0.16f 0.16f 1.0f
            elif value = 0 then color 0.12f 0.14f 0.17f 1.0f
            else color 0.20f 0.26f 0.33f 1.0f

    static let numberRemaining (gameplay : Gameplay) (number : int) =
        9 - List.length [for y in 0 .. 8 do for x in 0 .. 8 do if gameplay.Puzzle[y, x] = number then yield number]

    static let numberMissingFromSelectedBlock (gameplay : Gameplay) (number : int) =
        match gameplay.SelectedCellOpt with
        | Some selected ->
            let block = Gameplay.blockOfPosition selected
            block
            |> Gameplay.blockPositions
            |> List.exists (fun (position : Vector2i) -> gameplay.Puzzle[position.Y, position.X] = number)
            |> not
        | None -> false

    static let numberStatusColor (gameplay : Gameplay) (number : int) =
        let remaining = numberRemaining gameplay number
        if remaining <= 0 then color 0.10f 0.12f 0.14f 1.0f
        elif numberMissingFromSelectedBlock gameplay number then color 0.78f 0.58f 0.14f 1.0f
        else color 0.22f 0.36f 0.54f 1.0f

    static let numberStatusTextColor (gameplay : Gameplay) (number : int) =
        if numberRemaining gameplay number <= 0 then color 0.44f 0.48f 0.52f 1.0f
        else Color.GhostWhite

    static let difficultyButtonColor (selected : Difficulty) (difficulty : Difficulty) =
        if selected = difficulty then color 0.30f 0.48f 0.72f 1.0f
        else color 0.18f 0.22f 0.27f 1.0f

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.DeselectingEvent => FinishQuitting
         Screen.TimeUpdateEvent => TimeUpdate
         Game.MouseLeftDownEvent => SelectCellAtMouse
         Game.KeyboardKeyDownEvent =|> fun evt ->
            if evt.Data.Repeated then Nil
            else
                match tryKeyboardNumber evt.Data.KeyboardKey with
                | Some number -> EnterNumber number
                | None ->
                    match evt.Data.KeyboardKey with
                    | KeyboardKey.Left -> MoveSelection (v2i -1 0)
                    | KeyboardKey.Right -> MoveSelection (v2i 1 0)
                    | KeyboardKey.Up -> MoveSelection (v2i 0 -1)
                    | KeyboardKey.Down -> MoveSelection (v2i 0 1)
                    | KeyboardKey.Backspace | KeyboardKey.Delete | KeyboardKey.Num0 | KeyboardKey.Kp0 -> ClearCell
                    | KeyboardKey.P -> TogglePencilMode
                    | KeyboardKey.T -> SetDifficulty Trivial
                    | KeyboardKey.E -> SetDifficulty Easy
                    | KeyboardKey.N -> SetDifficulty Normal
                    | KeyboardKey.H -> SetDifficulty Hard
                    | KeyboardKey.R -> Restart
                    | KeyboardKey.F1 -> RequestHint
                    | _ -> Nil]

    // here we handle the above messages
    override this.Message (gameplay, message, _, world) =

        match message with
        | StartPlaying ->
            just (Gameplay.make gameplay.Difficulty gameplay.Score)

        | FinishQuitting ->
            just { gameplay with GameplayState = Quit; SelectedCellOpt = None; HintOpt = None; HintStatusOpt = None }

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            just { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }

        | SelectCellAtMouse ->
            match tryMouseCell world with
            | Some cell when gameplay.GameplayState <> Quit -> just { gameplay with SelectedCellOpt = Some cell }
            | _ -> just gameplay

        | MoveSelection delta ->
            if gameplay.GameplayState = Playing then just (Gameplay.moveSelection delta gameplay)
            else just gameplay

        | EnterNumber number ->
            just (Gameplay.withNumber number gameplay)

        | ClearCell ->
            just (Gameplay.clearSelected gameplay)

        | TogglePencilMode ->
            just { gameplay with PencilMode = not gameplay.PencilMode; HintStatusOpt = None }

        | SetDifficulty difficulty ->
            just (Gameplay.make difficulty gameplay.Score)

        | RequestHint ->
            just (Gameplay.withHint gameplay)

        | Restart ->
            just (Gameplay.make gameplay.Difficulty gameplay.Score)


        | Nil ->
            just gameplay

    // here we handle the above commands
    override this.Command (_, command, screen, world) =
        match command with
        | StartQuitting ->
            World.publish () screen.QuitEvent screen world

    // here we describe the content of the game including the board and the hud.
    override this.Content (gameplay, _) =

        [// the scene group while playing
         if gameplay.GameplayState <> Quit then
            Content.group Simulants.GameplayScene.Name []

                [Content.text "Title"
                    [Entity.Position == v3 -52.0f 176.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 22.0f
                     Entity.Text == "Sudoku"]

                 Content.text "Score"
                    [Entity.Position == v3 196.0f 142.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text := "Score: " + string gameplay.Score]

                 Content.text "Status"
                    [Entity.Position == v3 196.0f 102.0f 0.0f
                     Entity.Size == v3 178.0f 48.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 8.0f
                     Entity.Text :=
                        match gameplay.GameplayState with
                        | Won -> "You won!"
                        | Playing ->
                            match gameplay.HintStatusOpt with
                            | Some status -> status
                            | None -> "Fill every row, column, and block."
                        | Quit -> ""]

                 Content.text "Difficulty"
                    [Entity.Position == v3 196.0f 66.0f 0.0f
                     Entity.Size == v3 170.0f 24.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 9.0f
                     Entity.Text := "Difficulty: " + gameplay.Difficulty.Label]

                 Content.text "InputMode"
                    [Entity.Position == v3 196.0f 48.0f 0.0f
                     Entity.Size == v3 170.0f 20.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 8.0f
                     Entity.Text := if gameplay.PencilMode then "Pencil marks" else "Normal entry"]

                 for (i, difficulty) in List.indexed [Trivial; Easy; Normal; Hard] do
                    Content.button ("Difficulty+" + difficulty.Label)
                        [Entity.Position == v3 (160.0f + single (i % 2) * 96.0f) (22.0f - single (i / 2) * 34.0f) 0.0f
                         Entity.Size == v3 86.0f 28.0f 0.0f
                         Entity.Elevation == 10.0f
                         Entity.Color := difficultyButtonColor gameplay.Difficulty difficulty
                         Entity.Text := difficulty.Label
                         Entity.ClickEvent => SetDifficulty difficulty]

                 Content.text "NumberStatusTitle"
                    [Entity.Position == v3 (boardMin.X - 38.0f) (boardMin.Y + boardSize + 16.0f) 0.0f
                     Entity.Size == v3 52.0f 18.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 7.0f
                     Entity.Text == "Needed"]

                 for number in 1 .. 9 do
                    Content.panel ("NumberStatus+" + string number)
                        [Entity.Position == numberStatusPosition number
                         Entity.Size == v3 28.0f 28.0f 0.0f
                         Entity.Elevation == 4.0f
                         Entity.BackdropImageOpt == Some Assets.Default.White
                         Entity.Color := numberStatusColor gameplay number]
                        [Content.text "Value"
                            [Entity.PositionLocal == v3 0.0f 3.0f 0.0f
                             Entity.Size == v3 28.0f 18.0f 0.0f
                             Entity.ElevationLocal == 1.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.FontSizing == Some 10.0f
                             Entity.TextColor := numberStatusTextColor gameplay number
                             Entity.Text == string number]
                         Content.text "Remaining"
                            [Entity.PositionLocal == v3 0.0f -8.0f 0.0f
                             Entity.Size == v3 28.0f 10.0f 0.0f
                             Entity.ElevationLocal == 1.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.FontSizing == Some 5.0f
                             Entity.TextColor := numberStatusTextColor gameplay number
                             Entity.Text := string (numberRemaining gameplay number)]]

                 for y in 0 .. 8 do
                    for x in 0 .. 8 do
                        let value = gameplay.Puzzle[y, x]
                        let marks = gameplay.Marks[y, x]
                        let position = v2i x y
                        Content.panel ("Cell+" + string x + "+" + string y)
                            [Entity.Position == cellPosition x y
                             Entity.Size == v3 (cellSize - 2.0f) (cellSize - 2.0f) 0.0f
                             Entity.Elevation == 1.0f
                             Entity.BackdropImageOpt == Some Assets.Default.White
                             Entity.Color := cellColor gameplay position value]
                            [if value <> 0 then
                                Content.text "Value"
                                    [Entity.PositionLocal == v3Zero
                                     Entity.Size == v3 (cellSize - 2.0f) (cellSize - 2.0f) 0.0f
                                     Entity.ElevationLocal == 1.0f
                                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                                     Entity.FontSizing := if gameplay.Given[y, x] then Some 15.0f else Some 16.0f
                                     Entity.TextColor := if gameplay.Given[y, x] then Color.GhostWhite else color 0.78f 0.90f 1.0f 1.0f
                                     Entity.Text := string value]
                             else
                                for mark in marks do
                                    Content.text ("Mark+" + string mark)
                                        [Entity.PositionLocal == markPositionLocal mark
                                         Entity.Size == v3 10.0f 10.0f 0.0f
                                         Entity.ElevationLocal == 1.0f
                                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                                         Entity.FontSizing == Some 6.0f
                                         Entity.TextColor == color 0.66f 0.78f 0.90f 1.0f
                                         Entity.Text == string mark]]

                 for i in 0 .. 9 do
                    let lineSize = if i % 3 = 0 then 4.0f else 1.5f
                    let lineOffset = single i * cellSize
                    Content.staticSprite ("LineV+" + string i)
                        [Entity.Position == v3 (boardMin.X + lineOffset) boardCenter.Y 0.0f
                         Entity.Size == v3 lineSize boardSize 0.0f
                         Entity.Elevation == 6.0f
                         Entity.StaticImage == Assets.Default.White
                         Entity.Color == color 0.05f 0.06f 0.07f 1.0f]
                    Content.staticSprite ("LineH+" + string i)
                        [Entity.Position == v3 boardCenter.X (boardMin.Y + lineOffset) 0.0f
                         Entity.Size == v3 boardSize lineSize 0.0f
                         Entity.Elevation == 6.0f
                         Entity.StaticImage == Assets.Default.White
                         Entity.Color == color 0.05f 0.06f 0.07f 1.0f]

                 Content.button "Hint"
                    [Entity.Position == v3 196.0f -52.0f 0.0f
                     Entity.Size == v3 128.0f 28.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text := if gameplay.HintOpt.IsSome then "Apply Hint" else "Hint"
                     Entity.ClickEvent => RequestHint]

                 Content.button "Pencil"
                    [Entity.Position == v3 196.0f -86.0f 0.0f
                     Entity.Size == v3 128.0f 28.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Color := if gameplay.PencilMode then color 0.30f 0.48f 0.72f 1.0f else color 0.18f 0.22f 0.27f 1.0f
                     Entity.Text := if gameplay.PencilMode then "Pencil On" else "Pencil Off"
                     Entity.ClickEvent => TogglePencilMode]

                 Content.button "Restart"
                    [Entity.Position == v3 196.0f -120.0f 0.0f
                     Entity.Size == v3 128.0f 28.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text := if gameplay.GameplayState = Won then "New Board" else "Restart"
                     Entity.ClickEvent => Restart]

                 Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 196.0f -154.0f 0.0f
                     Entity.Size == v3 128.0f 28.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]]
