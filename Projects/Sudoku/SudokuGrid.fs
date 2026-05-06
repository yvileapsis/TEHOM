namespace Sudoku
open System
open System.Numerics
open Prime
open Nu
open Sudoku

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

    let makeMarks () =
        Array2D.create 9 9 Set.empty<int>

    let allPositions =
        [for y in 0 .. 8 do for x in 0 .. 8 -> v2i x y]

    let rowPositions (row : int) =
        [for x in 0 .. 8 -> v2i x row]

    let columnPositions (column : int) =
        [for y in 0 .. 8 -> v2i column y]

    let blockPositions (block : Vector2i) =
        [for y in block.Y * 3 .. block.Y * 3 + 2 do
            for x in block.X * 3 .. block.X * 3 + 2 -> v2i x y]

    let blockOfPosition (position : Vector2i) =
        v2i (position.X / 3) (position.Y / 3)

    let combinations (count : int) (items : 'a list) =
        let rec step (count : int) (items : 'a list) =
            match (count, items) with
            | (0, _) -> [[]]
            | (_, []) -> []
            | (count, head :: tail) ->
                let withHead = step (count - 1) tail |> List.map (fun items -> head :: items)
                let withoutHead = step count tail
                withHead @ withoutHead
        step count items

    let candidates (board : int[,]) (position : Vector2i) =
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

    let workingCandidates (state : SudokuBoardState) (position : Vector2i) =
        if state.Puzzle[position.Y, position.X] <> 0 then Set.empty
        else
            let marks = state.Marks[position.Y, position.X]
            if Set.notEmpty marks then marks
            else candidates state.Puzzle position

    let fillLegalMarks (state : SudokuBoardState) =
        let marks = Array2D.copy state.Marks
        for position in allPositions do
            if state.Puzzle[position.Y, position.X] = 0 &&
               not state.Given[position.Y, position.X] &&
               Set.isEmpty marks[position.Y, position.X] then
                marks[position.Y, position.X] <- candidates state.Puzzle position
        { state with Marks = marks }

    let hasOpenCellsWithoutMarks (state : SudokuBoardState) =
        allPositions
        |> List.exists (fun position ->
            state.Puzzle[position.Y, position.X] = 0 &&
            not state.Given[position.Y, position.X] &&
            Set.isEmpty state.Marks[position.Y, position.X])

    let correctedMarkSets (state : SudokuBoardState) =
        allPositions
        |> List.choose (fun position ->
            let marks = state.Marks[position.Y, position.X]
            if state.Puzzle[position.Y, position.X] = 0 &&
               not state.Given[position.Y, position.X] &&
               Set.notEmpty marks then
                let legal = candidates state.Puzzle position
                let solution = state.Solution[position.Y, position.X]
                if Set.contains solution legal then
                    let corrected = Set.add solution (Set.intersect marks legal)
                    if corrected <> marks then Some (position, corrected)
                    else None
                else None
            else None)

    let peerPositions (position : Vector2i) =
        let block = blockOfPosition position
        [rowPositions position.Y
         columnPositions position.X
         blockPositions block]
        |> List.concat
        |> List.filter (fun peer -> peer <> position)
        |> Set.ofList
        |> Set.toList

    let prunePeerMarks (position : Vector2i) (number : int) (marks : Set<int>[,]) =
        if number <> 0 then
            for peer in peerPositions position do
                marks[peer.Y, peer.X] <- Set.remove number marks[peer.Y, peer.X]

    let applyHint (hint : Hint) (state : SudokuBoardState) =
        match hint.Action with
        | PlaceNumber (target, number) ->
            let puzzle = Array2D.copy state.Puzzle
            let marks = Array2D.copy state.Marks
            puzzle[target.Y, target.X] <- number
            marks[target.Y, target.X] <- Set.empty
            prunePeerMarks target number marks
            { state with Puzzle = puzzle; Marks = marks }
        | RemoveMarks removals ->
            let marks = Array2D.copy state.Marks
            for (position, removed) in removals do
                marks[position.Y, position.X] <- Set.difference marks[position.Y, position.X] removed
            { state with Marks = marks }
        | CorrectMarks corrections ->
            let marks = Array2D.copy state.Marks
            for (position, corrected) in corrections do
                marks[position.Y, position.X] <- corrected
            { state with Marks = marks }

    let isSolved (board : int[,]) =
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

    let hasConflict (board : int[,]) (position : Vector2i) (value : int) =
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
                blockPositions (blockOfPosition position)
                |> List.exists (fun position2 -> position2 <> position && board[position2.Y, position2.X] = value)
            rowConflict || columnConflict || blockConflict

    let isPuzzleSolvedAgainstSolution (solution : int[,]) (puzzle : int[,]) =
        allPositions
        |> List.forall (fun position -> puzzle[position.Y, position.X] = solution[position.Y, position.X])

    let givenFromPuzzle (puzzle : int[,]) =
        let given = Array2D.create 9 9 false
        for position in allPositions do
            given[position.Y, position.X] <- puzzle[position.Y, position.X] <> 0
        given
