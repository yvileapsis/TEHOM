namespace Sudoku
open System.Numerics
open Prime

[<RequireQualifiedAccess>]
module SudokuHints =

    let private pencilMarkCorrections (state : SudokuPuzzle) =
        match SudokuPuzzleDisplay.correctedMarkSets state.Display with
        | (target, _) :: _ as corrections ->
            let correctionPositions =
                corrections
                |> List.map fst
                |> Set.ofList
            [{ Target = target
               Region = HintCells correctionPositions
               Technique = PencilMarkCorrection
               Action = CorrectMarks corrections }]
        | [] -> []

    let private tryMakeHint (technique : HintTechnique) (region : HintRegion) (position : Vector2i) (number : int) (state : SudokuPuzzle) =
        if state.Display.SolutionGrid[position.Y, position.X] = number
        then Some { Target = position; Region = region; Technique = technique; Action = PlaceNumber (position, number) }
        else None

    let private tryMakeEliminationHint (technique : HintTechnique) (region : HintRegion) (eliminations : (Vector2i * Set<int>) list) (state : SudokuPuzzle) =
        let wouldRemoveSolution =
            eliminations
            |> List.exists (fun (position, eliminated) ->
                let solution = state.Display.SolutionGrid[position.Y, position.X]
                let marks = state.Display.Marks[position.Y, position.X]
                Set.contains solution eliminated && Set.contains solution marks)
        if wouldRemoveSolution then None
        else
            let removals =
                eliminations
                |> List.choose (fun (position, eliminated) ->
                    let marks = state.Display.Marks[position.Y, position.X]
                    let removed = Set.intersect marks eliminated
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

    let private fullHousesInUnit (technique : HintTechnique) (region : HintRegion) (positions : Vector2i list) (state : SudokuPuzzle) =
        let emptyPositions = positions |> List.filter (fun position -> state.Display.PuzzleGrid[position.Y, position.X] = 0)
        match emptyPositions with
        | [position] ->
            let existing = set [for position in positions do if state.Display.PuzzleGrid[position.Y, position.X] <> 0 then yield state.Display.PuzzleGrid[position.Y, position.X]]
            let missing = Set.difference (set [1 .. 9]) existing
            if Set.count missing = 1 then
                tryMakeHint technique region position (Set.minElement missing) state |> Option.toList
            else []
        | _ -> []

    let private fullHouseRows (state : SudokuPuzzle) =
        [0 .. 8]
        |> List.collect (fun row ->
            fullHousesInUnit FullHouseRow (HintRow row) (SudokuPuzzleDisplay.rowPositions row) state)

    let private fullHouseColumns (state : SudokuPuzzle) =
        [0 .. 8]
        |> List.collect (fun column ->
            fullHousesInUnit FullHouseColumn (HintColumn column) (SudokuPuzzleDisplay.columnPositions column) state)

    let private fullHouseBlocks (state : SudokuPuzzle) =
        [for y in 0 .. 2 do for x in 0 .. 2 -> v2i x y]
        |> List.collect (fun block ->
            fullHousesInUnit FullHouseBlock (HintBlock block) (SudokuPuzzleDisplay.blockPositions block) state)

    let private nakedSingles (state : SudokuPuzzle) =
        SudokuPuzzleDisplay.allPositions
        |> List.choose (fun position ->
            let candidates = SudokuPuzzleDisplay.workingCandidates state.Display position
            if Set.count candidates = 1 then
                tryMakeHint NakedSingle (HintCell position) position (Set.minElement candidates) state
            else None)

    let private hiddenSinglesInRows (state : SudokuPuzzle) =
        [for row in 0 .. 8 do
            for number in 1 .. 9 do
                let positions =
                    [for x in 0 .. 8 do
                        let position = v2i x row
                        if Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position) then yield position]
                match positions with
                | [position] -> yield (row, number, position)
                | _ -> ()]
        |> List.choose (fun (row : int, number : int, position : Vector2i) ->
            tryMakeHint HiddenSingleRow (HintRow row) position number state)

    let private hiddenSinglesInColumns (state : SudokuPuzzle) =
        [for column in 0 .. 8 do
            for number in 1 .. 9 do
                let positions =
                    [for y in 0 .. 8 do
                        let position = v2i column y
                        if Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position) then yield position]
                match positions with
                | [position] -> yield (column, number, position)
                | _ -> ()]
        |> List.choose (fun (column : int, number : int, position : Vector2i) ->
            tryMakeHint HiddenSingleColumn (HintColumn column) position number state)

    let private hiddenSinglesInBlocks (state : SudokuPuzzle) =
        [for blockY in 0 .. 2 do
            for blockX in 0 .. 2 do
                let block = v2i blockX blockY
                for number in 1 .. 9 do
                    let positions =
                        [for y in blockY * 3 .. blockY * 3 + 2 do
                            for x in blockX * 3 .. blockX * 3 + 2 do
                                let position = v2i x y
                                if Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position) then yield position]
                    match positions with
                    | [position] -> yield (block, number, position)
                    | _ -> ()]
        |> List.choose (fun (block : Vector2i, number : int, position : Vector2i) ->
            tryMakeHint HiddenSingleBlock (HintBlock block) position number state)

    let private nakedSubsetsInUnit (technique : HintTechnique) (region : HintRegion) (size : int) (positions : Vector2i list) (state : SudokuPuzzle) =
        let candidatesByPosition =
            positions
            |> List.choose (fun position ->
                let candidates = SudokuPuzzleDisplay.workingCandidates state.Display position
                if Set.count candidates >= 2 && Set.count candidates <= size then Some (position, candidates)
                else None)
        SudokuPuzzleDisplay.combinations size candidatesByPosition
        |> List.choose (fun subset ->
            let subsetCandidates = subset |> List.map snd |> Set.unionMany
            if Set.count subsetCandidates = size then
                let subsetPositions = subset |> List.map fst |> Set.ofList
                let eliminations =
                    [for position in positions do
                        if not (Set.contains position subsetPositions) && state.Display.PuzzleGrid[position.Y, position.X] = 0 then
                            yield (position, subsetCandidates)]
                tryMakeEliminationHint technique region eliminations state
            else None)

    let private nakedPairsInRows (state : SudokuPuzzle) =
        [0 .. 8]
        |> List.collect (fun row ->
            nakedSubsetsInUnit NakedPairRow (HintRow row) 2 (SudokuPuzzleDisplay.rowPositions row) state)

    let private nakedPairsInColumns (state : SudokuPuzzle) =
        [0 .. 8]
        |> List.collect (fun column ->
            nakedSubsetsInUnit NakedPairColumn (HintColumn column) 2 (SudokuPuzzleDisplay.columnPositions column) state)

    let private nakedPairsInBlocks (state : SudokuPuzzle) =
        [for y in 0 .. 2 do for x in 0 .. 2 -> v2i x y]
        |> List.collect (fun block ->
            nakedSubsetsInUnit NakedPairBlock (HintBlock block) 2 (SudokuPuzzleDisplay.blockPositions block) state)

    let private hiddenSubsetsInUnit (technique : HintTechnique) (size : int) (positions : Vector2i list) (state : SudokuPuzzle) =
        let positionsByNumber =
            [1 .. 9]
            |> List.choose (fun (number : int) ->
                let candidatePositions =
                    positions
                    |> List.filter (fun (position : Vector2i) -> Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position))
                let positionCount = List.length candidatePositions
                if positionCount >= 2 && positionCount <= size then Some (number, candidatePositions)
                else None)
        SudokuPuzzleDisplay.combinations size positionsByNumber
        |> List.choose (fun (subset : (int * Vector2i list) list) ->
            let subsetNumbers =
                subset
                |> List.map fst
                |> Set.ofList
            let subsetPositions =
                subset
                |> List.collect snd
                |> Set.ofList
            if Set.count subsetPositions = size then
                let eliminations =
                    subsetPositions
                    |> Set.toList
                    |> List.map (fun (position : Vector2i) ->
                        let candidates = SudokuPuzzleDisplay.workingCandidates state.Display position
                        (position, Set.difference candidates subsetNumbers))
                tryMakeEliminationHint technique (HintCells subsetPositions) eliminations state
            else None)

    let private hiddenPairsInRows (state : SudokuPuzzle) =
        [0 .. 8]
        |> List.collect (fun row ->
            hiddenSubsetsInUnit HiddenPairRow 2 (SudokuPuzzleDisplay.rowPositions row) state)

    let private hiddenPairsInColumns (state : SudokuPuzzle) =
        [0 .. 8]
        |> List.collect (fun column ->
            hiddenSubsetsInUnit HiddenPairColumn 2 (SudokuPuzzleDisplay.columnPositions column) state)

    let private hiddenPairsInBlocks (state : SudokuPuzzle) =
        [for y in 0 .. 2 do for x in 0 .. 2 -> v2i x y]
        |> List.collect (fun block ->
            hiddenSubsetsInUnit HiddenPairBlock 2 (SudokuPuzzleDisplay.blockPositions block) state)

    let private nakedTriplesInRows (state : SudokuPuzzle) =
        [0 .. 8]
        |> List.collect (fun row ->
            nakedSubsetsInUnit NakedTripleRow (HintRow row) 3 (SudokuPuzzleDisplay.rowPositions row) state)

    let private nakedTriplesInColumns (state : SudokuPuzzle) =
        [0 .. 8]
        |> List.collect (fun column ->
            nakedSubsetsInUnit NakedTripleColumn (HintColumn column) 3 (SudokuPuzzleDisplay.columnPositions column) state)

    let private nakedTriplesInBlocks (state : SudokuPuzzle) =
        [for y in 0 .. 2 do for x in 0 .. 2 -> v2i x y]
        |> List.collect (fun block ->
            nakedSubsetsInUnit NakedTripleBlock (HintBlock block) 3 (SudokuPuzzleDisplay.blockPositions block) state)

    let private hiddenTriplesInRows (state : SudokuPuzzle) =
        [0 .. 8]
        |> List.collect (fun row ->
            hiddenSubsetsInUnit HiddenTripleRow 3 (SudokuPuzzleDisplay.rowPositions row) state)

    let private hiddenTriplesInColumns (state : SudokuPuzzle) =
        [0 .. 8]
        |> List.collect (fun column ->
            hiddenSubsetsInUnit HiddenTripleColumn 3 (SudokuPuzzleDisplay.columnPositions column) state)

    let private hiddenTriplesInBlocks (state : SudokuPuzzle) =
        [for y in 0 .. 2 do for x in 0 .. 2 -> v2i x y]
        |> List.collect (fun block ->
            hiddenSubsetsInUnit HiddenTripleBlock 3 (SudokuPuzzleDisplay.blockPositions block) state)

    let private pointingRowsOrColumns (state : SudokuPuzzle) =
        [for blockY in 0 .. 2 do
            for blockX in 0 .. 2 do
                let block = v2i blockX blockY
                let blockPositions = SudokuPuzzleDisplay.blockPositions block
                for number in 1 .. 9 do
                    let positions =
                        blockPositions
                        |> List.filter (fun position -> Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position))
                    if List.length positions >= 2 then
                        let rows = positions |> List.map (fun position -> position.Y) |> Set.ofList
                        let columns = positions |> List.map (fun position -> position.X) |> Set.ofList
                        if Set.count rows = 1 then
                            let row = Set.minElement rows
                            let eliminations =
                                [for position in SudokuPuzzleDisplay.rowPositions row do
                                    if SudokuPuzzleDisplay.blockOfPosition position <> block && Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position) then
                                        yield (position, set [number])]
                            yield (PointingRow, HintCells (Set.ofList positions), eliminations)
                        if Set.count columns = 1 then
                            let column = Set.minElement columns
                            let eliminations =
                                [for position in SudokuPuzzleDisplay.columnPositions column do
                                    if SudokuPuzzleDisplay.blockOfPosition position <> block && Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position) then
                                        yield (position, set [number])]
                            yield (PointingColumn, HintCells (Set.ofList positions), eliminations)]
        |> List.choose (fun (technique : HintTechnique, region : HintRegion, eliminations : (Vector2i * Set<int>) list) ->
            tryMakeEliminationHint technique region eliminations state)

    let private claimingRowsOrColumns (state : SudokuPuzzle) =
        let rowClaims =
            [for row in 0 .. 8 do
                let rowPositions = SudokuPuzzleDisplay.rowPositions row
                for number in 1 .. 9 do
                    let positions =
                        rowPositions
                        |> List.filter (fun position -> Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position))
                    if List.length positions >= 2 then
                        let blocks = positions |> List.map SudokuPuzzleDisplay.blockOfPosition |> Set.ofList
                        if Set.count blocks = 1 then
                            let block = Set.minElement blocks
                            let eliminations =
                                [for position in SudokuPuzzleDisplay.blockPositions block do
                                    if position.Y <> row && Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position) then
                                        yield (position, set [number])]
                            yield (ClaimingRow, HintCells (Set.ofList positions), eliminations)]
        let columnClaims =
            [for column in 0 .. 8 do
                let columnPositions = SudokuPuzzleDisplay.columnPositions column
                for number in 1 .. 9 do
                    let positions =
                        columnPositions
                        |> List.filter (fun position -> Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position))
                    if List.length positions >= 2 then
                        let blocks = positions |> List.map SudokuPuzzleDisplay.blockOfPosition |> Set.ofList
                        if Set.count blocks = 1 then
                            let block = Set.minElement blocks
                            let eliminations =
                                [for position in SudokuPuzzleDisplay.blockPositions block do
                                    if position.X <> column && Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position) then
                                        yield (position, set [number])]
                            yield (ClaimingColumn, HintCells (Set.ofList positions), eliminations)]
        rowClaims @ columnClaims
        |> List.choose (fun (technique : HintTechnique, region : HintRegion, eliminations : (Vector2i * Set<int>) list) ->
            tryMakeEliminationHint technique region eliminations state)

    let private fishRows (size : int) (technique : HintTechnique) (state : SudokuPuzzle) =
        [for number in 1 .. 9 do
            let rows =
                [for row in 0 .. 8 do
                    let columns =
                        [for position in SudokuPuzzleDisplay.rowPositions row do
                            if Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position) then yield position.X]
                    if List.length columns >= 2 && List.length columns <= size then yield (row, Set.ofList columns)]
            for rowSet in SudokuPuzzleDisplay.combinations size rows do
                let columns = rowSet |> List.map snd |> Set.unionMany
                if Set.count columns = size then
                    let selectedRows = rowSet |> List.map fst |> Set.ofList
                    let eliminations =
                        [for column in columns do
                            for row in 0 .. 8 do
                                if not (Set.contains row selectedRows) then
                                    let position = v2i column row
                                    if Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position) then
                                        yield (position, set [number])]
                    let regionPositions =
                        [for row in selectedRows do
                            for column in columns -> v2i column row]
                        |> Set.ofList
                    yield (technique, HintCells regionPositions, eliminations)]
        |> List.choose (fun (technique : HintTechnique, region : HintRegion, eliminations : (Vector2i * Set<int>) list) ->
            tryMakeEliminationHint technique region eliminations state)

    let private fishColumns (size : int) (technique : HintTechnique) (state : SudokuPuzzle) =
        [for number in 1 .. 9 do
            let columns =
                [for column in 0 .. 8 do
                    let rows =
                        [for position in SudokuPuzzleDisplay.columnPositions column do
                            if Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position) then yield position.Y]
                    if List.length rows >= 2 && List.length rows <= size then yield (column, Set.ofList rows)]
            for columnSet in SudokuPuzzleDisplay.combinations size columns do
                let rows = columnSet |> List.map snd |> Set.unionMany
                if Set.count rows = size then
                    let selectedColumns = columnSet |> List.map fst |> Set.ofList
                    let eliminations =
                        [for row in rows do
                            for column in 0 .. 8 do
                                if not (Set.contains column selectedColumns) then
                                    let position = v2i column row
                                    if Set.contains number (SudokuPuzzleDisplay.workingCandidates state.Display position) then
                                        yield (position, set [number])]
                    let regionPositions =
                        [for column in selectedColumns do
                            for row in rows -> v2i column row]
                        |> Set.ofList
                    yield (technique, HintCells regionPositions, eliminations)]
        |> List.choose (fun (technique : HintTechnique, region : HintRegion, eliminations : (Vector2i * Set<int>) list) ->
            tryMakeEliminationHint technique region eliminations state)

    let private hintGroups =
        [pencilMarkCorrections
         fullHouseRows
         fullHouseColumns
         fullHouseBlocks
         nakedSingles
         hiddenSinglesInRows
         hiddenSinglesInColumns
         hiddenSinglesInBlocks
         nakedPairsInRows
         nakedPairsInColumns
         nakedPairsInBlocks
         hiddenPairsInRows
         hiddenPairsInColumns
         hiddenPairsInBlocks
         nakedTriplesInRows
         nakedTriplesInColumns
         nakedTriplesInBlocks
         hiddenTriplesInRows
         hiddenTriplesInColumns
         hiddenTriplesInBlocks
         pointingRowsOrColumns
         claimingRowsOrColumns
         fishRows 2 XWingRow
         fishColumns 2 XWingColumn
         fishRows 3 SwordfishRow
         fishColumns 3 SwordfishColumn]

    let findAll (state : SudokuPuzzle) =
        hintGroups
        |> List.collect (fun find -> find state)

    let findFirst (state : SudokuPuzzle) =
        hintGroups
        |> List.tryPick (fun find ->
            match find state with
            | hint :: _ -> Some hint
            | [] -> None)
