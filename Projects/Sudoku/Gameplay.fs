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
    | Unavailable
    | Quit

// this contains display settings for the gameplay screen that can be adjusted from Gaia.
type [<SymbolicExpansion>] GameplayDisplay =
    { BoardCenter : Vector2
      CellSize : single
      CellGap : single
      BoardLineThin : single
      BoardLineThick : single
      NumberStatusOffsetX : single
      NumberStatusTitleOffsetY : single
      NumberStatusPanelSize : Vector3
      NumberStatusValueOffsetY : single
      NumberStatusRemainingOffsetY : single
      NumberStatusValueSize : Vector3
      NumberStatusRemainingSize : Vector3
      MarkSpacing : single
      MarkSize : Vector3
      TitlePosition : Vector3
      ScorePosition : Vector3
      StatusPosition : Vector3
      StatusSize : Vector3
      HeaderPosition : Vector3
      HeaderSize : Vector3
      InputModePosition : Vector3
      InputModeSize : Vector3
      DifficultyButtonOrigin : Vector2
      DifficultyButtonSpacing : Vector2
      DifficultyButtonSize : Vector3
      ActionButtonOrigin : Vector2
      ActionButtonSpacingY : single
      ActionButtonSize : Vector3
      HintTargetColor : Color
      HintRegionColor : Color
      SelectedCellColor : Color
      ConflictCellColor : Color
      SelectedNumberCellColor : Color
      SelectedAxisCellColor : Color
      GivenCellColor : Color
      EmptyCellColor : Color
      PlayerCellColor : Color
      NumberStatusCompleteColor : Color
      NumberStatusBlockMissingColor : Color
      NumberStatusAvailableColor : Color
      NumberStatusCompleteTextColor : Color
      NumberStatusAvailableTextColor : Color
      ButtonSelectedColor : Color
      ButtonNormalColor : Color
      GivenTextColor : Color
      PlayerTextColor : Color
      MarkTextColor : Color
      GridLineColor : Color }

    member this.BoardSize = this.CellSize * 9.0f

    member this.BoardMin =
        this.BoardCenter - v2Dup (this.BoardSize * 0.5f)

    static member initial =
        { BoardCenter = v2 -52.0f -10.0f
          CellSize = 34.0f
          CellGap = 2.0f
          BoardLineThin = 1.5f
          BoardLineThick = 4.0f
          NumberStatusOffsetX = -38.0f
          NumberStatusTitleOffsetY = 16.0f
          NumberStatusPanelSize = v3 28.0f 28.0f 0.0f
          NumberStatusValueOffsetY = 3.0f
          NumberStatusRemainingOffsetY = -8.0f
          NumberStatusValueSize = v3 28.0f 18.0f 0.0f
          NumberStatusRemainingSize = v3 28.0f 10.0f 0.0f
          MarkSpacing = 9.0f
          MarkSize = v3 10.0f 10.0f 0.0f
          TitlePosition = v3 -52.0f 176.0f 0.0f
          ScorePosition = v3 196.0f 142.0f 0.0f
          StatusPosition = v3 196.0f 102.0f 0.0f
          StatusSize = v3 178.0f 48.0f 0.0f
          HeaderPosition = v3 196.0f 66.0f 0.0f
          HeaderSize = v3 170.0f 24.0f 0.0f
          InputModePosition = v3 196.0f 48.0f 0.0f
          InputModeSize = v3 170.0f 20.0f 0.0f
          DifficultyButtonOrigin = v2 160.0f 22.0f
          DifficultyButtonSpacing = v2 96.0f -34.0f
          DifficultyButtonSize = v3 86.0f 28.0f 0.0f
          ActionButtonOrigin = v2 196.0f -52.0f
          ActionButtonSpacingY = -34.0f
          ActionButtonSize = v3 128.0f 28.0f 0.0f
          HintTargetColor = color 0.78f 0.58f 0.14f 1.0f
          HintRegionColor = color 0.18f 0.40f 0.26f 1.0f
          SelectedCellColor = color 0.34f 0.54f 0.80f 1.0f
          ConflictCellColor = color 0.58f 0.16f 0.16f 1.0f
          SelectedNumberCellColor = color 0.25f 0.37f 0.58f 1.0f
          SelectedAxisCellColor = color 0.16f 0.21f 0.27f 1.0f
          GivenCellColor = color 0.13f 0.16f 0.20f 1.0f
          EmptyCellColor = color 0.10f 0.12f 0.15f 1.0f
          PlayerCellColor = color 0.19f 0.24f 0.30f 1.0f
          NumberStatusCompleteColor = color 0.10f 0.12f 0.14f 1.0f
          NumberStatusBlockMissingColor = color 0.78f 0.58f 0.14f 1.0f
          NumberStatusAvailableColor = color 0.22f 0.36f 0.54f 1.0f
          NumberStatusCompleteTextColor = color 0.44f 0.48f 0.52f 1.0f
          NumberStatusAvailableTextColor = Color.GhostWhite
          ButtonSelectedColor = color 0.30f 0.48f 0.72f 1.0f
          ButtonNormalColor = color 0.18f 0.22f 0.27f 1.0f
          GivenTextColor = color 0.78f 0.82f 0.88f 1.0f
          PlayerTextColor = color 0.88f 0.97f 1.0f 1.0f
          MarkTextColor = color 0.66f 0.78f 0.90f 1.0f
          GridLineColor = color 0.05f 0.06f 0.07f 1.0f }

// this is our MMCC model type representing gameplay.
type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      Puzzle : SudokuPuzzle
      Display : GameplayDisplay
      SelectedCellOpt : Vector2i option
      PuzzleSource : PuzzleSource
      Difficulty : Difficulty
      PencilMode : bool
      HintOpt : Hint option
      HintStatusOpt : string option
      PuzzleNumber : int
      Score : int }

    member this.BoardSize = v2iDup 9

    member this.IsSolved =
        SudokuPuzzleDisplay.isSolved this.Puzzle.Display.PuzzleGrid

    member this.HasConflict (position : Vector2i) (value : int) =
        SudokuPuzzleDisplay.hasConflict this.Puzzle.Display.PuzzleGrid position value

    static member private fillLegalMarks (gameplay : Gameplay) =
        let puzzle = gameplay.Puzzle.Rehydrate ()
        let display = SudokuPuzzleDisplay.fillLegalMarks puzzle.Display
        { gameplay with
            Puzzle = { puzzle with DisplayOpt = Some display }
            PencilMode = true
            HintOpt = None
            HintStatusOpt = Some "No immediate hint found; filled legal pencil marks." }

    static member private hasOpenCellsWithoutMarks (gameplay : Gameplay) =
        SudokuPuzzleDisplay.hasOpenCellsWithoutMarks (gameplay.Puzzle.Rehydrate ()).Display

    static member private withNumberAt (position : Vector2i) (number : int) (gameplay : Gameplay) =
        let sudoku = gameplay.Puzzle.Rehydrate ()
        let display = sudoku.Display
        if gameplay.GameplayState = Playing && not display.Given[position.Y, position.X] then
            let grid = Array2D.copy display.PuzzleGrid
            let marks = Array2D.copy display.Marks
            grid[position.Y, position.X] <- number
            marks[position.Y, position.X] <- Set.empty
            SudokuPuzzleDisplay.prunePeerMarks position number marks
            let sudoku = { sudoku with DisplayOpt = Some { display with PuzzleGrid = grid; Marks = marks } }
            let gameplay =
                { gameplay with
                    Puzzle = sudoku
                    HintOpt = None
                    HintStatusOpt = None }
            if SudokuPuzzleDisplay.isSolved grid then { gameplay with GameplayState = Won; Score = inc gameplay.Score }
            else gameplay
        else gameplay

    static member private removeMarks (removals : (Vector2i * Set<int>) list) (gameplay : Gameplay) =
        let sudoku = gameplay.Puzzle.Rehydrate ()
        let display = sudoku.Display
        let marks = Array2D.copy display.Marks
        for (position, removed) in removals do
            marks[position.Y, position.X] <- Set.difference marks[position.Y, position.X] removed
        let sudoku = { sudoku with DisplayOpt = Some { display with Marks = marks } }
        { gameplay with
            Puzzle = sudoku
            HintOpt = None
            HintStatusOpt = None }

    static member private correctMarks (corrections : (Vector2i * Set<int>) list) (gameplay : Gameplay) =
        let sudoku = gameplay.Puzzle.Rehydrate ()
        let display = sudoku.Display
        let marks = Array2D.copy display.Marks
        for (position, corrected) in corrections do
            marks[position.Y, position.X] <- corrected
        let sudoku = { sudoku with DisplayOpt = Some { display with Marks = marks } }
        { gameplay with
            Puzzle = sudoku
            HintOpt = None
            HintStatusOpt = None }

    static member private toggleMarkAt (position : Vector2i) (number : int) (gameplay : Gameplay) =
        let sudoku = gameplay.Puzzle.Rehydrate ()
        let display = sudoku.Display
        if gameplay.GameplayState = Playing && not display.Given[position.Y, position.X] && display.PuzzleGrid[position.Y, position.X] = 0 then
            let marks = Array2D.copy display.Marks
            marks[position.Y, position.X] <-
                if Set.contains number marks[position.Y, position.X]
                then Set.remove number marks[position.Y, position.X]
                else Set.add number marks[position.Y, position.X]
            let sudoku = { sudoku with DisplayOpt = Some { display with Marks = marks } }
            { gameplay with
                Puzzle = sudoku
                HintOpt = None
                HintStatusOpt = None }
        else gameplay

    static member public withNumber (number : int) (gameplay : Gameplay) =
        let sudoku = gameplay.Puzzle.Rehydrate ()
        let display = sudoku.Display
        match gameplay.SelectedCellOpt with
        | Some position when gameplay.GameplayState = Playing && not display.Given[position.Y, position.X] && gameplay.PencilMode && number <> 0 ->
            Gameplay.toggleMarkAt position number gameplay
        | Some position when gameplay.GameplayState = Playing && not display.Given[position.Y, position.X] ->
            Gameplay.withNumberAt position number gameplay
        | _ -> gameplay

    static member public clearSelected (gameplay : Gameplay) =
        let sudoku = gameplay.Puzzle.Rehydrate ()
        let display = sudoku.Display
        match gameplay.SelectedCellOpt with
        | Some position when gameplay.GameplayState = Playing && not display.Given[position.Y, position.X] ->
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
                | CorrectMarks corrections ->
                    let correctedCount = List.length corrections
                    let gameplay = Gameplay.correctMarks corrections gameplay
                    { gameplay with HintStatusOpt = Some ("Corrected pencil marks in " + string correctedCount + " cell" + (if correctedCount = 1 then "" else "s") + ".") }
            | None ->
                match SudokuHints.findFirst (gameplay.Puzzle.Rehydrate ()) with
                | Some hint ->
                    let applyText =
                        match hint.Action with
                        | PlaceNumber _ -> " Press Hint again to place it."
                        | RemoveMarks _ -> " Press Hint again to remove the marks."
                        | CorrectMarks _ -> " Press Hint again to update them."
                    let hintText =
                        match hint.Action with
                        | CorrectMarks _ -> "Pencil marks can be corrected."
                        | _ -> hint.Label
                    { gameplay with
                        HintOpt = Some hint
                        HintStatusOpt = Some (hintText + applyText)
                        SelectedCellOpt = Some hint.Target }
                | None ->
                    if Gameplay.hasOpenCellsWithoutMarks gameplay
                    then Gameplay.fillLegalMarks gameplay
                    else
                        { gameplay with
                            HintOpt = None
                            HintStatusOpt = Some "No hint available from the current pencil marks." }
        else gameplay

    static member private inertPuzzle =
        SudokuPuzzle.empty

    static member private makeFromPuzzle source difficulty score (sudoku : SudokuPuzzle) =
        let sudoku = sudoku.Rehydrate ()
        { GameplayTime = 0L
          GameplayState = Playing
          Puzzle = sudoku
          Display = GameplayDisplay.initial
          SelectedCellOpt = None
          PuzzleSource = source
          Difficulty = difficulty
          PencilMode = false
          HintOpt = None
          HintStatusOpt = None
          PuzzleNumber = sudoku.Number
          Score = score }

    static member private unavailable source difficulty score =
        let sudoku = Gameplay.inertPuzzle.Rehydrate ()
        { GameplayTime = 0L
          GameplayState = Unavailable
          Puzzle = sudoku
          Display = GameplayDisplay.initial
          SelectedCellOpt = None
          PuzzleSource = source
          Difficulty = difficulty
          PencilMode = false
          HintOpt = None
          HintStatusOpt = Some ("No imported " + difficulty.Label + " classic puzzle is available.")
          PuzzleNumber = 0
          Score = score }

    static member make source difficulty score =
        match PuzzleBank.tryTake source difficulty with
        | Some generated -> Gameplay.makeFromPuzzle source difficulty score generated
        | None ->
            match source with
            | Generated -> Gameplay.makeFromPuzzle source difficulty score (PuzzleGeneration.make difficulty)
            | Classic -> Gameplay.unavailable source difficulty score

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        let sudoku = Gameplay.inertPuzzle.Rehydrate ()
        { GameplayTime = 0L
          GameplayState = Quit
          Puzzle = sudoku
          Display = GameplayDisplay.initial
          SelectedCellOpt = None
          PuzzleSource = Generated
          Difficulty = Normal
          PencilMode = false
          HintOpt = None
          HintStatusOpt = None
          PuzzleNumber = sudoku.Number
          Score = 0 }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = Gameplay.make Generated Normal 0

// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartPlaying of PuzzleSource
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

    static let tryMouseCell (display : GameplayDisplay) (world : World) =
        let mouse = World.getMousePosition2dWorld false world
        let boardMin = display.BoardMin
        let boardSize = display.BoardSize
        let local = mouse - boardMin
        if local.X >= 0.0f && local.Y >= 0.0f && local.X < boardSize && local.Y < boardSize then
            let column = int (local.X / display.CellSize)
            let row = 8 - int (local.Y / display.CellSize)
            Some (v2i column row)
        else None

    static let cellPosition (display : GameplayDisplay) (x : int) (y : int) =
        let boardMin = display.BoardMin
        v3 (boardMin.X + (single x + 0.5f) * display.CellSize) (boardMin.Y + (single (8 - y) + 0.5f) * display.CellSize) 0.0f

    static let numberStatusPosition (display : GameplayDisplay) (number : int) =
        let boardMin = display.BoardMin
        v3 (boardMin.X + display.NumberStatusOffsetX) (boardMin.Y + (single (9 - number) + 0.5f) * display.CellSize) 0.0f

    static let markPositionLocal (display : GameplayDisplay) (number : int) =
        let index = number - 1
        let column = index % 3
        let row = index / 3
        v3 ((single column - 1.0f) * display.MarkSpacing) ((1.0f - single row) * display.MarkSpacing) 0.0f

    static let positionInHintRegion (hint : Hint) (position : Vector2i) =
        match hint.Region with
        | HintCell cell -> position = cell
        | HintCells cells -> Set.contains position cells
        | HintRow row -> position.Y = row
        | HintColumn column -> position.X = column
        | HintBlock block -> position.X / 3 = block.X && position.Y / 3 = block.Y

    static let selectedCellValue (gameplay : Gameplay) =
        gameplay.SelectedCellOpt
        |> Option.map (fun selected -> gameplay.Puzzle.Display.PuzzleGrid[selected.Y, selected.X])
        |> Option.defaultValue 0

    static let positionSharesSelectedAxis (gameplay : Gameplay) (position : Vector2i) =
        match gameplay.SelectedCellOpt with
        | Some selected -> position.X = selected.X || position.Y = selected.Y
        | None -> false

    static let positionHasSelectedNumber (gameplay : Gameplay) (position : Vector2i) (value : int) =
        let selectedValue = selectedCellValue gameplay
        selectedValue <> 0 && gameplay.SelectedCellOpt <> Some position && value = selectedValue

    static let cellColor (gameplay : Gameplay) (position : Vector2i) (value : int) =
        let display = gameplay.Display
        match gameplay.HintOpt with
        | Some hint when position = hint.Target -> display.HintTargetColor
        | Some hint when positionInHintRegion hint position -> display.HintRegionColor
        | _ ->
            if gameplay.SelectedCellOpt = Some position then display.SelectedCellColor
            elif gameplay.HasConflict position value then display.ConflictCellColor
            elif positionHasSelectedNumber gameplay position value then display.SelectedNumberCellColor
            elif positionSharesSelectedAxis gameplay position then display.SelectedAxisCellColor
            elif gameplay.Puzzle.Display.Given[position.Y, position.X] then display.GivenCellColor
            elif value = 0 then display.EmptyCellColor
            else display.PlayerCellColor

    static let numberRemaining (gameplay : Gameplay) (number : int) =
        let puzzleDisplay = gameplay.Puzzle.Display
        9 - List.length [for y in 0 .. 8 do for x in 0 .. 8 do if puzzleDisplay.PuzzleGrid[y, x] = number then yield number]

    static let numberMissingFromSelectedBlock (gameplay : Gameplay) (number : int) =
        match gameplay.SelectedCellOpt with
        | Some selected ->
            let block = SudokuPuzzleDisplay.blockOfPosition selected
            block
            |> SudokuPuzzleDisplay.blockPositions
            |> List.exists (fun (position : Vector2i) -> gameplay.Puzzle.Display.PuzzleGrid[position.Y, position.X] = number)
            |> not
        | None -> false

    static let numberStatusColor (gameplay : Gameplay) (number : int) =
        let display = gameplay.Display
        let remaining = numberRemaining gameplay number
        if remaining <= 0 then display.NumberStatusCompleteColor
        elif numberMissingFromSelectedBlock gameplay number then display.NumberStatusBlockMissingColor
        else display.NumberStatusAvailableColor

    static let numberStatusTextColor (gameplay : Gameplay) (number : int) =
        if numberRemaining gameplay number <= 0 then gameplay.Display.NumberStatusCompleteTextColor
        else gameplay.Display.NumberStatusAvailableTextColor

    static let difficultyButtonColor (display : GameplayDisplay) (selected : Difficulty) (difficulty : Difficulty) =
        if selected = difficulty then display.ButtonSelectedColor
        else display.ButtonNormalColor

    static let puzzleHeaderText (gameplay : Gameplay) =
        match gameplay.GameplayState, gameplay.PuzzleNumber with
        | Unavailable, _ ->
            gameplay.PuzzleSource.Label + ": unavailable - " + gameplay.Difficulty.Label
        | _, number when number > 0 ->
            gameplay.PuzzleSource.Label + " #" + string number + " - " + gameplay.Difficulty.Label
        | _ ->
            gameplay.PuzzleSource.Label + ": Live - " + gameplay.Difficulty.Label

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) =
        [//Screen.SelectEvent => StartPlaying
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
        | StartPlaying source ->
            just { Gameplay.make source gameplay.Difficulty gameplay.Score with Display = gameplay.Display }

        | FinishQuitting ->
            just { gameplay with GameplayState = Quit; SelectedCellOpt = None; HintOpt = None; HintStatusOpt = None }

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            just { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }

        | SelectCellAtMouse ->
            match tryMouseCell gameplay.Display world with
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
            just { Gameplay.make gameplay.PuzzleSource difficulty gameplay.Score with Display = gameplay.Display }

        | RequestHint ->
            just (Gameplay.withHint gameplay)

        | Restart ->
            just { Gameplay.make gameplay.PuzzleSource gameplay.Difficulty gameplay.Score with Display = gameplay.Display }


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
            let display = gameplay.Display
            let puzzleDisplay = gameplay.Puzzle.Display
            let boardMin = display.BoardMin
            let boardSize = display.BoardSize
            let cellSize = display.CellSize
            Content.group Simulants.GameplayScene.Name []

                [Content.text "Title"
                    [Entity.Position := display.TitlePosition
                     Entity.Elevation == 10.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 22.0f
                     Entity.Text == "Sudoku"]

                 Content.text "Score"
                    [Entity.Position := display.ScorePosition
                     Entity.Elevation == 10.0f
                     Entity.Text := "Score: " + string gameplay.Score]

                 Content.text "Status"
                    [Entity.Position := display.StatusPosition
                     Entity.Size := display.StatusSize
                     Entity.Elevation == 10.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 8.0f
                     Entity.Text :=
                        match gameplay.GameplayState with
                        | Won -> "You won!"
                        | Unavailable ->
                            match gameplay.HintStatusOpt with
                            | Some status -> status
                            | None -> "No imported puzzle is available."
                        | Playing ->
                            match gameplay.HintStatusOpt with
                            | Some status -> status
                            | None -> "Fill every row, column, and block."
                        | Quit -> ""]

                 Content.text "Difficulty"
                    [Entity.Position := display.HeaderPosition
                     Entity.Size := display.HeaderSize
                     Entity.Elevation == 10.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 9.0f
                     Entity.Text := puzzleHeaderText gameplay]

                 Content.text "InputMode"
                    [Entity.Position := display.InputModePosition
                     Entity.Size := display.InputModeSize
                     Entity.Elevation == 10.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 8.0f
                     Entity.Text := if gameplay.PencilMode then "Pencil marks" else "Normal entry"]

                 for (i, difficulty) in List.indexed [Trivial; Easy; Normal; Hard] do
                    Content.button ("Difficulty+" + difficulty.Label)
                        [Entity.Position := v3 (display.DifficultyButtonOrigin.X + single (i % 2) * display.DifficultyButtonSpacing.X) (display.DifficultyButtonOrigin.Y + single (i / 2) * display.DifficultyButtonSpacing.Y) 0.0f
                         Entity.Size := display.DifficultyButtonSize
                         Entity.Elevation == 10.0f
                         Entity.Color := difficultyButtonColor display gameplay.Difficulty difficulty
                         Entity.Text := difficulty.Label
                         Entity.ClickEvent => SetDifficulty difficulty]

                 Content.text "NumberStatusTitle"
                    [Entity.Position := v3 (boardMin.X + display.NumberStatusOffsetX) (boardMin.Y + boardSize + display.NumberStatusTitleOffsetY) 0.0f
                     Entity.Size == v3 52.0f 18.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 7.0f
                     Entity.Text == "Needed"]

                 for number in 1 .. 9 do
                    Content.panel ("NumberStatus+" + string number)
                        [Entity.Position := numberStatusPosition display number
                         Entity.Size := display.NumberStatusPanelSize
                         Entity.Elevation == 4.0f
                         Entity.BackdropImageOpt == Some Assets.Default.White
                         Entity.Color := numberStatusColor gameplay number]
                        [Content.text "Value"
                            [Entity.PositionLocal := v3 0.0f display.NumberStatusValueOffsetY 0.0f
                             Entity.Size := display.NumberStatusValueSize
                             Entity.ElevationLocal == 1.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.FontSizing == Some 10.0f
                             Entity.TextColor := numberStatusTextColor gameplay number
                             Entity.Text == string number]
                         Content.text "Remaining"
                            [Entity.PositionLocal := v3 0.0f display.NumberStatusRemainingOffsetY 0.0f
                             Entity.Size := display.NumberStatusRemainingSize
                             Entity.ElevationLocal == 1.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.FontSizing == Some 5.0f
                             Entity.TextColor := numberStatusTextColor gameplay number
                             Entity.Text := string (numberRemaining gameplay number)]]

                 for y in 0 .. 8 do
                    for x in 0 .. 8 do
                        let value = puzzleDisplay.PuzzleGrid[y, x]
                        let marks = puzzleDisplay.Marks[y, x]
                        let position = v2i x y
                        Content.panel ("Cell+" + string x + "+" + string y)
                            [Entity.Position := cellPosition display x y
                             Entity.Size := v3 (cellSize - display.CellGap) (cellSize - display.CellGap) 0.0f
                             Entity.Elevation == 1.0f
                             Entity.BackdropImageOpt == Some Assets.Default.White
                             Entity.Color := cellColor gameplay position value]
                            [if value <> 0 then
                                Content.text "Value"
                                    [Entity.PositionLocal == v3Zero
                                     Entity.Size := v3 (cellSize - display.CellGap) (cellSize - display.CellGap) 0.0f
                                     Entity.ElevationLocal == 1.0f
                                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                                     Entity.FontSizing := if puzzleDisplay.Given[y, x] then Some 15.0f else Some 16.0f
                                     Entity.TextColor := if puzzleDisplay.Given[y, x] then display.GivenTextColor else display.PlayerTextColor
                                     Entity.Text := string value]
                             else
                                for mark in marks do
                                    Content.text ("Mark+" + string mark)
                                        [Entity.PositionLocal := markPositionLocal display mark
                                         Entity.Size := display.MarkSize
                                         Entity.ElevationLocal == 1.0f
                                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                                         Entity.FontSizing == Some 6.0f
                                         Entity.TextColor := display.MarkTextColor
                                         Entity.Text == string mark]]

                 for i in 0 .. 9 do
                    let lineSize = if i % 3 = 0 then display.BoardLineThick else display.BoardLineThin
                    let lineOffset = single i * cellSize
                    Content.staticSprite ("LineV+" + string i)
                        [Entity.Position := v3 (boardMin.X + lineOffset) display.BoardCenter.Y 0.0f
                         Entity.Size := v3 lineSize boardSize 0.0f
                         Entity.Elevation == 6.0f
                         Entity.StaticImage == Assets.Default.White
                         Entity.Color := display.GridLineColor]
                    Content.staticSprite ("LineH+" + string i)
                        [Entity.Position := v3 display.BoardCenter.X (boardMin.Y + lineOffset) 0.0f
                         Entity.Size := v3 boardSize lineSize 0.0f
                         Entity.Elevation == 6.0f
                         Entity.StaticImage == Assets.Default.White
                         Entity.Color := display.GridLineColor]

                 Content.button "Hint"
                    [Entity.Position := v3 display.ActionButtonOrigin.X display.ActionButtonOrigin.Y 0.0f
                     Entity.Size := display.ActionButtonSize
                     Entity.Elevation == 10.0f
                     Entity.Text := if gameplay.HintOpt.IsSome then "Apply Hint" else "Hint"
                     Entity.ClickEvent => RequestHint]

                 Content.button "Pencil"
                    [Entity.Position := v3 display.ActionButtonOrigin.X (display.ActionButtonOrigin.Y + display.ActionButtonSpacingY) 0.0f
                     Entity.Size := display.ActionButtonSize
                     Entity.Elevation == 10.0f
                     Entity.Color := if gameplay.PencilMode then display.ButtonSelectedColor else display.ButtonNormalColor
                     Entity.Text := if gameplay.PencilMode then "Pencil On" else "Pencil Off"
                     Entity.ClickEvent => TogglePencilMode]

                 Content.button "Restart"
                    [Entity.Position := v3 display.ActionButtonOrigin.X (display.ActionButtonOrigin.Y + display.ActionButtonSpacingY * 2.0f) 0.0f
                     Entity.Size := display.ActionButtonSize
                     Entity.Elevation == 10.0f
                     Entity.Text := if gameplay.GameplayState = Won then "New Board" else "Restart"
                     Entity.ClickEvent => Restart]

                 Content.button Simulants.GameplayQuit.Name
                    [Entity.Position := v3 display.ActionButtonOrigin.X (display.ActionButtonOrigin.Y + display.ActionButtonSpacingY * 3.0f) 0.0f
                     Entity.Size := display.ActionButtonSize
                     Entity.Elevation == 10.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]]
