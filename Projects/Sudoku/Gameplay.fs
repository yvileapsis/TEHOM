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
      TechniqueCounts : Map<HintTechnique, int>
      GenerationScore : int
      MaxEliminationChain : int
      Score : int }

    member this.BoardSize = v2iDup 9

    member this.IsSolved =
        SudokuGrid.isSolved this.Puzzle

    member this.HasConflict (position : Vector2i) (value : int) =
        SudokuGrid.hasConflict this.Puzzle position value

    static member private toBoardState (gameplay : Gameplay) =
        { Puzzle = gameplay.Puzzle
          Solution = gameplay.Solution
          Given = gameplay.Given
          Marks = gameplay.Marks }

    static member private fillLegalMarks (gameplay : Gameplay) =
        let state = Gameplay.toBoardState gameplay |> SudokuGrid.fillLegalMarks
        { gameplay with
            Marks = state.Marks
            PencilMode = true
            HintOpt = None
            HintStatusOpt = Some "No immediate hint found; filled legal pencil marks." }

    static member private hasOpenCellsWithoutMarks (gameplay : Gameplay) =
        gameplay
        |> Gameplay.toBoardState
        |> SudokuGrid.hasOpenCellsWithoutMarks

    static member private withNumberAt (position : Vector2i) (number : int) (gameplay : Gameplay) =
        if gameplay.GameplayState = Playing && not gameplay.Given[position.Y, position.X] then
            let puzzle = Array2D.copy gameplay.Puzzle
            let marks = Array2D.copy gameplay.Marks
            puzzle[position.Y, position.X] <- number
            marks[position.Y, position.X] <- Set.empty
            SudokuGrid.prunePeerMarks position number marks
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

    static member private correctMarks (corrections : (Vector2i * Set<int>) list) (gameplay : Gameplay) =
        let marks = Array2D.copy gameplay.Marks
        for (position, corrected) in corrections do
            marks[position.Y, position.X] <- corrected
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
                | CorrectMarks corrections ->
                    let correctedCount = List.length corrections
                    let gameplay = Gameplay.correctMarks corrections gameplay
                    { gameplay with HintStatusOpt = Some ("Corrected pencil marks in " + string correctedCount + " cell" + (if correctedCount = 1 then "" else "s") + ".") }
            | None ->
                match SudokuHints.findFirst true (Gameplay.toBoardState gameplay) with
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

    static member make (difficulty : Difficulty) (score : int) =
        let generated = PuzzleGeneration.make difficulty
        { GameplayTime = 0L
          GameplayState = Playing
          Puzzle = generated.Puzzle
          Solution = generated.Solution
          Given = generated.Given
          Marks = SudokuGrid.makeMarks ()
          SelectedCellOpt = Some (v2i 0 0)
          Difficulty = difficulty
          PencilMode = false
          HintOpt = None
          HintStatusOpt = None
          TechniqueCounts = generated.TechniqueCounts
          GenerationScore = generated.GenerationScore
          MaxEliminationChain = generated.MaxEliminationChain
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
            let block = SudokuGrid.blockOfPosition selected
            block
            |> SudokuGrid.blockPositions
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
