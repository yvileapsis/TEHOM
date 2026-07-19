namespace Sudoku
open System
open System.Numerics
open Prime
open Nu
open Sudoku

// this is our top-level MMCC model type. It determines what state the game is in. To learn about MMCC in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Model-View-Update-for-Games-via-MMCC
type Sudoku =
    | Splash
    | Title
    | StudyMap
    | Credits
    | Gameplay

// this is our top-level MMCC message type.
type SudokuMessage =
    | ShowTitle
    | ShowMap
    | ShowCredits
    | ShowGameplay of GameplayContext
    interface Message

// this is our top-level MMCC command type. Commands are used instead of messages when the world is to be transformed.
type SudokuCommand =
    | SetGameplay of GameplayContext
    | Exit
    interface Command

// this extends the Game API to expose the above MMCC model as a property.
[<AutoOpen>]
module SudokuExtensions =
    type Game with
        member this.GetSudoku world = this.GetModelGeneric<Sudoku> world
        member this.SetSudoku value world = this.SetModelGeneric<Sudoku> value world
        member this.Sudoku = this.ModelGeneric<Sudoku> ()

// this is the dispatcher that customizes the top-level behavior of our game. In here, we create screens as content and
// bind them up with events and properties.
type SudokuDispatcher () =
    inherit GameDispatcher<Sudoku, SudokuMessage, SudokuCommand> (Splash)

    // here we define the game's properties and event handling
    override this.Definitions (sudoku, _) =
        [Game.DesiredScreen :=
            match sudoku with
            | Splash -> Desire Simulants.Splash
            | Title -> Desire Simulants.Title
            | StudyMap -> Desire Simulants.Map
            | Credits -> Desire Simulants.Credits
            | Gameplay -> Desire Simulants.Gameplay
         if sudoku = Splash then Simulants.Splash.DeselectingEvent => ShowTitle
         Simulants.TitleCredits.ClickEvent => ShowCredits
         Simulants.TitlePlay.ClickEvent => ShowMap
         Simulants.TitleExit.ClickEvent => Exit
         Simulants.Map.MapBackEvent => ShowTitle
         Simulants.Map.MapNodeSelectedEvent =|> fun evt -> ShowGameplay evt.Data
         Simulants.CreditsBack.ClickEvent => ShowTitle
         Simulants.Gameplay.QuitEvent => ShowMap]

    // here we handle the above messages
    override this.Message (_, message, _, _) =
        match message with
        | ShowTitle -> just Title
        | ShowMap -> just StudyMap
        | ShowCredits -> just Credits
        | ShowGameplay context ->
            withSignal (SetGameplay context) Gameplay

    // here we handle the above commands
    override this.Command (_, command, _, world) =
        match command with
        | SetGameplay context ->
            let gameplay = Simulants.Gameplay.GetGameplay world
            Simulants.Gameplay.SetGameplay { Gameplay.make context gameplay.Score with Display = gameplay.Display } world
        | Exit -> if world.Unaccompanied then World.exit world

    // here we describe the content of the game, including all of its screens
    override this.Content (_, _) =
        [Content.screen Simulants.Splash.Name (Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)) [] []
         Content.screenWithGroupFromFile Simulants.Title.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Title.nugroup" [] []
         Content.screen<ProgressionMapDispatcher> Simulants.Map.Name (Dissolve (Constants.Dissolve.Default, None)) [] []
         Content.screenWithGroupFromFile Simulants.Credits.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Credits.nugroup" [] []
         Content.screen<GameplayDispatcher> Simulants.Gameplay.Name (Dissolve (Constants.Dissolve.Default, None)) [] []]
