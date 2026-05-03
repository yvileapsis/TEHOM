namespace Sudoku
open System
open Nu
open Sudoku

// this is a plugin for the Nu game engine that directs the execution of your application and editor.
type SudokuPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor.
    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetSudoku Splash world)
             ("Title", fun world -> Game.SetSudoku Title world)
             ("Credits", fun world -> Game.SetSudoku Credits world)
             ("Gameplay", fun world ->
                Simulants.Gameplay.SetGameplay Gameplay.initial world
                Game.SetSudoku Gameplay world)]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [Assets.Gui.PackageName
         Assets.Gameplay.PackageName]