namespace Sudoku
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

type Board =
    | NotTouched
    | Cells
    | Buttons

type Status =
    | Revealed
    | Unknown
    | Known
    | Mistake of Int32

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing of bool
    | Quit

type Value = Value of int

// this is our MMCC model type representing gameplay.
type Gameplay = {
    GameplayTime : int64
    GameplayState : GameplayState

    LastTouched : Board

    ButtonsSize : Vector2i
    ButtonsValues : Value array
    ButtonsSelected : int option

    CellsSize : Vector2i
    CellsValues : Value array
    CellsStatuses : Status array
    CellsSelected : int option

    Score : int
}
with
    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit

        LastTouched = NotTouched

        ButtonsSize = v2iDup 3
        ButtonsValues = Array.empty
        ButtonsSelected = None

        CellsSize = v2iDup 9
        CellsValues = Array.empty
        CellsStatuses = Array.empty
        CellsSelected = None

        Score = 0
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        let gameplay = Gameplay.empty
        let position = v2i (Gen.random1 gameplay.CellsSize.X) (Gen.random1 gameplay.CellsSize.Y)

        let value = if Gen.random1 10 = 0 then 4 else 2
        {
            gameplay with
                ButtonsValues = [| 1; 2; 3; 4; 5; 6; 7; 8; 9 |] |> Array.map Value
                CellsValues = [|
                    5; 3; 4; 6; 7; 8; 9; 1; 2
                    6; 7; 2; 1; 9; 5; 3; 4; 8
                    1; 9; 8; 3; 4; 2; 5; 6; 7
                    8; 5; 9; 7; 6; 1; 4; 2; 3
                    4; 2; 6; 8; 5; 3; 7; 9; 1
                    7; 1; 3; 9; 2; 4; 8; 5; 6
                    9; 6; 1; 5; 3; 7; 2; 8; 4
                    2; 8; 7; 4; 1; 9; 6; 3; 5
                    3; 4; 5; 2; 8; 6; 1; 7; 9
                |] |> Array.map Value
                CellsStatuses = [|
                    Revealed; Unknown; Revealed; Revealed; Revealed; Revealed; Revealed; Unknown; Revealed;
                    Revealed; Revealed; Revealed; Revealed; Revealed; Revealed; Revealed; Revealed; Revealed;
                    Revealed; Revealed; Unknown; Revealed; Revealed; Revealed; Revealed; Revealed; Revealed;
                    Revealed; Revealed; Revealed; Unknown; Revealed; Unknown; Revealed; Revealed; Revealed;
                    Revealed; Revealed; Unknown; Revealed; Revealed; Revealed; Revealed; Revealed; Revealed;
                    Revealed; Revealed; Revealed; Revealed; Revealed; Revealed; Unknown; Revealed; Revealed;
                    Revealed; Revealed; Revealed; Revealed; Revealed; Revealed; Unknown; Revealed; Revealed;
                    Revealed; Revealed; Revealed; Revealed; Revealed; Revealed; Revealed; Revealed; Revealed;
                    Revealed; Revealed; Revealed; Revealed; Revealed; Revealed; Revealed; Unknown; Revealed;
                |]

        }