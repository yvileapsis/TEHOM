namespace S49151
open System
open System.Numerics
open Prime
open Nu
open S49151

type Cell =
    | Nothing
    | Given of Int32
    | Guessed of Int32
    | Markers of List<Int32>

type Block = {
    Coordinates : Vector2i
    Size : Vector2i
    Acceptable : Set<Int32>
    Cells : List<Cell>
}
with
    static member initial3x3 = {
        Coordinates = v2i 0 0
        Size = v2i 3 3
        Acceptable = Set.ofList [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
        Cells = [
            Nothing; Nothing; Nothing
            Nothing; Nothing; Nothing
            Nothing; Nothing; Nothing
        ]
    }

    static member rule block =
        let numbers =
            block.Cells
            |> List.choose (fun cell ->
                match cell with
                | Given i -> Some i
                | Guessed i -> Some i
                | _ -> None
            )
            |> Set.ofList
        numbers = block.Acceptable

    static member cellsList block =
        block.Cells
        |> List.indexed
        |> List.choose (fun (i, cell) ->
            match cell with
            | Given i ->
                let x = i % block.Size.X + block.Coordinates.X
                let y = i / block.Size.Y + block.Coordinates.Y
                Some (v2i x y, cell)
            | Guessed i ->
                let x = i % block.Size.X + block.Coordinates.X
                let y = i / block.Size.Y + block.Coordinates.Y
                Some (v2i x y, cell)
            | _ -> None
        )

    static member isAcceptable cell coords block =
        let cellValue =
            match cell with
            | Guessed value -> value
            | Given value -> value
            | _ -> -1

        let ``is cell value within set of values`` =
            Set.contains cellValue block.Acceptable

        let ``is target cell not given`` =
            match List.item coords block.Cells with
            | Given _ -> false
            | _ -> true

        ``is cell value within set of values``
        && ``is target cell not given``

    static member withCell cell coords block =
        let block = {
            block with
                Cells = List.updateAt coords cell block.Cells
        }
        block

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type Gameplay = {
    GameplayTime : Int64
    GameplayState : GameplayState
    Field : List<Block>
    Selected : Option<Int32 * Int32>
}
with
    static member attemptSetCell value model =
        match model.Selected with
        | Some (coords_j, coords_i) ->

            let block = List.item coords_j model.Field
            let cell = Guessed value

            if Block.isAcceptable cell coords_i block then
                let block = Block.withCell cell coords_i block
                let field = List.updateAt coords_j block model.Field

                let model = { model with Field = field }

                model
            else
                model
        | None ->
            model


    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit
        Field = List.empty
        Selected = None
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Gameplay.empty with
            GameplayState = Playing
            Field = [
                {
                    Block.initial3x3 with
                        Coordinates = v2i 0 0
                        Cells = [
                            Given 2; Given 8; Given 7
                            Nothing; Given 5; Nothing
                            Nothing; Given 6; Nothing
                        ]
                }
                {
                    Block.initial3x3 with
                        Coordinates = v2i 3 0
                        Cells = [
                            Given 5; Given 4; Nothing
                            Nothing; Nothing; Given 2
                            Nothing; Given 3; Nothing
                        ]
                }
                {
                    Block.initial3x3 with
                        Coordinates = v2i 6 0
                        Cells = [
                            Nothing; Nothing; Given 6
                            Given 8; Given 4; Given 9
                            Nothing; Given 7; Nothing
                        ]
                }

                {
                    Block.initial3x3 with
                        Coordinates = v2i 0 3
                        Cells = [
                            Nothing; Nothing; Given 6
                            Given 1; Nothing; Given 8
                            Nothing; Given 7; Given 3
                        ]
                }
                {
                    Block.initial3x3 with
                        Coordinates = v2i 3 3
                        Cells = [
                            Nothing; Given 5; Given 7
                            Nothing; Nothing; Nothing
                            Nothing; Nothing; Nothing
                        ]
                }
                {
                    Block.initial3x3 with
                        Coordinates = v2i 6 3
                        Cells = [
                            Given 3; Nothing; Nothing
                            Given 4; Nothing; Given 7
                            Given 9; Given 6; Given 1
                        ]
                }

                {
                    Block.initial3x3 with
                        Coordinates = v2i 0 6
                        Cells = [
                            Given 6; Nothing; Nothing
                            Nothing; Nothing; Given 5
                            Given 7; Nothing; Nothing
                        ]
                }
                {
                    Block.initial3x3 with
                        Coordinates = v2i 3 6
                        Cells = [
                            Given 4; Nothing; Nothing
                            Given 7; Nothing; Given 3
                            Given 9; Given 2; Nothing
                        ]
                }
                {
                    Block.initial3x3 with
                        Coordinates = v2i 6 6
                        Cells = [
                            Nothing; Nothing; Nothing
                            Nothing; Given 9; Nothing
                            Nothing; Given 1; Given 5
                        ]
                }
            ]
    }