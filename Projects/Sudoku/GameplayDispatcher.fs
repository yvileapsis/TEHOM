namespace Sudoku
open System
open System.Numerics
open Prime
open Nu

type Direction =
    | Upward
    | Rightward
    | Downward
    | Leftward

// this is our MMCC model type representing gameplay.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | TimeUpdate
    | Keyboard of Direction
    | CellsSelectedSet of Int32
    | ButtonsSelectedSet of Int32
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

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (gameplay, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.DeselectingEvent => FinishQuitting
         Screen.TimeUpdateEvent => TimeUpdate
         Game.KeyboardKeyDownEvent =|> fun evt ->
            if not evt.Data.Repeated then
                match evt.Data.KeyboardKey with
                | KeyboardKey.Up -> Keyboard Upward
                | KeyboardKey.Down -> Keyboard Downward
                | KeyboardKey.Left -> Keyboard Leftward
                | KeyboardKey.Right -> Keyboard Rightward
                | _ -> Nil
            else Nil]

    // here we handle the above messages
    override this.Message (gameplay, message, _, world) =

        match message with
        | StartPlaying ->
            just Gameplay.initial

        | FinishQuitting ->
            just Gameplay.empty

        | TimeUpdate ->
            just { gameplay with GameplayTime = inc gameplay.GameplayTime }

        | Keyboard direction ->

            match gameplay.LastTouched with
            | NotTouched -> just { gameplay with CellsSelected = Some 40; LastTouched = Cells }
            | Cells ->
                match gameplay.CellsSelected with
                | None -> just { gameplay with CellsSelected = Some 40 }
                | Some x ->
                    match direction with
                    | Upward -> if (x > 8) then just { gameplay with CellsSelected = Some (x - 9) } else just gameplay
                    | Downward -> if (x < 72) then just { gameplay with CellsSelected = Some (x + 9) } else just gameplay
                    | Leftward -> if (x % 9 > 0) then just { gameplay with CellsSelected = Some (x - 1) } else just gameplay
                    | Rightward -> if (x % 9 < 8) then just { gameplay with CellsSelected = Some (x + 1) } else just gameplay
            | Buttons ->
                match gameplay.ButtonsSelected with
                | None -> just { gameplay with ButtonsSelected = Some 13 }
                | Some x ->
                    match direction with
                    | Upward -> if (x > 2) then just { gameplay with ButtonsSelected = Some (x - 3) } else just gameplay
                    | Downward -> if (x < 6) then just { gameplay with ButtonsSelected = Some (x + 3) } else just gameplay
                    | Leftward -> if (x % 3 > 0) then just { gameplay with ButtonsSelected = Some (x - 1) } else just gameplay
                    | Rightward -> if (x % 3 < 2) then just { gameplay with ButtonsSelected = Some (x + 1) } else just gameplay

        | CellsSelectedSet t ->
            just { gameplay with CellsSelected = Some t; LastTouched = Cells }

        | ButtonsSelectedSet t ->
            just { gameplay with ButtonsSelected = Some t; LastTouched = Buttons }

        | Nil ->
            just gameplay

    // here we handle the above commands
    override this.Command (_, command, screen, world) =

        match command with
        | StartQuitting ->
            let world = World.publish () screen.QuitEvent screen world
            just world

    // here we describe the content of the game including the level, the hud, and the player
    override this.Content (gameplay, _) = [
        // the gui group
        Content.group Simulants.GameplayGui.Name [] [
            Content.button Simulants.GameplayQuit.Name [
                Entity.Position == v3 232.0f -144.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Text == "Quit"
                Entity.ClickEvent => StartQuitting
            ]
        ]

        // the scene group
        Content.group Simulants.GameplayScene.Name [] [
            // score
            Content.text "Score" [
                Entity.Position == v3 232.0f 155.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Text := "Score: " + string gameplay.Score
            ]

            // game over
            match gameplay.GameplayState with
            | Playing true ->
                Content.text "GameOver" [
                    Entity.Position == v3 0.0f 155.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Text == "Game Over!"
                ]
            | Playing false | Quit -> ()

            // board
            let gutter = v3 2.0f 2.0f 0.0f
            let tileSize = v3 16.0f 16.0f 0.0f
            let tileOffset = (gameplay.ButtonsSize.V3 * tileSize + gutter * (gameplay.ButtonsSize - v2iOne).V3) * -0.5f
            Content.panel Simulants.GameplayBoard2.Name [
                Entity.Size == v3 56.0f 56.0f 0.0f
                Entity.Position == v3 128.0f 0.0f 0.0f
                Entity.Elevation == 1.0f
                Entity.BackdropImageOpt == Some Assets.Gameplay.BoardImage2
            ] [
                for i, value in gameplay.ButtonsValues |> Array.indexed do
                let x = float32 (i % 3) * (tileSize.X + gutter.X)
                let y = float32 (2 - i / 3) * (tileSize.Y + gutter.Y)
                let offset = tileSize * 0.5f + tileOffset

                let sameValueAsCell =
                    match gameplay.CellsSelected with
                    | None -> false
                    | Some x -> gameplay.CellsValues[x] = value
                let (Value value) = value

                Content.button $"Buttons+{i}" [
                    Entity.TextColor := Color.Gray
                    Entity.Text := string value

                    if gameplay.ButtonsSelected = Some i then
                        Entity.UpImage := Assets.Gameplay.TileImage 8
                        Entity.DownImage := Assets.Gameplay.TileImage 8
                    else if sameValueAsCell then
                        Entity.UpImage := Assets.Gameplay.TileImage 4
                        Entity.DownImage := Assets.Gameplay.TileImage 4
                    else
                        Entity.UpImage := Assets.Gameplay.TileImage 2
                        Entity.DownImage := Assets.Gameplay.TileImage 2

                    Entity.PositionLocal := v3 x y 0.0f + offset
                    Entity.Size == tileSize
                    Entity.ElevationLocal == 1.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing := Some 8
                    Entity.ClickEvent => ButtonsSelectedSet i
                    Entity.ClickSoundOpt := None
                ]

            ]

            // board
            let gutter = v3 2.0f 2.0f 0.0f
            let tileSize = v3 16.0f 16.0f 0.0f
            let tileOffset = (gameplay.CellsSize.V3 * tileSize + gutter * (gameplay.CellsSize - v2iOne).V3) * -0.5f
            Content.panel Simulants.GameplayBoard.Name [
                Entity.Size == v3 164.0f 164.0f 0.0f
                Entity.Elevation == 1.0f
                Entity.BackdropImageOpt == Some Assets.Gameplay.BoardImage
            ] [
                for i, (value, status) in Array.zip gameplay.CellsValues gameplay.CellsStatuses |> Array.indexed do
                let x = float32 (i % 9) * (tileSize.X + gutter.X)
                let y = float32 (8 - i / 9) * (tileSize.Y + gutter.Y)
                let offset = tileSize * 0.5f + tileOffset

                let sameValueAsButton =
                    match gameplay.ButtonsSelected with
                    | None -> false
                    | Some x -> gameplay.ButtonsValues[x] = value

                let (Value value) = value

                Content.button $"Tile+{i}" [
                    match status with
                    | Revealed ->
                        Entity.TextColor := Color.Gray
                        Entity.Text := string value
                    | Known ->
                        Entity.TextColor := Color.Blue
                        Entity.Text := string value
                    | Mistake m ->
                        Entity.TextColor := Color.Red
                        Entity.Text := string m
                    | Unknown ->
                        Entity.Text := String.empty

                    if gameplay.CellsSelected = Some i then
                        Entity.UpImage := Assets.Gameplay.TileImage 8
                        Entity.DownImage := Assets.Gameplay.TileImage 8
                    else if sameValueAsButton then
                        Entity.UpImage := Assets.Gameplay.TileImage 4
                        Entity.DownImage := Assets.Gameplay.TileImage 4
                    else
                        Entity.UpImage := Assets.Gameplay.TileImage 2
                        Entity.DownImage := Assets.Gameplay.TileImage 2

                    Entity.PositionLocal := v3 x y 0.0f + offset
                    Entity.Size == tileSize
                    Entity.ElevationLocal == 1.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing := Some 8
                    Entity.ClickEvent => CellsSelectedSet i
                    Entity.ClickSoundOpt := None
                ]
            ]
        ]
    ]