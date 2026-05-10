namespace Psychocrawl
open System
open System.Numerics
open FParsec
open Prime
open Nu
open Psychocrawl

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

type Direction =
    | North
    | South
    | East
    | West

type Player = {
    HasGun: Boolean
    Ammo: Int32
}
with
    static member empty = {
        HasGun = false
        Ammo = 0
    }

type Actor =
    | Player of Player
    | Enemy
    | Bullet of Direction
    | Gun
    | Ammo of Int32
    | Health
    | Exit
    | Wall
    | Floor
with
    static member isWalkable cell =
        List.contains Floor cell && not (List.contains Wall cell)

    static member get predicate (cell : Actor list) =
        cell |> List.tryFind predicate,
        cell |> List.remove predicate

    static member movePlayer (cell1 : Actor list) (cell2 : Actor list) =
        match cell1 |> List.tryFindIndex _.IsPlayer with
        | Some x ->
            let player = List.item x cell1
            let cell1' = List.removeAt x cell1
            let cell2' = player :: cell2
            cell1', cell2'
        | None ->
            cell1, cell2

type Cell = Actor list

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type Gameplay = {
    GameplayTime : Int64
    GameplayState : GameplayState
    Board : Map<Vector2i, Cell> * Vector2i
    PlayerLocation : Vector2i
}
with

    static member boardFromText text : Map<Vector2i, Cell> * Vector2i =
        let cells =
            text
            |> String.toArray
            |> Array.foldMap (fun c (state : Vector2i) ->
                let cell =
                    match c with
                    | '@' -> [ Floor; Player Player.empty ]
                    | 'E' -> [ Floor; Enemy ]
                    | 'G' -> [ Floor; Gun ]
                    | 'A' -> [ Floor; Ammo 10 ]
                    | '*' -> [ Floor; Bullet North ]
                    | 'H' -> [ Floor; Health ]
                    | '>' -> [ Floor; Exit ]
                    | '#' -> [ Floor; Wall ]
                    | '.' -> [ Floor ]
                    | _ -> []

                let state' =
                    if c = '\n' then
                        v2i 0 (state.Y + 1)
                    else
                        state + v2i 1 0

                (state, cell), state'
            ) v2iZero
            |> fst

        let indices = Array.map fst cells

        let min, max =
            indices
            |> Array.fold (fun ((min, max) : Vector2i * Vector2i) value ->

                let minX = if min.X < value.X then min.X else value.X
                let minY = if min.Y < value.Y then min.Y else value.Y

                let maxX = if max.X > value.X then max.X else value.X
                let maxY = if max.Y > value.Y then max.Y else value.Y

                (v2i minX minY), (v2i maxX maxY)
            ) (v2iZero, v2iZero)

        let size = max - min

        Map.ofArray cells, size

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit
        Board = Map.empty, v2iZero
        PlayerLocation = v2iZero
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Gameplay.empty with
            GameplayState = Playing
            Board = Gameplay.boardFromText """
################  ########
#..............#  #...>..#
#..............####......#
#...@....................#   ######
#..............####......#   #....#
#...G..A.......#  #..E...#####....#
#..............#  #...............# #########
#####..#########  ##############.## #.......#
    #..#                       #.####.......#
    #..#                       #............#
    #..#                       ######.......#
#####..###################          #.......#
#........................#          ####..###
#........................#             #..#
#........................###############..#
#.........................................#
#........................############.#####
#........................#         #...#
##########################         #####
"""
            PlayerLocation = v2i 4 4
    }

// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartPlaying
    | MovePlayer of Direction
    | CellUpdate of Vector2i
    | FinishQuitting
    | TimeUpdate
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
    override this.Definitions (_, _) = [
        Screen.SelectEvent => StartPlaying
        Screen.DeselectingEvent => FinishQuitting
        Screen.TimeUpdateEvent => TimeUpdate
        Game.KeyboardKeyDownEvent =|> fun evt ->
            match evt.Data.KeyboardKey with
            | KeyboardKey.W -> MovePlayer North
            | KeyboardKey.S -> MovePlayer South
            | KeyboardKey.A -> MovePlayer West
            | KeyboardKey.D -> MovePlayer East
            | _ -> TimeUpdate
    ]

    // here we handle the above messages
    override this.Message (gameplay, message, _, world) =

        match message with
        | StartPlaying ->
            let gameplay = Gameplay.initial
            just gameplay

        | FinishQuitting ->
            let gameplay = Gameplay.empty
            just gameplay

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
            just gameplay

        | MovePlayer direction ->

            let playerCell = gameplay.PlayerLocation

            let targetCell =
                playerCell +
                match direction with
                | North -> v2i 0 -1
                | South -> v2i 0 1
                | East -> v2i 1 0
                | West -> v2i -1 0

            let board, size = gameplay.Board

            match Map.tryFind playerCell board, Map.tryFind targetCell board with
            | Some cell1, Some cell2 when Actor.isWalkable cell2 ->

                let cell1, cell2 = Actor.movePlayer cell1 cell2

                let board =
                    board
                    |> Map.add playerCell cell1
                    |> Map.add targetCell cell2

                let gameplay = {
                    gameplay with
                        Board = board, size
                        PlayerLocation = targetCell
                }

                withSignals [
                    CellUpdate playerCell
                    CellUpdate targetCell
                ] gameplay

            | _ ->
                just gameplay

        | CellUpdate coords ->
            let board, size = gameplay.Board

            match Map.tryFind coords board with
            | Some cell ->

                match Actor.get _.IsPlayer cell with
                | Some (Player player), cell ->

                    let gun, cell = Actor.get _.IsGun cell
                    let ammo, cell = Actor.get _.IsAmmo cell

                    let shouldHaveGun = Option.isSome gun
                    let newAmmo =
                        match ammo with
                        | Some (Ammo count) ->
                            count
                        | _ -> 0

                    let player = Player {
                        player with
                            HasGun = player.HasGun || shouldHaveGun
                            Ammo = player.Ammo + newAmmo
                    }

                    let cell = player :: cell

                    let board =
                        board
                        |> Map.add coords cell

                    let gameplay = {
                        gameplay with
                            Board = board, size
                    }

                    just gameplay

                | _ ->
                    just gameplay
            | None ->

                just gameplay



    // here we handle the above commands
    override this.Command (_, command, screen, world) =
        match command with
        | StartQuitting ->
            World.publish () screen.QuitEvent screen world

    // here we describe the content of the game including the scene and the hud
    override this.Content (gameplay, _) = [
        // the scene group while playing
        if gameplay.GameplayState = Playing then
            Content.group Simulants.GameplayScene.Name [] [

                Content.composite "Cells" [] [
                    let board, size = gameplay.Board

                    for i, cell in Map.toList board do
                        let x = i.X - size.X / 2
                        let y = size.Y / 2 - i.Y

                        let symbol =
                            match cell with
                            | [] -> " "
                            | cell ->
                                match cell |> List.sort |> List.head with
                                | Player _ -> "@"
                                | Enemy -> "E"
                                | Bullet _ -> "*"
                                | Gun -> "G"
                                | Ammo _ -> "A"
                                | Health -> "H"
                                | Exit -> ">"
                                | Wall -> "#"
                                | Floor ->
                                    let seed = int64 (x * 73856093 ^^^ y * 19349663)
                                    //let lifetime = 15
                                    //if (((seed + int gameplay.GameplayTime) / lifetime) % 13) = 0 then
                                    let speed = 1L
                                    let rain_y = (int64 y + gameplay.GameplayTime * speed)

                                    if ((seed + rain_y) / 5L % 37L) = 0 then
                                        "."
                                    else
                                        " "

                        Content.text $"Cell{i}" [
                            Entity.PositionLocal := v3 (single x * 10f) (single y * 10f) 0f
                            Entity.Size := v3 12f 20f 0f
                            Entity.FontSizing == Some 10
                            Entity.Text := symbol
                            Entity.Color := Color.White
                            Entity.Font == Assets.Gameplay.FontSquare
                        ]
                ]
                Content.composite "HUD" [
                    Entity.Position == v3 0f 96f 0f
                ] [
                    let board, size = gameplay.Board
                    let playerPos = gameplay.PlayerLocation

                    match Map.tryFind playerPos board with
                    | Some playerCell ->
                        match Actor.get _.IsPlayer playerCell with
                        | Some (Player player), _ ->

                            Content.text $"State" [
                                Entity.Size := v3 120f 20f 0f
                                Entity.FontSizing == Some 5
                                Entity.Text :=
                                    if player.HasGun then
                                        "Has Gun! "
                                    else
                                        "No Gun! "
                                    +
                                    $"{player.Ammo} Bullets"

                                Entity.Color := Color.White
                                Entity.Font == Assets.Gameplay.FontSquare
                            ]
                        | _ ->
                            ()
                    | None ->
                        ()
                ]
                // quit
                Content.button Simulants.GameplayQuit.Name [
                    Entity.Position == v3 232.0f -144.0f 0.0f
                    Entity.Text == "Quit"
                    Entity.ClickEvent => StartQuitting
                ]
            ]
    ]