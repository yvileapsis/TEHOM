namespace S49151
open System
open System.Numerics
open Prime
open Nu
open S49151

// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | TimeUpdate
    | Select of Int32 * Int32
    | Enter of Int32
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
    override this.Definitions (_, _) = [
        Screen.SelectEvent => StartPlaying
        Screen.DeselectingEvent => FinishQuitting
        Screen.TimeUpdateEvent => TimeUpdate
        Game.KeyboardKeyDownEvent =|> fun evt ->
            if not evt.Data.Repeated then
                match evt.Data.KeyboardKey with
                | KeyboardKey.Num1 -> Enter 1
                | KeyboardKey.Num2 -> Enter 2
                | KeyboardKey.Num3 -> Enter 3
                | KeyboardKey.Num4 -> Enter 4
                | KeyboardKey.Num5 -> Enter 5
                | KeyboardKey.Num6 -> Enter 6
                | KeyboardKey.Num7 -> Enter 7
                | KeyboardKey.Num8 -> Enter 8
                | KeyboardKey.Num9 -> Enter 9
                | _ -> Nil
            else Nil
    ]

    // here we handle the above messages
    override this.Message (model, message, _, world) =

        match message with
        | StartPlaying ->
            let model = Gameplay.initial
            just model

        | FinishQuitting ->
            let model = Gameplay.empty
            just model

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let model = { model with GameplayTime = model.GameplayTime + gameDelta.Updates }
            just model

        | Select (block_i, cell_i) ->
            let model = { model with Selected = Some (block_i, cell_i) }
            just model

        | Enter value ->
            let model = Gameplay.attemptSetCell value model
            just model

        | Nil ->
            just model

    // here we handle the above commands
    override this.Command (_, command, screen, world) =
        match command with
        | StartQuitting ->
            World.publish () screen.QuitEvent screen world

    // here we describe the content of the game including the scene and the hud
    override this.Content (model, _) = [// the scene group while playing
        if model.GameplayState = Playing then

            Content.group "Sudoku" [] [

                Content.text "Selected" [
                    Entity.Size == v3 100f 20f 0f
                    Entity.Elevation == 1f
                    Entity.Position == v3 0f 135f 0f
                    Entity.Text := $"{model.Selected}"
                ]

                Content.panel $"Field" [
                    Entity.Size == v3 231.0f 231.0f 0.0f
                    Entity.Elevation == 1.0f
                    Entity.Layout == Layout.Grid (v2i 3 3, Some FlowRightward, false)
                ] [
                    for (i, block) in List.indexed model.Field do
                        Content.panel $"Block{i}" [
                            Entity.Size == v3 75.0f 75.0f 0.0f
                            Entity.Elevation == 2.0f
                            Entity.Layout == Layout.Grid (v2i 3 3, Some FlowRightward, false)
                        ] [
                            for (j, cell) in List.indexed block.Cells do

                                match cell with
                                | Given num ->
                                    Content.button $"Cell{j}" [
                                        Entity.Size == v3 24.0f 24.0f 0.0f
                                        Entity.Elevation == 3.0f
                                        Entity.Text := $"{num}"
                                        Entity.FontSizing == Some 15
                                        Entity.FontStyling := Set.ofList [ FontStyle.Bold ]
                                        Entity.ClickEvent => Select (i, j)
                                        Entity.UpImage == Assets.Default.EmptyImage
                                        Entity.DownImage == Assets.Default.White
                                    ]

                                | Guessed num ->
                                    Content.association $"Cell{j}" [
                                        Entity.Size == v3 24.0f 24.0f 0.0f
                                    ] [
                                        Content.button "Button" [
                                            Entity.Size == v3 24.0f 24.0f 0.0f
                                            Entity.Elevation == 3.0f
                                            Entity.Text := $"{num}"
                                            Entity.FontSizing == Some 15
                                            Entity.FontStyling := Set.ofList []
                                            Entity.ClickEvent => Select (i, j)
                                            Entity.UpImage == Assets.Default.EmptyImage
                                            Entity.DownImage == Assets.Default.White
                                        ]
                                        if Some (i, j) = model.Selected then
                                            Content.staticSprite "Selected" [
                                                Entity.Size == v3 24.0f 24.0f 0.0f
                                                Entity.StaticImage == Assets.Default.White
                                                Entity.Color == color 0.2f 0.2f 0.2f 1f
                                            ]
                                    ]

                                | Nothing ->
                                    Content.association $"Cell{j}" [
                                        Entity.Size == v3 24.0f 24.0f 0.0f
                                    ] [
                                        Content.button "Button" [
                                            Entity.Size == v3 24.0f 24.0f 0.0f
                                            Entity.Elevation == 3.0f
                                            Entity.Text := ""
                                            Entity.FontSizing == Some 15
                                            Entity.FontStyling := Set.ofList []
                                            Entity.ClickEvent => Select (i, j)
                                            Entity.UpImage == Assets.Default.EmptyImage
                                            Entity.DownImage == Assets.Default.White
                                        ]
                                        if Some (i, j) = model.Selected then
                                            Content.staticSprite "Selected" [
                                                Entity.Size == v3 24.0f 24.0f 0.0f
                                                Entity.StaticImage == Assets.Default.White
                                                Entity.Color == color 0.2f 0.2f 0.2f 1f
                                            ]
                                    ]

                                | Markers list ->
                                    Content.panel $"Markers{j}" [
                                        Entity.Size == v3 24.0f 24.0f 0.0f
                                        Entity.Elevation == 3.0f
                                        Entity.Layout == Layout.Grid (v2i 3 3, Some FlowRightward, true)
                                    ] [
                                        for i in List.init 9 id do
                                            Content.text $"Marker{i}" [
                                                Entity.Text == $"{i + 1}"
                                                Entity.FontSizing == Some 6
                                            ]
                                    ]

                        ]

                ]

                Content.panel "Numbers" [
                    Entity.Size == v3 75.0f 75.0f 0.0f
                    Entity.Position == v3 200.0f 0f 0f
                    Entity.Elevation == 1.0f
                    Entity.Layout == Layout.Grid (v2i 3 3, Some FlowRightward, false)
                ] [
                    for i in List.init 9 id do
                        if true then
                            Content.button $"Cell{i}" [
                                Entity.Size == v3 24.0f 24.0f 0.0f
                                Entity.Elevation == 2.0f
                                Entity.Text == $"{i + 1}"
                                Entity.FontSizing == Some 15
                                Entity.ClickEvent => Enter (i + 1)
                                Entity.UpImage == Assets.Default.Black
                                Entity.DownImage == Assets.Default.White
                            ]
                ]

            ]

            Content.group Simulants.GameplayScene.Name [] [

                // quit
                Content.button Simulants.GameplayQuit.Name [
                    Entity.Position == v3 232.0f -144.0f 0.0f
                    Entity.Text == "Quit"
                    Entity.ClickEvent => StartQuitting
                ]
            ]
    ]