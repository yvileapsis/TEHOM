namespace Tehom

open System
open System.Numerics
open Prime
open Nu

type GameplayState =
    | Playing
    | Quit

type [<SymbolicExpansion>] Gameplay = {
    GameplayTime : int64
    GameplayState : GameplayState
    CoordinatesAndRotations : (Vector3 * Quaternion) list
    TwoLists : int list * int list
}
with
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit
        CoordinatesAndRotations = []
        TwoLists = [], []
    }

    static member initial = {
        Gameplay.empty with
            GameplayState = Playing
            CoordinatesAndRotations =
                [
                    v3 0f 1f 2.75f,  Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 0f, 0f, 0f)
                    v3 0f 1f -2.75f, Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 180f, 0f, 0f)
                    v3 2.75f 1f 0f,  Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 90f, 0f, 0f)
                    v3 -2.75f 1f 0f, Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians -90f, 0f, 0f)
                ]
//                |> List.randomShuffle
            TwoLists = List.init 64 id, List.init 16 id
    }

type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | Update
    | Shuffle
    | Move of bool * int
    | TimeUpdate
    interface Message

type GameplayCommand =
    | SetupScene
    | StartQuitting
    interface Command

[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.QuitEvent = Events.QuitEvent --> this

type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.initial)

    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    override this.Definitions (_, _) = [
        Screen.SelectEvent => StartPlaying
        Screen.DeselectingEvent => FinishQuitting
        Screen.UpdateEvent => Update
        Screen.TimeUpdateEvent => TimeUpdate
    ]

    override this.Message (model, message, entity, world) =
        match message with
        | StartPlaying ->
            [SetupScene], model

        | FinishQuitting ->
            let model = Gameplay.empty
            just model

        | Update ->
            just model

        | Shuffle ->
            let model = Gameplay.initial
            just model

        | Move (left, num) ->

            let fromList, toList =
                if left then model.TwoLists else (fun (x, y) -> y, x) model.TwoLists

            let value = List.item num fromList
            let fromList = List.removeAt num fromList
            let toList = toList @ [ value ]

            let twoLists =
                if left then fromList, toList else toList, fromList

            let model = { model with TwoLists = twoLists }

            just model

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let model = { model with GameplayTime = model.GameplayTime + gameDelta.Updates }
            just model

    override this.Command (model, command, entity, world) =

        match command with
        | SetupScene ->
//            let world = World.setEye3dCenter (v3 0f 2f 3f) world
//            let world = World.setEye3dRotation (Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -20f, 0f)) world
            let world = World.setEye2dCenter (v2 0f 0f) world
            let world = World.setEye2dCenter (v2 640f 360f) world
            just world

        | StartQuitting ->
            let world = World.publish () entity.QuitEvent entity world
            just world


    override this.Content (model, entity) = [

        Content.group Simulants.GameplayGui.Name [] [
            Content.entity<CombatDispatcher> Simulants.GameplayCombat.Name [
                //Entity.PositionLocal == v3 -160f -90f 0f
            ]

            Content.button "Shuffle" [
                Entity.Position == v3 -192f 160f 0f
                Entity.Size == v3 64f 16f 0f
                Entity.FontSizing == Some 8
                Entity.Text == "Shuffle!"
                Entity.ClickEvent => Shuffle
            ]

            Content.composite "Test" [] [

                Content.association "Test1" [
                    Entity.PositionLocal == v3 -192f 80f 0f
                    Entity.Size == v3 64f 64f 0f
                    Entity.Layout == Flow (FlowDownward, FlowParent)
                ] [
                    for i, j in List.indexed (fst model.TwoLists) do
                        Content.button $"{i}" [
                            Entity.Text := $"{j}"
                            Entity.Size == v3 32.0f 4.0f 0.0f
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing == Some 4
                            Entity.ClickEvent => Move (true, i)
                        ]
                ]

                Content.association "Test2" [
                    Entity.PositionLocal == v3 -192f 0f 0f
                    Entity.Size == v3 64f 64f 0f
                    Entity.Layout == Flow (FlowDownward, FlowParent)
                ] [
                    for i, j in List.indexed (snd model.TwoLists) do
                        Content.button $"{i}" [
                            Entity.Text := $"{j}"
                            Entity.Size == v3 32.0f 4.0f 0.0f
                            Entity.Font == Assets.Gui.ClearSansFont
                            Entity.FontSizing == Some 4
                            Entity.ClickEvent => Move (false, i)
                        ]
                ]
            ]
        ]

        match model.GameplayState with
        | Playing ->

            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Room/Scene.nugroup" [] [
                let coordinates, rotations = model.CoordinatesAndRotations |> List.unzip

                Content.composite<Ball3dDispatcher> "Chisel" [
                    Entity.Position := coordinates[0]
                    Entity.Rotation := rotations[0]
                ] [
                    ContentEx.camera "Camera" [
                        Entity.PositionLocal == v3 0f 0.5f 0.25f
                        Entity.RotationLocal == Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -20f, 0f)
                    ]
                ]

                Content.ball3d "Maldoror" [
                    Entity.Position := coordinates[1]
                    Entity.Rotation := rotations[1]
                ]

                Content.ball3d "Sigetical" [
                    Entity.Position := coordinates[2]
                    Entity.Rotation := rotations[2]
                ]

                Content.ball3d "Nanghait" [
                    Entity.Position := coordinates[3]
                    Entity.Rotation := rotations[3]
                ]

            ]

            Content.group Simulants.GameplayCharacters.Name [] [
                character CharacterContent.player
                character CharacterContent.rat
            ]
        | Quit -> ()
    ]