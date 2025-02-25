namespace Tehom

open System
open System.Numerics
open Prime
open Nu

type GameplayState =
    | Playing
    | Quit

type [<SymbolicExpansion>] Gameplay = {
    GameplayTime : Int64
    GameplayState : GameplayState
    CoordinatesAndRotations : List<Vector3 * Quaternion>
    Positions : List<Vector3 * Quaternion>
    PositionOld : Option<Vector3 * Quaternion>
    LerpStepsLeft : Int32
    HoldingMouse : Boolean
}
with
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit
        CoordinatesAndRotations = []
        Positions = [
            v3 0f 0.5f -0.75f, Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -60f, 0f)
            v3 0f 0.5f 0.25f, Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -20f, 0f)
        ]
        PositionOld = None
        LerpStepsLeft = 0
        HoldingMouse = false
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
    }

type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | Update
    | Shuffle
    | TimeUpdate
    | PosMove of Boolean
    | Nil
    interface Message

type GameplayCommand =
    | SetupScene
    | StartCombat of (Entity * Entity * Entity)
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
        Game.KeyboardKeyDownEvent =|> fun evt ->
            if not evt.Data.Repeated then
                match evt.Data.KeyboardKey with
                | KeyboardKey.W -> PosMove true
                | KeyboardKey.S -> PosMove false
                | _ -> Nil
            else Nil
    ]

    override this.Message (model, message, entity, world) =
        match message with
        | StartPlaying ->
            [SetupScene], model

        | FinishQuitting ->
            let model = Gameplay.empty
            just model

        | Update ->

            let model = {
                model with
                    HoldingMouse = World.isMouseButtonDown MouseLeft world
            }

            let model =
                if model.LerpStepsLeft > 0 then
                    { model with LerpStepsLeft = model.LerpStepsLeft - 1 }
                else
                    { model with PositionOld = None }

            let area = Simulants.GameplayArea
            let player = Simulants.GameplayCharacters / "player"
            let rat = Simulants.GameplayCharacters / "rat"

            let ratModel = rat.GetCharacter world

            let areaModel = area.GetArea world

            let distance =
                Area.findPath rat.Name player.Name areaModel
                |> List.last
                |> snd

            let canSee = distance < 200u && not (World.getExists Simulants.GameplayCombat world) && Character.canAct ratModel

            if canSee then
                [StartCombat (area, player, rat)], model
            else
                just model

        | Shuffle ->
            let model = Gameplay.initial
            just model

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let model = { model with GameplayTime = model.GameplayTime + gameDelta.Updates }
            just model

        | PosMove b ->
            match model.Positions with
            | head::tail ->
                just {
                    model with
                        Positions = tail@[head]
                        PositionOld = Some head
                        LerpStepsLeft = 30
                }
            | _ ->
                just model

        | Nil ->
            just model

    override this.Command (model, command, entity, world) =

        match command with
        | SetupScene ->
//            let world = World.setEye3dCenter (v3 0f 2f 3f) world
//            let world = World.setEye3dRotation (Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -20f, 0f)) world
            let world = World.setEye2dCenter (v2 0f 0f) world
            let world = World.setEye2dSize (v2 640f 360f) world

            let area, world = World.createEntity<AreaDispatcher> NoOverlay (Some [|"Area"|]) Simulants.GameplayGui world
            let world = area.SetArea Area.initial world

            let player, world = World.createEntity<CharacterDispatcher> NoOverlay (Some [|CharacterContent.player.ID|]) Simulants.GameplayCharacters world
            let world = player.SetCharacter CharacterContent.player world

            let rat, world = World.createEntity<CharacterDispatcher> NoOverlay (Some [|CharacterContent.rat.ID|]) Simulants.GameplayCharacters world
            let world = rat.SetCharacter CharacterContent.rat world

            just world

        | StartQuitting ->
            let world = World.publish () entity.QuitEvent entity world
            just world

        | StartCombat (area, player, rat) ->
            let world = CombatDispatcher.makeCombat [player; rat] area world 
            just world


    override this.Content (model, entity) = [

        Content.group Simulants.GameplayGui.Name [] [
            Content.button "Shuffle" [
                Entity.Position == v3 -192f 160f 0f
                Entity.Size == v3 64f 16f 0f
                Entity.FontSizing == Some 8
                Entity.Text == "Shuffle!"
                Entity.ClickEvent => Shuffle
            ]

            Content.entity<GraphDispatcher> "Graph" [

            ]

            Content.entity<MenuDispatcher> "Menu" [

            ]

            ContentEx.cursor "Cursor" [
                Entity.CursorType :=
                    if model.HoldingMouse then
                        SoftwareImageCursor Assets.Gameplay.CursorSprite
                    else
                        HardwareImageCursor Assets.Gameplay.CursorSprite
                Entity.Size == v3 16f 16f 0f
            ]
        ]

        match model.GameplayState with
        | Playing ->

            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" [] [
                let coordinates, rotations = model.CoordinatesAndRotations |> List.unzip

                ContentEx.customFilter3dConfig "CustomFilter" [
                    Entity.CustomFilter3dConfig == { CustomFilter3dConfig.defaultConfig with CustomFilterEnabled = true }
                ]

                let cameraPos, cameraQuat =
                    let targetPos = List.head model.Positions
                    if model.PositionOld.IsSome then
                        let oldPos = model.PositionOld.Value
                        let curPos =
                            Vector3.Lerp (fst oldPos, fst targetPos, float32 (30 - model.LerpStepsLeft) / 30f),
                            Quaternion.Slerp (snd oldPos, snd targetPos, float32 (30 - model.LerpStepsLeft) / 30f)
                        curPos
                    else
                        targetPos

                Content.composite<Ball3dDispatcher> "Chisel" [
                    Entity.Position := coordinates[0]
                    Entity.Rotation := rotations[0]
                ] [
                    ContentEx.camera "Camera" [
                        Entity.PositionLocal := cameraPos
                        Entity.RotationLocal := cameraQuat
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

                Content.light3d "Lamp" [
                    Entity.Position := v3 0f 6f 0f
                    Entity.Rotation := Quaternion.CreateFromYawPitchRoll (Math.DegreesToRadians 3.5f * cos (11f * 0.0005f * float32 model.GameplayTime), 0f, Math.DegreesToRadians 3.5f * sin (13f * 0.0005f * float32 model.GameplayTime))
                    Entity.LightCutoff := 7.1f + 0.1f * (sin (0.001f * float32 model.GameplayTime)) + 0.005f * (sin (0.1f * float32 model.GameplayTime))
                    Entity.LightType := SpotLight (0.9f, 1.35f)
                ]

                Content.sphere3d "Sphere" [
                    Entity.Position == v3 1.5f 1.05f 1.6f
                    Entity.Scale == v3 0.25f 0.25f 0.25f
                    Entity.RenderStyle == Forward (0.0f, 0.0f)
                ]

                ContentEx.sign3d "Text" [
                    Entity.Position == v3 0.0f 2.5f -2.6f
                    Entity.Scale == v3 2f -0.5f 1f
                    Entity.Text == "TEHOM"
                    Entity.FontStyling == Set.ofList [ FontStyle.Bold ]
                    Entity.FontSizing == Some 40
                    Entity.RenderStyle == Forward (0.0f, 0.0f)
                    Entity.MaterialProperties == { MaterialProperties.empty with AlbedoOpt = ValueSome (color 0.3f 0.3f 0.3f 1f)}
                ]

            ]

            Content.group Simulants.GameplayCharacters.Name [] []
        | Quit -> ()
    ]