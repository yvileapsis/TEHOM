namespace Tehom

open System
open System.Numerics
open FParsec
open Prime
open Nu
open Graph
open Area

type GraphMessage =
    | Update
    | Select of string
    | Click of string
    interface Message

type GraphCommand =
    | Click2 of string
    interface Command

type SitesGraph = {
    Area : Area
    SelectedText : string
    Zoom : bool
}
with
    static member empty = {
        Area = Area.empty
        SelectedText = ""
        Zoom = false
    }

[<AutoOpen>]
module GraphExtensions =
    type Entity with
        member this.GetGraph world = this.GetModelGeneric<SitesGraph> world
        member this.SetGraph value world = this.SetModelGeneric<SitesGraph> value world
        member this.Graph = this.ModelGeneric<SitesGraph> ()

type GraphDispatcher () =
    inherit Entity2dDispatcher<SitesGraph, GraphMessage, GraphCommand> (false, false, false, SitesGraph.empty)

    override this.Definitions (model, _) = [
        Screen.UpdateEvent => Update
        Entity.AlwaysUpdate == true
        Entity.Size == v3 120f 120f 0f
        Entity.PositionLocal := if model.Zoom then v3 90f 90f 0f else v3 0f 0f 0f
    ]

    override this.Message (model, message, entity, world) =

        match message with
        | Update ->

            let area = Simulants.GameplayArea
            let area = area.GetArea world

            let model = { model with Area = area }

            let model =
                if (World.getExists Simulants.GameplayCombat world) then
                    { model with Zoom = true }
                else
                    model

            just model

        | Select str ->
            let model = { model with SelectedText = str }
            just model

        | Click s ->
            // TODO: take key through use action
            //let model = { model with Graph = Vertices.remove s model.Graph }
            [Click2 s], model

    override this.Command (model, command, entity, world) =

        match command with
        | Click2 s ->
            let player = Simulants.GameplayCharacters / "player"
            let world = player.SetCharacterWith (Character.addItem s) world
            just world

    override this.Content (model, _) = [

        Content.staticSprite "Background" [
            Entity.Size == v3 300f 300f 0f
            Entity.StaticImage == Assets.Default.Black
            Entity.Color == Color.White.WithA 0.5f
        ]

        let sprite name coords = Content.button $"Vertice-{name}" [
            Entity.Size := v3 6f 6f 0f
            Entity.PositionLocal := if model.Zoom then coords / 3f else coords
            Entity.UpImage == Assets.Default.Ball
            Entity.DownImage == Assets.Default.Ball
            Entity.ElevationLocal == 20f
            Entity.TextColor ==
                if name = "player" then
                    Color.Cyan
                elif name = "rat" then
                    Color.Red
                else
                    Color.White
            Entity.Text == string (Array.head (String.toArray name))
            Entity.FontSizing == Some 6
            Entity.ClickEvent => Select name
        ]

        let line label1 label2 (coord1 : Vector3) (coord2 : Vector3) relationship =
            let position = (coord2 + coord1) / 2f
            let distance = (coord2 - coord1).Length ()
            let vector = (coord2 - coord1)
            let angle = Math.Atan2 (float vector.Y, float vector.X)
            let rotation = Quaternion.CreateFromYawPitchRoll (0f, 0f, float32 angle)
            Content.staticSprite $"Line-{label1}-{label2}" [
                Entity.Size := v3 distance 2f 0f
                Entity.PositionLocal := position
                Entity.RotationLocal := rotation
                Entity.StaticImage == Assets.Default.White
                Entity.ElevationLocal == 10f
                Entity.Color ==
                    match relationship with
                    | Distance uint32 ->
                        Color.White
                    | Consists ->
                        Color.Black
                    | Contains ->
                        Color.Gray
                    | LiesAbove ->
                        Color.Green
                    | Covers ->
                        Color.Beige
                    | IsOnEdge ->
                        Color.BlueViolet
            ]

        for (v, l) in Vertices.toList model.Area.Sites do
            match l.Position with
            | Some pos ->
                sprite v pos
            | None ->
                ()

        for (label1, label2, relationship) in Graph.Directed.Edges.toList model.Area.Sites do
            let _, pos1 = Vertices.find label1 model.Area.Sites
            let _, pos2 = Vertices.find label2 model.Area.Sites
            match  pos1, pos2 with
            | { Position = Some pos1 }, { Position = Some pos2 } ->
                let pos1 = if model.Zoom then pos1 / 3f else pos1
                let pos2 = if model.Zoom then pos2 / 3f else pos2
                match relationship with
                | Consists ->
                    ()
                | _ ->
                    line label1 label2 pos1 pos2 relationship
            | _ ->
                ()

        Content.text "SelectedText" [
            Entity.Size == v3 120f 10f 0f
            Entity.PositionLocal := if model.Zoom then v3 0f -40f 0f else v3 0f -100f 0f
            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
            Entity.Text := model.SelectedText
            Entity.TextColor == Color.FloralWhite
            Entity.Font == Assets.Gui.ClearSansFont
            Entity.FontSizing == Some 8
            Entity.ClickEvent => Select ""
        ]

        Content.association "usables" [
            Entity.Absolute == false
            Entity.PositionLocal == v3 -180.0f 0.0f 0.0f
            Entity.Size == v3 60.0f 40.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
            Entity.Layout == Flow (FlowDownward, FlowUnlimited)

        ] [

            for (vertex, distance) in Area.getWithinReach "player" 150u model.Area do
                Content.button $"use{vertex}" [
                    Entity.Absolute == false
                    Entity.Size == v3 60.0f 5.0f 0.0f
                    Entity.Text := $"{vertex}"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 5
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                    Entity.TextColor := Color.FloralWhite
                    Entity.ClickEvent => Click vertex
                ]

        ]


    ]