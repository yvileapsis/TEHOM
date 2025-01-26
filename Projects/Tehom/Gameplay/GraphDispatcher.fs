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
    | Select of String
    | Click of String
    interface Message

type GraphCommand =
    | RemoveFromGraph of String
    | AddItemToPlayer of String
    | MoveToSelected of String
    | ReplaceSiteWith of String * Site
    interface Command

type SitesGraph = {
    Vertices : List<String * Site * Vector3>
    Edges : List<String * Vector3 * String * Vector3 * Relationship>
    Clickables : List<String * UInt32>

    SelectedText : String
    Zoom : Boolean
}
with
    static member empty = {

        Vertices = List.empty
        Edges = List.empty
        Clickables = List.empty

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

            let vertices = Area.getDisplayVertices area

            let clickables =
                Area.getWithinReach "player" 150u area
                @ (vertices |> List.choose (fun (s, site, _) -> if site.Type = Ground then Some (s, 0u) else None))
                |> List.distinctBy fst

            let model = {
                model with
                    Vertices = vertices
                    Edges = Area.getDisplayEdges area
                    Clickables = clickables
                    Zoom = World.getExists Simulants.GameplayCombat world
            }

            just model

        | Select str ->
            let model = { model with SelectedText = str }
            just model

        | Click s ->
            let area = Simulants.GameplayArea
            let area = area.GetArea world

            let (s, site) = Area.getSite s area

            match site.Type with
            | Abstract ->
                just model
            | Ground ->
                [MoveToSelected s], model
            | RoomParts ->
                just model
            | Item ->
                [RemoveFromGraph s; AddItemToPlayer s], model
            | Furniture ->
                just model
            | Safe (notclosed, key, items) ->
                let player = Simulants.GameplayCharacters / "player"
                let playerModel = player.GetCharacter world
                if notclosed || List.contains key playerModel.Items then
                    let site = { site with Type = Safe (true, key, []) }
                    [
                        ReplaceSiteWith (s, site)
                        for i in items do AddItemToPlayer i
                    ], model
                else
                    just model

    override this.Command (model, command, entity, world) =

        match command with
        | RemoveFromGraph s ->
            let area = Simulants.GameplayArea
            let world = area.SetAreaWith (Area.removeSite s) world
            just world

        | AddItemToPlayer s ->
            let player = Simulants.GameplayCharacters / "player"
            let world = player.SetCharacterWith (Character.addItem s) world
            just world

        | MoveToSelected s ->
            let player = Simulants.GameplayCharacters / "player"
            let playerModel = player.GetCharacter world

            let speed = Character.getSpeed playerModel
            let reach = Character.getReach playerModel

            let area = Simulants.GameplayArea

            let effect = GameEffect.travel player.Name s reach speed area world
            let world = player.ExecuteGameEffects effect world

            just world

        | ReplaceSiteWith (s, site) ->
            let area = Simulants.GameplayArea
            let world = area.SetAreaWith (Area.replaceSite s site) world
            just world

    override this.TruncateModel model = {
        model with
            Vertices = List.empty
            Edges = List.empty
            Clickables = List.empty
    }

    override this.UntruncateModel (model, model') = {
        model with
            Vertices = model'.Vertices
            Edges = model'.Edges
            Clickables = model'.Clickables
    }

    override this.Content (model, _) = [

        Content.staticSprite "Background" [
            Entity.Size == v3 300f 300f 0f
            Entity.StaticImage == Assets.Default.Black
            Entity.Color == Color.White.WithA 0.5f
        ]

        Content.button "Move" [
            Entity.PositionLocal == v3 0f -120f 0f
            Entity.Size == v3 64f 16f 0f
            Entity.FontSizing == Some 8
            Entity.Text == "Move!"
            Entity.ClickEvent => MoveToSelected model.SelectedText
        ]

        let sprite name site coords = Content.button $"Vertice-{name}" [
            Entity.Size := v3 6f 6f 0f
            Entity.PositionLocal := if model.Zoom then coords / 3f else coords
            Entity.UpImage ==
                match site.Type with
                | Actor ->
                    Assets.Default.Ball
                | Item ->
                    Assets.Default.Ball
                | Safe(``open``, key, items) ->
                    Assets.Default.Brick
                | _ ->
                    Assets.Default.White
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

        for (v, l, p) in model.Vertices do
            sprite v l p

        for (v1, p1, v2, p2, relationship) in model.Edges do
            let pos1 = if model.Zoom then p1 / 3f else p1
            let pos2 = if model.Zoom then p2 / 3f else p2
            line v1 v2 pos1 pos2 relationship

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

            for (vertex, distance) in model.Clickables do
                Content.button $"use{vertex}" [
                    Entity.Absolute == false
                    Entity.Size == v3 60.0f 5.0f 0.0f
                    Entity.Text := $"{vertex} [{distance}]"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.FontSizing == Some 5
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                    Entity.TextColor := Color.FloralWhite
                    Entity.ClickEvent => Click vertex
                ]

        ]


    ]