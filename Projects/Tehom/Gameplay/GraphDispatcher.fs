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
    | LeftClick
    | Select of String
    | Click of String
    interface Message

type GraphCommand =
    | ClickEvent of Entity
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
        Game.MouseLeftDownEvent => LeftClick
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

        | LeftClick ->
            let intersections =
                let ray = World.getMouseRay3dWorld world
                let origin = ray.Origin
                let finale = ray.Origin + 100f * ray.Direction
                let segment = segment3 origin finale
                let array = World.rayCast3dBodies segment 0xFFFFFFFF false world
                let array =
                    array
                    |> Array.choose (fun x ->
                        match x.BodyShapeIntersected.BodyId.BodySource with
                        | :? Entity as intersected when intersected.Is<Ball3dDispatcher> world ->
                            None
                        | :? Entity as intersected ->
                            Some intersected
                        | _ ->
                            None
                    )
                array

            match Array.tryHead intersections with
            | Some intersectionData ->
                [ClickEvent intersectionData], model
            | None ->
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

        | ClickEvent entity ->
            let eventTrace = EventTrace.debug "GraphDispatcher" "handleMouseLeftUp" "Click" EventTrace.empty
            let world = World.publishPlus () entity.ClickEvent eventTrace entity true false world
            World.playSound 1f Assets.Default.Sound world
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

        (*Content.staticSprite "Background" [
            Entity.Size == v3 300f 300f 0f
            Entity.StaticImage == Assets.Default.Black
            Entity.Color == Color.White.WithA 0.5f
        ]*)

        Content.button "Move" [
            Entity.PositionLocal == v3 0f -120f 0f
            Entity.Size == v3 64f 16f 0f
            Entity.FontSizing == Some 8
            Entity.Text == "Move!"
            Entity.ClickEvent => MoveToSelected model.SelectedText
        ]

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

        Content.composite "Container" [
            Entity.ScaleLocal := v3 1f 1f 0.02f
            Entity.PositionLocal == v3 0f 1.025f 1.5f
            Entity.RotationLocal == Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -90f, 0f)
        ] [
            Content.staticModel "Background" [
                Entity.ScaleLocal := v3 1.5f 1.5f 1f
                Entity.MaterialProperties == { MaterialProperties.defaultProperties with AlbedoOpt = ValueSome Color.Black; RoughnessOpt = ValueSome 0f }
            ]


            let sprite name site (coords : Vector3) = Content.sphere3d $"Vertice-{name}" [
                Entity.ScaleLocal :=
                    match site.Type with
                    | Actor ->
                        v3 0.04f 0.04f 2f
                    | Item ->
                        v3 0.03f 0.03f 2f
                    | Safe(``open``, key, items) ->
                        v3 0.04f 0.04f 2f
                    | Ground ->
                        v3 0.05f 0.05f 2f
                    | _ ->
                        v3 0.02f 0.02f 2f
                Entity.PositionLocal := coords / 300f
                Entity.StaticModel == Assets.Default.BallModel
                Entity.MaterialProperties == {
                    MaterialProperties.defaultProperties with
                        AlbedoOpt = ValueSome (
                            match site.Type with
                            | Actor when name = "player" ->
                                Color.Cyan
                            | Actor when name = "rat" ->
                                Color.Red
                            | Actor ->
                                Color.Green
                            | Safe (_) ->
                                Color.Aquamarine
                            | Ground ->
                                Color.White
                            | _ ->
                                Color.Gray
                        )
                        RoughnessOpt = ValueSome 0f
                        MetallicOpt = ValueSome 0f
                }
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
                Content.staticModel $"Line-{label1}-{label2}" [
                    Entity.ScaleLocal := v3 distance 0.005f 2f
                    Entity.PositionLocal := position
                    Entity.RotationLocal := rotation
                    Entity.StaticImage == Assets.Default.White
                    Entity.ElevationLocal == 10f
                    Entity.MaterialProperties == {
                        MaterialProperties.defaultProperties with
                            AlbedoOpt = ValueSome (
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
                            )
                            RoughnessOpt = ValueSome 0f
                            MetallicOpt = ValueSome 0f
                    }

                ]

            for (v, l, p) in model.Vertices do
                sprite v l p

            for (v1, p1, v2, p2, relationship) in model.Edges do
                let pos1 = p1 / 300f
                let pos2 = p2 / 300f
                line v1 v2 pos1 pos2 relationship

        ]

    ]