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
    | AddWeaponToPlayer of Weapon
    | MoveToSelected of String
    | ReplaceSiteWith of String * Site
    | StartCombat of Entity * Entity * Entity
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
        Entity.PositionLocal := v3 0f 0f 0f // if model.Zoom then v3 90f 90f 0f else
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
            if model.SelectedText <> str then
                let model = { model with SelectedText = str }
                just model
            else
                [Click str], model

        | Click s ->
            if not (List.contains s (model.Clickables |> List.map fst)) then

                let area = Simulants.GameplayArea
                let areaModel = area.GetArea world

                let (s, site) = Area.getSite s areaModel

                match site.Type with
                | Actor ->
                    printfn $"that's {s} {site.Label}"
                    let player = Simulants.GameplayCharacters / "player"
                    let rat = Simulants.GameplayCharacters / s
                    [
                        StartCombat (area, player, rat)
                    ], model
                | _ ->
                    printfn "not in range"
                    just model

            else
                let area = Simulants.GameplayArea
                let areaModel = area.GetArea world

                let (s, site) = Area.getSite s areaModel

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
                | Safe (notclosed, key, items, weapons) ->
                    let player = Simulants.GameplayCharacters / "player"
                    let playerModel = player.GetCharacter world
                    if notclosed || List.contains key playerModel.Items then
                        let site = { site with Type = Safe (true, key, [], []) }
                        [
                            ReplaceSiteWith (s, site)
                            for i in items do AddItemToPlayer i
                            for i in weapons do AddWeaponToPlayer i
                        ], model
                    else
                        just model
                | Actor ->
                    printfn $"that's {site.Label}"
                    let player = Simulants.GameplayCharacters / "player"
                    let rat = Simulants.GameplayCharacters / s
                    [
                        StartCombat (area, player, rat)
                    ], model

        | LeftClick ->
            let intersections =
                let ray = World.getMouseRay3dWorld world
                let segment = ray3 ray.Origin (100f * ray.Direction)
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

        | AddWeaponToPlayer s ->
            let player = Simulants.GameplayCharacters / "player"
            let world = player.SetCharacterWith (Character.addWeapon s) world
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

        | StartCombat (area, player, rat) ->
            let world = CombatDispatcher.makeCombat [player; rat] area world
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

        (*Content.association "usables" [
            Entity.Absolute == false
            Entity.PositionLocal == v3 -180.0f 0.0f 0.0f
            Entity.Size == v3 60.0f 40.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
            Entity.Layout == Flow (FlowDownward, FlowUnlimited)
            Entity.Visible == false

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

        ]*)


        Content.composite "usables2" [
            Entity.ScaleLocal := v3 1f 1f 0.02f
            Entity.PositionLocal == v3 -0.75f 1.025f 1.5f
            Entity.RotationLocal == Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -90f, 0f)
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)

        ] [
            Content.staticModel "Background" [
                Entity.ScaleLocal := v3 0.5f 1.5f 1f
                Entity.MaterialProperties == {
                    MaterialProperties.defaultProperties with
                        AlbedoOpt = ValueSome (Color.Black.WithA 0.5f)
                        RoughnessOpt = ValueSome 0f
                }
                Entity.RenderStyle == Forward (0f, 0f)
            ]

            for i, (vertex, distance) in List.indexed model.Clickables do
                ContentEx.sign3d $"use{vertex}" [
                    Entity.Absolute == false
                    Entity.Text := $"{vertex} [{distance}]"
                    Entity.Font == Assets.Gui.ClearSansFont
                    Entity.ScaleLocal := v3 0.25f -0.04f 0.125f
                    Entity.PositionLocal := v3 0f (0.7f - 0.05f * float32 i) 0.6f
                    Entity.FontSizing == Some 30
                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                    Entity.TextColor := Color.FloralWhite
                    Entity.ClickEvent => Click vertex
                    Entity.BodyShape == (BoxShape { Size = v3 1f 30f 1f; TransformOpt = None; PropertiesOpt = None })
                ]

        ]

        Content.composite "Container" [
            Entity.ScaleLocal := v3 0.6f 0.6f 0.02f
            Entity.PositionLocal == v3 0f 1.025f 1.5f
            Entity.RotationLocal == Quaternion.CreateFromYawPitchRoll (0f, Math.DegreesToRadians -90f, 0f)
        ] [
            Content.staticModel "Background" [
                Entity.ScaleLocal := v3 1.5f 1.5f 1f
                Entity.MaterialProperties == {
                    MaterialProperties.defaultProperties with
                        AlbedoOpt = ValueSome (Color.Black.WithA 0.5f)
                        RoughnessOpt = ValueSome 0f
                }
                Entity.RenderStyle == Forward (0f, 0f)
            ]

            ContentEx.sign3d "Act" [
                Entity.PositionLocal == v3 -0.5f -0.5f 1.0f
                Entity.ScaleLocal := v3 0.3f -0.05f 0.125f

                Entity.FontSizing == Some 30
                Entity.Text == "Use Selected"
                Entity.ClickEvent => Click model.SelectedText
                Entity.BodyShape == (BoxShape { Size = v3 1f 30f 1f; TransformOpt = None; PropertiesOpt = None })
            ]

            ContentEx.sign3d "SelectedText" [

                Entity.PositionLocal == v3 -0.5f -0.45f 1.0f
                Entity.ScaleLocal := v3 0.3f -0.05f 0.125f

                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Text := model.SelectedText
                Entity.TextColor == Color.FloralWhite
                Entity.Font == Assets.Gui.ClearSansFont
                Entity.FontSizing == Some 30
                Entity.ClickEvent => Select ""
                Entity.BodyShape == (BoxShape { Size = v3 1f 30f 1f; TransformOpt = None; PropertiesOpt = None })
            ]

            let sprite name site (coords : Vector3) = Content.sphere3d $"Vertice-{name}" [
                Entity.ScaleLocal :=
                    match site.Type with
                    | Actor ->
                        v3 0.04f 0.04f 2f
                    | Item ->
                        v3 0.03f 0.03f 2f
                    | Safe(``open``, key, items, weapons) ->
                        v3 0.04f 0.04f 2f
                    | Ground ->
                        v3 0.05f 0.05f 2f
                    | _ ->
                        v3 0.02f 0.02f 2f
                Entity.PositionLocal := coords / 300f
                Entity.StaticModel == Assets.Default.BallModel
                Entity.MaterialProperties := {
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
                    Entity.MaterialProperties := {
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