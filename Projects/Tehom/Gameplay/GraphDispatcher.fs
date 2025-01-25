namespace Tehom

open System
open System.Numerics
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
    Graph : Sites
    GraphStored : Sites
    DisplayGraph : Graph<String, Vector3, Relationship>
    Iterations : int
    Force : float32
    DistanceMult : float32
    ZeroDistanceForce : float32
    IterationsLeft : int
    NodeDistances : Map<String * String, uint option>
    SelectedText : string
    Zoom : bool
}
with
    static member empty = {
        Area = Area.empty
        Graph = Graph.empty
        GraphStored = Graph.empty
        DisplayGraph = Graph.empty
        Iterations = 10
        Force = 2f
        DistanceMult = 0.15f
        ZeroDistanceForce = 0.025f
        IterationsLeft = 20
        NodeDistances = Map.empty
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

            let model = { model with Area = { Sites = model.Graph; Name = "Whatever" } }

            let attraction (graph : Graph<String, Vector3, Relationship>) =
                graph
                |> Vertices.map (fun v l ->

                    graph
                    |> Vertices.fold l (fun l v' l' ->

                        match model.NodeDistances[v, v'] with
                        | Some targetDistance ->

                            let targetDistance = model.DistanceMult * float32 targetDistance
                            let delta = l - l'
                            let distance = float32 (delta.Length ()) + 0.01f
                            let deltaNorm = delta / distance
                            let distance' = distance - targetDistance
                            let force = distance' * (if targetDistance = 0f then model.ZeroDistanceForce else model.Force / targetDistance)
                            l - deltaNorm * force

                        | None ->

                            let targetDistance = 20f
                            let delta = l - l'
                            let distance = float32 (delta.Length ()) + 0.01f
                            let deltaNorm = delta / distance
                            let distance' = distance - targetDistance
                            let force = distance' * model.ZeroDistanceForce
                            l - deltaNorm * force

                    )
                )

            let recenter (graph : Graph<String, Vector3, Relationship>) =
                let center =
                    graph
                    |> Vertices.toList
                    |> List.map snd
                    |> fun list ->
                        let (sum : Vector3) = List.sum list
                        let (average : Vector3) = sum / float32 (List.length list)
                        average

                graph
                |> Vertices.map (fun v l ->
                    l - center
                )


            let iter = attraction >> recenter
            let fiveiters = List.init model.Iterations (fun _ -> iter) |> List.fold (>>) id

            if model.Graph <> model.GraphStored then
                let graph =
                    model.Graph
                    |> Vertices.map (fun v l ->
                        match Vertices.tryFind v model.DisplayGraph with
                        | Some (_, l) ->
                            l
                        | _ ->
                            let list =
                                model.Graph
                                |> Vertices.toList
                                |> List.sort
                                |> List.map fst

                            let i = List.findIndex ((=) v) list
                            let x = float32 (i % 6) * 16f - 3f * 16f
                            let y = float32 (i / 6) * 16f - 3f * 16f
                            v3 x y 0f
                    )

                let graph' =
                    graph
                    |> Edges.undirect (fun edge1 edge2 ->
                        edge1
                    )
                    |> Edges.choose (fun v1 v2 edge ->
                        match edge with
                        | Distance x -> Some (uint x)
                        | Consists -> None
                        | Contains -> Some 150u
                        | LiesAbove -> Some 150u
                        | Covers -> Some 150u
                        | IsOnEdge -> Some 50u
                    )

                let vertices =
                    graph' |> Vertices.toList |> List.map fst

                let nodeDistance =
                    vertices
                    |> List.fold (fun map v ->
                        let tree = Query.spTree v graph'

                        vertices
                        |> List.fold (fun (map : Map<String * String, uint option>) v' ->
                            let distance =
                                Query.getDistance v' tree

                            Map.add (v, v') distance map
                        ) map
                    ) Map.empty


                let model = { model with DisplayGraph = graph; GraphStored = model.Graph; NodeDistances = nodeDistance; IterationsLeft = 100 }
                just model
            elif model.IterationsLeft > 0 then
                let graph = model.DisplayGraph
                let graph = graph |> fiveiters
                let model = { model with DisplayGraph = graph; IterationsLeft = model.IterationsLeft - model.Iterations }
                just model
            else
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
            let model = { model with Graph = Vertices.remove s model.Graph }
            [Click2 s], model

    override this.Command (model, command, entity, world) =

        match command with
        | Click2 s ->
            let player = Simulants.GameplayCharacters / "player"
            let world = player.SetCharacterWith (Character.addItem s) world
            just world

    override this.Content (model, _) = [

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

        let graph = model.DisplayGraph

        for (v, l) in Vertices.toList graph do
            sprite v l

        for (label1, label2, relationship) in Graph.Directed.Edges.toList graph do
            let _, pos1 = Vertices.find label1 graph
            let _, pos2 = Vertices.find label2 graph
            let pos1 = if model.Zoom then pos1 / 3f else pos1
            let pos2 = if model.Zoom then pos2 / 3f else pos2
            match relationship with
            | Consists ->
                ()
            | _ ->
                line label1 label2 pos1 pos2 relationship


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

        let graph' =
            model.Graph
            |> Edges.undirect2 (fun vertex1 vertex2 edge ->
                match edge with
                | Contains -> true
                | LiesAbove -> true
                | Covers -> true
                | IsOnEdge -> true
                | _ -> false
            ) (fun edge1 edge2 ->
                edge1
            )
            |> Edges.choose (fun v1 v2 edge ->
                match edge with
                | Distance x -> Some (uint x)
                | Consists -> None
                | Contains -> Some 0u
                | LiesAbove -> Some 0u
                | Covers -> Some 0u
                | IsOnEdge -> Some 0u
            )

        let vertices =
            graph'
            |> Vertices.toList |> List.map fst

        let nodeDistance =
            vertices
            |> List.fold (fun map v ->
                let tree = Query.spTree v graph'

                vertices
                |> List.fold (fun (map : Map<String * String, uint option>) v' ->
                    let distance =
                        Query.getDistance v' tree

                    Map.add (v, v') distance map
                ) map
            ) Map.empty


        let allwithinreach =
            nodeDistance
            |> Map.toList
            |> List.filter (fun ((v, v'), distance) -> v = "player" && distance.IsSome && (distance < Some 150u))
            |> List.map (fun ((v, v'), distance) -> v', distance)

        Content.association "usables" [
            Entity.Absolute == false
            Entity.PositionLocal == v3 -180.0f 0.0f 0.0f
            Entity.Size == v3 60.0f 40.0f 0.0f
            Entity.Elevation == 10.0f
            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
            Entity.Layout == Flow (FlowDownward, FlowUnlimited)

        ] [

            for (vertex, distance) in allwithinreach do
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