namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open Graph
open Area

type GraphMessage =
    | Update
    interface Message

type SitesGraph = {
    Graph : Sites
    GraphStored : Sites
    DisplayGraph : Graph<String, Vector3, Relationship>
    Iterations : int
    AttractionForce : float32
    RepulsionForce : float32
    CenterForce : float32
    IterationsLeft : int
}
with
    static member empty = {
        Graph = Graph.empty
        GraphStored = Graph.empty
        DisplayGraph = Graph.empty
        Iterations = 50
        AttractionForce = 0.002f
        RepulsionForce = 0.05f
        CenterForce = 0.007f
        IterationsLeft = 1000
    }

[<AutoOpen>]
module GraphExtensions =
    type Entity with
        member this.GetGraph world = this.GetModelGeneric<SitesGraph> world
        member this.SetGraph value world = this.SetModelGeneric<SitesGraph> value world
        member this.Graph = this.ModelGeneric<SitesGraph> ()

type GraphDispatcher () =
    inherit Entity2dDispatcher<SitesGraph, GraphMessage, Command> (false, false, false, SitesGraph.empty)

    override this.Definitions (_, _) = [
        Screen.UpdateEvent => Update
        Entity.AlwaysUpdate == true
        Entity.Size == v3 120f 120f 0f
    ]

    override this.Message (model, message, entity, world) =

        match message with
        | _ ->


            let repulsion (graph : Graph<String, Vector3, Relationship>) =
                graph
                |> Vertices.map (fun v l ->
                    graph
                    |> Vertices.fold l (fun l v' l' ->
                        let delta = l - l'
                        let distance = float32 (delta.Length ()) + 0.01f
                        let force = model.RepulsionForce / distance
                        l + (delta / distance) * force
                    )
                )

            let attraction (graph : Graph<String, Vector3, Relationship>) =
                graph
                |> Vertices.map (fun v l ->
                    let e1, _, _, e2 = Graph.getContext v graph
                    let list = e1 @ e2

                    list
                    |> List.fold (fun l (v', e) ->
                        let _, _, l', _ = Graph.getContext v' graph
                        let delta = l - l'
                        let distance = float32 (delta.Length ()) + 0.01f
                        let force = distance * distance * model.AttractionForce
                        l - (delta / distance) * force
                    ) l
                )

            let center (graph : Graph<String, Vector3, Relationship>) =
                graph
                |> Vertices.map (fun v l ->
                    let e1, _, _, e2 = Graph.getContext v graph
                    let list = e1 @ e2

                    let centerDistance = v3 0f 0f 0f
                    let numberedges = List.length list
                    let delta = l - centerDistance
                    let distance = float32 (delta.Length ()) + 0.01f
                    let force = (float32 numberedges) * model.CenterForce * distance

                    l - (delta / distance) * force
                )

            let iter = repulsion >> attraction >> center
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
                let model = { model with DisplayGraph = graph; GraphStored = model.Graph; IterationsLeft = 1000 }
                just model
            elif model.IterationsLeft > 0 then
                let graph = model.DisplayGraph
                let graph = graph |> fiveiters
                let model = { model with DisplayGraph = graph; IterationsLeft = model.IterationsLeft - model.Iterations }
                just model
            else
                just model

    override this.Command (model, command, entity, world) =

        match command with
        | _ ->
            just world

    override this.Content (model, _) = [

        let sprite name coords = Content.staticSprite name [
            Entity.Size := v3 6f 6f 0f
            Entity.PositionLocal := coords
            Entity.StaticImage == Assets.Default.Ball
            Entity.Elevation == 10f
        ]

        let line name (coord1 : Vector3) (coord2 : Vector3) =
            let position = (coord2 + coord1) / 2f
            let distance = (coord2 - coord1).Length ()
            let vector = (coord2 - coord1)
            let angle = Math.Atan2 (float vector.Y, float vector.X)
            let rotation = Quaternion.CreateFromYawPitchRoll (0f, 0f, float32 angle)
            Content.staticSprite name [
                Entity.Size := v3 distance 2f 0f
                Entity.PositionLocal := position
                Entity.RotationLocal := rotation
                Entity.StaticImage == Assets.Default.White
                Entity.Elevation == 5f
            ]

        let graph = model.DisplayGraph

        for (v, l) in Vertices.toList graph do
            sprite $"Vertice-{v}" l

        for (label1, label2, _) in Graph.Directed.Edges.toList graph do
            let _, pos1 = Vertices.find label1 graph
            let _, pos2 = Vertices.find label2 graph
            line $"Line-{label1}-{label2}" pos1 pos2
    ]