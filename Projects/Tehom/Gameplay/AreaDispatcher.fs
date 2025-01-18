namespace Tehom

open System
open Prime
open Nu

[<AutoOpen>]
module AreaExtensions =
    type Entity with
        member this.GetArea world = this.GetModelGeneric<Area> world
        member this.SetArea value world = this.SetModelGeneric<Area> value world
        member this.Area = this.ModelGeneric<Area> ()
        member this.SetAreaWith updater world =
            let character = this.GetArea world
            let character = updater character
            this.SetArea character world

type AreaDispatcher () =
    inherit EntityDispatcher<Area, Message, Command> (true, false, false, false, Area.empty)

    override this.Definitions (_, _) =
        []

    override this.Render (_, _, _, _) =
        ()

    override this.Content (model, entity) = [
        Content.staticSprite "Background" [
            Entity.Size == v3 300f 300f 0f
            Entity.StaticImage == Assets.Default.Black
            Entity.Color == Color.White.WithA 0.5f
        ]

        Content.entity<GraphDispatcher> "Graph" [
        ]
    ]

    override this.Update (entity, world) =

        let model = entity.GetArea world
        let graph = entity / "Graph"
        let graphSites = graph.GetGraph world
        let graphSites = { graphSites with Graph = model.Sites }
        let world = graph.SetGraph graphSites world

        world

[<AutoOpen>]
module AreaEntity =
    let area (character : Area) =
        Content.composite<AreaDispatcher> character.Name [
            Entity.Area == character
        ] [

        ]