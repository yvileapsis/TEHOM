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

    override this.Update (entity, world) =
        entity.SetAreaWith (Area.iterateDisplay 1) world

    override this.Render (_, _, _, _) =
        ()


[<AutoOpen>]
module AreaEntity =
    let area (area : Area) =
        Content.composite<AreaDispatcher> area.Name [
            Entity.Area == area
        ] [

        ]