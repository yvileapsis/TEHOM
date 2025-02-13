namespace MyGame
open System
open Nu

[<RequireQualifiedAccess>]
module Events =

    let AttackEvent = stoa<Entity> "Attack/Event"
    let DieEvent = stoa<Entity> "Die/Event"
    let QuitEvent = stoa<unit> "Quit/Event"

// these events are sent from all over the place TO the handling entities
// very convenient as you don't need to make up a complicated wildcard sequence for all possible event generators
// inconvenient as it's harder to tell where it's from
[<RequireQualifiedAccess>]
module EventsInverse =

    let TakeItemEvent = stoa<Entity> "TakeItem/Event"

[<AutoOpen>]
module GenericExtensions =
    type Entity with
        member this.TakeItemEvent = EventsInverse.TakeItemEvent --> this