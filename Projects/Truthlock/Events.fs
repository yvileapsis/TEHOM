namespace Truthlock
open System
open Nu

// User-defined screen events.
[<RequireQualifiedAccess>]
module Events =

    let QuitEvent = stoa<unit> "Quit/Event"
    let NovelCompleteEvent = stoa<unit> "Novel/Complete/Event"
    let DebateSolvedEvent = stoa<unit> "Debate/Solved/Event"

[<AutoOpen>]
module EventExtensions =

    type Screen with
        member this.QuitEvent = Events.QuitEvent --> this
        member this.NovelCompleteEvent = Events.NovelCompleteEvent --> this
        member this.DebateSolvedEvent = Events.DebateSolvedEvent --> this
