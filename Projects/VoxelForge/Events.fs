namespace VoxelForge
open System
open Nu

[<RequireQualifiedAccess>]
module Events =

    let QuitEvent = stoa<unit> "Quit/Event"
