namespace VoxelForge
open System
open Nu

[<RequireQualifiedAccess>]
module Events =

    let QuitEvent = stoa<unit> "Quit/Event"
    let WorldGeneratedEvent = stoa<GeneratedWorldPackage> "WorldGenerated/Event"
