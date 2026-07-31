namespace VoxelForge
open System
open Nu

[<RequireQualifiedAccess>]
module Events =

    let MainMenuEvent = stoa<unit> "MainMenu/Event"
    let WorldGeneratedEvent = stoa<GeneratedWorldPackage> "WorldGenerated/Event"
