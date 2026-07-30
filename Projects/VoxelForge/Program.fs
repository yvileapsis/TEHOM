namespace VoxelForge
open System
open System.IO
open Nu

module Program =

    let private tryGetProfileStartupMode (args : string array) =
        if Array.exists ((=) "--profile-start-world-generation") args then Some WorldGeneration
        elif Array.exists ((=) "--profile-start-gameplay") args then Some Gameplay
        elif Array.exists ((=) "--profile-start-title") args then Some Title
        else None

    let [<EntryPoint; STAThread>] main args =

        Directory.SetCurrentDirectory AppContext.BaseDirectory
        Nu.init ()
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "VoxelForge" }
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
        World.run ignore worldConfig (VoxelForgePlugin (tryGetProfileStartupMode args))
