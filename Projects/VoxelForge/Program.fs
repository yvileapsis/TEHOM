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

    let private tryGetVoxelRenderMode (args : string array) =
        if Array.exists ((=) "--voxel-splats") args then Some VoxelRenderMode.Splats
        elif Array.exists ((=) "--voxel-faces") args then Some VoxelRenderMode.Faces
        else
            match Environment.GetEnvironmentVariable "VOXELFORGE_VOXEL_RENDER_MODE" with
            | mode when String.Equals (mode, "splats", StringComparison.OrdinalIgnoreCase) -> Some VoxelRenderMode.Splats
            | mode when String.Equals (mode, "faces", StringComparison.OrdinalIgnoreCase) -> Some VoxelRenderMode.Faces
            | _ -> None

    let [<EntryPoint; STAThread>] main args =

        Directory.SetCurrentDirectory AppContext.BaseDirectory
        Nu.init ()
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "VoxelForge" }
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
        World.run ignore worldConfig (VoxelForgePlugin (tryGetProfileStartupMode args, tryGetVoxelRenderMode args))
