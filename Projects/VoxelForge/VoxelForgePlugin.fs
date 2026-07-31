namespace VoxelForge
open System
open Nu

type VoxelForgePlugin (profileStartupModeOpt : VoxelForgeMode option, voxelRenderModeOpt : VoxelRenderMode option) =
    inherit NuPlugin ()

    let mutable profileStartupModeApplied = false

    new () =
        VoxelForgePlugin (None, None)

    override this.PreProcess world =
        if not profileStartupModeApplied then
            profileStartupModeApplied <- true
            match voxelRenderModeOpt with
            | Some voxelRenderMode ->
                let renderer3dConfig =
                    { World.getRenderer3dConfig world with
                        VoxelRenderMode = voxelRenderMode }
                World.configureRenderer3d renderer3dConfig world
            | None -> ()
            match profileStartupModeOpt with
            | Some Splash -> Game.SetVoxelForge VoxelForge.splash world
            | Some Title -> Game.SetVoxelForge VoxelForge.title world
            | Some Credits -> Game.SetVoxelForge VoxelForge.credits world
            | Some WorldGeneration -> Game.SetVoxelForge VoxelForge.worldGeneration world
            | Some Gameplay -> Game.SetVoxelForge (VoxelForge.gameplay None) world
            | None -> ()

    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetVoxelForge VoxelForge.splash world)
             ("Title", fun world -> Game.SetVoxelForge VoxelForge.title world)
             ("Credits", fun world -> Game.SetVoxelForge VoxelForge.credits world)
             ("WorldGeneration", fun world -> Game.SetVoxelForge VoxelForge.worldGeneration world)
             ("Gameplay", fun world ->
                Simulants.Gameplay.SetGameplay Gameplay.initial world
                Game.SetVoxelForge (VoxelForge.gameplay None) world)]

    override this.InitialPackages =
        [Assets.Gui.PackageName
         Assets.Gameplay.PackageName
         Assets.Voxels.PackageName]
