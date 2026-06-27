namespace VoxelForge
open System
open Nu

type VoxelForgePlugin () =
    inherit NuPlugin ()

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
