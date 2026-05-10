namespace VoxelForge
open System
open Nu
open VoxelForge

type VoxelForgePlugin () =
    inherit NuPlugin ()

    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetVoxelForge Splash world)
             ("Title", fun world -> Game.SetVoxelForge Title world)
             ("Credits", fun world -> Game.SetVoxelForge Credits world)
             ("Gameplay", fun world ->
                Simulants.Gameplay.SetGameplay Gameplay.initial world
                Game.SetVoxelForge Gameplay world)]

    override this.InitialPackages =
        [Assets.Gui.PackageName
         Assets.Gameplay.PackageName
         Assets.Voxels.PackageName]
