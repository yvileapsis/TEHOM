namespace VoxelForge
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Assets =

    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"

    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"

    [<RequireQualifiedAccess>]
    module Voxels =

        let PackageName = "Voxels"

        let BrickBlock = asset<Image> PackageName "brick_block"
        let CobblestoneBlock = asset<Image> PackageName "cobblestone_block"
        let DirtBlock = asset<Image> PackageName "dirt_block"
        let GlassBlock = asset<Image> PackageName "glass_block"
        let LeavesBlock = asset<Image> PackageName "leaves_block"
        let StoneBlock = asset<Image> PackageName "stone_block"
        let WaterBlock = asset<Image> PackageName "water_block"
        let FabricationSamplePreview index = asset<VoxelModel> PackageName ("FabricationSamplePreview" + string index)
        let FacilityOverview = asset<VoxelModel> PackageName "FacilityOverview"
        let FacilityLevelChunkRevision x y z revision = asset<VoxelModel> PackageName ("FacilityLevel" + string x + "_" + string y + "_" + string z + "_" + string revision)
