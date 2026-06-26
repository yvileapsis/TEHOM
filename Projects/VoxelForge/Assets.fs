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
        let Cars = asset<Image> PackageName "cars"
        let ComputerU = asset<Image> PackageName "computer_u"
        let CobblestoneBlock = asset<Image> PackageName "cobblestone_block"
        let DirtBlock = asset<Image> PackageName "dirt_block"
        let GlassBlock = asset<Image> PackageName "glass_block"
        let GrassBlock = asset<Image> PackageName "grass_block"
        let LeavesBlock = asset<Image> PackageName "leaves_block"
        let Minecraft = asset<Image> PackageName "minecraft"
        let OakLogBlock = asset<Image> PackageName "oak_log_block"
        let OakPlanksBlock = asset<Image> PackageName "oak_planks_block"
        let SandBlock = asset<Image> PackageName "sand_block"
        let StoneBlock = asset<Image> PackageName "stone_block"
        let WaterBlock = asset<Image> PackageName "water_block"
        let WoodBlock = asset<Image> PackageName "wood_block"
        let PlaceableBlockPreview index = asset<VoxelModel> PackageName ("PlaceableBlockPreview" + string index)
        let MinecraftLevelChunk x y z = asset<VoxelModel> PackageName ("MinecraftLevel" + string x + "_" + string y + "_" + string z)
        let MinecraftLevelChunkRevision x y z revision = asset<VoxelModel> PackageName ("MinecraftLevel" + string x + "_" + string y + "_" + string z + "_" + string revision)
