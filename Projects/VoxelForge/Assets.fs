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

        let Cars = asset<Image> PackageName "cars"
        let ComputerU = asset<Image> PackageName "computer_u"
        let GrassBlock = asset<Image> PackageName "grass_block"
        let Minecraft = asset<Image> PackageName "minecraft"
        let WoodBlock = asset<Image> PackageName "wood_block"
        let MinecraftLevelChunk x y z = asset<VoxelModel> PackageName ("MinecraftLevel" + string x + "_" + string y + "_" + string z)
        let MinecraftLevelChunkRevision x y z revision = asset<VoxelModel> PackageName ("MinecraftLevel" + string x + "_" + string y + "_" + string z + "_" + string revision)
