namespace VoxelForge
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module VoxelPalettes =

    let private placeableBlockSources : struct (string * VoxelMaterialKind * bool * Image AssetTag) array =
        [|struct ("Grass", Grass, true, Assets.Voxels.GrassBlock)
          struct ("Dirt", Dirt, true, Assets.Voxels.DirtBlock)
          struct ("Stone", Stone, true, Assets.Voxels.StoneBlock)
          struct ("Cobblestone", Stone, true, Assets.Voxels.CobblestoneBlock)
          struct ("Sand", Sand, true, Assets.Voxels.SandBlock)
          struct ("Oak Log", Wood, true, Assets.Voxels.OakLogBlock)
          struct ("Oak Planks", Wood, true, Assets.Voxels.OakPlanksBlock)
          struct ("Leaves", Leaves, true, Assets.Voxels.LeavesBlock)
          struct ("Glass", Glass, true, Assets.Voxels.GlassBlock)
          struct ("Water", Water, false, Assets.Voxels.WaterBlock)
          struct ("Brick", Brick, true, Assets.Voxels.BrickBlock)|]

    let private cell material solid albedo =
        { Albedo = albedo
          Solid = solid
          Material = material }

    let private cellsFromVolume material solid (volume : VoxelVolumeDescriptor) =
        volume.OccupiedVoxels
        |> Array.map (fun struct (coord, albedo) -> struct (coord, cell material solid albedo))

    let private cellMap (voxels : struct (Vector3i * VoxelCell) array) =
        let cells = Dictionary<Vector3i, VoxelCell> (HashIdentity.Structural)
        for struct (coord, cell) in voxels do
            cells[coord] <- cell
        cells

    let tryBakeBlockTemplate name material solid image voxelSize =
        match VoxelBake.tryBakeSliceAtlasVolume image voxelSize with
        | Some volume ->
            let voxels = cellsFromVolume material solid volume
            Some
                { Name = name
                  Material = material
                  Solid = solid
                  Voxels = voxels
                  Cells = cellMap voxels }
        | None -> None

    let createBlockTemplates voxelSize =
        [|for struct (name, material, solid, image) in placeableBlockSources do
            match tryBakeBlockTemplate name material solid image voxelSize with
            | Some template -> yield template
            | None -> Log.warnOnce ("VoxelForge could not bake block template '" + name + "'.")|]

    let createPlaceableBlocks voxelSize (world : World) =
        [|for i in 0 .. dec placeableBlockSources.Length do
            let struct (name, material, solid, image) = placeableBlockSources[i]
            match VoxelBake.tryBakeSliceAtlasVolume image voxelSize with
            | Some volume ->
                let previewModel = Assets.Voxels.PlaceableBlockPreview i
                World.createUserDefinedVoxelModel volume.VoxelModel previewModel world
                yield
                    { Name = name
                      Voxels = cellsFromVolume material solid volume
                      PreviewModel = previewModel }
            | None ->
                Log.warnOnce ("VoxelForge could not bake placeable block '" + name + "'.")|]

    let requireTemplate name (templates : VoxelBlockTemplate array) =
        match templates |> Array.tryFind (fun template -> template.Name = name) with
        | Some template -> template
        | None -> failwith ("VoxelForge missing generated-world block template '" + name + "'.")

    let tintColor (amount : single) (tint : Color) (albedo : Color) =
        let amount = Math.Clamp (amount, 0.0f, 1.0f)
        let keep = 1.0f - amount
        color
            (albedo.R * keep + tint.R * amount)
            (albedo.G * keep + tint.G * amount)
            (albedo.B * keep + tint.B * amount)
            albedo.A

    let deriveTintedTemplate name material solid (tint : Color) (amount : single) (template : VoxelBlockTemplate) =
        let voxels =
            template.Voxels
            |> Array.map (fun struct (coord, cell) ->
                struct
                    (coord,
                     { cell with
                        Albedo = tintColor amount tint cell.Albedo
                        Solid = solid
                        Material = material }))
        { Name = name
          Material = material
          Solid = solid
          Voxels = voxels
          Cells = cellMap voxels }
