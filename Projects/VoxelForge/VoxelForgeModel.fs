namespace VoxelForge
open System
open Prime
open Nu

type VoxelForgeMode =
    | Splash
    | Title
    | Credits
    | WorldGeneration
    | Gameplay

type VoxelForge =
    { Mode : VoxelForgeMode
      GeneratedWorldPackageOpt : GeneratedWorldPackage option }

[<RequireQualifiedAccess>]
module VoxelForge =

    let initial =
        { Mode = Splash
          GeneratedWorldPackageOpt = None }

    let splash =
        { initial with Mode = Splash }

    let title =
        { initial with Mode = Title }

    let credits =
        { initial with Mode = Credits }

    let worldGeneration =
        { initial with Mode = WorldGeneration }

    let gameplay generatedWorldPackageOpt =
        { Mode = Gameplay
          GeneratedWorldPackageOpt = generatedWorldPackageOpt }

[<AutoOpen>]
module VoxelForgeExtensions =
    type Game with
        member this.GetVoxelForge world = this.GetModelGeneric<VoxelForge> world
        member this.SetVoxelForge value world = this.SetModelGeneric<VoxelForge> value world
        member this.VoxelForge = this.ModelGeneric<VoxelForge> ()
