// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Numerics
open Prime


/// Dense palette-indexed voxel data used by editable splat rendering.
type VoxelGridDescriptor =
    { Size : Vector3i
      Origin : Vector3
      IndexBits : int
      Indices : uint array
      Palette : Color array }

/// Identifies the exposed faces of a voxel splat.
[<Flags>]
type VoxelFaces =
    | NoFaces = 0
    | RightFace = 1
    | LeftFace = 2
    | UpFace = 4
    | DownFace = 8
    | ForwardFace = 16
    | BackFace = 32
    | AllFaces = 63


/// Describes a single voxel splat in local model space.
type [<Struct>] VoxelSplat =
    { Position : Vector3
      Albedo : Color
      Normal : Vector3
      Faces : VoxelFaces }


/// Describes a user-defined voxel model for high-count splat rendering.
type VoxelModelDescriptor =
    { Splats : VoxelSplat array
      Grid : VoxelGridDescriptor option
      Bounds : Box3
      VoxelSize : Vector3 }
