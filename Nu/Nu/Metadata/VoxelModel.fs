// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System.Numerics
open Prime

/// Describes a single voxel splat in local model space.
type [<Struct>] VoxelSplat =
    { Position : Vector3
      Albedo : Color
      Normal : Vector3 }

/// Describes a user-defined voxel model for high-count splat rendering.
type VoxelModelDescriptor =
    { Splats : VoxelSplat array
      Bounds : Box3
      VoxelSize : Vector3 }
