﻿// MIT License - Copyright (C) The Mono.Xna Team
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.

using System;

namespace Nu
{
    /// <summary>
    /// Defines how the bounding volumes intersects or contain one another.
    /// Copied from - https://github.com/MonoGame/MonoGame/blob/v3.8/MonoGame.Framework/ContainmentType.cs
    /// </summary>
    public enum ContainmentType
    {
        /// <summary>
        /// Indicates that there is no overlap between two bounding volumes.
        /// </summary>
        Disjoint,
        /// <summary>
        /// Indicates that one bounding volume completely contains another volume.
        /// </summary>
        Contains,
        /// <summary>
        /// Indicates that bounding volumes partially overlap one another.
        /// </summary>
        Intersects
    }
}