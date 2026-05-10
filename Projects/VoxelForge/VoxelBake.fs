namespace VoxelForge
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

type VoxelColorByteOrder =
    | Rgba
    | Bgra

[<RequireQualifiedAccess>]
module VoxelBake =

    let private directions =
        [|struct (v3iRight, v3Right)
          struct (v3iLeft, v3Left)
          struct (v3iUp, v3Up)
          struct (v3iDown, v3Down)
          struct (v3iForward, v3Forward)
          struct (v3iBack, v3Back)|]

    let private tryInferCubeSide width height =
        let volume = width * height
        let side = int (Math.Round (Math.Pow (float volume, 1.0 / 3.0)))
        if  side > 0 &&
            side * side * side = volume &&
            width % side = 0 &&
            height % side = 0 &&
            width / side * (height / side) >= side
        then Some side
        else None

    let private readColor byteOrder (bytes : byte array) i =
        let scalar = 1.0f / single Byte.MaxValue
        match byteOrder with
        | Rgba ->
            color
                (single bytes[i] * scalar)
                (single bytes[i+1] * scalar)
                (single bytes[i+2] * scalar)
                (single bytes[i+3] * scalar)
        | Bgra ->
            color
                (single bytes[i+2] * scalar)
                (single bytes[i+1] * scalar)
                (single bytes[i] * scalar)
                (single bytes[i+3] * scalar)

    let tryDecodeSliceAtlasBytes (byteOrder : VoxelColorByteOrder) (width : int) (height : int) (bytes : byte array) (voxelSize : Vector3) =
        if bytes.Length < width * height * 4 then None
        else
            match tryInferCubeSide width height with
            | Some side ->
                let atlasColumns = width / side
                let occupied = Dictionary<Vector3i, Color> (HashIdentity.Structural)
                for y in 0 .. dec height do
                    for x in 0 .. dec width do
                        let sliceX = x / side
                        let sliceY = y / side
                        let z = sliceY * atlasColumns + sliceX
                        if z < side then
                            let i = (y * width + x) * 4
                            let albedo = readColor byteOrder bytes i
                            if albedo.A > 0.0f then
                                let coord = v3i (x % side) (y % side) z
                                occupied[coord] <- albedo
                let size = v3 (single side * voxelSize.X) (single side * voxelSize.Y) (single side * voxelSize.Z)
                let half = size * 0.5f
                let splats = List ()
                for entry in occupied do
                    let coord : Vector3i = entry.Key
                    let mutable exposed = false
                    let mutable normal = v3Zero
                    for struct (offset, direction) in directions do
                        if not (occupied.ContainsKey (coord + offset)) then
                            exposed <- true
                            normal <- normal + direction
                    if exposed then
                        let normal = if normal.LengthSquared () > 0.0f then normal.Normalized else v3Up
                        let position =
                            v3
                                ((single coord.X + 0.5f) * voxelSize.X)
                                ((single coord.Y + 0.5f) * voxelSize.Y)
                                ((single coord.Z + 0.5f) * voxelSize.Z) - half
                        splats.Add
                            { Position = position
                              Albedo = entry.Value
                              Normal = normal }
                Some
                    { Splats = splats.ToArray ()
                      Bounds = box3 (size * -0.5f) size
                      VoxelSize = voxelSize }
            | None -> None

    let tryBakeSliceAtlas image voxelSize =
        match Metadata.tryGetFilePath image with
        | Some filePath ->
            match OpenGL.Texture.TryCreateTextureData (false, filePath) with
            | Some textureData ->
                let metadata = textureData.Metadata
                let (compressed, bytes) = textureData.Bytes
                textureData.Dispose ()
                if compressed then None
                else tryDecodeSliceAtlasBytes Bgra metadata.TextureWidth metadata.TextureHeight bytes voxelSize
            | None -> None
        | None -> None

    let tile tilesX tilesZ (descriptor : VoxelModelDescriptor) =
        let tilesX = max 1 tilesX
        let tilesZ = max 1 tilesZ
        let stride = descriptor.Bounds.Size
        let baseOffset = v3 (single (dec tilesX) * stride.X * -0.5f) 0.0f (single (dec tilesZ) * stride.Z * -0.5f)
        let splats = Array.zeroCreate (descriptor.Splats.Length * tilesX * tilesZ)
        let mutable i = 0
        for z in 0 .. dec tilesZ do
            for x in 0 .. dec tilesX do
                let offset = baseOffset + v3 (single x * stride.X) 0.0f (single z * stride.Z)
                for splat in descriptor.Splats do
                    splats[i] <- { splat with Position = splat.Position + offset }
                    i <- inc i
        { descriptor with
            Splats = splats
            Bounds =
                box3
                    (descriptor.Bounds.Min + baseOffset)
                    (v3 (stride.X * single tilesX) stride.Y (stride.Z * single tilesZ)) }

    let chunk (chunkSize : Vector3i) (descriptor : VoxelModelDescriptor) =
        let chunkSize = v3i (max 1 chunkSize.X) (max 1 chunkSize.Y) (max 1 chunkSize.Z)
        let chunks = Dictionary<Vector3i, List<VoxelSplat>> (HashIdentity.Structural)
        let origin = descriptor.Bounds.Min
        for splat in descriptor.Splats do
            let coord =
                v3i
                    (int (floor ((splat.Position.X - origin.X) / descriptor.VoxelSize.X)))
                    (int (floor ((splat.Position.Y - origin.Y) / descriptor.VoxelSize.Y)))
                    (int (floor ((splat.Position.Z - origin.Z) / descriptor.VoxelSize.Z)))
            let chunkCoord = v3i (coord.X / chunkSize.X) (coord.Y / chunkSize.Y) (coord.Z / chunkSize.Z)
            match chunks.TryGetValue chunkCoord with
            | (true, splats) -> splats.Add splat
            | (false, _) ->
                let splats = List ()
                splats.Add splat
                chunks.Add (chunkCoord, splats)
        [|for entry in chunks |> Seq.sortBy (fun entry -> struct (entry.Key.Z, entry.Key.Y, entry.Key.X)) do
            let splats = entry.Value
            let halfVoxelSize = descriptor.VoxelSize * 0.5f
            let mutable min = v3Dup Single.MaxValue
            let mutable max = v3Dup Single.MinValue
            for splat in splats do
                min <- Vector3.Min (min, splat.Position - halfVoxelSize)
                max <- Vector3.Max (max, splat.Position + halfVoxelSize)
            let size = max - min
            let center = min + size * 0.5f
            let splats =
                splats
                |> Seq.map (fun splat -> { splat with Position = splat.Position - center })
                |> Array.ofSeq
            struct
                (entry.Key,
                 center,
                 { descriptor with
                    Splats = splats
                    Bounds = box3 (min - center) size })|]
