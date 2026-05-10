namespace VoxelForge
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

type VoxelColorByteOrder =
    | Rgba
    | Bgra

type VoxelVolumeDescriptor =
    { VoxelModel : VoxelModelDescriptor
      OccupiedCoords : Vector3i array }

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

    let tryDecodeSliceAtlasVolumeBytes (byteOrder : VoxelColorByteOrder) (width : int) (height : int) (bytes : byte array) (voxelSize : Vector3) =
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
                        let yLayer = sliceY * atlasColumns + sliceX
                        if yLayer < side then
                            let i = (y * width + x) * 4
                            let albedo = readColor byteOrder bytes i
                            if albedo.A > 0.0f then
                                let coord = v3i (x % side) yLayer (y % side)
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
                    { VoxelModel =
                        { Splats = splats.ToArray ()
                          Bounds = box3 (size * -0.5f) size
                          VoxelSize = voxelSize }
                      OccupiedCoords = occupied.Keys |> Seq.toArray }
            | None -> None

    let tryDecodeSliceAtlasBytes byteOrder width height bytes voxelSize =
        match tryDecodeSliceAtlasVolumeBytes byteOrder width height bytes voxelSize with
        | Some volume -> Some volume.VoxelModel
        | None -> None

    let tryBakeSliceAtlasVolume image voxelSize =
        match Metadata.tryGetFilePath image with
        | Some filePath ->
            match OpenGL.Texture.TryCreateTextureData (false, filePath) with
            | Some textureData ->
                let metadata = textureData.Metadata
                let (compressed, bytes) = textureData.Bytes
                textureData.Dispose ()
                if compressed then None
                else tryDecodeSliceAtlasVolumeBytes Bgra metadata.TextureWidth metadata.TextureHeight bytes voxelSize
            | None -> None
        | None -> None

    let tryBakeSliceAtlas image voxelSize =
        match tryBakeSliceAtlasVolume image voxelSize with
        | Some volume -> Some volume.VoxelModel
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

    let chunkBodyShapes (chunkSize : Vector3i) (volume : VoxelVolumeDescriptor) =
        let chunkSize = v3i (max 1 chunkSize.X) (max 1 chunkSize.Y) (max 1 chunkSize.Z)
        let chunks = Dictionary<Vector3i, List<Vector3i>> (HashIdentity.Structural)
        for coord in volume.OccupiedCoords do
            let chunkCoord = v3i (coord.X / chunkSize.X) (coord.Y / chunkSize.Y) (coord.Z / chunkSize.Z)
            let localCoord = v3i (coord.X % chunkSize.X) (coord.Y % chunkSize.Y) (coord.Z % chunkSize.Z)
            match chunks.TryGetValue chunkCoord with
            | (true, coords) -> coords.Add localCoord
            | (false, _) ->
                let coords = List ()
                coords.Add localCoord
                chunks.Add (chunkCoord, coords)
        [|for entry in chunks |> Seq.sortBy (fun entry -> struct (entry.Key.Z, entry.Key.Y, entry.Key.X)) do
            let chunkCoord = entry.Key
            let filled = Array3D.zeroCreate<bool> chunkSize.X chunkSize.Y chunkSize.Z
            let visited = Array3D.zeroCreate<bool> chunkSize.X chunkSize.Y chunkSize.Z
            for coord in entry.Value do
                filled[coord.X, coord.Y, coord.Z] <- true
            let origin = volume.VoxelModel.Bounds.Min
            let voxelSize = volume.VoxelModel.VoxelSize
            let chunkMin =
                origin +
                v3
                    (single (chunkCoord.X * chunkSize.X) * voxelSize.X)
                    (single (chunkCoord.Y * chunkSize.Y) * voxelSize.Y)
                    (single (chunkCoord.Z * chunkSize.Z) * voxelSize.Z)
            let chunkWorldSize =
                v3
                    (single chunkSize.X * voxelSize.X)
                    (single chunkSize.Y * voxelSize.Y)
                    (single chunkSize.Z * voxelSize.Z)
            let chunkCenter = chunkMin + chunkWorldSize * 0.5f
            let canUse x y z = filled[x, y, z] && not visited[x, y, z]
            let canGrowZ x y z sizeX sizeZ =
                let z = z + sizeZ
                let mutable canGrow = z < chunkSize.Z
                let mutable ix = 0
                while canGrow && ix < sizeX do
                    canGrow <- canUse (x + ix) y z
                    ix <- inc ix
                canGrow
            let canGrowY x y z sizeX sizeY sizeZ =
                let y = y + sizeY
                let mutable canGrow = y < chunkSize.Y
                let mutable iz = 0
                while canGrow && iz < sizeZ do
                    let mutable ix = 0
                    while canGrow && ix < sizeX do
                        canGrow <- canUse (x + ix) y (z + iz)
                        ix <- inc ix
                    iz <- inc iz
                canGrow
            let bodyShapes = List<BodyShape> ()
            for y in 0 .. dec chunkSize.Y do
                for z in 0 .. dec chunkSize.Z do
                    for x in 0 .. dec chunkSize.X do
                        if canUse x y z then
                            let mutable sizeX = 1
                            while x + sizeX < chunkSize.X && canUse (x + sizeX) y z do
                                sizeX <- inc sizeX
                            let mutable sizeZ = 1
                            while canGrowZ x y z sizeX sizeZ do
                                sizeZ <- inc sizeZ
                            let mutable sizeY = 1
                            while canGrowY x y z sizeX sizeY sizeZ do
                                sizeY <- inc sizeY
                            for iy in 0 .. dec sizeY do
                                for iz in 0 .. dec sizeZ do
                                    for ix in 0 .. dec sizeX do
                                        visited[x + ix, y + iy, z + iz] <- true
                            let boxSize =
                                v3
                                    (single sizeX * voxelSize.X)
                                    (single sizeY * voxelSize.Y)
                                    (single sizeZ * voxelSize.Z)
                            let boxMin =
                                chunkMin +
                                v3
                                    (single x * voxelSize.X)
                                    (single y * voxelSize.Y)
                                    (single z * voxelSize.Z)
                            let boxCenter = boxMin + boxSize * 0.5f
                            bodyShapes.Add (BoxShape { Size = boxSize; TransformOpt = Some (Affine.makeTranslation (boxCenter - chunkCenter)); PropertiesOpt = None })
            struct (chunkCoord, chunkCenter, BodyShapes (bodyShapes |> Seq.toList), bodyShapes.Count)|]
