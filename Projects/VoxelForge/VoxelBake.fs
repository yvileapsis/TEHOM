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
      OccupiedCoords : Vector3i array
      OccupiedVoxels : struct (Vector3i * Color) array }

[<RequireQualifiedAccess>]
module VoxelBake =

    let private directions =
        [|struct (v3iRight, v3Right, VoxelFaces.RightFace)
          struct (v3iLeft, v3Left, VoxelFaces.LeftFace)
          struct (v3iUp, v3Up, VoxelFaces.UpFace)
          struct (v3iDown, v3Down, VoxelFaces.DownFace)
          struct (v3iForward, v3Forward, VoxelFaces.ForwardFace)
          struct (v3iBack, v3Back, VoxelFaces.BackFace)|]

    let makeGridDescriptor (size : Vector3i) (origin : Vector3) (voxelSize : Vector3) (occupiedVoxels : struct (Vector3i * Color) seq) : VoxelGridDescriptor =
        if size.X <= 0 || size.Y <= 0 || size.Z <= 0 || size.X > 64 || size.Y > 64 || size.Z > 64 then
            invalidArg (nameof size) "Voxel grid dimensions must each be between 1 and 64."
        let occupiedVoxels = occupiedVoxels |> Seq.toArray
        if occupiedVoxels.Length = 0 then
            invalidArg (nameof occupiedVoxels) "A voxel grid must contain at least one occupied voxel."
        let mutable minX = size.X
        let mutable minY = size.Y
        let mutable minZ = size.Z
        let mutable maxX = -1
        let mutable maxY = -1
        let mutable maxZ = -1
        for struct (coord, _) in occupiedVoxels do
            if coord.X < 0 || coord.X >= size.X ||
               coord.Y < 0 || coord.Y >= size.Y ||
               coord.Z < 0 || coord.Z >= size.Z then
                invalidArg (nameof occupiedVoxels) "Voxel grid coordinates must lie within its dimensions."
            minX <- min minX coord.X
            minY <- min minY coord.Y
            minZ <- min minZ coord.Z
            maxX <- max maxX coord.X
            maxY <- max maxY coord.Y
            maxZ <- max maxZ coord.Z
        let croppedSize = v3i (inc maxX - minX) (inc maxY - minY) (inc maxZ - minZ)
        let croppedOrigin =
            origin +
            v3
                (single minX * voxelSize.X)
                (single minY * voxelSize.Y)
                (single minZ * voxelSize.Z)
        let volume = croppedSize.X * croppedSize.Y * croppedSize.Z
        let paletteIndices = Dictionary<Color, int> ()
        let palette = ResizeArray<Color> ()
        let values = Array.zeroCreate<uint> volume
        for struct (coord, albedo) in occupiedVoxels do
            let mutable paletteIndex = 0
            if not (paletteIndices.TryGetValue (albedo, &paletteIndex)) then
                if palette.Count >= 65535 then
                    invalidArg (nameof occupiedVoxels) "A voxel grid palette cannot exceed 65535 colors."
                paletteIndex <- palette.Count
                paletteIndices[albedo] <- paletteIndex
                palette.Add albedo
            let x = coord.X - minX
            let y = coord.Y - minY
            let z = coord.Z - minZ
            let linearIndex = x + croppedSize.X * (y + croppedSize.Y * z)
            values[linearIndex] <- uint (inc paletteIndex)
        let indexBits = if palette.Count <= 255 then 8 else 16
        let indicesPerWord = 32 / indexBits
        let packed = Array.zeroCreate<uint> ((volume + dec indicesPerWord) / indicesPerWord)
        let mask = if indexBits = 8 then 0xFFu else 0xFFFFu
        for i in 0 .. dec values.Length do
            let value = values[i] &&& mask
            packed[i / indicesPerWord] <- packed[i / indicesPerWord] ||| (value <<< ((i % indicesPerWord) * indexBits))
        { Size = croppedSize
          Origin = croppedOrigin
          IndexBits = indexBits
          Indices = packed
          Palette = palette.ToArray () }

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
                    let mutable faces = VoxelFaces.NoFaces
                    for struct (offset, direction, face) in directions do
                        if not (occupied.ContainsKey (coord + offset)) then
                            exposed <- true
                            normal <- normal + direction
                            faces <- faces ||| face
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
                              Normal = normal
                              Faces = faces }
                let occupiedVoxels =
                    occupied
                    |> Seq.map (fun entry -> struct (entry.Key, entry.Value))
                    |> Seq.toArray
                let gridOrigin = size * -0.5f + voxelSize * 0.5f
                let gridOpt =
                    if side <= 64
                    then Some (makeGridDescriptor (v3i side side side) gridOrigin voxelSize occupiedVoxels)
                    else None
                Some
                    { VoxelModel =
                        { Splats = splats.ToArray ()
                          Grid = gridOpt
                          Bounds = box3 (size * -0.5f) size
                          VoxelSize = voxelSize }
                      OccupiedCoords = occupied.Keys |> Seq.toArray
                      OccupiedVoxels = occupiedVoxels }
            | None -> None

    let tryDecodeSliceAtlasBytes byteOrder width height bytes voxelSize =
        match tryDecodeSliceAtlasVolumeBytes byteOrder width height bytes voxelSize with
        | Some volume -> Some volume.VoxelModel
        | None -> None

    let tryBakeSliceAtlasVolume image voxelSize =
        match Metadata.tryGetFilePath image with
        | Some filePath ->
            match Vulkan.TextureData.tryCreate false filePath with
            | Some textureData ->
                let metadata = textureData.Metadata
                let (compressed, bytes) = textureData.Bytes
                match textureData with
                | Vulkan.TextureDataNative (_, _, disposer) -> disposer.Dispose ()
                | _ -> ()
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
            Grid = None
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
                    Grid = None
                    Bounds = box3 (min - center) size })|]

    let occupiedDictionary (volume : VoxelVolumeDescriptor) =
        let occupied = Dictionary<Vector3i, Color> (HashIdentity.Structural)
        for struct (coord, albedo) in volume.OccupiedVoxels do
            occupied[coord] <- albedo
        occupied

    let private chunkMinCoord (chunkSize : Vector3i) (chunkCoord : Vector3i) =
        v3i
            (chunkCoord.X * chunkSize.X)
            (chunkCoord.Y * chunkSize.Y)
            (chunkCoord.Z * chunkSize.Z)

    let private coordCenter (origin : Vector3) (voxelSize : Vector3) (coord : Vector3i) =
        origin +
        v3
            ((single coord.X + 0.5f) * voxelSize.X)
            ((single coord.Y + 0.5f) * voxelSize.Y)
            ((single coord.Z + 0.5f) * voxelSize.Z)

    let chunkModelFromCells (chunkSize : Vector3i) (bounds : Box3) (voxelSize : Vector3) (tryGetCell : Vector3i -> VoxelCell voption) (chunkCoord : Vector3i) =
        let chunkSize = v3i (max 1 chunkSize.X) (max 1 chunkSize.Y) (max 1 chunkSize.Z)
        let origin = bounds.Min
        let globalMinCoord = chunkMinCoord chunkSize chunkCoord
        let chunkWorldSize =
            v3
                (single chunkSize.X * voxelSize.X)
                (single chunkSize.Y * voxelSize.Y)
                (single chunkSize.Z * voxelSize.Z)
        let chunkMin =
            origin +
            v3
                (single globalMinCoord.X * voxelSize.X)
                (single globalMinCoord.Y * voxelSize.Y)
                (single globalMinCoord.Z * voxelSize.Z)
        let chunkCenter = chunkMin + chunkWorldSize * 0.5f
        let splats = List ()
        let occupiedVoxels = ResizeArray<struct (Vector3i * Color)> ()
        let mutable occupiedAny = false
        for y in globalMinCoord.Y .. globalMinCoord.Y + chunkSize.Y - 1 do
            for z in globalMinCoord.Z .. globalMinCoord.Z + chunkSize.Z - 1 do
                for x in globalMinCoord.X .. globalMinCoord.X + chunkSize.X - 1 do
                    let coord = v3i x y z
                    match tryGetCell coord with
                    | ValueSome cell ->
                        occupiedAny <- true
                        occupiedVoxels.Add (struct (coord - globalMinCoord, cell.Albedo))
                        let mutable exposed = false
                        let mutable normal = v3Zero
                        let mutable faces = VoxelFaces.NoFaces
                        for struct (offset, direction, face) in directions do
                            match tryGetCell (coord + offset) with
                            | ValueSome _ -> ()
                            | ValueNone ->
                                exposed <- true
                                normal <- normal + direction
                                faces <- faces ||| face
                        if exposed then
                            let normal = if normal.LengthSquared () > 0.0f then normal.Normalized else v3Up
                            splats.Add
                                { Position = coordCenter origin voxelSize coord
                                  Albedo = cell.Albedo
                                  Normal = normal
                                  Faces = faces }
                    | ValueNone -> ()
        if occupiedAny then
            let center = chunkCenter
            let descriptorBounds = box3 (chunkWorldSize * -0.5f) chunkWorldSize
            let splats =
                let splatsArray = Array.zeroCreate splats.Count
                for i in 0 .. dec splats.Count do
                    let splat = splats[i]
                    splatsArray[i] <- { splat with Position = splat.Position - center }
                splatsArray
            let gridOrigin = descriptorBounds.Min + voxelSize * 0.5f
            let grid = makeGridDescriptor chunkSize gridOrigin voxelSize occupiedVoxels
            Some
                struct
                    (center,
                     { Splats = splats
                       Grid = Some grid
                       Bounds = descriptorBounds
                       VoxelSize = voxelSize })
        else None

    let chunkModelFromOccupied (chunkSize : Vector3i) (bounds : Box3) (voxelSize : Vector3) (occupied : Dictionary<Vector3i, Color>) (chunkCoord : Vector3i) =
        let tryGetCell coord =
            match occupied.TryGetValue coord with
            | (true, albedo) -> ValueSome { Albedo = albedo; Solid = true; Material = Crafted }
            | (false, _) -> ValueNone
        chunkModelFromCells chunkSize bounds voxelSize tryGetCell chunkCoord

    let chunkBodyShapeFromCells (chunkSize : Vector3i) (bounds : Box3) (voxelSize : Vector3) (tryGetCell : Vector3i -> VoxelCell option) (chunkCoord : Vector3i) =
        let chunkSize = v3i (max 1 chunkSize.X) (max 1 chunkSize.Y) (max 1 chunkSize.Z)
        let filled = Array3D.zeroCreate<bool> chunkSize.X chunkSize.Y chunkSize.Z
        let visited = Array3D.zeroCreate<bool> chunkSize.X chunkSize.Y chunkSize.Z
        let globalMinCoord = chunkMinCoord chunkSize chunkCoord
        let mutable occupiedAny = false
        for y in 0 .. dec chunkSize.Y do
            for z in 0 .. dec chunkSize.Z do
                for x in 0 .. dec chunkSize.X do
                    let coord = v3i (globalMinCoord.X + x) (globalMinCoord.Y + y) (globalMinCoord.Z + z)
                    match tryGetCell coord with
                    | Some cell when cell.Solid ->
                        filled[x, y, z] <- true
                        occupiedAny <- true
                    | Some _ | None -> ()
        if occupiedAny then
            let chunkMin =
                bounds.Min +
                v3
                    (single globalMinCoord.X * voxelSize.X)
                    (single globalMinCoord.Y * voxelSize.Y)
                    (single globalMinCoord.Z * voxelSize.Z)
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
            Some struct (chunkCenter, BodyShapes (bodyShapes |> Seq.toList), bodyShapes.Count)
        else None

    let chunkBodyShapeFromOccupied (chunkSize : Vector3i) (bounds : Box3) (voxelSize : Vector3) (occupied : Dictionary<Vector3i, Color>) (chunkCoord : Vector3i) =
        let tryGetCell coord =
            match occupied.TryGetValue coord with
            | (true, albedo) -> Some { Albedo = albedo; Solid = true; Material = Crafted }
            | (false, _) -> None
        chunkBodyShapeFromCells chunkSize bounds voxelSize tryGetCell chunkCoord

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
