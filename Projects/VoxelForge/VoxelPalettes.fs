namespace VoxelForge
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module VoxelPalettes =

    type private BlockSource =
        { Name : string
          Material : VoxelMaterialKind
          Solid : bool
          Placeable : bool }

    let private blockSources =
        [|{ Name = "Aggregate Structural Concrete"; Material = Concrete; Solid = true; Placeable = true }
          { Name = "Spalled Aggregate Debris"; Material = SpalledConcrete; Solid = true; Placeable = false }
          { Name = "Ochre Ceramic Tile"; Material = Ceramic; Solid = true; Placeable = true }
          { Name = "Sage Enamel Panel"; Material = Enamel; Solid = true; Placeable = true }
          { Name = "Ivory Acoustic Ceiling Panel"; Material = CeilingPanel; Solid = true; Placeable = false }
          { Name = "Wire-Reinforced Laboratory Glass"; Material = ReinforcedGlass; Solid = true; Placeable = true }
          { Name = "Open Service Grating"; Material = ServiceMetal; Solid = true; Placeable = true }
          { Name = "Cast Terrazzo Stair Tread"; Material = StairTread; Solid = true; Placeable = false }
          { Name = "Signal Red Stair Handrail"; Material = StairRail; Solid = true; Placeable = false }
          { Name = "Glazed Containment Brick"; Material = ContainmentBrick; Solid = true; Placeable = true }
          { Name = "Hydroponic Planter"; Material = HydroponicBed; Solid = true; Placeable = true }
          { Name = "Arboretum Vegetation"; Material = Vegetation; Solid = true; Placeable = false }
          { Name = "Chlorinated Process Water"; Material = ProcessWater; Solid = false; Placeable = false }
          { Name = "Relay Control Terminal"; Material = Terminal; Solid = true; Placeable = false }
          { Name = "Walnut Veneer Panel"; Material = WoodVeneer; Solid = true; Placeable = true }
          { Name = "Moss Auditorium Upholstery"; Material = Upholstery; Solid = true; Placeable = true }
          { Name = "Stainless Process Machinery"; Material = MachineCasing; Solid = true; Placeable = true }
          { Name = "Black-Ochre Hazard Marking"; Material = HazardStripe; Solid = true; Placeable = true }
          { Name = "Ceramic Fluorescent Luminaire"; Material = FluorescentFixture; Solid = true; Placeable = true }
          { Name = "Painted Process Pipe Assembly"; Material = PipeAssembly; Solid = true; Placeable = true }
          { Name = "Analog Instrument Bank"; Material = InstrumentPanel; Solid = true; Placeable = true }
          { Name = "Complex 17 Sector Placard"; Material = FacilityPlacard; Solid = false; Placeable = false }|]

    let private directions =
        [|struct (v3iRight, v3Right, VoxelFaces.RightFace)
          struct (v3iLeft, v3Left, VoxelFaces.LeftFace)
          struct (v3iUp, v3Up, VoxelFaces.UpFace)
          struct (v3iDown, v3Down, VoxelFaces.DownFace)
          struct (v3iForward, v3Forward, VoxelFaces.ForwardFace)
          struct (v3iBack, v3Back, VoxelFaces.BackFace)|]

    let private clampColor value =
        Math.Clamp (value, 0.0f, 1.0f)

    let private shade amount (albedo : Color) =
        color
            (clampColor (albedo.R + amount))
            (clampColor (albedo.G + amount))
            (clampColor (albedo.B + amount))
            albedo.A

    let private hashCoord x y z =
        let mutable h =
            uint32 x * 73856093u ^^^
            uint32 y * 19349663u ^^^
            uint32 z * 83492791u
        h <- (h ^^^ (h >>> 13)) * 1274126177u
        h ^^^ (h >>> 16)

    let private variedColor baseColor x y z =
        let variation = single (int (hashCoord x y z % 7u) - 3) * 0.012f
        shade variation baseColor

    let private materialVoxel material (coord : Vector3i) =
        let x = coord.X
        let y = coord.Y
        let z = coord.Z
        let nearEdge value = value <= 1 || value >= 14
        let onSurface =
            x = 0 || x = 15 || y = 0 || y = 15 || z = 0 || z = 15
        let onXFace = x = 0 || x = 15
        let onYFace = y = 0 || y = 15
        let onZFace = z = 0 || z = 15
        let hash = hashCoord x y z
        match material with
        | Concrete ->
            let formJoint =
                (onXFace || onZFace) && y = 0 ||
                onYFace && (x = 0 || z = 0)
            if onSurface && y <= 2 && hash % 11u = 0u then Some (color 0.31f 0.24f 0.17f 1.0f)
            elif onSurface && hash % 43u = 0u then Some (color 0.34f 0.35f 0.33f 1.0f)
            elif onSurface && hash % 37u = 0u then Some (color 0.67f 0.65f 0.59f 1.0f)
            elif formJoint then Some (color 0.49f 0.51f 0.48f 1.0f)
            else Some (variedColor (color 0.57f 0.58f 0.55f 1.0f) x y z)
        | SpalledConcrete ->
            let core =
                y <= 5 &&
                ((x - 7) * (x - 7) + (z - 8) * (z - 8) <= (7 - y) * (7 - y))
            let brokenEdge =
                y <= 3 &&
                ((x <= 3 && z >= 4 && z <= 12) ||
                 (z >= 12 && x >= 5 && x <= 13))
            if (core || brokenEdge) && hash % 11u <> 0u then
                if hash % 7u = 0u then Some (color 0.28f 0.22f 0.16f 1.0f)
                elif hash % 5u = 0u then Some (color 0.43f 0.41f 0.36f 1.0f)
                else Some (variedColor (color 0.53f 0.52f 0.47f 1.0f) x y z)
            else None
        | Ceramic ->
            let grout =
                onZFace && (x = 0 || x = 8 || y = 0 || y = 8) ||
                onXFace && (z = 0 || z = 8 || y = 0 || y = 8) ||
                onYFace && (x = 0 || x = 8 || z = 0 || z = 8)
            if grout then Some (color 0.42f 0.37f 0.27f 1.0f)
            elif onSurface && hash % 31u = 0u then Some (color 0.37f 0.30f 0.20f 1.0f)
            else Some (variedColor (color 0.64f 0.51f 0.31f 1.0f) x y z)
        | Enamel ->
            let panelJoint =
                onZFace && (x = 0 || x = 15 || y = 0 || y = 15) ||
                onXFace && (z = 0 || z = 15 || y = 0 || y = 15) ||
                onYFace && (x = 0 || x = 15 || z = 0 || z = 15)
            let corrosion =
                onSurface &&
                (hash % 29u = 0u || y <= 2 && hash % 7u = 0u)
            if corrosion then Some (color 0.43f 0.27f 0.13f 1.0f)
            elif panelJoint then Some (color 0.24f 0.32f 0.28f 1.0f)
            else Some (variedColor (color 0.33f 0.45f 0.38f 1.0f) x y z)
        | CeilingPanel ->
            if y >= 13 then
                let rail = x = 0 || x = 15 || z = 0 || z = 15
                let joint = x = 7 || x = 8 || z = 7 || z = 8
                let stained = onSurface && hash % 113u = 0u
                if rail then Some (color 0.31f 0.32f 0.29f 1.0f)
                elif joint then Some (color 0.53f 0.52f 0.45f 1.0f)
                elif stained then Some (color 0.45f 0.42f 0.31f 1.0f)
                else Some (variedColor (color 0.76f 0.74f 0.62f 1.0f) x y z)
            else None
        | StairTread ->
            let rise = 3 + z / 4 * 4
            if y <= rise then
                let nosing = y = rise || z % 4 = 0
                let aggregate = hash % 37u = 0u
                if nosing then Some (color 0.33f 0.29f 0.24f 1.0f)
                elif aggregate then Some (color 0.69f 0.64f 0.54f 1.0f)
                else Some (variedColor (color 0.55f 0.51f 0.43f 1.0f) x y z)
            else None
        | StairRail ->
            let center = x >= 7 && x <= 8
            let topRail = y >= 12 && y <= 14
            let post = (z <= 1 || z >= 14) && y <= 14
            let foot = (z <= 2 || z >= 13) && y <= 2 && x >= 6 && x <= 9
            if center && (topRail || post) || foot then
                let corrosion = onSurface && hash % 71u = 0u
                if corrosion then Some (color 0.38f 0.18f 0.10f 1.0f)
                else Some (variedColor (color 0.58f 0.18f 0.13f 1.0f) x y z)
            else None
        | ReinforcedGlass ->
            let frame =
                nearEdge x && (nearEdge y || nearEdge z) ||
                nearEdge y && nearEdge z
            let wire =
                onSurface &&
                ((x = 5 || x = 10) && (y = 5 || y = 10) ||
                 (z = 5 || z = 10) && (y = 5 || y = 10))
            if frame then Some (color 0.28f 0.34f 0.32f 1.0f)
            elif wire then Some (color 0.48f 0.55f 0.51f 0.92f)
            else Some (color 0.46f 0.72f 0.70f 0.24f)
        | ServiceMetal ->
            let bar = x % 4 <= 1 || z % 4 <= 1
            if y <= 4 && bar then
                let bright = if y = 4 then 0.08f else 0.0f
                Some (shade bright (variedColor (color 0.24f 0.29f 0.29f 1.0f) x y z))
            else None
        | ContainmentBrick ->
            let row = y / 5
            let horizontalMortar = (onXFace || onZFace) && y % 5 = 0
            let verticalMortar =
                onZFace && (x + (row % 2) * 4) % 8 = 0 ||
                onXFace && (z + (row % 2) * 4) % 8 = 0
            if horizontalMortar || verticalMortar then Some (color 0.31f 0.25f 0.20f 1.0f)
            elif onSurface && hash % 97u = 0u then Some (color 0.61f 0.32f 0.20f 1.0f)
            elif y % 5 = 1 then Some (variedColor (color 0.51f 0.24f 0.16f 1.0f) x y z)
            else Some (variedColor (color 0.45f 0.19f 0.13f 1.0f) x y z)
        | HydroponicBed ->
            let rim = x <= 2 || x >= 13 || z <= 2 || z >= 13
            if y <= 2 then Some (color 0.32f 0.33f 0.29f 1.0f)
            elif rim then Some (variedColor (color 0.48f 0.49f 0.43f 1.0f) x y z)
            elif y >= 13 then Some (variedColor (color 0.16f 0.10f 0.055f 1.0f) x y z)
            else None
        | Vegetation ->
            let dx = x - 8
            let dy = y - 7
            let dz = z - 8
            let trunk = abs dx <= 1 && abs dz <= 1 && y <= 11
            let leafCloud = dx * dx + dz * dz + dy * dy * 2 <= 68 && hash % 100u < 72u
            if trunk then Some (color 0.24f 0.15f 0.07f 1.0f)
            elif leafCloud then Some (variedColor (color 0.10f 0.30f 0.13f 1.0f) x y z)
            else None
        | ProcessWater ->
            if y = 15 then
                let ripple = if (x + z) % 5 = 0 then 0.10f else 0.0f
                Some (color (0.08f + ripple) (0.36f + ripple) (0.32f + ripple) 0.62f)
            else Some (color 0.055f 0.26f 0.25f 0.42f)
        | Terminal ->
            let horizontalScreen =
                y >= 4 && y <= 11 &&
                ((z = 0 || z = 15) && x >= 4 && x <= 11 ||
                 (x = 0 || x = 15) && z >= 4 && z <= 11)
            let vent =
                y >= 2 && y <= 12 &&
                (onZFace && x % 3 = 0 ||
                 onXFace && z % 3 = 0)
            if horizontalScreen then
                if (x + y + z) % 11 = 0
                then Some (color 0.82f 0.24f 0.12f 1.0f)
                else Some (color 0.04f 0.72f 0.72f 1.0f)
            elif vent then Some (color 0.09f 0.12f 0.12f 1.0f)
            elif nearEdge x || nearEdge y || nearEdge z then Some (color 0.30f 0.35f 0.33f 1.0f)
            else Some (color 0.18f 0.22f 0.21f 1.0f)
        | WoodVeneer ->
            let seam =
                onZFace && (x = 0 || x = 15 || y = 0 || y = 15) ||
                onXFace && (z = 0 || z = 15 || y = 0 || y = 15) ||
                onYFace && (x = 0 || x = 15 || z = 0 || z = 15)
            let grain =
                onZFace && (x + int (hash % 3u)) % 7 = 0 ||
                onXFace && (z + int (hash % 3u)) % 7 = 0 ||
                onYFace && (x + z + int (hash % 3u)) % 9 = 0
            if seam then Some (color 0.27f 0.16f 0.085f 1.0f)
            elif grain then Some (color 0.45f 0.28f 0.14f 1.0f)
            else Some (variedColor (color 0.36f 0.22f 0.11f 1.0f) x y z)
        | Upholstery ->
            let roundedCorner =
                (x <= 2 || x >= 13) && (z <= 3 || z >= 12)
            if y <= 10 && x >= 1 && x <= 14 && z >= 2 && z <= 13 && not roundedCorner then
                let seam = x = 7 || x = 8 || z = 7
                if seam then Some (color 0.19f 0.25f 0.18f 1.0f)
                else Some (variedColor (color 0.38f 0.49f 0.34f 1.0f) x y z)
            else None
        | MachineCasing ->
            let rivet =
                onSurface &&
                (x = 2 || x = 13) &&
                (y = 2 || y = 13 || z = 2 || z = 13)
            let vent =
                y >= 5 && y <= 10 &&
                (onZFace && x % 3 = 0 ||
                 onXFace && z % 3 = 0)
            if rivet then Some (color 0.72f 0.70f 0.61f 1.0f)
            elif vent then Some (color 0.08f 0.10f 0.10f 1.0f)
            else Some (variedColor (color 0.41f 0.46f 0.45f 1.0f) x y z)
        | HazardStripe ->
            let stripe = ((x + y + z) / 3) % 2 = 0
            if stripe then Some (color 0.79f 0.57f 0.09f 1.0f)
            else Some (color 0.075f 0.07f 0.055f 1.0f)
        | FluorescentFixture ->
            if y >= 14 && x >= 1 && x <= 14 && z >= 4 && z <= 11 then
                let frame = x = 1 || x = 14 || z = 4 || z = 11
                let tube = z = 6 || z = 9
                if frame then Some (color 0.29f 0.31f 0.27f 1.0f)
                elif tube then Some (variedColor (color 0.92f 0.90f 0.68f 1.0f) x y z)
                else Some (color 0.70f 0.70f 0.61f 1.0f)
            else None
        | PipeAssembly ->
            let pipeColor centerX centerY albedo =
                let dx = x - centerX
                let dy = y - centerY
                if dx * dx + dy * dy <= 5 then Some (variedColor albedo x y z)
                else None
            let clamp =
                (z = 3 || z = 12) &&
                ((x >= 1 && x <= 7 && y >= 1 && y <= 7) ||
                 (x >= 8 && x <= 14 && y >= 1 && y <= 7) ||
                 (x >= 4 && x <= 12 && y >= 7 && y <= 14))
            if clamp then Some (color 0.16f 0.18f 0.17f 1.0f)
            else
                match pipeColor 4 4 (color 0.54f 0.18f 0.11f 1.0f) with
                | Some albedo -> Some albedo
                | None ->
                    match pipeColor 11 4 (color 0.17f 0.31f 0.38f 1.0f) with
                    | Some albedo -> Some albedo
                    | None -> pipeColor 8 10 (color 0.50f 0.44f 0.22f 1.0f)
        | InstrumentPanel ->
            if x >= 1 && x <= 14 && y >= 1 && y <= 14 && z >= 9 then
                if z = 9 then
                    let screen = x >= 2 && x <= 7 && y >= 8 && y <= 12
                    let gaugeDistance = (x - 11) * (x - 11) + (y - 10) * (y - 10)
                    let gauge = gaugeDistance >= 4 && gaugeDistance <= 9
                    let gaugeFace = gaugeDistance < 4
                    let knob = y >= 3 && y <= 5 && (x = 4 || x = 8 || x = 12)
                    if screen then
                        if (x + y) % 7 = 0 then Some (color 0.85f 0.30f 0.13f 1.0f)
                        else Some (color 0.055f 0.62f 0.58f 1.0f)
                    elif gauge then Some (color 0.75f 0.71f 0.56f 1.0f)
                    elif gaugeFace then Some (color 0.12f 0.13f 0.11f 1.0f)
                    elif knob then Some (color 0.68f 0.16f 0.10f 1.0f)
                    else Some (color 0.16f 0.20f 0.18f 1.0f)
                else Some (variedColor (color 0.28f 0.32f 0.29f 1.0f) x y z)
            else None
        | FacilityPlacard ->
            if x >= 1 && x <= 14 && y >= 3 && y <= 12 && z >= 13 then
                let border = x = 1 || x = 14 || y = 3 || y = 12
                let digitOne = (x = 4 || x = 5) && y >= 5 && y <= 10 || x = 3 && y = 9
                let digitSeven = y = 10 && x >= 8 && x <= 12 || x = 11 && y >= 5 && y <= 9
                if border then Some (color 0.58f 0.12f 0.085f 1.0f)
                elif digitOne || digitSeven then Some (color 0.11f 0.12f 0.10f 1.0f)
                else Some (variedColor (color 0.78f 0.75f 0.61f 1.0f) x y z)
            else None

    let private makeBlock (source : BlockSource) (voxelSize : Vector3) =
        let cells = Dictionary<Vector3i, VoxelCell> (HashIdentity.Structural)
        let voxels = ResizeArray<struct (Vector3i * VoxelCell)> ()
        for y in 0 .. 15 do
            for z in 0 .. 15 do
                for x in 0 .. 15 do
                    let coord = v3i x y z
                    match materialVoxel source.Material coord with
                    | Some albedo ->
                        let cell =
                            { Albedo = albedo
                              Solid = source.Solid
                              Material = source.Material }
                        cells[coord] <- cell
                        voxels.Add (struct (coord, cell))
                    | None -> ()
        let splats = ResizeArray<VoxelSplat> ()
        let size = voxelSize * 16.0f
        let half = size * 0.5f
        for struct (coord, cell) in voxels do
            let mutable normal : Vector3 = v3Zero
            let mutable faces = VoxelFaces.NoFaces
            for struct (offset, direction, face) in directions do
                if not (cells.ContainsKey (coord + offset)) then
                    normal <- normal + direction
                    faces <- faces ||| face
            if faces <> VoxelFaces.NoFaces then
                let normal = if normal.LengthSquared () > 0.0f then Vector3.Normalize normal else v3Up
                let position =
                    (v3
                        ((single coord.X + 0.5f) * voxelSize.X)
                        ((single coord.Y + 0.5f) * voxelSize.Y)
                        ((single coord.Z + 0.5f) * voxelSize.Z)) - half
                splats.Add
                    { Position = position
                      Albedo = cell.Albedo
                      Normal = normal
                      Faces = faces }
        let voxels = voxels.ToArray ()
        let occupiedVoxels =
            voxels |> Array.map (fun struct (coord, cell) -> struct (coord, cell.Albedo))
        let descriptor =
            { Splats = splats.ToArray ()
              Grid = Some (VoxelBake.makeGridDescriptor (v3i 16 16 16) (size * -0.5f + voxelSize * 0.5f) voxelSize occupiedVoxels)
              Bounds = box3 (size * -0.5f) size
              VoxelSize = voxelSize }
        struct
            ({ Name = source.Name
               Material = source.Material
               Solid = source.Solid
               Voxels = voxels
               Cells = cells },
             descriptor)

    let createBlockTemplates voxelSize =
        blockSources
        |> Array.map (fun source ->
            let struct (template, _) = makeBlock source voxelSize
            template)

    let createPlaceableBlocks voxelSize (world : World) =
        [|let mutable previewIndex = 0
          for source in blockSources do
            if source.Placeable then
                let struct (template, descriptor) = makeBlock source voxelSize
                let previewModel = Assets.Voxels.FabricationSamplePreview previewIndex
                previewIndex <- inc previewIndex
                World.createUserDefinedVoxelModel descriptor previewModel world
                yield
                    { Name = source.Name
                      Voxels = template.Voxels
                      Template = template
                      PreviewModel = previewModel }|]

    let requireTemplate name (templates : VoxelBlockTemplate array) =
        match templates |> Array.tryFind (fun template -> template.Name = name) with
        | Some template -> template
        | None -> failwith ("VoxelForge missing facility block template '" + name + "'.")
