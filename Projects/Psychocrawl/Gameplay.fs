namespace Psychocrawl
open System
open System.IO
open System.Collections.Generic
open System.Numerics
open Prime
open Nu
open Psychocrawl

[<RequireQualifiedAccess>]
module DreamGlyph =

    let PlayerNorth = "▴"
    let PlayerEast = "▸"
    let PlayerSouth = "▾"
    let PlayerWest = "◂"
    let Wall = "■"
    let Floor = "·"
    let Door = "⊞"
    let Portal = "◇"
    let CameraCone = "░"
    let TurretCone = "▒"
    let EnemyCone = "░"
    let Projectile = "•"
    let Block = "▣"
    let Camera = "◉"
    let Turret = "⊕"
    let Enemy = "◆"
    let Reticle = "◎"

type Direction =
    | North
    | East
    | South
    | West

    member this.Delta =
        match this with
        | North -> (0, -1)
        | East -> (1, 0)
        | South -> (0, 1)
        | West -> (-1, 0)

    member this.Opposite =
        match this with
        | North -> South
        | East -> West
        | South -> North
        | West -> East

    member this.Clockwise =
        match this with
        | North -> East
        | East -> South
        | South -> West
        | West -> North

    member this.Glyph =
        match this with
        | North -> DreamGlyph.PlayerNorth
        | East -> DreamGlyph.PlayerEast
        | South -> DreamGlyph.PlayerSouth
        | West -> DreamGlyph.PlayerWest

    static member parse value =
        match value with
        | "N" | "North" | "north" -> North
        | "E" | "East" | "east" -> East
        | "S" | "South" | "south" -> South
        | "W" | "West" | "west" -> West
        | _ -> East

[<RequireQualifiedAccess>]
module DreamProjection =

    let normalizeRotation rotation =
        let rotation = rotation % 4
        if rotation < 0 then rotation + 4 else rotation

    let rotateDirection rotation (direction : Direction) =
        let mutable direction = direction
        for _ in 1 .. normalizeRotation rotation do
            direction <- direction.Clockwise
        direction

    let inverseRotateDirection rotation (direction : Direction) =
        rotateDirection -rotation direction

    let transformDirection sourceRotation destinationRotation direction =
        direction
        |> rotateDirection sourceRotation
        |> inverseRotateDirection destinationRotation

type Cell =
    { X : int
      Y : int
      Z : int }

    member this.Step (direction : Direction) =
        let (dx, dy) = direction.Delta
        { this with X = this.X + dx; Y = this.Y + dy }

[<RequireQualifiedAccess>]
module DreamProjectionCell =

    let rotateOffset rotation (offset : Cell) =
        match DreamProjection.normalizeRotation rotation with
        | 0 -> { offset with Z = 0 }
        | 1 -> { X = -offset.Y; Y = offset.X; Z = 0 }
        | 2 -> { X = -offset.X; Y = -offset.Y; Z = 0 }
        | _ -> { X = offset.Y; Y = -offset.X; Z = 0 }

    let inverseRotateOffset rotation offset =
        rotateOffset -rotation offset

    let transformOffset sourceRotation destinationRotation offset =
        offset
        |> rotateOffset sourceRotation
        |> inverseRotateOffset destinationRotation

type Location =
    { SectionId : string
      Cell : Cell }

    member this.Step (direction : Direction) =
        { this with Cell = this.Cell.Step direction }

type Terrain =
    | Wall
    | Floor
    | Door
    | Portal
    | Upper of char
    | Void

type Link =
    { LinkId : string
      From : Location
      Destination : Location }

type DreamSection =
    { SectionId : string
      Width : int
      Height : int
      PreferredView : int * int
      Layers : Map<int, Terrain[,]>
      Links : Link list }

type MirrorKind =
    | Slash
    | Backslash

    member this.Glyph =
        match this with
        | Slash -> "╱"
        | Backslash -> "╲"

    member this.Reflect direction =
        match this, direction with
        | Slash, North -> East
        | Slash, East -> North
        | Slash, South -> West
        | Slash, West -> South
        | Backslash, North -> West
        | Backslash, West -> North
        | Backslash, South -> East
        | Backslash, East -> South

    member this.Toggle =
        match this with
        | Slash -> Backslash
        | Backslash -> Slash

    static member parse value =
        match value with
        | "slash" | "/" -> Slash
        | "backslash" | "\\" -> Backslash
        | _ -> Slash

type Camera =
    { CameraId : string
      Location : Location
      Facing : Direction
      Range : int }

type Turret =
    { TurretId : string
      Location : Location
      Facing : Direction
      Range : int }

type Mirror =
    { MirrorId : string
      Location : Location
      Kind : MirrorKind }

type Block =
    { BlockId : string
      Location : Location }

type Enemy =
    { EnemyId : string
      Location : Location
      Facing : Direction
      VisionRange : int
      Alert : int }

type PickupKind =
    | Ammo
    | Med
    | CellBattery

    member this.Glyph =
        match this with
        | Ammo -> "▪"
        | Med -> "✚"
        | CellBattery -> "◈"

    static member parse value =
        match value with
        | "ammo" -> Ammo
        | "med" -> Med
        | "cell" -> CellBattery
        | _ -> Ammo

type Pickup =
    { PickupId : string
      Location : Location
      Kind : PickupKind
      Amount : int }

type Weapon =
    | Pistol
    | Needle

    member this.Name =
        match this with
        | Pistol -> "PSTL"
        | Needle -> "NDLE"

    member this.Cost =
        match this with
        | Pistol -> 1
        | Needle -> 2

type Projectile =
    { ProjectileId : int
      Location : Location
      Facing : Direction
      RangeLeft : int }

type Resources =
    { Health : int
      Focus : int
      Ammo : int
      AlarmDebt : int }

type HeldBlock =
    { BlockId : string
      Offset : Cell }

type DreamRun =
    { Sections : Map<string, DreamSection>
      SectionOrigins : Map<string, Cell>
      Player : Location
      Facing : Direction
      Reticle : Cell
      ReticleOffset : Cell
      Resources : Resources
      Weapon : Weapon
      Cameras : Map<string, Camera>
      Turrets : Map<string, Turret>
      Mirrors : Map<string, Mirror>
      Blocks : Map<string, Block>
      Enemies : Map<string, Enemy>
      Pickups : Map<string, Pickup>
      Projectiles : Projectile list
      ProjectileSerial : int
      HeldBlock : HeldBlock option
      VisibleCells : Set<Location>
      RememberedCells : Map<Location, MsdfGlyphValue>
      Tick : int64
      Log : string list }

type QueuedCommand =
    | MoveQueued of Direction
    | FireQueued
    | GrabToggleQueued

[<RequireQualifiedAccess>]
type DreamProjectionMode =
    | Compressed
    | Unfolded

    member this.Name =
        match this with
        | DreamProjectionMode.Compressed -> "COMPRESSED"
        | DreamProjectionMode.Unfolded -> "UNFOLDED"

    member this.Toggle =
        match this with
        | DreamProjectionMode.Compressed -> DreamProjectionMode.Unfolded
        | DreamProjectionMode.Unfolded -> DreamProjectionMode.Compressed

type DreamProjectedCell =
    { ProjectionId : string
      Location : Location }

type DreamProjectionInstance =
    { ProjectionId : string
      SectionId : string
      Origin : Cell
      Rotation : int
      Depth : int }

type Gameplay =
    { Run : DreamRun
      CommandQueue : QueuedCommand list
      ProjectionMode : DreamProjectionMode
      CurrentProjectionId : string
      UnfoldedInstances : Map<string, DreamProjectionInstance>
      UnfoldedVisibleCells : Set<DreamProjectedCell>
      UnfoldedRememberedCells : Map<DreamProjectedCell, MsdfGlyphValue> }

type GameplayMessage =
    | StartPlaying
    | Update
    | KeyPressed of KeyboardKeyData
    interface Message

type GameplayCommand =
    | Noop
    interface Command

[<RequireQualifiedAccess>]
module DreamGrid =

    let DefaultWidth = 48
    let DefaultHeight = 24
    let PlayerVisionRange = 10
    let UnfoldedMaxDepth = 8
    let UnfoldedMaxInstances = 96
    let TickDivisor = 6L
    let HeldMoveTickDivisor = 12L
    let CellWidth = 6.0f
    let CellHeight = 6.0f
    let GlyphWidthScale = 1.0f
    let GlyphHeightScale = 1.0f
    let Left = -282.0f
    let Top = 165.0f
    let ReticleStartDistance = 2
    let HudTop = -102.0f
    let HudLineHeight = 9.0f
    let HudFontSize = 8.0f
    let HudWidth = 600.0f

    let cellPosition x y =
        v3 (Left + single x * CellWidth) (Top - single y * CellHeight) 0.0f

    let cellSize =
        v3 CellWidth CellHeight 0.0f

    let gridPosition width height =
        v3
            (Left + single (width - 1) * CellWidth * 0.5f)
            (Top - single (height - 1) * CellHeight * 0.5f)
            0.0f

    let gridSize width height =
        v3 (single width * CellWidth) (single height * CellHeight) 0.0f

    let mouseToCell (mousePosition : Vector2) =
        let x = int (MathF.Floor ((mousePosition.X - Left + CellWidth * 0.5f) / CellWidth))
        let y = int (MathF.Floor ((Top - mousePosition.Y + CellHeight * 0.5f) / CellHeight))
        if x >= 0 && x < DefaultWidth && y >= 0 && y < DefaultHeight then Some { X = x; Y = y; Z = 0 }
        else None

type DreamGlyphGrid =
    { Width : int
      Height : int
      Glyphs : MsdfGlyphValue array }

    static member Empty =
        { Width = 0
          Height = 0
          Glyphs = [||] }

[<AutoOpen>]
module DreamGlyphGridExtensions =

    type Entity with
        member this.GetGlyphCutTop world : single = this.Get (nameof this.GlyphCutTop) world
        member this.SetGlyphCutTop (value : single) world = this.Set (nameof this.GlyphCutTop) value world
        member this.GlyphCutTop = lens (nameof this.GlyphCutTop) this this.GetGlyphCutTop this.SetGlyphCutTop
        member this.GetGlyphCutBottom world : single = this.Get (nameof this.GlyphCutBottom) world
        member this.SetGlyphCutBottom (value : single) world = this.Set (nameof this.GlyphCutBottom) value world
        member this.GlyphCutBottom = lens (nameof this.GlyphCutBottom) this this.GetGlyphCutBottom this.SetGlyphCutBottom

[<RequireQualifiedAccess>]
module DreamPalette =

    let rgba r g b a =
        Color (byte r, byte g, byte b, byte a)

    let Background = rgba 8 10 14 255
    let Wall = rgba 68 82 91 255
    let Floor = rgba 62 72 78 255
    let Upper = rgba 104 122 130 255
    let Door = rgba 226 181 82 255
    let Portal = rgba 96 215 205 255
    let CameraCone = rgba 81 133 166 255
    let TurretCone = rgba 175 103 80 255
    let EnemyCone = rgba 126 75 89 255
    let Player = rgba 241 247 245 255
    let Reticle = rgba 122 233 221 255
    let Enemy = rgba 255 84 96 255
    let Camera = rgba 77 169 230 255
    let Turret = rgba 244 109 82 255
    let Mirror = rgba 148 239 219 255
    let Block = rgba 199 186 141 255
    let Ammo = rgba 250 205 108 255
    let Med = rgba 115 220 132 255
    let Focus = rgba 170 122 255 255
    let Projectile = rgba 255 241 177 255
    let Hud = rgba 196 207 207 255
    let Muted = rgba 99 111 119 255
    let Alert = rgba 255 95 111 255

[<RequireQualifiedAccess>]
module DreamMap =

    let terrainOfChar z glyph =
        match glyph with
        | '#' -> Wall
        | '.' | '@' | 'B' -> Floor
        | '+' -> Door
        | 'o' -> Portal
        | '=' | '/' | '\\' when z > 0 -> Upper glyph
        | ' ' -> Void
        | _ -> Floor

    let terrainGlyph terrain =
        match terrain with
        | Wall -> DreamGlyph.Wall
        | Floor -> DreamGlyph.Floor
        | Door -> DreamGlyph.Door
        | Portal -> DreamGlyph.Portal
        | Upper glyph -> string glyph
        | Void -> " "

    let terrainColor terrain =
        match terrain with
        | Wall -> DreamPalette.Wall
        | Floor -> DreamPalette.Floor
        | Door -> DreamPalette.Door
        | Portal -> DreamPalette.Portal
        | Upper _ -> DreamPalette.Upper
        | Void -> DreamPalette.Background

    let tryGetSection (run : DreamRun) sectionId =
        Map.tryFind sectionId run.Sections

    let containsCell (section : DreamSection) (cell : Cell) =
        cell.X >= 0 && cell.X < section.Width && cell.Y >= 0 && cell.Y < section.Height

    let clampCell (section : DreamSection) (cell : Cell) =
        { cell with
            X = max 0 (min (section.Width - 1) cell.X)
            Y = max 0 (min (section.Height - 1) cell.Y) }

    let terrainAtLayer z (section : DreamSection) (cell : Cell) =
        if not (containsCell section cell) then Wall
        else
            match Map.tryFind z section.Layers with
            | Some layer -> layer[cell.X, cell.Y]
            | None when z < 0 -> Floor
            | None -> Wall

    let terrainAt (run : DreamRun) (location : Location) =
        match tryGetSection run location.SectionId with
        | Some section -> terrainAtLayer location.Cell.Z section location.Cell
        | None -> Wall

    let isWalkableTerrain (run : DreamRun) (location : Location) =
        match terrainAt run location with
        | Floor | Door | Portal -> true
        | Upper _ -> true
        | Wall | Void -> false

    let tryLinkAt (run : DreamRun) (location : Location) =
        match tryGetSection run location.SectionId with
        | Some section ->
            section.Links
            |> List.tryFind (fun link -> link.From.Cell = location.Cell)
        | None -> None

    let private exitDirection (section : DreamSection) (cell : Cell) =
        if cell.X = 0 then Some West
        elif cell.X = section.Width - 1 then Some East
        elif cell.Y = 0 then Some North
        elif cell.Y = section.Height - 1 then Some South
        else None

    let tryExitLink (run : DreamRun) (location : Location) direction =
        match tryGetSection run location.SectionId, tryLinkAt run location with
        | Some section, Some link ->
            match exitDirection section location.Cell with
            | Some exitDirection when exitDirection = direction -> Some link.Destination
            | _ -> None
        | _ -> None

    let step (run : DreamRun) (location : Location) direction =
        match tryExitLink run location direction with
        | Some destination -> destination
        | None -> location.Step direction

[<RequireQualifiedAccess>]
module DreamLookup =

    let tryFindCameraAt (location : Location) (run : DreamRun) =
        run.Cameras
        |> Map.toSeq
        |> Seq.tryPick (fun (id, camera) -> if camera.Location = location then Some (id, camera) else None)

    let tryFindTurretAt (location : Location) (run : DreamRun) =
        run.Turrets
        |> Map.toSeq
        |> Seq.tryPick (fun (id, turret) -> if turret.Location = location then Some (id, turret) else None)

    let tryFindMirrorAt (location : Location) (run : DreamRun) =
        run.Mirrors
        |> Map.toSeq
        |> Seq.tryPick (fun (id, mirror) -> if mirror.Location = location then Some (id, mirror) else None)

    let tryFindBlockAt (location : Location) (run : DreamRun) =
        run.Blocks
        |> Map.toSeq
        |> Seq.tryPick (fun (id, block) -> if block.Location = location then Some (id, block) else None)

    let tryFindEnemyAt (location : Location) (run : DreamRun) =
        run.Enemies
        |> Map.toSeq
        |> Seq.tryPick (fun (id, enemy) -> if enemy.Location = location then Some (id, enemy) else None)

    let tryFindPickupAt (location : Location) (run : DreamRun) =
        run.Pickups
        |> Map.toSeq
        |> Seq.tryPick (fun (id, pickup) -> if pickup.Location = location then Some (id, pickup) else None)

    let tryFindProjectileAt (location : Location) (run : DreamRun) =
        run.Projectiles
        |> List.tryFind (fun projectile -> projectile.Location = location)

    let isDeviceAt (location : Location) (run : DreamRun) =
        Option.isSome (tryFindCameraAt location run) ||
        Option.isSome (tryFindTurretAt location run) ||
        Option.isSome (tryFindMirrorAt location run)

    let isOccupiedForPlayer (location : Location) (run : DreamRun) =
        Option.isSome (tryFindBlockAt location run) ||
        Option.isSome (tryFindEnemyAt location run) ||
        isDeviceAt location run

[<RequireQualifiedAccess>]
module DreamContent =

    let private parseInt value =
        Int32.Parse value

    let private defaultEnemyVisionRange = 6

    let private parseCell x y z =
        { X = parseInt x; Y = parseInt y; Z = parseInt z }

    let private parseLocation sectionId x y z =
        { SectionId = sectionId; Cell = parseCell x y z }

    let private parseMapFile (path : string) =
        let mutable sectionId = Path.GetFileNameWithoutExtension path
        let mutable preferred = (DreamGrid.DefaultWidth, DreamGrid.DefaultHeight)
        let mutable currentLayer = None
        let mutable layerRows = Map.empty<int, string list>
        for rawLine in File.ReadAllLines path do
            let line = rawLine.TrimEnd ()
            if line.StartsWith "@section" then
                let parts = line.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)
                if parts.Length > 1 then sectionId <- parts[1]
            elif line.StartsWith "@preferred_view" then
                let parts = line.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)
                if parts.Length > 2 then preferred <- (parseInt parts[1], parseInt parts[2])
            elif line.StartsWith "@layer" then
                let parts = line.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)
                if parts.Length > 1 then
                    let z = parts[1].TrimStart('z') |> parseInt
                    currentLayer <- Some z
            elif not (String.IsNullOrWhiteSpace line) then
                match currentLayer with
                | Some z ->
                    let rows = Map.tryFind z layerRows |> Option.defaultValue []
                    layerRows <- Map.add z (rows @ [line]) layerRows
                | None -> ()
        let (width, height) = preferred
        let layers =
            layerRows
            |> Map.map (fun z rows ->
                let rows = List.toArray rows
                let layer = Array2D.create width height Wall
                for y in 0 .. height - 1 do
                    let row =
                        if y < rows.Length then rows[y]
                        else String.replicate width "#"
                    let row =
                        if row.Length < width then row.PadRight (width, '#')
                        else row.Substring (0, width)
                    for x in 0 .. width - 1 do
                        layer[x, y] <- DreamMap.terrainOfChar z row[x]
                layer)
        { SectionId = sectionId
          Width = width
          Height = height
          PreferredView = preferred
          Layers = layers
          Links = [] },
        path

    let private parseMetaFile sectionId path =
        let mutable player = None
        let mutable links = []
        let mutable cameras = []
        let mutable turrets = []
        let mutable mirrors = []
        let mutable blocks = []
        let mutable enemies = []
        let mutable pickups = []
        if File.Exists path then
            for rawLine in File.ReadAllLines path do
                let line = rawLine.Trim ()
                if not (String.IsNullOrWhiteSpace line) && not (line.StartsWith "#") then
                    let parts = line.Split ([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
                    if parts.Length > 0 then
                        match parts[0] with
                        | "player" when parts.Length >= 4 ->
                            player <- Some (parseLocation sectionId parts[1] parts[2] parts[3])
                        | "link" when parts.Length >= 9 ->
                            let link =
                                { LinkId = parts[1]
                                  From = parseLocation sectionId parts[2] parts[3] parts[4]
                                  Destination = parseLocation parts[5] parts[6] parts[7] parts[8] }
                            links <- link :: links
                        | "camera" when parts.Length >= 7 ->
                            let camera =
                                { CameraId = parts[1]
                                  Location = parseLocation sectionId parts[2] parts[3] parts[4]
                                  Facing = Direction.parse parts[5]
                                  Range = parseInt parts[6] }
                            cameras <- camera :: cameras
                        | "turret" when parts.Length >= 7 ->
                            let turret =
                                { TurretId = parts[1]
                                  Location = parseLocation sectionId parts[2] parts[3] parts[4]
                                  Facing = Direction.parse parts[5]
                                  Range = parseInt parts[6] }
                            turrets <- turret :: turrets
                        | "mirror" when parts.Length >= 6 ->
                            let mirror =
                                { MirrorId = parts[1]
                                  Location = parseLocation sectionId parts[2] parts[3] parts[4]
                                  Kind = MirrorKind.parse parts[5] }
                            mirrors <- mirror :: mirrors
                        | "block" when parts.Length >= 5 ->
                            let block =
                                { BlockId = parts[1]
                                  Location = parseLocation sectionId parts[2] parts[3] parts[4] }
                            blocks <- block :: blocks
                        | "enemy" when parts.Length >= 6 ->
                            let enemy =
                                { EnemyId = parts[1]
                                  Location = parseLocation sectionId parts[2] parts[3] parts[4]
                                  Facing = Direction.parse parts[5]
                                  VisionRange = if parts.Length >= 7 then parseInt parts[6] else defaultEnemyVisionRange
                                  Alert = 0 }
                            enemies <- enemy :: enemies
                        | "pickup" when parts.Length >= 7 ->
                            let pickup =
                                { PickupId = parts[1]
                                  Location = parseLocation sectionId parts[2] parts[3] parts[4]
                                  Kind = PickupKind.parse parts[5]
                                  Amount = parseInt parts[6] }
                            pickups <- pickup :: pickups
                        | _ -> ()
        player, links, cameras, turrets, mirrors, blocks, enemies, pickups

    let private aheadReticle (player : Location) (facing : Direction) (sections : Map<string, DreamSection>) =
        let section = Map.find player.SectionId sections
        let (dx, dy) = facing.Delta
        let mutable reticle = player.Cell
        let mutable blocked = false
        for distance in 1 .. DreamGrid.ReticleStartDistance do
            if not blocked then
                let candidate =
                    { player.Cell with X = player.Cell.X + dx * distance; Y = player.Cell.Y + dy * distance }
                    |> DreamMap.clampCell section
                match DreamMap.terrainAtLayer player.Cell.Z section candidate with
                | Floor | Door | Portal | Upper _ when candidate <> player.Cell -> reticle <- candidate
                | _ -> blocked <- true
        reticle

    let private cameraMap (values : Camera list) =
        values |> List.map (fun value -> value.CameraId, value) |> Map.ofList

    let private turretMap (values : Turret list) =
        values |> List.map (fun value -> value.TurretId, value) |> Map.ofList

    let private mirrorMap (values : Mirror list) =
        values |> List.map (fun value -> value.MirrorId, value) |> Map.ofList

    let private blockMap (values : Block list) =
        values |> List.map (fun value -> value.BlockId, value) |> Map.ofList

    let private enemyMap (values : Enemy list) =
        values |> List.map (fun value -> value.EnemyId, value) |> Map.ofList

    let private pickupMap (values : Pickup list) =
        values |> List.map (fun value -> value.PickupId, value) |> Map.ofList

    let private layoutMargin : Cell = { X = 2; Y = 1; Z = 0 }

    let private tryEdgeDirection (section : DreamSection) (cell : Cell) =
        if cell.X = 0 then Some West
        elif cell.X = section.Width - 1 then Some East
        elif cell.Y = 0 then Some North
        elif cell.Y = section.Height - 1 then Some South
        else None

    let private sectionOverlaps (origin : Cell) (section : DreamSection) (otherOrigin : Cell) (otherSection : DreamSection) =
        origin.X < otherOrigin.X + otherSection.Width &&
        origin.X + section.Width > otherOrigin.X &&
        origin.Y < otherOrigin.Y + otherSection.Height &&
        origin.Y + section.Height > otherOrigin.Y

    let private collidesWithPlaced sectionId (origin : Cell) (section : DreamSection) (sections : Map<string, DreamSection>) (origins : Map<string, Cell>) =
        origins
        |> Map.exists (fun otherId otherOrigin ->
            otherId <> sectionId &&
            match Map.tryFind otherId sections with
            | Some otherSection -> sectionOverlaps origin section otherOrigin otherSection
            | None -> false)

    let private tryMountLink (sections : Map<string, DreamSection>) (origins : Map<string, Cell>) (link : Link) =
        if link.From.SectionId = link.Destination.SectionId then None
        else
            match Map.tryFind link.From.SectionId sections, Map.tryFind link.Destination.SectionId sections with
            | Some sourceSection, Some destinationSection ->
                match tryEdgeDirection sourceSection link.From.Cell, tryEdgeDirection destinationSection link.Destination.Cell with
                | Some sourceSide, Some destinationSide when destinationSide = sourceSide.Opposite ->
                    let dx, dy = sourceSide.Delta
                    match Map.tryFind link.From.SectionId origins, Map.tryFind link.Destination.SectionId origins with
                    | Some sourceOrigin, None ->
                        let destinationOrigin =
                            { X = sourceOrigin.X + link.From.Cell.X + dx - link.Destination.Cell.X
                              Y = sourceOrigin.Y + link.From.Cell.Y + dy - link.Destination.Cell.Y
                              Z = 0 }
                        Some (link.Destination.SectionId, destinationOrigin)
                    | None, Some destinationOrigin ->
                        let sourceOrigin =
                            { X = destinationOrigin.X + link.Destination.Cell.X - link.From.Cell.X - dx
                              Y = destinationOrigin.Y + link.Destination.Cell.Y - link.From.Cell.Y - dy
                              Z = 0 }
                        Some (link.From.SectionId, sourceOrigin)
                    | _ -> None
                | _ -> None
            | _ -> None

    let private rootSectionId (sections : Map<string, DreamSection>) (playerOpt : Location option) =
        match playerOpt with
        | Some player -> player.SectionId
        | None when Map.containsKey "hub" sections -> "hub"
        | None ->
            sections
            |> Map.toSeq
            |> Seq.map fst
            |> Seq.tryHead
            |> Option.defaultValue ""

    let private normalizeOrigins (sections : Map<string, DreamSection>) (origins : Map<string, Cell>) =
        if Map.isEmpty origins then origins
        else
            let bounds =
                origins
                |> Map.toSeq
                |> Seq.choose (fun (sectionId, origin) ->
                    Map.tryFind sectionId sections
                    |> Option.map (fun section ->
                        origin.X,
                        origin.Y,
                        origin.X + section.Width,
                        origin.Y + section.Height))
                |> Seq.toArray
            if bounds.Length = 0 then origins
            else
                let minX = bounds |> Array.map (fun (minX, _, _, _) -> minX) |> Array.min
                let minY = bounds |> Array.map (fun (_, minY, _, _) -> minY) |> Array.min
                origins
                |> Map.map (fun _ origin ->
                    { origin with
                        X = origin.X - minX + layoutMargin.X
                        Y = origin.Y - minY + layoutMargin.Y })

    let private packUnplacedSections (sections : Map<string, DreamSection>) (origins : Map<string, Cell>) =
        let mutable origins = origins
        let mutable cursorX =
            origins
            |> Map.toSeq
            |> Seq.choose (fun (sectionId, origin) ->
                Map.tryFind sectionId sections
                |> Option.map (fun section -> origin.X + section.Width + 2))
            |> Seq.fold max 0
        for KeyValue (sectionId, section) in sections do
            if not (Map.containsKey sectionId origins) then
                let origin = { X = cursorX; Y = 0; Z = 0 }
                origins <- Map.add sectionId origin origins
                cursorX <- cursorX + section.Width + 2
        origins

    let private layoutSections (sections : Map<string, DreamSection>) (playerOpt : Location option) (links : Link list) =
        let root = rootSectionId sections playerOpt
        let mutable origins =
            if String.IsNullOrWhiteSpace root then Map.empty
            else Map.add root { X = 0; Y = 0; Z = 0 } Map.empty
        let mutable changed = true
        while changed do
            changed <- false
            for link in links do
                match tryMountLink sections origins link with
                | Some (sectionId, origin) when not (Map.containsKey sectionId origins) ->
                    match Map.tryFind sectionId sections with
                    | Some section when not (collidesWithPlaced sectionId origin section sections origins) ->
                        origins <- Map.add sectionId origin origins
                        changed <- true
                    | _ -> ()
                | _ -> ()
        origins
        |> packUnplacedSections sections
        |> normalizeOrigins sections

    let loadRun () =
        let mapFiles =
            if Directory.Exists Assets.Gameplay.SectionsDirectoryPath then
                Directory.GetFiles (Assets.Gameplay.SectionsDirectoryPath, "*.dreammap")
            else [||]
        let parsedSections = mapFiles |> Array.map parseMapFile
        let mutable sections =
            parsedSections
            |> Array.map fst
            |> Array.map (fun section -> section.SectionId, section)
            |> Map.ofArray
        let mutable player = None
        let mutable allLinks = []
        let mutable allCameras = []
        let mutable allTurrets = []
        let mutable allMirrors = []
        let mutable allBlocks = []
        let mutable allEnemies = []
        let mutable allPickups = []
        for (section, mapPath) in parsedSections do
            let metaPath = Path.ChangeExtension (mapPath, ".dreammeta")
            let (playerOpt, links, cameras, turrets, mirrors, blocks, enemies, pickups) = parseMetaFile section.SectionId metaPath
            player <- Option.orElse player playerOpt
            allLinks <- links @ allLinks
            allCameras <- cameras @ allCameras
            allTurrets <- turrets @ allTurrets
            allMirrors <- mirrors @ allMirrors
            allBlocks <- blocks @ allBlocks
            allEnemies <- enemies @ allEnemies
            allPickups <- pickups @ allPickups
        for link in allLinks do
            match Map.tryFind link.From.SectionId sections with
            | Some section -> sections <- Map.add section.SectionId { section with Links = link :: section.Links } sections
            | None -> ()
        let sectionOrigins = layoutSections sections player allLinks
        let player =
            player
            |> Option.defaultWith (fun _ -> { SectionId = "hub"; Cell = { X = 4; Y = 6; Z = 0 } })
        let facing = East
        let (reticleDx, reticleDy) = facing.Delta
        { Sections = sections
          SectionOrigins = sectionOrigins
          Player = player
          Facing = facing
          Reticle = aheadReticle player facing sections
          ReticleOffset =
            { X = reticleDx * DreamGrid.ReticleStartDistance
              Y = reticleDy * DreamGrid.ReticleStartDistance
              Z = 0 }
          Resources = { Health = 100; Focus = 60; Ammo = 30; AlarmDebt = 0 }
          Weapon = Pistol
          Cameras = cameraMap allCameras
          Turrets = turretMap allTurrets
          Mirrors = mirrorMap allMirrors
          Blocks = blockMap allBlocks
          Enemies = enemyMap allEnemies
          Pickups = pickupMap allPickups
          Projectiles = []
          ProjectileSerial = 0
          HeldBlock = None
          VisibleCells = Set.empty
          RememberedCells = Map.empty
          Tick = 0L
          Log = ["Wake vector stabilized"; "WASD move  Arrows aim  Space fire"] }

[<RequireQualifiedAccess>]
module DreamInput =

    let reticleDirectionOfKey key =
        match key with
        | KeyboardKey.Up -> Some North
        | KeyboardKey.Right -> Some East
        | KeyboardKey.Down -> Some South
        | KeyboardKey.Left -> Some West
        | _ -> None

    let commandOfKey key =
        match key with
        | KeyboardKey.W -> Some (MoveQueued North)
        | KeyboardKey.D -> Some (MoveQueued East)
        | KeyboardKey.S -> Some (MoveQueued South)
        | KeyboardKey.A -> Some (MoveQueued West)
        | KeyboardKey.Space -> Some FireQueued
        | KeyboardKey.G -> Some GrabToggleQueued
        | _ -> None

    let weaponOfKey key =
        match key with
        | KeyboardKey.Num1 -> Some Pistol
        | KeyboardKey.Num2 -> Some Needle
        | _ -> None

    let togglesProjection key =
        key = KeyboardKey.Tab

[<RequireQualifiedAccess>]
module DreamSensors =

    let private lateralCell (source : Cell) facing distance lateral =
        match facing with
        | North -> { source with X = source.X + lateral; Y = source.Y - distance }
        | East -> { source with X = source.X + distance; Y = source.Y + lateral }
        | South -> { source with X = source.X + lateral; Y = source.Y + distance }
        | West -> { source with X = source.X - distance; Y = source.Y + lateral }

    let private terrainBlocksVision (run : DreamRun) (location : Location) =
        match DreamMap.terrainAt run location with
        | Wall | Door | Void -> true
        | Floor | Portal | Upper _ -> false

    let private blocksVision (run : DreamRun) (location : Location) =
        terrainBlocksVision run location ||
        Option.isSome (DreamLookup.tryFindBlockAt location run)

    let private lineCells (start : Cell) (finish : Cell) =
        let cells = ResizeArray<Cell> ()
        let dx = abs (finish.X - start.X)
        let dy = abs (finish.Y - start.Y)
        let sx = Math.Sign (finish.X - start.X)
        let sy = Math.Sign (finish.Y - start.Y)
        let mutable x = start.X
        let mutable y = start.Y
        let mutable error = dx - dy
        while x <> finish.X || y <> finish.Y do
            let error2 = error * 2
            if error2 > -dy then
                error <- error - dy
                x <- x + sx
            if error2 < dx then
                error <- error + dx
                y <- y + sy
            cells.Add { start with X = x; Y = y }
        cells

    let private hasLineOfSight (run : DreamRun) (section : DreamSection) (source : Cell) (target : Cell) =
        let cells = lineCells source target
        let mutable clear = true
        let mutable index = 0
        while clear && index < cells.Count do
            let cell = cells[index]
            let location = { SectionId = section.SectionId; Cell = cell }
            if not (DreamMap.containsCell section cell) || blocksVision run location then clear <- false
            index <- index + 1
        clear

    let private hasPlayerLineOfSight (run : DreamRun) (section : DreamSection) (source : Cell) (target : Cell) =
        let cells = lineCells source target
        let mutable clear = true
        let mutable index = 0
        while clear && index < cells.Count do
            let cell = cells[index]
            let location = { SectionId = section.SectionId; Cell = cell }
            if not (DreamMap.containsCell section cell) then clear <- false
            elif cell <> target && blocksVision run location then clear <- false
            index <- index + 1
        clear

    let private facingCoordinates (source : Cell) facing (cell : Cell) =
        match facing with
        | North -> source.Y - cell.Y, cell.X - source.X
        | East -> cell.X - source.X, cell.Y - source.Y
        | South -> cell.Y - source.Y, cell.X - source.X
        | West -> source.X - cell.X, cell.Y - source.Y

    let playerCells (run : DreamRun) range =
        match DreamMap.tryGetSection run run.Player.SectionId with
        | Some section ->
            let cells = ResizeArray<Location> ()
            for y in 0 .. section.Height - 1 do
                for x in 0 .. section.Width - 1 do
                    let cell = { X = x; Y = y; Z = run.Player.Cell.Z }
                    let forward, _ = facingCoordinates run.Player.Cell run.Facing cell
                    if (cell = run.Player.Cell ||
                        forward >= 0 && forward <= range) &&
                       hasPlayerLineOfSight run section run.Player.Cell cell then
                        cells.Add { SectionId = run.Player.SectionId; Cell = cell }
            cells |> Set.ofSeq
        | None -> Set.empty

    let cells (run : DreamRun) (source : Location) facing range =
        let cells = ResizeArray<Location> ()
        let mutable rayLocation = source
        let mutable rayFacing = facing
        let mutable blocked = false
        for _ in 1 .. range do
            if not blocked then
                rayLocation <- DreamMap.step run rayLocation rayFacing
                if blocksVision run rayLocation then blocked <- true
                else
                    if DreamMap.isWalkableTerrain run rayLocation then cells.Add rayLocation
                    match DreamLookup.tryFindMirrorAt rayLocation run with
                    | Some (_, mirror) -> rayFacing <- mirror.Kind.Reflect rayFacing
                    | None -> ()
        match DreamMap.tryGetSection run source.SectionId with
        | Some section ->
            for distance in 2 .. range do
                let width = distance / 4
                for lateral in -width .. width do
                    if lateral <> 0 then
                        let cell = lateralCell source.Cell facing distance lateral
                        let location = { source with Cell = cell }
                        if hasLineOfSight run section source.Cell cell && DreamMap.isWalkableTerrain run location then cells.Add location
        | None -> ()
        cells |> Seq.distinct |> List.ofSeq

    let cameraCovers (location : Location) (run : DreamRun) =
        run.Cameras
        |> Map.toSeq
        |> Seq.exists (fun (_, camera) -> cells run camera.Location camera.Facing camera.Range |> List.contains location)

    let turretCovers (location : Location) (run : DreamRun) =
        run.Turrets
        |> Map.toSeq
        |> Seq.exists (fun (_, turret) -> cells run turret.Location turret.Facing turret.Range |> List.contains location)

    let enemyCovers (location : Location) (run : DreamRun) =
        run.Enemies
        |> Map.toSeq
        |> Seq.exists (fun (_, enemy) -> cells run enemy.Location enemy.Facing enemy.VisionRange |> List.contains location)

[<RequireQualifiedAccess>]
module DreamSim =

    let private trimLog (run : DreamRun) =
        { run with Log = run.Log |> Seq.truncate 5 |> List.ofSeq }

    let private addLog message (run : DreamRun) =
        { run with Log = message :: run.Log } |> trimLog

    let private updateFacing (run : DreamRun) =
        let dx = run.Reticle.X - run.Player.Cell.X
        let dy = run.Reticle.Y - run.Player.Cell.Y
        if dx = 0 && dy = 0 then run
        else
            let facing =
                if abs dx >= abs dy then
                    if dx >= 0 then East else West
                else
                    if dy >= 0 then South else North
            { run with Facing = facing }

    let private reticleCellIsUsable (run : DreamRun) (cell : Cell) =
        let location = { SectionId = run.Player.SectionId; Cell = cell }
        cell <> run.Player.Cell && DreamMap.isWalkableTerrain run location

    let private reticleCellOfOffset (player : Location) (offset : Cell) =
        { player.Cell with
            X = player.Cell.X + offset.X
            Y = player.Cell.Y + offset.Y
            Z = player.Cell.Z }

    let private reticleOffsetOfDirection (direction : Direction) =
        let (dx, dy) = direction.Delta
        { X = dx; Y = dy; Z = 0 }

    let private reticleOffsetIsZero (offset : Cell) =
        offset.X = 0 && offset.Y = 0

    let private constrainReticleOffset (run : DreamRun) (offset : Cell) =
        match DreamMap.tryGetSection run run.Player.SectionId with
        | Some section ->
            { X = max -section.Width (min section.Width offset.X)
              Y = max -section.Height (min section.Height offset.Y)
              Z = 0 }
        | None -> { offset with Z = 0 }

    let private roundedOffsetStep offsetComponent index steps =
        if offsetComponent = 0 then 0
        else Math.Round (float offsetComponent * float index / float steps, MidpointRounding.AwayFromZero) |> int

    let private tryProjectReticleCell (run : DreamRun) =
        let offset = run.ReticleOffset
        if reticleOffsetIsZero offset then None
        else
            match DreamMap.tryGetSection run run.Player.SectionId with
            | Some _ ->
                let desired = reticleCellOfOffset run.Player offset
                if reticleCellIsUsable run desired then Some desired
                else
                    let steps = max (abs offset.X) (abs offset.Y)
                    let mutable reticleOpt = None
                    let mutable blocked = false
                    for index in 1 .. steps do
                        if not blocked then
                            let candidate =
                                { run.Player.Cell with
                                    X = run.Player.Cell.X + roundedOffsetStep offset.X index steps
                                    Y = run.Player.Cell.Y + roundedOffsetStep offset.Y index steps }
                            if candidate <> run.Player.Cell then
                                if reticleCellIsUsable run candidate then reticleOpt <- Some candidate
                                else blocked <- true
                    reticleOpt
            | None -> None

    let private projectReticle (run : DreamRun) =
        match tryProjectReticleCell run with
        | Some reticle -> { run with Reticle = reticle } |> updateFacing
        | None -> { run with Reticle = run.Player.Cell }

    let moveReticle direction (run : DreamRun) =
        let step = reticleOffsetOfDirection direction
        let offset =
            { run.ReticleOffset with
                X = run.ReticleOffset.X + step.X
                Y = run.ReticleOffset.Y + step.Y }
            |> constrainReticleOffset run
        if reticleOffsetIsZero offset then
            let offset = step |> constrainReticleOffset run
            if reticleOffsetIsZero offset then run
            else { run with ReticleOffset = offset } |> projectReticle
        else { run with ReticleOffset = offset } |> projectReticle

    let rotatePlayerProjection sourceRotation destinationRotation (run : DreamRun) =
        let facing = DreamProjection.transformDirection sourceRotation destinationRotation run.Facing
        let reticleOffset =
            run.ReticleOffset
            |> DreamProjectionCell.transformOffset sourceRotation destinationRotation
            |> constrainReticleOffset run
        let run =
            { run with
                Facing = facing
                ReticleOffset = reticleOffset }
        if Option.isSome run.HeldBlock then { run with Reticle = run.Player.Cell }
        else projectReticle run

    let private canPlayerEnter (location : Location) (run : DreamRun) =
        DreamMap.isWalkableTerrain run location && not (DreamLookup.isOccupiedForPlayer location run)

    let private isOtherBlockAt (heldBlockId : string) (location : Location) (run : DreamRun) =
        run.Blocks
        |> Map.exists (fun blockId block -> blockId <> heldBlockId && block.Location = location)

    let private isActorOrDeviceAt location (run : DreamRun) =
        Option.isSome (DreamLookup.tryFindEnemyAt location run) ||
        DreamLookup.isDeviceAt location run

    let private canPlayerEnterWhileHolding heldBlockId (location : Location) (run : DreamRun) =
        DreamMap.isWalkableTerrain run location &&
        not (isOtherBlockAt heldBlockId location run) &&
        not (isActorOrDeviceAt location run)

    let private canHeldBlockEnter heldBlockId (playerDestination : Location) (location : Location) (run : DreamRun) =
        DreamMap.isWalkableTerrain run location &&
        location <> playerDestination &&
        not (isOtherBlockAt heldBlockId location run) &&
        not (isActorOrDeviceAt location run)

    let private tryHeldBlockDestination heldBlockId direction heldOffset (playerDestination : Location) (block : Block) folded run =
        let offsetDestination = { playerDestination with Cell = reticleCellOfOffset playerDestination heldOffset }
        let steppedDestination = DreamMap.step run block.Location direction
        let adjacentDirections =
            [ direction
              direction.Opposite
              direction.Clockwise
              direction.Clockwise.Opposite ]
        let candidates =
            if folded then
                [ yield offsetDestination
                  yield steppedDestination
                  for direction in adjacentDirections do
                      yield playerDestination.Step direction ]
            else [offsetDestination]
        candidates
        |> List.distinct
        |> List.tryFind (fun location -> canHeldBlockEnter heldBlockId playerDestination location run)

    let private heldBlockOffset (player : Location) (block : Location) =
        { X = block.Cell.X - player.Cell.X
          Y = block.Cell.Y - player.Cell.Y
          Z = 0 }

    let private defaultReticleOffset (facing : Direction) =
        let (dx, dy) = facing.Delta
        { X = dx * DreamGrid.ReticleStartDistance
          Y = dy * DreamGrid.ReticleStartDistance
          Z = 0 }

    let private grabBlock (run : DreamRun) =
        let target = run.Player.Step run.Facing
        match DreamLookup.tryFindBlockAt target run with
        | Some (blockId, block) ->
            { run with
                HeldBlock = Some { BlockId = blockId; Offset = heldBlockOffset run.Player block.Location }
                Reticle = run.Player.Cell }
            |> addLog "Box gripped"
        | None -> addLog "No box in reach" run

    let private releaseBlock (run : DreamRun) =
        { run with
            HeldBlock = None
            ReticleOffset = defaultReticleOffset run.Facing }
        |> projectReticle
        |> addLog "Box released"

    let toggleGrab (run : DreamRun) =
        match run.HeldBlock with
        | Some _ -> releaseBlock run
        | None -> grabBlock run

    let private movePlayerAlone direction (run : DreamRun) =
        let destination = DreamMap.step run run.Player direction
        if canPlayerEnter destination run then
            { run with Player = destination } |> projectReticle
        else run

    let private movePlayerHolding direction held (run : DreamRun) =
        match Map.tryFind held.BlockId run.Blocks with
        | Some block ->
            let playerDestination = DreamMap.step run run.Player direction
            let folded =
                Option.isSome (DreamMap.tryExitLink run run.Player direction) ||
                Option.isSome (DreamMap.tryExitLink run block.Location direction)
            match tryHeldBlockDestination held.BlockId direction held.Offset playerDestination block folded run with
            | Some blockDestination when canPlayerEnterWhileHolding held.BlockId playerDestination run ->
                let block = { block with Location = blockDestination }
                { run with
                    Player = playerDestination
                    HeldBlock = Some { held with Offset = heldBlockOffset playerDestination blockDestination }
                    Blocks = Map.add held.BlockId block run.Blocks }
            | _ -> addLog "Box drag blocked" run
        | None ->
            { run with HeldBlock = None }
            |> movePlayerAlone direction

    let private movePlayer direction (run : DreamRun) =
        match run.HeldBlock with
        | Some held -> movePlayerHolding direction held run
        | None -> movePlayerAlone direction run

    let rotateHeld direction (run : DreamRun) =
        match run.HeldBlock with
        | Some held ->
            match Map.tryFind held.BlockId run.Blocks with
            | Some block ->
                let offset = reticleOffsetOfDirection direction
                let destination = { run.Player with Cell = reticleCellOfOffset run.Player offset }
                if block.Location = destination then { run with Facing = direction; HeldBlock = Some { held with Offset = offset } }
                elif canHeldBlockEnter held.BlockId run.Player destination run then
                    let block = { block with Location = destination }
                    { run with
                        Facing = direction
                        HeldBlock = Some { held with Offset = offset }
                        Blocks = Map.add held.BlockId block run.Blocks }
                else addLog "Box rotation blocked" run
            | None -> { run with HeldBlock = None }
        | None -> moveReticle direction run

    let private damageAt (location : Location) (run : DreamRun) =
        match DreamLookup.tryFindEnemyAt location run with
        | Some (enemyId, _) ->
            Some ({ run with Enemies = Map.remove enemyId run.Enemies }, "Sleeper dissolved")
        | None ->
            match DreamLookup.tryFindCameraAt location run with
            | Some (cameraId, _) ->
                Some ({ run with Cameras = Map.remove cameraId run.Cameras }, "Camera optic cracked")
            | None ->
                match DreamLookup.tryFindTurretAt location run with
                | Some (turretId, _) ->
                    Some ({ run with Turrets = Map.remove turretId run.Turrets }, "Turret motor cut")
                | None -> None

    let private fireHitscan (run : DreamRun) =
        let mutable current = run.Player
        let mutable remaining = 18
        let mutable running = run
        let mutable stopped = false
        while remaining > 0 && not stopped do
            current <- DreamMap.step running current running.Facing
            if not (DreamMap.isWalkableTerrain running current) then
                stopped <- true
            else
                match damageAt current running with
                | Some (run', message) ->
                    running <- addLog message run'
                    stopped <- true
                | None -> ()
            remaining <- remaining - 1
        if stopped then running else addLog "Round lost in foldspace" running

    let private fireNeedle (run : DreamRun) =
        let projectile =
            { ProjectileId = run.ProjectileSerial
              Location = run.Player
              Facing = run.Facing
              RangeLeft = 18 }
        { run with
            ProjectileSerial = run.ProjectileSerial + 1
            Projectiles = projectile :: run.Projectiles }
        |> addLog "Needle launched"

    let private fireWeapon (run : DreamRun) =
        let cost = run.Weapon.Cost
        if Option.isSome run.HeldBlock then addLog "Grip blocks trigger" run
        elif run.Resources.Ammo < cost then addLog "Dry trigger" run
        else
            let run =
                { run with Resources = { run.Resources with Ammo = run.Resources.Ammo - cost } }
            match run.Weapon with
            | Pistol -> fireHitscan run
            | Needle -> fireNeedle run

    let private advanceProjectiles (run : DreamRun) =
        let mutable running = { run with Projectiles = [] }
        let mutable active = []
        for projectile in run.Projectiles do
            let next = DreamMap.step running projectile.Location projectile.Facing
            if projectile.RangeLeft <= 0 || not (DreamMap.isWalkableTerrain running next) then
                ()
            else
                match damageAt next running with
                | Some (run', message) -> running <- addLog message run'
                | None ->
                    active <- { projectile with Location = next; RangeLeft = projectile.RangeLeft - 1 } :: active
        { running with Projectiles = List.rev active }

    let private pathNeighbors (goal : Location) (run : DreamRun) (location : Location) =
        [North; East; South; West]
        |> List.choose (fun direction ->
            let arrived = DreamMap.step run location direction
            let occupied =
                if arrived = goal then false
                else
                    Option.isSome (DreamLookup.tryFindBlockAt arrived run) ||
                    Option.isSome (DreamLookup.tryFindEnemyAt arrived run) ||
                    DreamLookup.isDeviceAt arrived run
            if DreamMap.isWalkableTerrain run arrived && not occupied then Some arrived
            else None)

    let private tryNextStep (start : Location) (goal : Location) (run : DreamRun) =
        if start = goal then None
        else
            let visited = HashSet<Location> ()
            let firstSteps = Dictionary<Location, Location> ()
            let queue = Queue<Location> ()
            visited.Add start |> ignore
            queue.Enqueue start
            let mutable found = None
            while queue.Count > 0 && Option.isNone found do
                let current = queue.Dequeue ()
                for next in pathNeighbors goal run current do
                    if visited.Add next then
                        let first =
                            if current = start then next
                            else firstSteps[current]
                        firstSteps[next] <- first
                        if next = goal then found <- Some first
                        queue.Enqueue next
            found

    let private stableIdValue (value : string) =
        value
        |> Seq.fold (fun hash character -> (hash * 31 + int character) &&& 0x7fffffff) 17

    let private tickMatches divisor phase (run : DreamRun) =
        (run.Tick + int64 phase) % divisor = 0L

    let private directionOfStep (run : DreamRun) source destination fallback =
        [North; East; South; West]
        |> List.tryFind (fun direction -> DreamMap.step run source direction = destination)
        |> Option.defaultValue fallback

    let private canEnemyEnter enemyId location (run : DreamRun) =
        DreamMap.isWalkableTerrain run location &&
        location <> run.Player &&
        Option.isNone (DreamLookup.tryFindBlockAt location run) &&
        not (DreamLookup.isDeviceAt location run) &&
        not (
            run.Enemies
            |> Map.exists (fun otherId other -> otherId <> enemyId && other.Location = location))

    let private idleEnemy enemyId enemy (run : DreamRun) =
        let phase = stableIdValue enemyId % 7
        let alert = max 0 (enemy.Alert - 1)
        if tickMatches 6L phase run then
            let destination = DreamMap.step run enemy.Location enemy.Facing
            if canEnemyEnter enemyId destination run then
                { enemy with Location = destination; Alert = alert }
            else { enemy with Facing = enemy.Facing.Clockwise; Alert = alert }
        elif tickMatches 3L phase run then
            { enemy with Facing = enemy.Facing.Clockwise; Alert = alert }
        else { enemy with Alert = alert }

    let private moveEnemies (run : DreamRun) =
        let mutable running = run
        let enemies = running.Enemies |> Map.toList
        for (enemyId, _) in enemies do
            match Map.tryFind enemyId running.Enemies with
            | Some enemy ->
                if DreamSensors.cells running enemy.Location enemy.Facing enemy.VisionRange |> List.contains running.Player then
                    match tryNextStep enemy.Location running.Player running with
                    | Some next when next = running.Player ->
                        let resources = { running.Resources with Health = max 1 (running.Resources.Health - 4) }
                        let enemy = { enemy with Alert = min 9 (enemy.Alert + 1) }
                        running <-
                            { running with
                                Resources = resources
                                Enemies = Map.add enemyId enemy running.Enemies }
                            |> addLog "Sleeper strike: health lost"
                    | Some next ->
                        let facing =
                            let dx = next.Cell.X - enemy.Location.Cell.X
                            let dy = next.Cell.Y - enemy.Location.Cell.Y
                            if abs dx >= abs dy then if dx >= 0 then East else West
                            else if dy >= 0 then South else North
                        let facing = directionOfStep running enemy.Location next facing
                        let enemy = { enemy with Location = next; Facing = facing; Alert = min 9 (enemy.Alert + 1) }
                        running <- { running with Enemies = Map.add enemyId enemy running.Enemies }
                    | None -> ()
                else
                    let enemy = idleEnemy enemyId enemy running
                    running <- { running with Enemies = Map.add enemyId enemy running.Enemies }
            | None -> ()
        running

    let private collectPickup (run : DreamRun) =
        match DreamLookup.tryFindPickupAt run.Player run with
        | Some (pickupId, pickup) ->
            let resources =
                match pickup.Kind with
                | Ammo -> { run.Resources with Ammo = run.Resources.Ammo + pickup.Amount }
                | Med -> { run.Resources with Health = min 100 (run.Resources.Health + pickup.Amount * 25) }
                | CellBattery -> { run.Resources with Focus = min 100 (run.Resources.Focus + pickup.Amount * 25) }
            let message =
                match pickup.Kind with
                | Ammo -> "Ammo recovered"
                | Med -> "Somatic pattern restored"
                | CellBattery -> "Focus cell absorbed"
            { run with
                Resources = resources
                Pickups = Map.remove pickupId run.Pickups }
            |> addLog message
        | None -> run

    let private applySensors (run : DreamRun) =
        let seenByCamera = DreamSensors.cameraCovers run.Player run
        let seenByTurret = DreamSensors.turretCovers run.Player run
        let seenByEnemy = DreamSensors.enemyCovers run.Player run
        let alarmDelta = if seenByCamera then 2 else -1
        let focusDelta = if seenByCamera || seenByEnemy then -1 else 0
        let healthDelta = if seenByTurret then 5 else 0
        let resources =
            { run.Resources with
                AlarmDebt = max 0 (min 99 (run.Resources.AlarmDebt + alarmDelta))
                Focus = max 0 (run.Resources.Focus + focusDelta)
                Health = max 1 (run.Resources.Health - healthDelta) }
        let run = { run with Resources = resources }
        if seenByTurret then addLog "Turret lane burned health" run
        elif seenByCamera then addLog "Camera debt accrued" run
        elif seenByEnemy then addLog "Sleeper gaze drains focus" run
        else run

    let tick commandOpt (run : DreamRun) =
        let run =
            let run = { run with Tick = run.Tick + 1L }
            if Option.isSome run.HeldBlock then run else projectReticle run
        let run, consumed =
            match commandOpt with
            | Some (MoveQueued direction) -> movePlayer direction run, true
            | Some FireQueued -> fireWeapon run, true
            | Some GrabToggleQueued -> toggleGrab run, true
            | None -> run, false
        let run =
            run
            |> advanceProjectiles
            |> moveEnemies
            |> collectPickup
            |> applySensors
            |> trimLog
        run, consumed

type DreamGlyphGridDispatcher () =
    inherit GuiDispatcher<DreamGlyphGrid, Message, Command> (DreamGlyphGrid.Empty)

    static member Properties =
        [define Entity.MsdfFont Assets.Gameplay.DejaVuSansMonoMtsdf
         define Entity.FontSizing (Some 9.0f)
         define Entity.TextShift Constants.Gui.TextShiftDefault
         define Entity.TextDirection TextDirectionAuto
         define Entity.LanguageOpt None
         define Entity.MsdfEdgeOffset MsdfTextShader.defaultShader.EdgeOffset
         define Entity.MsdfSoftness MsdfTextShader.defaultShader.Softness
         define Entity.MsdfOutlineColor MsdfTextShader.defaultShader.OutlineColor
         define Entity.MsdfOutlineThickness MsdfTextShader.defaultShader.OutlineThickness
         define Entity.MsdfOutlineSoftness MsdfTextShader.defaultShader.OutlineSoftness
         define Entity.GlyphCutTop 2.5f
         define Entity.GlyphCutBottom 1.0f]

    override this.Render (grid, _, entity, world) =
        if grid.Width > 0 && grid.Height > 0 && grid.Glyphs.Length > 0 then
            let mutable transform = entity.GetTransform world
            transform.Elevation <- transform.Elevation + entity.GetTextShift world
            let clipOpt = ValueSome transform.Bounds2d.Box2
            let msdfFont = entity.GetMsdfFont world
            let shader =
                { EdgeOffset = entity.GetMsdfEdgeOffset world
                  Softness = entity.GetMsdfSoftness world
                  OutlineColor = entity.GetMsdfOutlineColor world
                  OutlineThickness = entity.GetMsdfOutlineThickness world
                  OutlineSoftness = entity.GetMsdfOutlineSoftness world }
            let descriptor =
                { Transform = transform
                  ClipOpt = clipOpt
                  Glyphs = grid.Glyphs
                  Columns = grid.Width
                  Rows = grid.Height
                  CellSize = v2 DreamGrid.CellWidth DreamGrid.CellHeight
                  GlyphScale = v2 DreamGrid.GlyphWidthScale DreamGrid.GlyphHeightScale
                  GlyphCutTop = entity.GetGlyphCutTop world
                  GlyphCutBottom = entity.GetGlyphCutBottom world
                  MsdfFont = msdfFont
                  FontSizing = entity.GetFontSizing world
                  Shader = shader
                  TextDirection = entity.GetTextDirection world
                  LanguageOpt = entity.GetLanguageOpt world }
            World.enqueueLayeredOperation2d
                { Elevation = transform.Elevation
                  Horizon = transform.Perimeter.Center.Y
                  AssetTag = msdfFont
                  RenderOperation2d = RenderMsdfGlyphGrid descriptor }
                world

[<RequireQualifiedAccess>]
module DreamView =

    type private ViewIndex =
        { CameraCells : HashSet<Location>
          TurretCells : HashSet<Location>
          EnemyCells : HashSet<Location>
          Pickups : Dictionary<Location, Pickup>
          Projectiles : HashSet<Location>
          Blocks : HashSet<Location>
          Mirrors : Dictionary<Location, Mirror>
          Cameras : HashSet<Location>
          Turrets : HashSet<Location>
          Enemies : HashSet<Location> }

    let private viewIndex (run : DreamRun) =
        let cameraCells = HashSet<Location> ()
        let turretCells = HashSet<Location> ()
        let enemyCells = HashSet<Location> ()
        let pickups = Dictionary<Location, Pickup> ()
        let projectiles = HashSet<Location> ()
        let blocks = HashSet<Location> ()
        let mirrors = Dictionary<Location, Mirror> ()
        let cameras = HashSet<Location> ()
        let turrets = HashSet<Location> ()
        let enemies = HashSet<Location> ()
        for KeyValue (_, camera) in run.Cameras do
            for location in DreamSensors.cells run camera.Location camera.Facing camera.Range do
                cameraCells.Add location |> ignore
            cameras.Add camera.Location |> ignore
        for KeyValue (_, turret) in run.Turrets do
            for location in DreamSensors.cells run turret.Location turret.Facing turret.Range do
                turretCells.Add location |> ignore
            turrets.Add turret.Location |> ignore
        for KeyValue (_, pickup) in run.Pickups do
            pickups[pickup.Location] <- pickup
        for projectile in run.Projectiles do
            projectiles.Add projectile.Location |> ignore
        for KeyValue (_, block) in run.Blocks do
            blocks.Add block.Location |> ignore
        for KeyValue (_, mirror) in run.Mirrors do
            mirrors[mirror.Location] <- mirror
        for KeyValue (_, enemy) in run.Enemies do
            for location in DreamSensors.cells run enemy.Location enemy.Facing enemy.VisionRange do
                enemyCells.Add location |> ignore
            enemies.Add enemy.Location |> ignore
        { CameraCells = cameraCells
          TurretCells = turretCells
          EnemyCells = enemyCells
          Pickups = pickups
          Projectiles = projectiles
          Blocks = blocks
          Mirrors = mirrors
          Cameras = cameras
          Turrets = turrets
          Enemies = enemies }

    let private blankGlyph =
        { Text = " "
          Color = DreamPalette.Background }

    let private dimByte scalar value =
        byte (max 0 (min 255 (int (single value * scalar))))

    let private dimColor (color : Color) =
        Color (dimByte 0.38f color.R8, dimByte 0.38f color.G8, dimByte 0.38f color.B8, color.A8)

    let private dimGlyph (glyph : MsdfGlyphValue) : MsdfGlyphValue =
        { glyph with Color = dimColor glyph.Color }

    let private inGrid width height (cell : Cell) =
        cell.X >= 0 && cell.X < width && cell.Y >= 0 && cell.Y < height

    let private tryEdgeDirection (section : DreamSection) (cell : Cell) =
        if cell.X = 0 then Some West
        elif cell.X = section.Width - 1 then Some East
        elif cell.Y = 0 then Some North
        elif cell.Y = section.Height - 1 then Some South
        else None

    let private projectedSize rotation (section : DreamSection) =
        if DreamProjection.normalizeRotation rotation % 2 = 0 then section.Width, section.Height
        else section.Height, section.Width

    let private rotatedCellOffset rotation (section : DreamSection) (cell : Cell) =
        match DreamProjection.normalizeRotation rotation with
        | 0 -> { cell with Z = 0 }
        | 1 -> { X = section.Height - 1 - cell.Y; Y = cell.X; Z = 0 }
        | 2 -> { X = section.Width - 1 - cell.X; Y = section.Height - 1 - cell.Y; Z = 0 }
        | _ -> { X = cell.Y; Y = section.Width - 1 - cell.X; Z = 0 }

    let private projectedCell (section : DreamSection) (instance : DreamProjectionInstance) (cell : Cell) =
        let offset = rotatedCellOffset instance.Rotation section cell
        { X = instance.Origin.X + offset.X
          Y = instance.Origin.Y + offset.Y
          Z = 0 }

    let private sectionIntersectsGrid width height (origin : Cell) rotation (section : DreamSection) =
        let projectedWidth, projectedHeight = projectedSize rotation section
        origin.X < width &&
        origin.X + projectedWidth > 0 &&
        origin.Y < height &&
        origin.Y + projectedHeight > 0

    let private instanceKey sectionId (origin : Cell) rotation =
        sprintf "%s:%i:%i:%i:%i" sectionId origin.X origin.Y origin.Z (DreamProjection.normalizeRotation rotation)

    let private projectionId sectionId origin rotation =
        instanceKey sectionId origin rotation

    let private rootInstanceOrigin width height (run : DreamRun) =
        Map.tryFind run.Player.SectionId run.SectionOrigins
        |> Option.defaultValue
            { X = width / 2 - run.Player.Cell.X
              Y = height / 2 - run.Player.Cell.Y
              Z = 0 }

    let private tryDestinationRotation (sourceSide : Direction) (destinationSide : Direction) =
        [0; 1; 2; 3]
        |> List.tryFind (fun rotation ->
            DreamProjection.rotateDirection rotation destinationSide = sourceSide.Opposite)

    let private tryLinkMount (run : DreamRun) (sourceInstance : DreamProjectionInstance) (link : Link) =
        match Map.tryFind link.From.SectionId run.Sections, Map.tryFind link.Destination.SectionId run.Sections with
        | Some sourceSection, Some destinationSection ->
            match tryEdgeDirection sourceSection link.From.Cell, tryEdgeDirection destinationSection link.Destination.Cell with
            | Some sourceSide, Some destinationSide ->
                let sourceSide = DreamProjection.rotateDirection sourceInstance.Rotation sourceSide
                match tryDestinationRotation sourceSide destinationSide with
                | Some destinationRotation ->
                    let sourceCell = projectedCell sourceSection sourceInstance link.From.Cell
                    let dx, dy = sourceSide.Delta
                    let destinationCell = { X = sourceCell.X + dx; Y = sourceCell.Y + dy; Z = 0 }
                    let destinationOffset = rotatedCellOffset destinationRotation destinationSection link.Destination.Cell
                    Some
                        (destinationSection,
                         { X = destinationCell.X - destinationOffset.X
                           Y = destinationCell.Y - destinationOffset.Y
                           Z = 0 },
                         destinationRotation)
                | None -> None
            | _ -> None
        | _ -> None

    let private compressedInstances (run : DreamRun) =
        run.Sections
        |> Map.toSeq
        |> Seq.choose (fun (sectionId, _) ->
            Map.tryFind sectionId run.SectionOrigins
            |> Option.map (fun origin ->
                { ProjectionId = sectionId
                  SectionId = sectionId
                  Origin = origin
                  Rotation = 0
                  Depth = 0 }))
        |> Seq.toList

    let initialUnfoldedProjection (run : DreamRun) =
        let width = DreamGrid.DefaultWidth
        let height = DreamGrid.DefaultHeight
        let origin = rootInstanceOrigin width height run
        let rotation = 0
        let projectionId = projectionId run.Player.SectionId origin rotation
        let instance =
            { ProjectionId = projectionId
              SectionId = run.Player.SectionId
              Origin = origin
              Rotation = rotation
              Depth = 0 }
        Map.add projectionId instance Map.empty, projectionId

    let ensureCurrentUnfoldedProjection (run : DreamRun) (instances : Map<string, DreamProjectionInstance>) currentProjectionId =
        match Map.tryFind currentProjectionId instances with
        | Some instance when instance.SectionId = run.Player.SectionId ->
            instances, currentProjectionId
        | _ ->
            let newInstances, currentProjectionId = initialUnfoldedProjection run
            let currentInstance = Map.find currentProjectionId newInstances
            Map.add currentProjectionId currentInstance instances, currentProjectionId

    let advanceUnfoldedProjection direction (oldRun : DreamRun) (newRun : DreamRun) instances currentProjectionId =
        let instances, currentProjectionId = ensureCurrentUnfoldedProjection oldRun instances currentProjectionId
        match DreamMap.tryExitLink oldRun oldRun.Player direction, Map.tryFind currentProjectionId instances with
        | Some destination, Some currentInstance when newRun.Player = destination ->
            match DreamMap.tryLinkAt oldRun oldRun.Player with
            | Some link ->
                match tryLinkMount oldRun currentInstance link with
                | Some (_, destinationOrigin, destinationRotation) ->
                    let destinationProjectionId = projectionId link.Destination.SectionId destinationOrigin destinationRotation
                    let destinationInstance =
                        { ProjectionId = destinationProjectionId
                          SectionId = link.Destination.SectionId
                          Origin = destinationOrigin
                          Rotation = destinationRotation
                          Depth = currentInstance.Depth + 1 }
                    if Map.containsKey destinationProjectionId instances then
                        instances, destinationProjectionId
                    elif Map.count instances < DreamGrid.UnfoldedMaxInstances then
                        Map.add destinationProjectionId destinationInstance instances, destinationProjectionId
                    else
                        instances, currentProjectionId
                | None -> ensureCurrentUnfoldedProjection newRun instances currentProjectionId
            | None -> instances, currentProjectionId
        | _ -> instances, currentProjectionId

    let private discoveredUnfoldedInstances width height (instances : Map<string, DreamProjectionInstance>) (run : DreamRun) =
        instances
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.filter (fun instance ->
            match Map.tryFind instance.SectionId run.Sections with
            | Some section -> sectionIntersectsGrid width height instance.Origin instance.Rotation section
            | None -> false)
        |> Seq.toList

    let private projectionInstances projectionMode discoveredInstances width height (run : DreamRun) =
        match projectionMode with
        | DreamProjectionMode.Compressed -> compressedInstances run
        | DreamProjectionMode.Unfolded -> discoveredUnfoldedInstances width height discoveredInstances run

    let private tryGlobalCell (run : DreamRun) (location : Location) =
        match Map.tryFind location.SectionId run.Sections, Map.tryFind location.SectionId run.SectionOrigins with
        | Some section, Some origin when DreamMap.containsCell section location.Cell ->
            Some
                { X = origin.X + location.Cell.X
                  Y = origin.Y + location.Cell.Y
                  Z = 0 }
        | _ -> None

    let private tryOutsideCell (run : DreamRun) (location : Location) =
        match Map.tryFind location.SectionId run.Sections, Map.tryFind location.SectionId run.SectionOrigins with
        | Some section, Some origin when DreamMap.containsCell section location.Cell ->
            let globalCell =
                { X = origin.X + location.Cell.X
                  Y = origin.Y + location.Cell.Y
                  Z = 0 }
            if location.Cell.X = 0 then Some { globalCell with X = globalCell.X - 1 }
            elif location.Cell.X = section.Width - 1 then Some { globalCell with X = globalCell.X + 1 }
            elif location.Cell.Y = 0 then Some { globalCell with Y = globalCell.Y - 1 }
            elif location.Cell.Y = section.Height - 1 then Some { globalCell with Y = globalCell.Y + 1 }
            else None
        | _ -> None

    let private rangeBetween first last =
        if first <= last then [first .. last]
        else [first .. -1 .. last]

    let private linePoints start finish =
        if start.X = finish.X then
            rangeBetween start.Y finish.Y
            |> List.map (fun y -> { X = start.X; Y = y; Z = 0 })
        elif start.Y = finish.Y then
            rangeBetween start.X finish.X
            |> List.map (fun x -> { X = x; Y = start.Y; Z = 0 })
        else []

    let private routePoints start finish =
        if start = finish then [start]
        else
            let pivot = { X = finish.X; Y = start.Y; Z = 0 }
            let first = linePoints start pivot
            let second = linePoints pivot finish
            match second with
            | _ :: rest -> first @ rest
            | [] -> first

    let private neighborCells cell =
        [ { cell with X = cell.X + 1 }
          { cell with X = cell.X - 1 }
          { cell with Y = cell.Y + 1 }
          { cell with Y = cell.Y - 1 } ]

    let private canRoutePath width height (glyphs : MsdfGlyphValue array) start finish cell =
        inGrid width height cell &&
        (cell = start || cell = finish || glyphs[cell.Y * width + cell.X].Text = " ")

    let private reconstructRoute (parents : Dictionary<Cell, Cell>) start finish =
        let mutable current = finish
        let mutable route = [finish]
        while current <> start do
            current <- parents[current]
            route <- current :: route
        route

    let private tryRoutePoints width height glyphs start finish =
        let visited = HashSet<Cell> ()
        let parents = Dictionary<Cell, Cell> ()
        let queue = Queue<Cell> ()
        visited.Add start |> ignore
        queue.Enqueue start
        let mutable found = start = finish
        while queue.Count > 0 && not found do
            let current = queue.Dequeue ()
            for neighbor in neighborCells current do
                if canRoutePath width height glyphs start finish neighbor && visited.Add neighbor then
                    parents[neighbor] <- current
                    if neighbor = finish then found <- true
                    queue.Enqueue neighbor
        if found then Some (reconstructRoute parents start finish)
        else None

    let private pathGlyph previousOpt point nextOpt =
        let directionTo (other : Cell) =
            Math.Sign (other.X - point.X), Math.Sign (other.Y - point.Y)
        let directions =
            [ match previousOpt with
              | Some previous -> yield directionTo previous
              | None -> ()
              match nextOpt with
              | Some next -> yield directionTo next
              | None -> () ]
        let has direction = List.contains direction directions
        let north = has (0, -1)
        let east = has (1, 0)
        let south = has (0, 1)
        let west = has (-1, 0)
        if (east || west) && not (north || south) then "─"
        elif (north || south) && not (east || west) then "│"
        elif north && east then "└"
        elif north && west then "┘"
        elif south && east then "┌"
        elif south && west then "┐"
        else "•"

    let private trySetPathGlyph width height (glyphs : MsdfGlyphValue array) cell glyph color =
        if inGrid width height cell then
            let index = cell.Y * width + cell.X
            if glyphs[index].Text = " " then
                glyphs[index] <- { Text = glyph; Color = color }

    let private drawLinkPath width height glyphs run link color =
        match tryGlobalCell run link.From, tryGlobalCell run link.Destination with
        | Some source, Some destination when abs (source.X - destination.X) + abs (source.Y - destination.Y) > 1 ->
            let start = tryOutsideCell run link.From |> Option.defaultValue source
            let finish = tryOutsideCell run link.Destination |> Option.defaultValue destination
            let points = tryRoutePoints width height glyphs start finish |> Option.defaultWith (fun _ -> routePoints start finish)
            points
            |> List.iteri (fun index point ->
                let previousOpt = if index > 0 then Some points[index - 1] else Some source
                let nextOpt = if index + 1 < points.Length then Some points[index + 1] else Some destination
                trySetPathGlyph width height glyphs point (pathGlyph previousOpt point nextOpt) color)
        | _ -> ()

    let private terrainGlyphColor (run : DreamRun) (location : Location) =
        let baseTerrain = DreamMap.terrainAt run location
        let upper =
            match DreamMap.tryGetSection run location.SectionId with
            | Some section ->
                match DreamMap.terrainAtLayer 1 section location.Cell with
                | Upper glyph -> Some glyph
                | _ -> None
            | None -> None
        match upper with
        | Some glyph when baseTerrain = Floor -> string glyph, DreamPalette.Upper
        | _ -> DreamMap.terrainGlyph baseTerrain, DreamMap.terrainColor baseTerrain

    let private pickupColor (pickup : Pickup) =
        match pickup.Kind with
        | Ammo -> DreamPalette.Ammo
        | Med -> DreamPalette.Med
        | CellBattery -> DreamPalette.Focus

    let private mirrorGlyph rotation (mirror : Mirror) =
        if DreamProjection.normalizeRotation rotation % 2 = 0 then mirror.Kind.Glyph
        else mirror.Kind.Toggle.Glyph

    let private cellGlyphColor index (run : DreamRun) rotation location =
        let baseGlyph, baseColor =
            if index.TurretCells.Contains location then DreamGlyph.TurretCone, DreamPalette.TurretCone
            elif index.CameraCells.Contains location then DreamGlyph.CameraCone, DreamPalette.CameraCone
            elif index.EnemyCells.Contains location then DreamGlyph.EnemyCone, DreamPalette.EnemyCone
            else terrainGlyphColor run location
        match index.Pickups.TryGetValue location with
        | true, pickup -> pickup.Kind.Glyph, pickupColor pickup
        | _ ->
            if index.Projectiles.Contains location then DreamGlyph.Projectile, DreamPalette.Projectile
            elif index.Blocks.Contains location then DreamGlyph.Block, DreamPalette.Block
            else
                match index.Mirrors.TryGetValue location with
                | true, mirror -> mirrorGlyph rotation mirror, DreamPalette.Mirror
                | _ ->
                    if index.Cameras.Contains location then DreamGlyph.Camera, DreamPalette.Camera
                    elif index.Turrets.Contains location then DreamGlyph.Turret, DreamPalette.Turret
                    elif index.Enemies.Contains location then DreamGlyph.Enemy, DreamPalette.Enemy
                    elif location = run.Player then (DreamProjection.rotateDirection rotation run.Facing).Glyph, DreamPalette.Player
                    elif Option.isNone run.HeldBlock && location.SectionId = run.Player.SectionId && location.Cell = run.Reticle then DreamGlyph.Reticle, DreamPalette.Reticle
                    else baseGlyph, baseColor

    let private memoryGlyphColor index (run : DreamRun) rotation location =
        let baseGlyph, baseColor =
            if index.TurretCells.Contains location then DreamGlyph.TurretCone, DreamPalette.TurretCone
            elif index.CameraCells.Contains location then DreamGlyph.CameraCone, DreamPalette.CameraCone
            elif index.EnemyCells.Contains location then DreamGlyph.EnemyCone, DreamPalette.EnemyCone
            else terrainGlyphColor run location
        match index.Pickups.TryGetValue location with
        | true, pickup -> pickup.Kind.Glyph, pickupColor pickup
        | _ ->
            if index.Blocks.Contains location then DreamGlyph.Block, DreamPalette.Block
            else
                match index.Mirrors.TryGetValue location with
                | true, mirror -> mirrorGlyph rotation mirror, DreamPalette.Mirror
                | _ ->
                    if index.Cameras.Contains location then DreamGlyph.Camera, DreamPalette.Camera
                    elif index.Turrets.Contains location then DreamGlyph.Turret, DreamPalette.Turret
                    elif index.Enemies.Contains location then DreamGlyph.Enemy, DreamPalette.Enemy
                    else baseGlyph, baseColor

    let refreshFog (run : DreamRun) =
        let index = viewIndex run
        let visibleCells = DreamSensors.playerCells run DreamGrid.PlayerVisionRange
        let rememberedCells =
            visibleCells
            |> Set.fold (fun remembered location ->
                let glyph, color = memoryGlyphColor index run 0 location
                Map.add location { Text = glyph; Color = color } remembered)
                run.RememberedCells
        { run with
            VisibleCells = visibleCells
            RememberedCells = rememberedCells }

    let refreshUnfoldedFog currentProjectionId currentRotation (run : DreamRun) (rememberedCells : Map<DreamProjectedCell, MsdfGlyphValue>) =
        let index = viewIndex run
        let visibleCells =
            DreamSensors.playerCells run DreamGrid.PlayerVisionRange
            |> Set.map (fun location ->
                { ProjectionId = currentProjectionId
                  Location = location })
        let rememberedCells =
            visibleCells
            |> Set.fold (fun remembered projectedCell ->
                let glyph, color = memoryGlyphColor index run currentRotation projectedCell.Location
                Map.add projectedCell { Text = glyph; Color = color } remembered)
                rememberedCells
        visibleCells, rememberedCells

    let mergeUnfoldedKnowledge (rememberedCells : Map<DreamProjectedCell, MsdfGlyphValue>) (run : DreamRun) =
        let rememberedCells =
            rememberedCells
            |> Map.fold (fun canonicalRemembered projectedCell glyph ->
                Map.add projectedCell.Location glyph canonicalRemembered)
                run.RememberedCells
        { run with RememberedCells = rememberedCells }

    let private actionText (run : DreamRun) =
        match run.HeldBlock with
        | Some held -> "GRIP " + held.BlockId
        | None -> "READY"

    let private hudLines (projectionMode : DreamProjectionMode) (run : DreamRun) =
        let sectionName = run.Player.SectionId.ToUpperInvariant ()
        [$"╔ PSYCHOCRAWL // %s{sectionName} ══════════════════════════════════════╗"
         $"║ HP %03i{run.Resources.Health}  FOCUS %03i{run.Resources.Focus}  AMMO %03i{run.Resources.Ammo}  DEBT %02i{run.Resources.AlarmDebt}  WPN %s{run.Weapon.Name}  %s{actionText run}  VIEW %s{projectionMode.Name} ║"
         $"║ WASD move  Arrows aim/turn  Space fire  G grab/release  1/2 guns  Tab view ║"
         yield! run.Log |> List.map (sprintf "║ %s" )]

    let private tryLocationGlyph index (run : DreamRun) location =
        if Set.contains location run.VisibleCells then
            let text, color = cellGlyphColor index run 0 location
            Some { Text = text; Color = color }
        else
            Map.tryFind location run.RememberedCells
            |> Option.map dimGlyph

    let private tryProjectedLocationGlyph index (run : DreamRun) visibleCells rememberedCells (instance : DreamProjectionInstance) location =
        let projectedCell =
            { ProjectionId = projectionId instance.SectionId instance.Origin instance.Rotation
              Location = location }
        if Set.contains projectedCell visibleCells then
            let text, color = cellGlyphColor index run instance.Rotation location
            Some { Text = text; Color = color }
        else
            Map.tryFind projectedCell rememberedCells
            |> Option.map dimGlyph

    let private instanceRenderPriority projectionMode currentProjectionId (instance : DreamProjectionInstance) =
        match projectionMode with
        | DreamProjectionMode.Unfolded when instance.ProjectionId = currentProjectionId -> -1
        | _ -> instance.Depth

    let private drawSectionInstance (run : DreamRun) tryGlyph priority width height (glyphs : MsdfGlyphValue array) (priorities : int array) (instance : DreamProjectionInstance) =
        match Map.tryFind instance.SectionId run.Sections with
        | Some section ->
            for y in 0 .. section.Height - 1 do
                for x in 0 .. section.Width - 1 do
                    let localCell = { X = x; Y = y; Z = 0 }
                    let globalCell = projectedCell section instance localCell
                    if inGrid width height globalCell then
                        let location = { SectionId = instance.SectionId; Cell = localCell }
                        match tryGlyph instance location with
                        | Some glyph ->
                            let glyphIndex = globalCell.Y * width + globalCell.X
                            if priority < priorities[glyphIndex] then
                                priorities[glyphIndex] <- priority
                                glyphs[glyphIndex] <- glyph
                        | None -> ()
        | None -> ()

    let private glyphGrid projectionMode currentProjectionId discoveredInstances visibleCells rememberedCells (run : DreamRun) =
        let width = DreamGrid.DefaultWidth
        let height = DreamGrid.DefaultHeight
        let index = viewIndex run
        let glyphs = Array.create (width * height) blankGlyph
        let priorities = Array.create (width * height) Int32.MaxValue
        let instances = projectionInstances projectionMode discoveredInstances width height run
        let tryGlyph =
            match projectionMode with
            | DreamProjectionMode.Compressed ->
                fun _ location -> tryLocationGlyph index run location
            | DreamProjectionMode.Unfolded ->
                fun instance location -> tryProjectedLocationGlyph index run visibleCells rememberedCells instance location
        for instance in instances do
            let priority = instanceRenderPriority projectionMode currentProjectionId instance
            drawSectionInstance run tryGlyph priority width height glyphs priorities instance
        if projectionMode = DreamProjectionMode.Compressed then
            let drawnLinks = HashSet<string> ()
            for KeyValue (_, section) in run.Sections do
                for link in section.Links do
                    let fromKey = sprintf "%s:%i:%i:%i" link.From.SectionId link.From.Cell.X link.From.Cell.Y link.From.Cell.Z
                    let destinationKey = sprintf "%s:%i:%i:%i" link.Destination.SectionId link.Destination.Cell.X link.Destination.Cell.Y link.Destination.Cell.Z
                    let key =
                        if String.CompareOrdinal (fromKey, destinationKey) <= 0 then fromKey + "|" + destinationKey
                        else destinationKey + "|" + fromKey
                    if drawnLinks.Add key then
                        if Set.contains link.From run.VisibleCells || Set.contains link.Destination run.VisibleCells then
                            drawLinkPath width height glyphs run link DreamPalette.Portal
                        elif Map.containsKey link.From run.RememberedCells || Map.containsKey link.Destination run.RememberedCells then
                            drawLinkPath width height glyphs run link (dimColor DreamPalette.Portal)
        { Width = width; Height = height; Glyphs = glyphs }

    let grid (projectionMode : DreamProjectionMode) currentProjectionId discoveredInstances visibleCells rememberedCells run =
        let grid = glyphGrid projectionMode currentProjectionId discoveredInstances visibleCells rememberedCells run
        Content.entity<DreamGlyphGridDispatcher> "GlyphGrid"
            [Entity.ModelGeneric<DreamGlyphGrid> () := grid
             Entity.Position := DreamGrid.gridPosition grid.Width grid.Height
             Entity.Size := DreamGrid.gridSize grid.Width grid.Height
             Entity.Elevation == 10.0f]

    let hud (projectionMode : DreamProjectionMode) (run : DreamRun) =
        hudLines projectionMode run
        |> List.mapi (fun index line ->
            Content.msdfText ("Hud+" + string index)
                [Entity.Position == v3 0.0f (DreamGrid.HudTop - single index * DreamGrid.HudLineHeight) 0.0f
                 Entity.Size == v3 DreamGrid.HudWidth DreamGrid.HudLineHeight 0.0f
                 Entity.Elevation == 20.0f
                 Entity.Text := line
                 Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                 Entity.FontSizing := Some DreamGrid.HudFontSize
                 Entity.TextColor := if index < 2 then DreamPalette.Hud elif index = 2 then DreamPalette.Muted else DreamPalette.Alert])

[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

[<RequireQualifiedAccess>]
module DreamGameplay =

    let initial () =
        let run = DreamContent.loadRun () |> DreamView.refreshFog
        let unfoldedInstances, currentProjectionId = DreamView.initialUnfoldedProjection run
        let currentRotation = unfoldedInstances[currentProjectionId].Rotation
        let visibleCells, rememberedCells = DreamView.refreshUnfoldedFog currentProjectionId currentRotation run Map.empty
        { Run = run
          CommandQueue = []
          ProjectionMode = DreamProjectionMode.Unfolded
          CurrentProjectionId = currentProjectionId
          UnfoldedInstances = unfoldedInstances
          UnfoldedVisibleCells = visibleCells
          UnfoldedRememberedCells = rememberedCells }

    let private refreshFog gameplay =
        let run = DreamView.refreshFog gameplay.Run
        match gameplay.ProjectionMode with
        | DreamProjectionMode.Unfolded ->
            let unfoldedInstances, currentProjectionId =
                DreamView.ensureCurrentUnfoldedProjection run gameplay.UnfoldedInstances gameplay.CurrentProjectionId
            let currentRotation = unfoldedInstances[currentProjectionId].Rotation
            let visibleCells, rememberedCells = DreamView.refreshUnfoldedFog currentProjectionId currentRotation run gameplay.UnfoldedRememberedCells
            { gameplay with
                Run = run
                CurrentProjectionId = currentProjectionId
                UnfoldedInstances = unfoldedInstances
                UnfoldedVisibleCells = visibleCells
                UnfoldedRememberedCells = rememberedCells }
        | DreamProjectionMode.Compressed ->
            { gameplay with
                Run = run
                UnfoldedVisibleCells = Set.empty }

    let private currentRotation gameplay =
        match gameplay.ProjectionMode, Map.tryFind gameplay.CurrentProjectionId gameplay.UnfoldedInstances with
        | DreamProjectionMode.Unfolded, Some instance -> instance.Rotation
        | _ -> 0

    let private screenToCanonicalDirection gameplay direction =
        DreamProjection.inverseRotateDirection (currentRotation gameplay) direction

    let private toggleProjection gameplay =
        match gameplay.ProjectionMode with
        | DreamProjectionMode.Unfolded ->
            let run =
                gameplay.Run
                |> DreamView.mergeUnfoldedKnowledge gameplay.UnfoldedRememberedCells
                |> DreamView.refreshFog
            { gameplay with
                Run = run
                ProjectionMode = DreamProjectionMode.Compressed
                UnfoldedVisibleCells = Set.empty }
        | DreamProjectionMode.Compressed ->
            { gameplay with ProjectionMode = DreamProjectionMode.Unfolded } |> refreshFog

    let private enqueue command gameplay =
        { gameplay with CommandQueue = gameplay.CommandQueue @ [command] }

    let private commandUsesHeldMoveDelay gameplay command =
        match gameplay.Run.HeldBlock, command with
        | Some _, MoveQueued _ -> true
        | _ -> false

    let private tickOnce gameplay =
        let commandOpt = List.tryHead gameplay.CommandQueue
        let oldRun = gameplay.Run
        let oldProjectionRotation = currentRotation gameplay
        let run, consumed = DreamSim.tick commandOpt oldRun
        let commandQueue =
            if consumed then
                match gameplay.CommandQueue with
                | _ :: rest -> rest
                | [] -> []
            else gameplay.CommandQueue
        let gameplay = { gameplay with Run = run; CommandQueue = commandQueue }
        let gameplay =
            match gameplay.ProjectionMode, commandOpt with
            | DreamProjectionMode.Unfolded, Some (MoveQueued direction) when run.Player <> oldRun.Player ->
                let unfoldedInstances, currentProjectionId =
                    DreamView.advanceUnfoldedProjection direction oldRun run gameplay.UnfoldedInstances gameplay.CurrentProjectionId
                let newProjectionRotation =
                    Map.tryFind currentProjectionId unfoldedInstances
                    |> Option.map (fun instance -> instance.Rotation)
                    |> Option.defaultValue oldProjectionRotation
                let run =
                    if newProjectionRotation = oldProjectionRotation then run
                    else DreamSim.rotatePlayerProjection oldProjectionRotation newProjectionRotation run
                { gameplay with
                    Run = run
                    CurrentProjectionId = currentProjectionId
                    UnfoldedInstances = unfoldedInstances }
            | _ -> gameplay
        gameplay |> refreshFog

    let applyKeyPress (data : KeyboardKeyData) gameplay =
        if data.Repeated then gameplay
        elif DreamInput.togglesProjection data.KeyboardKey then
            toggleProjection gameplay
        else
            match DreamInput.reticleDirectionOfKey data.KeyboardKey with
            | Some direction ->
                let direction = screenToCanonicalDirection gameplay direction
                { gameplay with Run = DreamSim.rotateHeld direction gameplay.Run } |> refreshFog
            | None ->
                match DreamInput.commandOfKey data.KeyboardKey with
                | Some GrabToggleQueued ->
                    { gameplay with
                        Run = DreamSim.toggleGrab gameplay.Run
                        CommandQueue = [] }
                    |> refreshFog
                | Some (MoveQueued direction) ->
                    let command = MoveQueued (screenToCanonicalDirection gameplay direction)
                    let gameplay = enqueue command gameplay
                    if commandUsesHeldMoveDelay gameplay command then gameplay else tickOnce gameplay
                | Some command ->
                    let gameplay = enqueue command gameplay
                    tickOnce gameplay
                | None ->
                    match DreamInput.weaponOfKey data.KeyboardKey with
                    | Some weapon ->
                        { gameplay with Run = { gameplay.Run with Weapon = weapon } } |> refreshFog
                    | None -> gameplay

    let update (world : World) gameplay =
        let tickDivisor =
            match gameplay.Run.HeldBlock, List.tryHead gameplay.CommandQueue with
            | Some _, Some (MoveQueued _) -> DreamGrid.HeldMoveTickDivisor
            | _ -> DreamGrid.TickDivisor
        if world.Advancing && world.UpdateTime % tickDivisor = 0L
        then tickOnce gameplay
        else gameplay

type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (DreamGameplay.initial ())

    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world then DreamGameplay.initial ()
        else DreamGameplay.initial ()

    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.UpdateEvent => Update
         Game.KeyboardKeyDownEvent =|> fun evt -> KeyPressed evt.Data]

    override this.Message (gameplay, message, _, world) =
        match message with
        | StartPlaying ->
            just (DreamGameplay.initial ())
        | KeyPressed data ->
            just (DreamGameplay.applyKeyPress data gameplay)
        | Update ->
            just (DreamGameplay.update world gameplay)

    override this.Command (_, command, _, _) =
        match command with
        | Noop -> ()

    override this.Content (gameplay, _) =
        let projectionMode = gameplay.ProjectionMode
        let run = gameplay.Run
        [Content.group Simulants.GameplayScene.Name []
            [DreamView.grid projectionMode gameplay.CurrentProjectionId gameplay.UnfoldedInstances gameplay.UnfoldedVisibleCells gameplay.UnfoldedRememberedCells run
             yield! DreamView.hud projectionMode run]]
