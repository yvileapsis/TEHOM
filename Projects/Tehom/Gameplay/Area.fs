namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open Graph

module Area =

    type SiteType =
        | Ground
        | NotGround

    type Site = {
        ID : String
        Label : String
        Position : Option<Vector3>
        Type : SiteType
    }
    with
        static member empty = {
            ID = ""
            Label = ""
            Position = None
            Type = NotGround
        }

        static member make id label =
            { Site.empty with ID = id; Label = label }

        static member makeGround id =
            { Site.empty with ID = id; Type = Ground }

    type Relationship =
        | Distance of Distance: UInt32
        | Consists
        | Contains
        | LiesAbove
        | Covers
        | IsOnEdge

    type Sites = Graph<String, Site, Relationship>
    type SitesDisplay = Graph<String, Vector3, UInt32>

    type Area = {
        Name : String
        Sites : Sites

        DistancesCache : Map<String * String, UInt32>

        Force : Single
        DistanceMult : Single
        ZeroDistanceForce : Single

    }
    with

        static member find finder (area : Area) =
            area.Sites
            |> Vertices.toList
            |> List.tryFind finder
            |> function | Some v -> Some (fst v) | None -> None

        static member getAsNavigation graph =
            graph
            |> Edges.undirect2 (fun _ _ edge ->
                match edge with
                | Contains -> true
                | LiesAbove -> true
                | Covers -> true
                | IsOnEdge -> true
                | _ -> false
            ) (fun edge1 _ -> edge1)
            |> Edges.choose (fun _ _ edge ->
                match edge with
                | Distance x -> Some (uint x)
                | Consists -> None
                | Contains -> Some 0u
                | LiesAbove -> Some 0u
                | Covers -> Some 0u
                | IsOnEdge -> Some 0u
            )

        static member getAsDisplay graph =
            graph
            |> Edges.undirect (fun edge1 _ -> edge1)
            |> Edges.choose (fun _ _ edge ->
                match edge with
                | Distance x -> Some (uint x)
                | Consists -> None
                | Contains -> Some 150u
                | LiesAbove -> Some 150u
                | Covers -> Some 150u
                | IsOnEdge -> Some 50u
            )
            |> Vertices.choose (fun _ l -> l.Position)

        static member findPath fromDestination toDestination area =
            Area.getAsNavigation area.Sites
            |> Query.sp2 fromDestination toDestination

        static member findReachableSitePath (reach: uint32) path =
            match path with
            | [] -> []
            | path ->
                let _, fullDistance = List.last path
                List.takeTillInclusive (fun (_, distance) -> fullDistance - distance <= reach) path

        static member moveWithinReach actor target reach speed area =

            let path = Area.findPath actor target area
            let path = Area.findReachableSitePath reach path

            match path with
            | _::_ ->

                let index =
                    List.findIndexBack (fun (_, distance) -> distance <= speed) path

                let (targetInter, distanceInter) =
                    List.item index path

                let moveInter =
                    if targetInter <> actor then
                        Some (actor, targetInter)
                    else
                        None

                let moveIntra =
                    let speedIntra = max 0u (speed - distanceInter)
                    if speedIntra > 0u then
                        match List.tryItem (index + 1) path with
                        | Some (targetIntra, distanceIntra) ->
                            Some (actor, targetIntra, distanceIntra - speedIntra)
                        | None ->
                            None
                    else
                        None

                moveInter, moveIntra
            | _ ->
                printf $"err: {actor} {target} {speed} {area} {path}"
                None, None

        static member moveSite site toDestination area =
            // resets all relations, maybe this isn't the best
            let sites =
                match Graph.tryDecompose site area.Sites with
                | Some (p, v, l, s), sites' ->
                    sites'
                    |> Vertices.add (site, Site.empty)
                    |> Directed.Edges.add (toDestination, site, LiesAbove)
                | None, sites' ->
                    sites'
            { area with Sites = sites }

        static member establishDistance distance site toDestination area =
            let sites = area.Sites

            let sites =
                match Undirected.Edges.tryFind site toDestination sites with
                | Some _ ->
                    sites
                    |> Undirected.Edges.remove (site, toDestination)
                | None ->
                    sites

            let sites =
                sites
                |> Undirected.Edges.add (site, toDestination, Distance distance)

            { area with Sites = sites }

        static member getConnections site area =
            area.Sites
            |> Graph.tryDecompose site
            |> fun (c, _) ->
                match c with
                | Some (p, v, l, s) -> $"{p} {v} {l} {s}"
                | None -> String.empty

        static member getWithinReach site reach area =

            let graph' = Area.getAsNavigation area.Sites

            Query.spTree site graph'
            |> List.concat
            |> List.filter (fun (_, distance) -> distance < reach)
            |> List.distinct

        static member iterateDisplay iterations area =

            let graph =
                area.Sites
                |> Vertices.map (fun v l ->
                    match l.Position with
                    | Some _ ->
                        l
                    | None ->

                        let list =
                            area.Sites
                            |> Vertices.toList
                            |> List.map fst
                            |> List.sort

                        let i = List.findIndex ((=) v) list
                        let x = float32 (i % 6) * 16f - 3f * 16f
                        let y = float32 (i / 6) * 16f - 3f * 16f
                        { l with Position = Some (v3 x y 0f) }
                )
                |> Area.getAsDisplay

            let nodeDistances =
                let vertices =
                    graph
                    |> Vertices.toList
                    |> List.map fst

                let vertices' =
                    area.DistancesCache
                    |> Map.toList
                    |> List.map (fst >> fst)

                if (Map.notEmpty area.DistancesCache) && (Set.ofList vertices = Set.ofList vertices') then
                    area.DistancesCache
                else
                    vertices
                    |> List.collect (fun v ->
                        let tree = Query.spTree v graph

                        vertices
                        |> List.choose (fun v' ->
                            match Query.getDistance v' tree with
                            | Some distance -> Some ((v, v'), distance)
                            | None -> None
                        )
                    )
                    |> Map.ofList

            let attraction (graph : SitesDisplay) =
                graph
                |> Vertices.map (fun v l ->

                    graph
                    |> Vertices.fold l (fun l v' l' ->

                        match Map.tryFind (v, v') nodeDistances with
                        | Some targetDistance ->

                            let targetDistance = area.DistanceMult * float32 targetDistance
                            let delta = l - l'
                            let distance = float32 (delta.Length ()) + 0.01f
                            let deltaNorm = delta / distance
                            let distance' = distance - targetDistance
                            let force = distance' * (if targetDistance = 0f then area.ZeroDistanceForce else area.Force / targetDistance)
                            l - deltaNorm * force

                        | None ->

                            let targetDistance = 20f
                            let delta = l - l'
                            let distance = float32 (delta.Length ()) + 0.01f
                            let deltaNorm = delta / distance
                            let distance' = distance - targetDistance
                            let force = distance' * area.ZeroDistanceForce
                            l - deltaNorm * force

                    )
                )

            let recenter (graph : SitesDisplay) =
                let list =
                    graph
                    |> Vertices.toList
                    |> List.map snd
                let sum = List.sum list
                let center = sum / float32 (List.length list)
                let graph = Vertices.map (fun _ l -> l - center) graph
                graph

            let positions =
                graph
                |> (List.init iterations (fun _ -> attraction) |> List.fold (>>) id)
                |> recenter
                |> Vertices.toList
                |> Map.ofList

            let sites =
                area.Sites
                |> Vertices.map (fun v l ->
                    match Map.tryFind v positions with
                    | Some pos ->
                        { l with Position = Some pos }
                    | None ->
                        l
                )

            { area with Sites = sites; DistancesCache = nodeDistances }
        static member empty = {
            Name = String.empty
            Sites = Graph.empty

            DistancesCache = Map.empty

            Force = 2f
            DistanceMult = 0.15f
            ZeroDistanceForce = 0.025f
        }

        static member add id =
            let site = Site.make id ""
            Vertices.add (site.ID, site)

        static member addNamed id name =
            let site = Site.make id name
            Vertices.add (site.ID, site)

        static member addPart where id =
            let site = Site.make id ""
            Vertices.add (site.ID, site)
            >> Directed.Edges.add (where, id, Consists)

        static member addFloor where id =
            let site = Site.makeGround id
            Vertices.add (site.ID, site)
            >> Directed.Edges.add (where, id, Consists)

        static member relationship left right relationship =
            Directed.Edges.add (left, right, relationship)
            >> Directed.Edges.add (right, left, relationship)

        static member room id name (size : Vector3i) =
            let x = uint (size.X / 2)
            let y = uint (size.Y / 2)
            let z = uint (size.Z / 2)
            Area.addNamed id name
            // room parts
            >> Area.addFloor id $"{id}floor"
            >> Area.addPart id $"{id}ceiling"
//            >> Area.addPart id $"{id}wallN"
//            >> Area.addPart id $"{id}wallE"
//            >> Area.addPart id $"{id}wallS"
//            >> Area.addPart id $"{id}wallW"
            >> Area.addPart id $"{id}walls"
            >> Area.addPart id $"{id}corners"
            >> Area.addPart id $"{id}air"
            // room size
            >> Area.relationship $"{id}air" $"{id}floor" (Distance z)
            >> Area.relationship $"{id}air" $"{id}ceiling" (Distance z)
            >> Area.relationship $"{id}air" $"{id}walls" (Distance x)
//            >> Area.relationship $"{id}air" $"{id}wallN" (Distance x)
//            >> Area.relationship $"{id}air" $"{id}wallE" (Distance y)
//            >> Area.relationship $"{id}air" $"{id}wallS" (Distance x)
//            >> Area.relationship $"{id}air" $"{id}wallW" (Distance y)
//            >> Area.relationship $"{id}floor" $"{id}walls" (Distance x)
//            >> Area.relationship $"{id}floor" $"{id}wallN" (Distance x)
//            >> Area.relationship $"{id}floor" $"{id}wallE" (Distance y)
//            >> Area.relationship $"{id}floor" $"{id}wallS" (Distance x)
//            >> Area.relationship $"{id}floor" $"{id}wallW" (Distance y)
//            >> Area.relationship $"{id}ceiling" $"{id}walls" (Distance x)
//            >> Area.relationship $"{id}ceiling" $"{id}wallN" (Distance x)
//            >> Area.relationship $"{id}ceiling" $"{id}wallE" (Distance y)
//            >> Area.relationship $"{id}ceiling" $"{id}wallS" (Distance x)
//            >> Area.relationship $"{id}ceiling" $"{id}wallW" (Distance y)
            >> Area.relationship $"{id}corners" $"{id}floor" IsOnEdge

        static member exit where id =
            Area.add id
            >> Area.relationship where id IsOnEdge

        // * Waitroom, barricaded windows, rolling hospital bed, locked exit door, you wake up here
        static member room1waitroom : Sites =
            Graph.empty
            // room itself
            |> Area.room "room1" "Waitroom" (v3i 1200 1200 250)
            |> Area.exit "room1walls" "room1exit2"
            |> Area.exit "room1walls" "room1exit3"
            |> Area.exit "room1walls" "room1exit6"
            |> Area.exit "room1walls" "room1exit8"
            // actors inside the room
            |> Vertices.add ("room1barricadedWindow1", Site.empty)
            |> Vertices.add ("room1barricadedWindow2", Site.empty)
            |> Vertices.add ("room1windowroom3", Site.empty)
            |> Vertices.add ("room1windowroom6", Site.empty)
            |> Vertices.add ("room1gurney", Site.empty)
            |> Vertices.add ("room1chairs", Site.empty)
            // locations of actors inside the room
            |> Directed.Edges.add ("room1walls", "room1barricadedWindow1", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1barricadedWindow2", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1windowroom3", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1windowroom6", IsOnEdge)
            |> Directed.Edges.add ("room1floor", "room1gurney", LiesAbove)
            |> Directed.Edges.add ("room1floor", "room1chairs", LiesAbove)

        // * Main hall, chairs, first rat attacks
        static member room2mainhall : Sites =
            Graph.empty
            // room itself
            |> Area.room "room2" "Main Hall" (v3i 1200 1200 400)
            |> Area.exit "room2walls" "room2exit1"
            |> Area.exit "room2walls" "room2exit4"
            |> Area.exit "room2walls" "room2exit5"
            |> Area.exit "room2walls" "room2exit7"
            // actors inside the room
            |> Vertices.add ("room2chair1", Site.empty)
            |> Vertices.add ("room2chair2", Site.empty)
            |> Vertices.add ("room2chair3", Site.empty)
            // locations of actors inside the room
            |> Directed.Edges.add ("room2floor", "room2chair1", LiesAbove)
            |> Directed.Edges.add ("room2floor", "room2chair2", LiesAbove)
            |> Directed.Edges.add ("room2floor", "room2chair3", LiesAbove)

            |> Vertices.add ("rat", Site.empty)
            |> Directed.Edges.add ("room2floor", "rat", LiesAbove)

        // * Registration room, safe with useful stuff like a pistol maybe, code locked, code is gotten from a book
        //   (can be seen from waitroom through glass)
        static member room3registration : Sites =
            Graph.empty
            |> Area.room "room3" "Registration" (v3i 800 800 250)
            |> Area.exit "room3walls" "room3exit1"
            // actors inside the room
            |> Vertices.add ("room3windowroom1", Site.empty)
            |> Vertices.add ("room3registrationdesk", Site.empty)
            |> Vertices.add ("room3chairs", Site.empty)
            |> Vertices.add ("room3safe", Site.empty)
            |> Vertices.add ("room3book", Site.empty)
            // locations of actors inside the room
            |> Directed.Edges.add ("room3walls", "room3windowroom1", IsOnEdge)
            |> Directed.Edges.add ("room3floor", "room3registrationdesk", LiesAbove)
            |> Directed.Edges.add ("room3floor", "room3chairs", LiesAbove)
            |> Directed.Edges.add ("room3floor", "room3safe", LiesAbove)
            |> Directed.Edges.add ("room3registrationdesk", "room3book", LiesAbove)
            //|> Vertices.add ("rat", Site.empty)
            //|> Directed.Edges.add ("room2floor", "rat", LiesAbove)

        // * Electrical room to fix the lights, second rat attacks
        static member room4electrical : Sites =
            Graph.empty
            |> Area.room "room4" "Electrical Room" (v3i 400 400 250)
            |> Area.exit "room4walls" "room4exit2"
            // actors inside the room
            |> Vertices.add ("room4generator", Site.empty)
            |> Vertices.add ("room4generatorcontrols", Site.empty)
            // locations of actors inside the room
            |> Directed.Edges.add ("room4walls", "room4generator", IsOnEdge)
            |> Directed.Edges.add ("room4floor", "room4generatorcontrols", LiesAbove)

        // * Surgery room, surgery table, cat on the table, note (cat ate the key), opening it lets spider chandalier escape
        static member room5surgery : Sites =
            Graph.empty
            |> Area.room "room5" "Surgical Room" (v3i 400 400 250)
            |> Area.exit "room5walls" "room5exit2"
            // actors inside the room
            |> Vertices.add ("room5surgerytable", Site.empty)
            |> Vertices.add ("room5instruments", Site.empty)
            |> Vertices.add ("room5cat", Site.empty)
            |> Vertices.add ("room5key", Site.empty)
            // locations of actors inside the room
            |> Directed.Edges.add ("room5floor", "room5surgerytable", LiesAbove)
            |> Directed.Edges.add ("room5surgerytable", "room5instruments", LiesAbove)
            |> Directed.Edges.add ("room5surgerytable", "room5cat", LiesAbove)
            |> Directed.Edges.add ("room5cat", "room5key", LiesAbove)

        // * Pharmacy shop, drugs you can use
        static member room6pharmacy : Sites =
            Graph.empty
            |> Area.room "room6" "Pharmacy" (v3i 400 400 250)
            |> Area.exit "room6walls" "room6exit1"
            // actors inside the room
            |> Vertices.add ("room6shelves", Site.empty)
            |> Vertices.add ("room6drugs", Site.empty)
            // locations of actors inside the room
            |> Directed.Edges.add ("room6floor", "room6shelves", LiesAbove)
            |> Directed.Edges.add ("room6shelves", "room6drugs", LiesAbove)

        // * Staircase, other floors blocked, but can move up and down.
        static member room7staircase : Sites =
            Graph.empty
            |> Area.room "room7" "Staircase" (v3i 400 400 900)
            |> Area.exit "room7walls" "room7exit2"

        static member level1clinic = {
            Area.empty with
                Name = "Clinic"
                Sites =
                    Graph.empty
                    |> Graph.join Area.room1waitroom
                    |> Graph.join Area.room2mainhall
                    |> Area.relationship "room1exit2" "room2exit1" IsOnEdge
                    |> Graph.join Area.room3registration
                    |> Area.relationship "room1exit3" "room3exit1" IsOnEdge
                    |> Area.relationship "room1windowroom3" "room3windowroom1" IsOnEdge
                    |> Graph.join Area.room4electrical
                    |> Area.relationship "room2exit4" "room4exit2" IsOnEdge
                    |> Graph.join Area.room5surgery
                    |> Area.relationship "room2exit5" "room5exit2" IsOnEdge
                    |> Graph.join Area.room6pharmacy
                    |> Area.relationship "room1exit6" "room6exit1" IsOnEdge
                    |> Graph.join Area.room7staircase
                    |> Area.relationship "room2exit7" "room7exit2" IsOnEdge
                    // characters inside the room
                    |> Vertices.add ("player", Site.empty)
                    |> Directed.Edges.add ("room1gurney", "player", LiesAbove)
        }

        static member initial =
            Area.level1clinic
            |> Area.iterateDisplay 200

type Area = Area.Area