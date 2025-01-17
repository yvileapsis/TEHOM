namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open Graph

module Area =

    type Site =
        | Site
    with
        static member empty = Site

    type Relationship =
        | Distance of Distance: uint32
        | Consists
        | Contains
        | LiesAbove
        | Covers
        | IsOnEdge

    type Sites = Graph<String, Site, Relationship>

    type Area = {
        Name : string
        Sites : Sites
    }
    with
        static member empty = {
            Name = String.empty
            Sites = Graph.empty
        }

        // * Waitroom, barricaded windows, rolling hospital bed, locked exit door, you wake up here
        static member room1 : Sites =
            Graph.empty
            // room itself
            |> Vertices.add ("room1", Site.empty)
            // room parts
            |> Vertices.add ("room1floor", Site.empty)
            |> Vertices.add ("room1ceiling", Site.empty)
            |> Vertices.add ("room1walls", Site.empty)
            |> Vertices.add ("room1corners", Site.empty)
            |> Vertices.add ("room1air", Site.empty)
            // room consists of its parts
            |> Directed.Edges.add ("room1", "room1floor", Consists)
            |> Directed.Edges.add ("room1", "room1ceiling", Consists)
            |> Directed.Edges.add ("room1", "room1walls", Consists)
            |> Directed.Edges.add ("room1", "room1corners", Consists)
            |> Directed.Edges.add ("room1", "room1air", Consists)
            // room size
            |> Directed.Edges.add ("room1air", "room1floor", Distance 150u)
            |> Directed.Edges.add ("room1floor", "room1air", Distance 150u)
            |> Directed.Edges.add ("room1air", "room1ceiling", Distance 100u)
            |> Directed.Edges.add ("room1ceiling", "room1air", Distance 100u)
            |> Directed.Edges.add ("room1air", "room1walls", Distance 600u)
            |> Directed.Edges.add ("room1walls", "room1air", Distance 600u)
            |> Directed.Edges.add ("room1floor", "room1walls", Distance 600u)
            |> Directed.Edges.add ("room1walls", "room1floor", Distance 600u)
            |> Directed.Edges.add ("room1corners", "room1floor", IsOnEdge)
            // actors inside the room
            |> Vertices.add ("room1barricadedWindow1", Site.empty)
            |> Vertices.add ("room1barricadedWindow2", Site.empty)
            |> Vertices.add ("room1exitFinal", Site.empty)
            |> Vertices.add ("room1exitMainHall", Site.empty)
            |> Vertices.add ("room1gurney", Site.empty)
            // locations of actors inside the room
            |> Directed.Edges.add ("room1walls", "room1barricadedWindow1", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1barricadedWindow2", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1exitFinal", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1exitMainHall", IsOnEdge)
            |> Directed.Edges.add ("room1floor", "room1gurney", LiesAbove)

            // characters inside the room
            |> Vertices.add ("player", Site.empty)
            |> Directed.Edges.add ("room1gurney", "player", LiesAbove)


        // main hall, rat
        static member room2 : Sites =
            Graph.empty
            |> Vertices.add ("room2", Site.empty)

            |> Vertices.add ("room2floor", Site.empty)
            |> Vertices.add ("room2ceiling", Site.empty)
            |> Vertices.add ("room2walls", Site.empty)
            |> Vertices.add ("room2corners", Site.empty)
            |> Vertices.add ("room2air", Site.empty)

            |> Directed.Edges.add ("room2", "room2floor", Consists)
            |> Directed.Edges.add ("room2", "room2ceiling", Consists)
            |> Directed.Edges.add ("room2", "room2walls", Consists)
            |> Directed.Edges.add ("room2", "room2corners", Consists)
            |> Directed.Edges.add ("room2", "room2air", Consists)

            |> Directed.Edges.add ("room2air", "room2floor", Distance 200u)
            |> Directed.Edges.add ("room2floor", "room2air", Distance 200u)
            |> Directed.Edges.add ("room2air", "room2ceiling", Distance 200u)
            |> Directed.Edges.add ("room2ceiling", "room2air", Distance 200u)
            |> Directed.Edges.add ("room2air", "room2walls", Distance 600u)
            |> Directed.Edges.add ("room2walls", "room2air", Distance 600u)
            |> Directed.Edges.add ("room2floor", "room2walls", Distance 600u)
            |> Directed.Edges.add ("room2walls", "room2floor", Distance 600u)
            |> Directed.Edges.add ("room2corners", "room2floor", IsOnEdge)

            |> Vertices.add ("room2table1", Site.empty)
            |> Vertices.add ("room2table2", Site.empty)
            |> Vertices.add ("room2table3", Site.empty)
            |> Vertices.add ("room2exitWaitingRoom", Site.empty)

            |> Directed.Edges.add ("room2walls", "room2exitWaitingRoom", IsOnEdge)
            |> Directed.Edges.add ("room2floor", "room2table1", LiesAbove)
            |> Directed.Edges.add ("room2floor", "room2table2", LiesAbove)
            |> Directed.Edges.add ("room2floor", "room2table3", LiesAbove)

            |> Vertices.add ("rat", Site.empty)
            |> Directed.Edges.add ("room2floor", "rat", LiesAbove)

        static member level1 = {
            Name = "Clinic"
            Sites =
                Graph.empty
                |> Graph.join Area.room1
                |> Graph.join Area.room2
                |> Undirected.Edges.add ("room1exitMainHall", "room2exitWaitingRoom", IsOnEdge)
        }

        static member find finder (area : Area) =
            area.Sites
            |> Vertices.toList
            |> List.tryFind finder
            |> function | Some v -> Some (fst v) | None -> None

        static member findPath fromDestination toDestination area =
            area.Sites
            |> Edges.undirect2 (fun vertex1 vertex2 edge ->
                match edge with
                | Contains -> true
                | LiesAbove -> true
                | Covers -> true
                | IsOnEdge -> true
                | _ -> false
            ) (fun edge1 edge2 ->
                edge1
            )
            |> Edges.choose (fun v1 v2 edge ->
                match edge with
                | Distance x -> Some (uint x)
                | Consists -> None
                | Contains -> Some 0u
                | LiesAbove -> Some 0u
                | Covers -> Some 0u
                | IsOnEdge -> Some 0u
            )
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


type Area = Area.Area