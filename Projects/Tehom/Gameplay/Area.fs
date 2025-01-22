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
        static member room1waitroom : Sites =
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
            |> Vertices.add ("room1windowroom3", Site.empty)
            |> Vertices.add ("room1windowroom6", Site.empty)
            |> Vertices.add ("room1exit2", Site.empty)
            |> Vertices.add ("room1exit3", Site.empty)
            |> Vertices.add ("room1exit6", Site.empty)
            |> Vertices.add ("room1exit8", Site.empty)
            |> Vertices.add ("room1gurney", Site.empty)
            |> Vertices.add ("room1chairs", Site.empty)
            // locations of actors inside the room
            |> Directed.Edges.add ("room1walls", "room1barricadedWindow1", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1barricadedWindow2", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1windowroom3", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1windowroom6", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1exit2", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1exit3", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1exit6", IsOnEdge)
            |> Directed.Edges.add ("room1walls", "room1exit8", IsOnEdge)
            |> Directed.Edges.add ("room1floor", "room1gurney", LiesAbove)
            |> Directed.Edges.add ("room1floor", "room1chairs", LiesAbove)



        // * Main hall, chairs, first rat attacks
        static member room2mainhall : Sites =
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

            |> Vertices.add ("room2chair1", Site.empty)
            |> Vertices.add ("room2chair2", Site.empty)
            |> Vertices.add ("room2chair3", Site.empty)
            |> Vertices.add ("room2exit1", Site.empty)
            |> Vertices.add ("room2exit4", Site.empty)
            |> Vertices.add ("room2exit5", Site.empty)
            |> Vertices.add ("room2exit7", Site.empty)

            |> Directed.Edges.add ("room2walls", "room2exit1", IsOnEdge)
            |> Directed.Edges.add ("room2walls", "room2exit4", IsOnEdge)
            |> Directed.Edges.add ("room2walls", "room2exit5", IsOnEdge)
            |> Directed.Edges.add ("room2walls", "room2exit7", IsOnEdge)
            |> Directed.Edges.add ("room2floor", "room2chair1", LiesAbove)
            |> Directed.Edges.add ("room2floor", "room2chair2", LiesAbove)
            |> Directed.Edges.add ("room2floor", "room2chair3", LiesAbove)

            |> Vertices.add ("rat", Site.empty)
            |> Directed.Edges.add ("room2floor", "rat", LiesAbove)

        // * Registration room, safe with useful stuff like a pistol maybe, code locked, code is gotten from a book
        //   (can be seen from waitroom through glass)
        static member room3registration : Sites =
            Graph.empty
            |> Vertices.add ("room3", Site.empty)

            |> Vertices.add ("room3floor", Site.empty)
            |> Vertices.add ("room3ceiling", Site.empty)
            |> Vertices.add ("room3walls", Site.empty)
            |> Vertices.add ("room3corners", Site.empty)
            |> Vertices.add ("room3air", Site.empty)

            |> Directed.Edges.add ("room3", "room3floor", Consists)
            |> Directed.Edges.add ("room3", "room3ceiling", Consists)
            |> Directed.Edges.add ("room3", "room3walls", Consists)
            |> Directed.Edges.add ("room3", "room3corners", Consists)
            |> Directed.Edges.add ("room3", "room3air", Consists)

            |> Directed.Edges.add ("room3air", "room3floor", Distance 150u)
            |> Directed.Edges.add ("room3floor", "room3air", Distance 150u)
            |> Directed.Edges.add ("room3air", "room3ceiling", Distance 100u)
            |> Directed.Edges.add ("room3ceiling", "room3air", Distance 100u)
            |> Directed.Edges.add ("room3air", "room3walls", Distance 400u)
            |> Directed.Edges.add ("room3walls", "room3air", Distance 400u)
            |> Directed.Edges.add ("room3floor", "room3walls", Distance 400u)
            |> Directed.Edges.add ("room3walls", "room3floor", Distance 400u)
            |> Directed.Edges.add ("room3corners", "room3floor", IsOnEdge)

            |> Vertices.add ("room3exit1", Site.empty)

            |> Directed.Edges.add ("room3walls", "room3exit1", IsOnEdge)

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
            |> Vertices.add ("room4", Site.empty)

            |> Vertices.add ("room4floor", Site.empty)
            |> Vertices.add ("room4ceiling", Site.empty)
            |> Vertices.add ("room4walls", Site.empty)
            |> Vertices.add ("room4corners", Site.empty)
            |> Vertices.add ("room4air", Site.empty)

            |> Directed.Edges.add ("room4", "room4floor", Consists)
            |> Directed.Edges.add ("room4", "room4ceiling", Consists)
            |> Directed.Edges.add ("room4", "room4walls", Consists)
            |> Directed.Edges.add ("room4", "room4corners", Consists)
            |> Directed.Edges.add ("room4", "room4air", Consists)

            |> Directed.Edges.add ("room4air", "room4floor", Distance 150u)
            |> Directed.Edges.add ("room4floor", "room4air", Distance 150u)
            |> Directed.Edges.add ("room4air", "room4ceiling", Distance 100u)
            |> Directed.Edges.add ("room4ceiling", "room4air", Distance 100u)
            |> Directed.Edges.add ("room4air", "room4walls", Distance 200u)
            |> Directed.Edges.add ("room4walls", "room4air", Distance 200u)
            |> Directed.Edges.add ("room4floor", "room4walls", Distance 200u)
            |> Directed.Edges.add ("room4walls", "room4floor", Distance 200u)
            |> Directed.Edges.add ("room4corners", "room4floor", IsOnEdge)

            |> Vertices.add ("room4exit2", Site.empty)

            |> Directed.Edges.add ("room4walls", "room4exit2", IsOnEdge)

            // actors inside the room
            |> Vertices.add ("room4generator", Site.empty)
            |> Vertices.add ("room4generatorcontrols", Site.empty)
            // locations of actors inside the room
            |> Directed.Edges.add ("room4walls", "room4generator", IsOnEdge)
            |> Directed.Edges.add ("room4floor", "room4generatorcontrols", LiesAbove)

        // * Surgery room, surgery table, cat on the table, note (cat ate the key), opening it lets spider chandalier escape
        static member room5surgery : Sites =
            Graph.empty
            |> Vertices.add ("room5", Site.empty)

            |> Vertices.add ("room5floor", Site.empty)
            |> Vertices.add ("room5ceiling", Site.empty)
            |> Vertices.add ("room5walls", Site.empty)
            |> Vertices.add ("room5corners", Site.empty)
            |> Vertices.add ("room5air", Site.empty)

            |> Directed.Edges.add ("room5", "room5floor", Consists)
            |> Directed.Edges.add ("room5", "room5ceiling", Consists)
            |> Directed.Edges.add ("room5", "room5walls", Consists)
            |> Directed.Edges.add ("room5", "room5corners", Consists)
            |> Directed.Edges.add ("room5", "room5air", Consists)

            |> Directed.Edges.add ("room5air", "room5floor", Distance 150u)
            |> Directed.Edges.add ("room5floor", "room5air", Distance 150u)
            |> Directed.Edges.add ("room5air", "room5ceiling", Distance 100u)
            |> Directed.Edges.add ("room5ceiling", "room5air", Distance 100u)
            |> Directed.Edges.add ("room5air", "room5walls", Distance 400u)
            |> Directed.Edges.add ("room5walls", "room5air", Distance 400u)
            |> Directed.Edges.add ("room5floor", "room5walls", Distance 400u)
            |> Directed.Edges.add ("room5walls", "room5floor", Distance 400u)
            |> Directed.Edges.add ("room5corners", "room5floor", IsOnEdge)

            |> Vertices.add ("room5exit2", Site.empty)

            |> Directed.Edges.add ("room5walls", "room5exit2", IsOnEdge)

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
            |> Vertices.add ("room6", Site.empty)

            |> Vertices.add ("room6floor", Site.empty)
            |> Vertices.add ("room6ceiling", Site.empty)
            |> Vertices.add ("room6walls", Site.empty)
            |> Vertices.add ("room6corners", Site.empty)
            |> Vertices.add ("room6air", Site.empty)

            |> Directed.Edges.add ("room6", "room6floor", Consists)
            |> Directed.Edges.add ("room6", "room6ceiling", Consists)
            |> Directed.Edges.add ("room6", "room6walls", Consists)
            |> Directed.Edges.add ("room6", "room6corners", Consists)
            |> Directed.Edges.add ("room6", "room6air", Consists)

            |> Directed.Edges.add ("room6air", "room6floor", Distance 150u)
            |> Directed.Edges.add ("room6floor", "room6air", Distance 150u)
            |> Directed.Edges.add ("room6air", "room6ceiling", Distance 100u)
            |> Directed.Edges.add ("room6ceiling", "room6air", Distance 100u)
            |> Directed.Edges.add ("room6air", "room6walls", Distance 200u)
            |> Directed.Edges.add ("room6walls", "room6air", Distance 200u)
            |> Directed.Edges.add ("room6floor", "room6walls", Distance 200u)
            |> Directed.Edges.add ("room6walls", "room6floor", Distance 200u)
            |> Directed.Edges.add ("room6corners", "room6floor", IsOnEdge)

            |> Vertices.add ("room6exit1", Site.empty)

            |> Directed.Edges.add ("room6walls", "room6exit1", IsOnEdge)
            // actors inside the room
            |> Vertices.add ("room6shelves", Site.empty)
            |> Vertices.add ("room6drugs", Site.empty)
            // locations of actors inside the room
            |> Directed.Edges.add ("room6floor", "room6shelves", LiesAbove)
            |> Directed.Edges.add ("room6shelves", "room6drugs", LiesAbove)

        // * Staircase, other floors blocked, but can move up and down.
        static member room7staircase : Sites =
            Graph.empty
            |> Vertices.add ("room7", Site.empty)

            |> Vertices.add ("room7floor", Site.empty)
            |> Vertices.add ("room7ceiling", Site.empty)
            |> Vertices.add ("room7walls", Site.empty)
            |> Vertices.add ("room7corners", Site.empty)
            |> Vertices.add ("room7air", Site.empty)

            |> Directed.Edges.add ("room7", "room7floor", Consists)
            |> Directed.Edges.add ("room7", "room7ceiling", Consists)
            |> Directed.Edges.add ("room7", "room7walls", Consists)
            |> Directed.Edges.add ("room7", "room7corners", Consists)
            |> Directed.Edges.add ("room7", "room7air", Consists)

            |> Directed.Edges.add ("room7air", "room7floor", Distance 450u)
            |> Directed.Edges.add ("room7floor", "room7air", Distance 450u)
            |> Directed.Edges.add ("room7air", "room7ceiling", Distance 300u)
            |> Directed.Edges.add ("room7ceiling", "room7air", Distance 300u)
            |> Directed.Edges.add ("room7air", "room7walls", Distance 200u)
            |> Directed.Edges.add ("room7walls", "room7air", Distance 200u)
            |> Directed.Edges.add ("room7floor", "room7walls", Distance 200u)
            |> Directed.Edges.add ("room7walls", "room7floor", Distance 200u)
            |> Directed.Edges.add ("room7corners", "room7floor", IsOnEdge)

            |> Vertices.add ("room7exit2", Site.empty)

            |> Directed.Edges.add ("room7walls", "room7exit2", IsOnEdge)

        static member level1clinic = {
            Name = "Clinic"
            Sites =
                Graph.empty
                |> Graph.join Area.room1waitroom
                |> Graph.join Area.room2mainhall
                |> Directed.Edges.add ("room1exit2", "room2exit1", IsOnEdge)
                |> Directed.Edges.add ("room2exit1", "room1exit2", IsOnEdge)
                |> Graph.join Area.room3registration
                |> Directed.Edges.add ("room1exit3", "room3exit1", IsOnEdge)
                |> Directed.Edges.add ("room3exit1", "room1exit3", IsOnEdge)
                |> Directed.Edges.add ("room1windowroom3", "room3windowroom1", IsOnEdge)
                |> Directed.Edges.add ("room3windowroom1", "room1windowroom3", IsOnEdge)
                |> Graph.join Area.room4electrical
                |> Directed.Edges.add ("room2exit4", "room4exit2", IsOnEdge)
                |> Directed.Edges.add ("room4exit2", "room2exit4", IsOnEdge)
                |> Graph.join Area.room5surgery
                |> Directed.Edges.add ("room2exit5", "room5exit2", IsOnEdge)
                |> Directed.Edges.add ("room5exit2", "room2exit5", IsOnEdge)
                |> Graph.join Area.room6pharmacy
                |> Directed.Edges.add ("room1exit6", "room6exit1", IsOnEdge)
                |> Directed.Edges.add ("room6exit1", "room1exit6", IsOnEdge)
                |> Graph.join Area.room7staircase
                |> Directed.Edges.add ("room2exit7", "room7exit2", IsOnEdge)
                |> Directed.Edges.add ("room7exit2", "room2exit7", IsOnEdge)

                // characters inside the room
                |> Vertices.add ("player", Site.empty)
                |> Directed.Edges.add ("room1gurney", "player", LiesAbove)
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