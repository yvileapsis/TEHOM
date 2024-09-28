namespace Tehom

open System
open System.Numerics
open Prime
open Nu
open FGL

// TODO: merge graphs
// TODO: moving in graph

(*
* 5 Rooms:
* Waitroom, barricaded windows, rolling hospital bed, locked exit door, you wake up here
* Registration room, safe with useful stuff like a pistol maybe
* Main hall
* Electrical room to fix the lights, rat attacks
* Surgery room, surgery table, dog on the table, note (dog ate the key), opening it lets spider chandalier escape
* Goal is to go to surgery room, take the key, return to waitroom, open the door and exit.
* Optionally you can fix lights and fight spider chandalier.
*)


module Area =

    type Site =
        | Site
    with
        static member empty = Site

    type Relationship =
        | Distance of Distance: float
        | Consists
        | Contains
        | LiesAbove
        | Covers
        | OnEdge

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
            |> Undirected.Edges.add ("room1air", "room1floor", Distance 150)
            |> Undirected.Edges.add ("room1air", "room1ceiling", Distance 100)
            |> Undirected.Edges.add ("room1air", "room1walls", Distance 600)
            |> Directed.Edges.add ("room1corners", "room1floor", OnEdge)
            // actors inside the room
            |> Vertices.add ("room1barricadedWindow1", Site.empty)
            |> Vertices.add ("room1barricadedWindow2", Site.empty)
            |> Vertices.add ("room1exitFinal", Site.empty)
            |> Vertices.add ("room1exitMainHall", Site.empty)
            |> Vertices.add ("room1gurney", Site.empty)
            // locations of actors inside the room
            |> Directed.Edges.add ("room1walls", "room1barricadedWindow1", OnEdge)
            |> Directed.Edges.add ("room1walls", "room1barricadedWindow2", OnEdge)
            |> Directed.Edges.add ("room1walls", "room1exitFinal", OnEdge)
            |> Directed.Edges.add ("room1walls", "room1exitMainHall", OnEdge)
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

            |> Undirected.Edges.add ("room2air", "room2floor", Distance 200)
            |> Undirected.Edges.add ("room2air", "room2ceiling", Distance 200)
            |> Undirected.Edges.add ("room2air", "room2walls", Distance 600)
            |> Directed.Edges.add ("room2corners", "room2floor", OnEdge)

            |> Vertices.add ("room2table1", Site.empty)
            |> Vertices.add ("room2table2", Site.empty)
            |> Vertices.add ("room2table3", Site.empty)
            |> Vertices.add ("room2exitWaitingRoom", Site.empty)

            |> Directed.Edges.add ("room2walls", "room2exitWaitingRoom", LiesAbove)
            |> Directed.Edges.add ("room2floor", "room2table1", LiesAbove)
            |> Directed.Edges.add ("room2floor", "room2table2", LiesAbove)
            |> Directed.Edges.add ("room2floor", "room2table3", LiesAbove)

            |> Vertices.add ("rat", Site.empty)
            |> Directed.Edges.add ("room2floor", "player", LiesAbove)

        static member level1 = {
            Name = "Clinic"
            Sites =
                Graph.empty
                |> Graph.join Area.room1
                |> Graph.join Area.room2
                |> Undirected.Edges.add ("room1exitMainHall", "room2exitWaitingRoom", OnEdge)
        }

        static member find finder (level : Area) =
            level.Sites
            |> Vertices.toVertexList
            |> List.tryFind finder
            |> function | Some v -> Some (fst v) | None -> None

        static member findSite site (area : Area) =
            Area.find (fun (string, vertex) -> string = site) area

        static member findPath fromDestination toDestination level =
            level.Sites
            |> Directed.Edges.map (fun v1 v2 edge ->
                match edge with
                | Distance x -> uint x
                | Consists -> UInt32.MaxValue
                | Contains -> 0u
                | LiesAbove -> 0u
                | Covers -> 0u
                | OnEdge -> 0u
            )
            |> Query.sp fromDestination toDestination

        static member moveActor actor fromDestination toDestination level =
            if not (fromDestination = toDestination) then
                {
                    level with
                        Sites = Vertices.map (fun string vertex ->
                            if string = fromDestination then
                                { vertex with Actors = Set.remove actor vertex.Actors }
                            elif string = toDestination then
                                { vertex with Actors = Set.add actor vertex.Actors }
                            else
                                vertex
                        ) level.Sites
                }
            else
                level

type Area = Area.Area