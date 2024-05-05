namespace Tehom
open System
open Nu
open FSharp.FGL

type TehomID =
    | ID of string
    | GUID of Guid
with
    static member empty = ID ""
    static member guid = GUID (Guid.NewGuid())


module Abyss =

    type Limb = {
        Name : string
        // simplest implementation of future system
        CanBeSeen: bool
        Contains: Set<TehomID>
    }
    with
        static member empty = {
            Name = "err"
            CanBeSeen = false
            Contains = Set.empty
        }

    type Tissue = {
        Distance : float

        Bones : bool
        Muscles : bool

        Veins : bool
        Arteries : bool

        Sensory : bool
        Motor : bool
    }
    with
        static member empty = {
            Distance = 0.0

            Bones = false
            Muscles = false

            Veins = false
            Arteries = false

            Sensory = false
            Motor = true
        }

        static member healthy = {
            Distance = 1.0

            Bones = true
            Muscles = true

            Veins = true
            Arteries = true

            Sensory = true
            Motor = true
        }

    type Actor = {
        Name : string
        Limbs : Graph<TehomID, Limb, Tissue>
    }
    with
        static member empty = {
            Name = "err"
            Limbs = Graph.empty
        }

        static member player = {
            Actor.empty with
                Name = "Player"
                Limbs =
                    Graph.empty
                    |> Vertices.addMany [
                        ID "Head", { Limb.empty with Name = "Head"; CanBeSeen = true }
                        ID "Legs", { Limb.empty with Name = "Legs"; CanBeSeen = true }
                        ID "Arms", { Limb.empty with Name = "Arms"; CanBeSeen = true }
                        ID "Torso", { Limb.empty with Name = "Torso"; CanBeSeen = true}
                    ]
                    |> Undirected.Edges.addMany [
                        ID "Head", ID "Torso", { Tissue.healthy with Distance = 0.1 }
                        ID "Legs", ID "Torso", { Tissue.healthy with Distance = 0.1 }
                        ID "Arms", ID "Torso", { Tissue.healthy with Distance = 0.1 }
                    ]
        }

        static member cat = {
            Actor.empty with
                Name = "Cat"
                Limbs =
                    Graph.empty
                    |> Vertices.addMany [
                        ID "Head", { Limb.empty with Name = "Head"; CanBeSeen = true }
                        ID "Forelegs", { Limb.empty with Name = "Forelegs"; CanBeSeen = true }
                        ID "Backlegs", { Limb.empty with Name = "Backlegs"; CanBeSeen = true }
                        ID "Torso", { Limb.empty with Name = "Torso"; CanBeSeen = true }
                        ID "Stomach", { Limb.empty with Name = "Stomach"; Contains = Set.ofList [ID "Key"] }
                    ]
                    |> Undirected.Edges.addMany [
                        ID "Head", ID "Torso", { Tissue.healthy with Distance = 0.1 }
                        ID "Legs", ID "Torso", { Tissue.healthy with Distance = 0.1 }
                        ID "Arms", ID "Torso", { Tissue.healthy with Distance = 0.1 }
                        ID "Stomach", ID "Torso", { Tissue.healthy with Distance = 0.0 }
                    ]
        }

        static member table = {
            Actor.empty with
                Name = "Table"
                Limbs =
                    Graph.empty
                    |> Vertices.addMany [
                        ID "Top", { Limb.empty with Name = "Top"; CanBeSeen = true }
                        ID "Leg 1", { Limb.empty with Name = "Leg 1"; CanBeSeen = true }
                        ID "Leg 2", { Limb.empty with Name = "Leg 2"; CanBeSeen = true }
                        ID "Leg 3", { Limb.empty with Name = "Leg 3"; CanBeSeen = true }
                    ]
                    |> Undirected.Edges.addMany [
                        ID "Leg 1", ID "Top", { Tissue.healthy with Distance = 0.3 }
                        ID "Leg 2", ID "Top", { Tissue.healthy with Distance = 0.3 }
                        ID "Leg 3", ID "Top", { Tissue.healthy with Distance = 0.3 }
                    ]
        }

        static member key = {
            Actor.empty with Name = "Key"
        }

    type Relation =
        | Contains of Limb: TehomID option
        | HasOnTop

    type Relations = Relation list

    type Area = {
        Name : string
        Actors : Set<TehomID>
    }
    with
        static member room1 = {
            Name = "Dark Room"
            Actors =
                Set.empty
                |> Set.add (ID "Player")
                |> Set.add (ID "Table")
                |> Set.add (ID "Cat")
                |> Set.add (ID "Key")
        }

        static member room2 = {
            Name = "Bright Room"
            Actors = Set.empty
        }

    type Pathway = {
        Open : bool
    }
    with
        static member empty = { Open = false }

        static member passage1to2 = {
            Open = false
        }

    type Level = {
        Name : string
        Areas : Graph<TehomID, Area, Pathway>
    }
    with
        static member empty = {
            Name = ""
            Areas = Graph.empty
        }

        static member level1 = {
            Name = "Sheol"
            Areas =
                Graph.empty
                |> Vertices.add (ID "Room 1", Area.room1)
                |> Vertices.add (ID "Room 2", Area.room2)
                |> Undirected.Edges.add (ID "Room 1", ID "Room 2", Pathway.passage1to2)
        }

    type Portal = Portal

    type Abyss = {
        Name : string
        Levels : Graph<TehomID, Level, Portal>
        Actors : Graph<TehomID, Actor, Relations>
    }
    with
        static member empty = {
            Name = ""
            Levels = Graph.empty
            Actors = Graph.empty
        }

        static member initial = {
            Abyss.empty with
                Name = "Tehom"
                Levels =
                    Graph.empty
                    |> Vertices.add (ID "Level 1", Level.level1)
                Actors =
                    Graph.empty
                    |> Vertices.add (ID "Player", Actor.player)
                    |> Vertices.add (ID "Table", Actor.table)
                    |> Vertices.add (ID "Cat", Actor.cat)
                    |> Vertices.add (ID "Key", Actor.key)

                    |> Directed.Edges.add (ID "Cat", ID "Key", [Contains (Some (ID "Stomach"))])
                    |> Directed.Edges.add (ID "Table", ID "Cat", [HasOnTop])
        }