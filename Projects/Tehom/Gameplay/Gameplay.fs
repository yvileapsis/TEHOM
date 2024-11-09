namespace Tehom
open System
open System.Numerics
open Prime
open Nu
open Graph


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
                        ID "Forelegs", ID "Torso", { Tissue.healthy with Distance = 0.1 }
                        ID "Backlegs", ID "Torso", { Tissue.healthy with Distance = 0.1 }
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


module TehomTime =
    // Time is a 360-based clock and a day counter.
    // Day is composed of 12 hours, each separated into 30 minutes
    // Each hour is named astrologically.
    // Point is to avoid AM/PM/Military stuff and also distance the world.

    type Hours =
        | Aries
        | Taurus
        | Gemini
        | Cancer
        | Leo
        | Virgo
        | Libra
        | Scorpio
        | Sagittarius
        | Capricorn
        | Aquarius
        | Pisces

    type Minutes =
        | Begin = 0
        | End = 30

    type Time = Time of uint
    with
        static member empty = Time 0u
        static member advance add (Time time) = Time (time + add)
        static member getMinutes (Time time) = time
        static member getMinute (Time time) = time % 30u
        static member getHour (Time time) =
            match (time / 30u) % 12u with
            | 0u -> Aries
            | 1u -> Taurus
            | 2u -> Gemini
            | 3u -> Cancer
            | 4u -> Leo
            | 5u -> Virgo
            | 6u -> Libra
            | 7u -> Scorpio
            | 8u -> Sagittarius
            | 9u -> Capricorn
            | 10u -> Aquarius
            | _ -> Pisces
        static member getDay (Time time) = time / 30u / 12u

type TehomTime = TehomTime.Time

open Abyss

// gameworld + time + quest stages
type Stage = {
    Abyss : Abyss
    Time : TehomTime
}
with
    static member empty = {
        Abyss = Abyss.empty
        Time = TehomTime.empty
    }

    static member initial = { Stage.empty with Abyss = Abyss.initial }



module Act =
    type Condition =
         | Condition

    // world + time + quest changes
    type Effect = Effect

    // Scene is a constraint on what player should be able to do
    type Text = {
        Name: string
        Text: Stage -> TehomID -> string
        // Limitations:
        // Actors/stars
    }

    // TODO: I should design these blocks actually
    type Scene =
        | Text of Text
        | Action
        | Container
        | Combat
        | Death
    with
        static member getName scene =
            // TODO: every scene has to have a name, the type has to be re-thought
            // also effects should be included too
            let (Text scene) = scene
            scene.Name

        static member getText scene gameplay player =
            // TODO: not every scene is displayed as text
            let (Text scene) = scene
            scene.Text gameplay player



    let firstScene =
        // TODO: make this functional
        let currentLocation world = "Dark Room"
        let stuffInside world = "Table, Key, Cat"
        Text {
            Name = "Beginning"
            Text = fun world player ->
                $"You wake up in a {currentLocation world}.
You look at your surroundings and see {stuffInside world}.

You look at yourself and realize you're {player}"
        }, List.empty

    let secondScene =
        Text {
            Name = "Inspecting Table"
            Text = fun world player ->
                $"As you inspect the table you see a cat on it."
        }, List.empty

    // random thought - scene end is basically a writing carriage

    (*

        Minimal plan:
        1. I need to make a very simple key is in the cat scene.
        2. It has to be properly displayed.
        3. It must be properly selected from the world.
        4. Should highlight cat.

    *)

    type Deck =
        | Conditioned of List<Condition> * Deck
        | List of Deck list
        | NormalScene of Scene


    type Act = {
        Stage: Stage
        Player: TehomID
        Scenes: (Scene * Effect list) list
        ActEnd: int option
    }
    with
        static member empty : Act = {
            Stage = Stage.empty
            Player = TehomID.empty
            Scenes = List.empty
            ActEnd = None
        }

        static member getName act =
            act.Scenes
            |> List.head
            |> fst
            |> Scene.getName


    let initial = {
        Act.empty with
            Player = ID "Player"
            Stage = Stage.initial
            Scenes = [ firstScene; secondScene ]
    }

// this is what gets stored in world model
type Story = Story of Act.Act list
with
    static member empty : Story = Story List.empty

    static member initial : Story = Story [
        Act.initial
    ]

    static member getAct index story =
        story
        |> fun (Story story) -> story
        |> List.tryItem index

    static member getActName index story =
        match Story.getAct index story with
        | None -> None
        | Some act -> Act.Act.getName act |> Some








// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type Gameplay = {

    GameplayTime : int64
    GameplayState : GameplayState
    Story : Story
    CurrentAct : int

}
with
    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty = {
        GameplayTime = 0L
        GameplayState = Quit
        Story = Story.empty
        CurrentAct = 0
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Gameplay.empty with
            GameplayState = Playing
            Story = Story.initial
    }