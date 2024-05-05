namespace Tehom

open Nu

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
        let currentLocation world = "Sterile Room"
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


    let gameStart = {
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
        Act.gameStart
    ]

    static member getAct index story =
        story
        |> fun (Story story) -> story
        |> List.tryItem index

    static member getActName index story =
        match Story.getAct index story with
        | None -> None
        | Some act -> Act.Act.getName act |> Some