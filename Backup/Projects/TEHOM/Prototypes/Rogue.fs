namespace Tehom

open System
open System.Numerics
open Prime
open Nu

type RogueState =
    | Playing
    | Quit

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Rogue = {
    RogueTime : int64
    RogueState : RogueState
}
with
    // this represents the gameplay model in a vacant state, such as when the gameplay screen is not selected.
    static member empty = {
        RogueTime = 0L
        RogueState = Quit
    }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial = {
        Rogue.empty with
            RogueState = Playing
    }

    // this updates the gameplay model every frame that gameplay is active.
    static member update gameplay world =
        match gameplay.RogueState with
        | Playing
        | Playing | Quit -> gameplay

// this is our gameplay MMCC message type.
type RogueMessage =
    | StartPlaying
    | FinishQuitting
    | Update
    | TimeUpdate
    interface Message

// this is our gameplay MMCC command type.
type RogueCommand =
    | StartQuitting
    interface Command

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module RogueExtensions =
    type Screen with
        member this.GetRogue world = this.GetModelGeneric<Rogue> world
        member this.SetRogue value world = this.SetModelGeneric<Rogue> value world
        member this.Rogue = this.ModelGeneric<Rogue> ()
        member this.QuitEvent = Events.QuitEvent --> this

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type RogueDispatcher () =
    inherit ScreenDispatcher<Rogue, RogueMessage, RogueCommand> (Rogue.empty)

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Rogue.initial
        else Rogue.empty

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) = [
        Screen.SelectEvent => StartPlaying
        Screen.DeselectingEvent => FinishQuitting
        Screen.UpdateEvent => Update
        Screen.TimeUpdateEvent => TimeUpdate
    ]

    // here we handle the above messages
    override this.Message (gameplay, message, _, world) =

        match message with
        | StartPlaying ->
            let gameplay = Rogue.initial
            just gameplay

        | FinishQuitting ->
            let gameplay = Rogue.empty
            just gameplay

        | Update ->
            let gameplay = Rogue.update gameplay world
            just gameplay

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { gameplay with RogueTime = gameplay.RogueTime + gameDelta.Updates }
            just gameplay

    // here we handle the above commands
    override this.Command (_, command, screen, world) =

        match command with
        | StartQuitting ->
            let world = World.publish () screen.QuitEvent screen world
            just world

    // here we describe the content of the game including the hud, the scene, and the player
    override this.Content (gameplay, _) = [
        let text =
            "
!12345678901234567890123456789012345
A....!!....!!....!!....!!....!!....!
B....!!....!!....!!....!!....!!....!
C....!!....!!....!!....!!....!!....!
D....!!....!!....!!....!!....!!....!
E....!!....!!....!!....!!....!!....!
F....!!....!!....!!....!!....!!....!
G....!!....!!....!!....!!....!!....!
H....!!....!!....!!....!!....!!....!
I....!!....!!....!!....!!....!!....!
J....!!....!!....!!....!!....!!....!
K....!!....!!....!!....!!....!!....!
L....!!....!!....!!....!!....!!....!
M....!!....!!....!!....!!....!!....!
N....!!....!!....!!....!!....!!....!
O....!!....!!....!!....!!....!!....!
P....!!....!!....!!....!!....!!....!
Q....!!....!!....!!....!!....!!....!
R....!!....!!....!!....!!....!!....!
S....!!....!!....!!....!!....!!....!
T....!!....!!....!!....!!....!!....!
U....!!....!!....!!....!!....!!....!
V....!!....!!....!!....!!....!!....!
W....!!....!!....!!....!!....!!....!"

        // the gui group
        Content.group "RogueBackground" [] [
            Content.staticSprite "Background" [
                Entity.Size == Constants.Render.DisplayVirtualResolution.V3
                Entity.StaticImage == Assets.Default.White
                Entity.Color == Color.White
            ]

            ContentEx.glyph "GlyphDisplay" [
                Entity.Position == v3 0.0f 32.0f 0.0f
                Entity.Size == v3 360.0f 240.0f 0.0f
                Entity.Elevation == 10.0f
                Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                Entity.Layout == Flow (FlowRightward, FlowParent)
                Entity.LayoutMargin == v2 0.0f 0.0f
                Entity.Text := text
                Entity.Font == Assets.Gui.MonaspaceFont
                Entity.FontSizing == Some 10
                Entity.Justification == Justified (JustifyRight, JustifyMiddle)
                Entity.TextColor := Color.Cyan
                Entity.Visible == false
            ]

            for i in List.init 100 id do
                Content.text $"Testing{i}" [
                    Entity.Text == "TESTING"
                    Entity.Position == v3 240f (-160f + (float32 i) * 12f) 0f
                    Entity.Visible == true
                ]


            let text = [
                100, "You wake up on a rusted **gurney**, its thin mattress stiff with age. The wheels are misaligned, one of them jammed with something brittle, as if shattered glass had been ground into the mechanism. A faint, lingering warmth clings to the sheets beneath you—like someone else had just been here."
                90, "The air smells of old antiseptic, overtaken by dust and something faintly metallic."
                80, "The windows are barricaded, wooden planks bolted over the glass, gaps thin enough to let in only slivers of light. Some of the boards are deeply gouged."
                70, "A **security camera**, lens cracked. The power light is off, but it still seems to be aimed directly at you."
                60, "At the far end of the hall, a **heavy metal door** stands locked, secured by a **keypad** mechanism. The numbers are worn, dulled by countless presses. Someone scratched something into the metal just beneath the keypad, but it has been deliberately scraped away. Only faint indentations remain."
                50, "A wall-mounted **payphone** is bolted to the wall nearby. The handset dangles from its cord, swaying slightly. A dial tone hums softly from the receiver."
                40, "An old **vending machine**, dark and unpowered, its glass front smeared with handprints. One of the buttons has been jammed inward, as if someone pressed it too hard. The snack behind it is missing."
                30, "A directory sign, most of the text scratched away—except for one word: DOORS. Someone has circled it in ink, pressing so hard that the plastic beneath is slightly warped."
                20, "A toppled wheelchair, its footrests twisted as if someone forced their way out of it in a hurry. The wheels are caked in dried grime, except for one small, spotless streak."
                10, "A row of waiting chairs lines the wall. The plastic seats are cracked, their metal frames rusted. One chair near the back is missing entirely, but the dust outline where it once sat is perfectly clean."
            ]

            let text =
                text
                |> List.sortBy fst
                |> List.rev
                |> List.choose (fun (_, str) ->
                    Some str
                )
                |> List.join "\n\n"

            ContentEx.richText "Text" [
                Entity.FontSizing == Some 10
                Entity.PositionLocal == v3 0f 60f 0f
                Entity.Size == v3 300f 32f 0f
                Entity.TextColor == Color.Cyan
                Entity.Text := text
            ]

        ]



        // the scene group while playing
        match gameplay.RogueState with
        | Playing -> ()
        // no scene group otherwise
        | Quit -> ()
    ]