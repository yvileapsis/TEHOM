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
                Entity.Size == Constants.Render.VirtualResolution.V3
                Entity.StaticImage == Assets.Default.Black
                Entity.Color == Color.Black
            ]

            glyph "GlyphDisplay" [
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
                Entity.TextColor := Color.FloralWhite
            ]
        ]



        // the scene group while playing
        match gameplay.RogueState with
        | Playing -> ()
        // no scene group otherwise
        | Quit -> ()
    ]