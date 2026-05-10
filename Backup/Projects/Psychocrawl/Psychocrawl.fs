namespace Psychocrawl
open System
open System.Numerics
open Prime
open Nu
open Psychocrawl

// this is our top-level MMCC model type. It determines what state the game is in. To learn about MMCC in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Model-View-Update-for-Games-via-MMCC
type Psychocrawl =
    | Splash
    | Title
    | Credits
    | Gameplay

// this is our top-level MMCC message type.
type PsychocrawlMessage =
    | ShowTitle
    | ShowCredits
    | ShowGameplay
    interface Message

// this is our top-level MMCC command type. Commands are used instead of messages when the world is to be transformed.
type PsychocrawlCommand =
    | Exit
    interface Command

// this extends the Game API to expose the above MMCC model as a property.
[<AutoOpen>]
module PsychocrawlExtensions =
    type Game with
        member this.GetPsychocrawl world = this.GetModelGeneric<Psychocrawl> world
        member this.SetPsychocrawl value world = this.SetModelGeneric<Psychocrawl> value world
        member this.Psychocrawl = this.ModelGeneric<Psychocrawl> ()

// this is the dispatcher that customizes the top-level behavior of our game. In here, we create screens as content and
// bind them up with events and properties.
type MyGameDispatcher () =
    inherit GameDispatcher<Psychocrawl, PsychocrawlMessage, PsychocrawlCommand> (Splash)

    // here we define the game's properties and event handling
    override this.Definitions (myGame, _) = [
        Game.DesiredScreen :=
            match myGame with
            | Splash -> Desire Simulants.Splash
            | Title -> Desire Simulants.Title
            | Credits -> Desire Simulants.Credits
            | Gameplay -> Desire Simulants.Gameplay
        if myGame = Splash then Simulants.Splash.DeselectingEvent => ShowTitle
        Simulants.TitleCredits.ClickEvent => ShowCredits
        Simulants.TitlePlay.ClickEvent => ShowGameplay
        Simulants.TitleExit.ClickEvent => Exit
        Simulants.CreditsBack.ClickEvent => ShowTitle
        Simulants.Gameplay.QuitEvent => ShowTitle
    ]

    // here we handle the above messages
    override this.Message (_, message, _, _) =
        match message with
        | ShowTitle -> just Title
        | ShowCredits -> just Credits
        | ShowGameplay -> just Gameplay

    // here we handle the above commands
    override this.Command (_, command, _, world) =
        match command with
        | Exit -> if world.Unaccompanied then World.exit world

    // here we describe the content of the game, including all of its screens
    override this.Content (_, _) = [
        Content.screen Simulants.Splash.Name (Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)) [] []
        Content.screenWithGroupFromFile Simulants.Title.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Title.nugroup" [] []
        Content.screenWithGroupFromFile Simulants.Credits.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Credits.nugroup" [] []
        Content.screen<GameplayDispatcher> Simulants.Gameplay.Name (Dissolve (Constants.Dissolve.Default, None)) [] []
    ]