namespace Truthlock
open System
open System.Numerics
open Prime
open Nu
open Truthlock

// Top-level MMCC model for project navigation.
type Truthlock =
    | Splash
    | Title
    | Novel
    | Debate
    | Ending
    | Credits

type TruthlockMessage =
    | ShowTitle
    | ShowNovel
    | ShowDebate
    | ShowEnding
    | ShowCredits
    interface Message

type TruthlockCommand =
    | Exit
    interface Command

[<AutoOpen>]
module TruthlockExtensions =

    type Game with
        member this.GetTruthlock world = this.GetModelGeneric<Truthlock> world
        member this.SetTruthlock value world = this.SetModelGeneric<Truthlock> value world
        member this.Truthlock = this.ModelGeneric<Truthlock> ()

type TruthlockDispatcher () =
    inherit GameDispatcher<Truthlock, TruthlockMessage, TruthlockCommand> (Splash)

    override this.Definitions (truthlock, _) =
        [Game.DesiredScreen :=
            match truthlock with
            | Splash -> Desire Simulants.Splash
            | Title -> Desire Simulants.Title
            | Novel -> Desire Simulants.Novel
            | Debate -> Desire Simulants.Debate
            | Ending -> Desire Simulants.Ending
            | Credits -> Desire Simulants.Credits
         if truthlock = Splash then Simulants.Splash.DeselectingEvent => ShowTitle
         Simulants.TitleStart.ClickEvent => ShowNovel
         Simulants.TitleCredits.ClickEvent => ShowCredits
         Simulants.TitleExit.ClickEvent => Exit
         Simulants.Novel.NovelCompleteEvent => ShowDebate
         Simulants.Novel.QuitEvent => ShowTitle
         Simulants.Debate.DebateSolvedEvent => ShowEnding
         Simulants.Debate.QuitEvent => ShowTitle
         Simulants.EndingReplay.ClickEvent => ShowNovel
         Simulants.EndingCredits.ClickEvent => ShowCredits
         Simulants.EndingTitle.ClickEvent => ShowTitle
         Simulants.CreditsBack.ClickEvent => ShowTitle]

    override this.Message (_, message, _, _) =
        match message with
        | ShowTitle -> just Title
        | ShowNovel -> just Novel
        | ShowDebate -> just Debate
        | ShowEnding -> just Ending
        | ShowCredits -> just Credits

    override this.Command (_, command, _, world) =
        match command with
        | Exit -> if world.Unaccompanied then World.exit world

    override this.Content (_, _) =
        [Content.screen Simulants.Splash.Name (Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)) [] []
         Content.screen<TitleDispatcher> Simulants.Title.Name (Dissolve (Constants.Dissolve.Default, None)) [] []
         Content.screen<NovelDispatcher> Simulants.Novel.Name (Dissolve (Constants.Dissolve.Default, None)) [] []
         Content.screen<DebateDispatcher> Simulants.Debate.Name (Dissolve (Constants.Dissolve.Default, None)) [] []
         Content.screen<EndingDispatcher> Simulants.Ending.Name (Dissolve (Constants.Dissolve.Default, None)) [] []
         Content.screen<CreditsDispatcher> Simulants.Credits.Name (Dissolve (Constants.Dissolve.Default, None)) [] []]
