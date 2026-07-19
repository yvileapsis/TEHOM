namespace Truthlock
open System
open System.Numerics
open Prime
open Nu
open Truthlock

type DebateState =
    | InDebate
    | Solved
    | DebateQuit

type Debate =
    { DebateState : DebateState
      SelectedEvidenceOpt : EvidenceId option
      FeedbackOpt : string option }

    static member empty =
        { DebateState = DebateQuit
          SelectedEvidenceOpt = None
          FeedbackOpt = None }

    static member initial =
        { Debate.empty with
            DebateState = InDebate
            FeedbackOpt = Some "Select a Truth Key, then click the statement it breaks." }

    static member selectTruthKey evidenceId debate =
        { debate with
            SelectedEvidenceOpt = Some evidenceId
            FeedbackOpt = Some ("Loaded Truth Key: " + evidenceId.Title) }

    static member pressStatement statementId debate =
        match debate.DebateState, debate.SelectedEvidenceOpt with
        | InDebate, Some evidenceId when evidenceId = DemoContent.debateRound.CorrectEvidence && statementId = DemoContent.debateRound.CorrectStatementId ->
            { debate with DebateState = Solved; FeedbackOpt = Some DemoContent.debateRound.SolvedText }
        | InDebate, Some _ ->
            { debate with FeedbackOpt = Some DemoContent.debateRound.WrongText }
        | InDebate, None ->
            { debate with FeedbackOpt = Some "Pick a Truth Key before pressing a statement." }
        | (Solved, _) | (DebateQuit, _) -> debate

type DebateMessage =
    | StartDebate
    | FinishDebate
    | LoadTruthKey of EvidenceId
    | ChallengeStatement of int
    | DebateNil
    interface Message

type DebateCommand =
    | PublishSolved
    | StartDebateQuitting
    interface Command

[<AutoOpen>]
module DebateExtensions =

    type Screen with
        member this.GetDebate world = this.GetModelGeneric<Debate> world
        member this.SetDebate value world = this.SetModelGeneric<Debate> value world
        member this.Debate = this.ModelGeneric<Debate> ()

[<RequireQualifiedAccess>]
module DebateContent =

    let private truthKeyName (evidenceId : EvidenceId) = (Simulants.DebateTruthKey evidenceId.Key).Name
    let private statementName (statementId : int) = (Simulants.DebateStatement statementId).Name

    let private truthKeyColor (debate : Debate) (evidenceId : EvidenceId) =
        match debate.SelectedEvidenceOpt with
        | Some selected when selected = evidenceId -> Style.AccentAlt
        | _ -> Style.PanelLight

    let private statementColor debate statementId =
        if debate.DebateState = Solved && statementId = DemoContent.debateRound.CorrectStatementId
        then Style.AccentAlt
        else Style.Panel

    let content (debate : Debate) =
        let round = DemoContent.debateRound
        [Content.group Simulants.DebateScene.Name []
            [Content.panel "Backdrop"
                [Entity.Size == v3 640.0f 360.0f 0.0f
                 Entity.Elevation == -10.0f
                 Entity.Color == Style.BackdropAlt]
                []
             Content.text "Header"
                [Entity.Position == v3 0.0f 152.0f 0.0f
                 Entity.Size == v3 560.0f 28.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.FontSizing == Some 16.0f
                 Entity.TextColor == Style.Text
                 Entity.Text == "Class Trial: Nonstop Logic"]
             Content.text "Prompt"
                [Entity.Position == v3 0.0f 124.0f 0.0f
                 Entity.Size == v3 560.0f 24.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.FontSizing == Some 8.0f
                 Entity.TextColor == Style.MutedText
                 Entity.Text == round.Prompt]]
         Content.group Simulants.DebateGui.Name []
            [for (index, truthKey) in List.indexed round.TruthKeys do
                Content.button (truthKeyName truthKey.EvidenceId)
                    [Entity.Position := v3 (-154.0f + single index * 308.0f) 82.0f 0.0f
                     Entity.Size == v3 284.0f 48.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Color := truthKeyColor debate truthKey.EvidenceId
                     Entity.Text := truthKey.Title + ": " + truthKey.Summary
                     Entity.ClickEvent => (LoadTruthKey truthKey.EvidenceId)]
             for (index, statement) in List.indexed round.Statements do
                Content.button (statementName statement.StatementId)
                    [Entity.Position := v3 0.0f (20.0f - single index * 46.0f) 0.0f
                     Entity.Size == v3 560.0f 36.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Color := statementColor debate statement.StatementId
                     Entity.Text := statement.Speaker.Label + ": " + statement.Text
                     Entity.ClickEvent => (ChallengeStatement statement.StatementId)]
             Content.text "Feedback"
                [Entity.Position == v3 0.0f -142.0f 0.0f
                 Entity.Size == v3 552.0f 34.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.FontSizing == Some 8.0f
                 Entity.TextColor := if debate.DebateState = Solved then Style.AccentAlt else Style.Text
                 Entity.Text := defaultArg debate.FeedbackOpt ""]
             Content.button Simulants.DebateQuit.Name
                [Entity.Position == v3 -264.0f -164.0f 0.0f
                 Entity.Size == v3 82.0f 26.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Color == Style.PanelLight
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartDebateQuitting]]]

type DebateDispatcher () =
    inherit ScreenDispatcher<Debate, DebateMessage, DebateCommand> (Debate.empty)

    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world then Debate.initial else Debate.empty

    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartDebate
         Screen.DeselectingEvent => FinishDebate]

    override this.Message (debate, message, _, _) =
        match message with
        | StartDebate ->
            just Debate.initial
        | FinishDebate ->
            just Debate.empty
        | LoadTruthKey evidenceId ->
            just (Debate.selectTruthKey evidenceId debate)
        | ChallengeStatement statementId ->
            let debate = Debate.pressStatement statementId debate
            if debate.DebateState = Solved then withSignal PublishSolved debate else just debate
        | DebateNil ->
            just debate

    override this.Command (_, command, screen, world) =
        match command with
        | PublishSolved ->
            World.publish () screen.DebateSolvedEvent screen world
        | StartDebateQuitting ->
            World.publish () screen.QuitEvent screen world

    override this.Content (debate, _) =
        match debate.DebateState with
        | InDebate | Solved -> DebateContent.content debate
        | DebateQuit -> []
