namespace Truthlock
open System
open System.Numerics
open Prime
open Nu
open Truthlock

type NovelState =
    | Reading
    | Complete
    | Quit

type Novel =
    { BeatIndex : int
      Evidence : Set<EvidenceId>
      NovelState : NovelState
      FeedbackOpt : string option }

    member this.CurrentBeat =
        DemoContent.novelScene.Beats
        |> List.tryItem this.BeatIndex
        |> Option.defaultValue (List.last DemoContent.novelScene.Beats)

    member this.ProgressText =
        string (min DemoContent.novelScene.BeatCount (this.BeatIndex + 1)) + " / " + string DemoContent.novelScene.BeatCount

    static member empty =
        { BeatIndex = 0
          Evidence = Set.empty
          NovelState = Quit
          FeedbackOpt = None }

    static member initial =
        { Novel.empty with NovelState = Reading }

    static member private grantCurrentBeatEvidence (novel : Novel) =
        match novel.CurrentBeat.EvidenceGrantOpt with
        | Some evidenceId -> { novel with Evidence = Set.add evidenceId novel.Evidence }
        | None -> novel

    static member advance (novel : Novel) =
        match novel.NovelState with
        | Reading ->
            let novel = Novel.grantCurrentBeatEvidence novel
            if novel.BeatIndex >= DemoContent.novelScene.BeatCount - 1 then
                { novel with NovelState = Complete; FeedbackOpt = Some "Truth Keys loaded." }
            else { novel with BeatIndex = novel.BeatIndex + 1; FeedbackOpt = None }
        | Complete | Quit -> novel

type NovelMessage =
    | StartReading
    | FinishQuitting
    | Advance
    | NovelNil
    interface Message

type NovelCommand =
    | PublishComplete
    | StartNovelQuitting
    interface Command

[<AutoOpen>]
module NovelExtensions =

    type Screen with
        member this.GetNovel world = this.GetModelGeneric<Novel> world
        member this.SetNovel value world = this.SetModelGeneric<Novel> value world
        member this.Novel = this.ModelGeneric<Novel> ()

[<RequireQualifiedAccess>]
module NovelContent =

    let private speakerText (beat : NovelBeat) =
        match beat.SpeakerOpt with
        | Some speaker -> speaker.Label
        | None -> ""

    let private portraitColor expression =
        match expression with
        | Neutral -> Style.PanelLight
        | Focused -> Style.AccentAlt
        | Uneasy -> Style.Warning
        | Sharp -> Style.Accent

    let private evidenceSummary (evidence : Set<EvidenceId>) =
        if Set.isEmpty evidence then "Truth Keys: none"
        else
            evidence
            |> Set.toList
            |> List.map (fun evidenceId -> evidenceId.Title)
            |> String.concat "  |  "
            |> fun text -> "Truth Keys: " + text

    let content (novel : Novel) =
        let beat = novel.CurrentBeat
        [Content.group Simulants.NovelScene.Name []
            [Content.panel "Backdrop"
                [Entity.Size == v3 640.0f 360.0f 0.0f
                 Entity.Elevation == -10.0f
                 Entity.Color := if beat.Background = TrialRoom then Style.BackdropAlt else Style.Backdrop]
                []
             Content.text "Location"
                [Entity.Position == v3 -274.0f 152.0f 0.0f
                 Entity.Size == v3 190.0f 24.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.FontSizing == Some 8.0f
                 Entity.TextColor == Style.MutedText
                 Entity.Text := beat.Background.Label]
             Content.panel "Portrait"
                [Entity.Position == v3 -186.0f 18.0f 0.0f
                 Entity.Size == v3 126.0f 172.0f 0.0f
                 Entity.Elevation == 1.0f
                 Entity.Color := portraitColor beat.Expression]
                [Content.text "PortraitName"
                    [Entity.PositionLocal == v3 0.0f -68.0f 0.0f
                     Entity.Size == v3 118.0f 24.0f 0.0f
                     Entity.ElevationLocal == 2.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 9.0f
                     Entity.TextColor == Style.Text
                     Entity.Text := speakerText beat]]]
         Content.group Simulants.NovelGui.Name []
            [Content.panel "DialoguePanel"
                [Entity.Position == v3 0.0f -118.0f 0.0f
                 Entity.Size == v3 590.0f 96.0f 0.0f
                 Entity.Elevation == 4.0f
                 Entity.Color == Style.Panel]
                [Content.text "Speaker"
                    [Entity.PositionLocal == v3 -258.0f 34.0f 0.0f
                     Entity.Size == v3 142.0f 22.0f 0.0f
                     Entity.ElevationLocal == 2.0f
                     Entity.FontSizing == Some 9.0f
                     Entity.TextColor == Style.AccentAlt
                     Entity.Text := speakerText beat]
                 Content.text "Line"
                    [Entity.PositionLocal == v3 0.0f -6.0f 0.0f
                     Entity.Size == v3 536.0f 56.0f 0.0f
                     Entity.ElevationLocal == 2.0f
                     Entity.FontSizing == Some 9.0f
                     Entity.TextColor == Style.Text
                     Entity.Text := beat.Text]]
             Content.text "Progress"
                [Entity.Position == v3 248.0f -68.0f 0.0f
                 Entity.Size == v3 80.0f 20.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.FontSizing == Some 7.0f
                 Entity.TextColor == Style.MutedText
                 Entity.Text := novel.ProgressText]
             Content.panel Simulants.NovelEvidencePanel.Name
                [Entity.Position == v3 0.0f 130.0f 0.0f
                 Entity.Size == v3 520.0f 36.0f 0.0f
                 Entity.Elevation == 4.0f
                 Entity.Color == Style.Panel]
                [Content.text "Evidence"
                    [Entity.PositionLocal == v3 0.0f 0.0f 0.0f
                     Entity.Size == v3 492.0f 22.0f 0.0f
                     Entity.ElevationLocal == 2.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 8.0f
                     Entity.TextColor == Style.Text
                     Entity.Text := evidenceSummary novel.Evidence]]
             Content.button Simulants.NovelAdvance.Name
                [Entity.Position == v3 214.0f -158.0f 0.0f
                 Entity.Size == v3 126.0f 28.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Color := if novel.NovelState = Complete then Style.Accent else Style.PanelLight
                 Entity.Text := if novel.NovelState = Complete then "Begin Trial" else "Continue"
                 Entity.ClickEvent => Advance]
             Content.button Simulants.NovelQuit.Name
                [Entity.Position == v3 -254.0f -158.0f 0.0f
                 Entity.Size == v3 86.0f 28.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Color == Style.PanelLight
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartNovelQuitting]]]

type NovelDispatcher () =
    inherit ScreenDispatcher<Novel, NovelMessage, NovelCommand> (Novel.empty)

    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world then Novel.initial else Novel.empty

    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartReading
         Screen.DeselectingEvent => FinishQuitting
         Game.KeyboardKeyDownEvent =|> fun evt ->
            if not evt.Data.Repeated then
                match evt.Data.KeyboardKey with
                | KeyboardKey.Space | KeyboardKey.Enter -> Advance
                | _ -> NovelNil
            else NovelNil]

    override this.Message (novel, message, screen, world) =
        match message with
        | StartReading ->
            just Novel.initial
        | FinishQuitting ->
            just Novel.empty
        | Advance ->
            if screen.GetSelected world && world.Advancing then
                let novel = Novel.advance novel
                if novel.NovelState = Complete then withSignal PublishComplete novel else just novel
            else just novel
        | NovelNil ->
            just novel

    override this.Command (_, command, screen, world) =
        match command with
        | PublishComplete ->
            World.publish () screen.NovelCompleteEvent screen world
        | StartNovelQuitting ->
            World.publish () screen.QuitEvent screen world

    override this.Content (novel, _) =
        match novel.NovelState with
        | Reading | Complete -> NovelContent.content novel
        | Quit -> []
