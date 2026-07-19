namespace Truthlock
open System
open Nu

// Stable simulant handles for the project.
[<RequireQualifiedAccess>]
module Simulants =

    let Splash = Game / "Splash"

    let Title = Game / "Title"
    let TitleGui = Title / "Gui"
    let TitleStart = TitleGui / "Start"
    let TitleCredits = TitleGui / "Credits"
    let TitleExit = TitleGui / "Exit"

    let Novel = Game / "Novel"
    let NovelScene = Novel / "Scene"
    let NovelGui = Novel / "Gui"
    let NovelAdvance = NovelGui / "Advance"
    let NovelQuit = NovelGui / "Quit"
    let NovelEvidencePanel = NovelGui / "EvidencePanel"

    let Debate = Game / "Debate"
    let DebateScene = Debate / "Scene"
    let DebateGui = Debate / "Gui"
    let DebateQuit = DebateGui / "Quit"
    let DebateTruthKey name = DebateGui / ("TruthKey+" + name)
    let DebateStatement statementId = DebateGui / ("Statement+" + string statementId)

    let Ending = Game / "Ending"
    let EndingGui = Ending / "Gui"
    let EndingReplay = EndingGui / "Replay"
    let EndingCredits = EndingGui / "Credits"
    let EndingTitle = EndingGui / "BackToTitle"

    let Credits = Game / "Credits"
    let CreditsGui = Credits / "Gui"
    let CreditsBack = CreditsGui / "Back"
