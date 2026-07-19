namespace Truthlock
open System
open Nu

type CharacterId =
    | Rowan
    | Mina
    | Vale
    | Narrator

    member this.Label =
        match this with
        | Rowan -> "Rowan"
        | Mina -> "Mina"
        | Vale -> "Vale"
        | Narrator -> "Truthlock"

type ExpressionId =
    | Neutral
    | Focused
    | Uneasy
    | Sharp

type BackgroundId =
    | ArchiveRoom
    | TrialRoom

    member this.Label =
        match this with
        | ArchiveRoom -> "Locked Archive"
        | TrialRoom -> "Class Trial"

type EvidenceId =
    | TimerLog
    | WetInk

    member this.Key =
        match this with
        | TimerLog -> "TimerLog"
        | WetInk -> "WetInk"

    member this.Title =
        match this with
        | TimerLog -> "Timer Log"
        | WetInk -> "Wet Ink Note"

type NovelBeat =
    { SpeakerOpt : CharacterId option
      Expression : ExpressionId
      Background : BackgroundId
      Text : string
      EvidenceGrantOpt : EvidenceId option }

type NovelScene =
    { SceneId : string
      Title : string
      Beats : NovelBeat list }

    member this.BeatCount = this.Beats.Length

type TruthKey =
    { EvidenceId : EvidenceId
      Title : string
      Summary : string }

type DebateStatement =
    { StatementId : int
      Speaker : CharacterId
      Text : string }

type DebateRound =
    { RoundId : string
      Prompt : string
      TruthKeys : TruthKey list
      Statements : DebateStatement list
      CorrectEvidence : EvidenceId
      CorrectStatementId : int
      WrongText : string
      SolvedText : string }

[<RequireQualifiedAccess>]
module DemoContent =

    let truthKeys =
        [{ EvidenceId = TimerLog
           Title = TimerLog.Title
           Summary = "The archive door unlocked at 22:14 and locked again at 22:17." }
         { EvidenceId = WetInk
           Title = WetInk.Title
           Summary = "A note beside the podium reads 'midnight', but its ink is still wet." }]

    let novelScene =
        { SceneId = "archive-microcase"
          Title = "Case 00: The Midnight Note"
          Beats =
            [{ SpeakerOpt = Some Narrator
               Expression = Neutral
               Background = ArchiveRoom
               Text = "A sealed archive. A fresh note. One locked door. Truthlock begins with a tiny contradiction."
               EvidenceGrantOpt = None }
             { SpeakerOpt = Some Mina
               Expression = Uneasy
               Background = ArchiveRoom
               Text = "The note says it was written at midnight, but the room was sealed long before then."
               EvidenceGrantOpt = Some WetInk }
             { SpeakerOpt = Some Rowan
               Expression = Focused
               Background = ArchiveRoom
               Text = "The door timer remembers what everyone else wants to forget."
               EvidenceGrantOpt = Some TimerLog }
             { SpeakerOpt = Some Vale
               Expression = Sharp
               Background = ArchiveRoom
               Text = "Then prove it. If the note is impossible, there has to be a single statement that breaks."
               EvidenceGrantOpt = None }
             { SpeakerOpt = Some Narrator
               Expression = Neutral
               Background = TrialRoom
               Text = "Truth Keys loaded. Select evidence, then click the statement it contradicts."
               EvidenceGrantOpt = None }] }

    let debateRound =
        { RoundId = "midnight-note"
          Prompt = "Find the statement contradicted by the Truth Keys."
          TruthKeys = truthKeys
          Statements =
            [{ StatementId = 0
               Speaker = Mina
               Text = "The note was probably written before anyone entered the archive." }
             { StatementId = 1
               Speaker = Vale
               Text = "No one touched the archive door after ten o'clock." }
             { StatementId = 2
               Speaker = Rowan
               Text = "The ink still looked wet when we found the note." }
             { StatementId = 3
               Speaker = Vale
               Text = "If it says midnight, then the culprit wrote it at midnight." }]
          CorrectEvidence = TimerLog
          CorrectStatementId = 1
          WrongText = "That key does not crack this statement. Try matching the door record to a claim about access."
          SolvedText = "Truth unlocked: the door opened at 22:14, so the archive was not sealed after ten." }

    let truthKey evidenceId =
        truthKeys |> List.find (fun truthKey -> truthKey.EvidenceId = evidenceId)
