namespace Sudoku
open System
open System.Numerics
open Prime
open Nu

// this describes the concrete gameplay selection made from the progression map.
type GameplayContext =
    { NodeId : string
      NodeTitle : string
      NodeSummary : string
      PuzzleSource : PuzzleSource
      Difficulty : Difficulty }

    member this.WithDifficulty difficulty =
        { this with Difficulty = difficulty }

// this describes a visible study node in the high-level progression map.
type ProgressionNode =
    { Id : string
      Title : string
      Summary : string
      ParentIdOpt : string option
      Context : GameplayContext
      LayoutPosition : Vector2 }

[<RequireQualifiedAccess>]
module ProgressionCatalog =

    let private makeContext id title summary source difficulty =
        { NodeId = id
          NodeTitle = title
          NodeSummary = summary
          PuzzleSource = source
          Difficulty = difficulty }

    let Classic9x9 =
        let id = "classic-9x9"
        let title = "Classic 9x9"
        let summary = "Standard rows, columns, and boxes."
        { Id = id
          Title = title
          Summary = summary
          ParentIdOpt = None
          Context = makeContext id title summary Generated Normal
          LayoutPosition = v2 -115.0f 82.0f }

    let GeneratedPractice =
        let id = "generated-practice"
        let title = "Generated Practice"
        let summary = "Fresh generated classic boards."
        { Id = id
          Title = title
          Summary = summary
          ParentIdOpt = Some Classic9x9.Id
          Context = makeContext id title summary Generated Normal
          LayoutPosition = v2 -205.0f -52.0f }

    let ClassicCorpus =
        let id = "classic-corpus"
        let title = "Classic Corpus"
        let summary = "Imported classic puzzle collection."
        { Id = id
          Title = title
          Summary = summary
          ParentIdOpt = Some Classic9x9.Id
          Context = makeContext id title summary Classic Normal
          LayoutPosition = v2 -25.0f -52.0f }

    let Nodes =
        [Classic9x9
         GeneratedPractice
         ClassicCorpus]

    let tryFindNode nodeId =
        Nodes |> List.tryFind (fun node -> node.Id = nodeId)

    let findNode nodeId =
        tryFindNode nodeId |> Option.defaultValue Classic9x9

    let DefaultContext = GeneratedPractice.Context
