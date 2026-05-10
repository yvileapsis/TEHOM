namespace Sudoku
open System
open System.Numerics
open Prime
open Nu
open Sudoku

// this is our MMCC model type representing the high-level progression map.
type ProgressionMap =
    { ProgressionMapTime : int64
      SelectedNodeId : string }

    static member empty =
        { ProgressionMapTime = 0L
          SelectedNodeId = ProgressionCatalog.Classic9x9.Id }

    static member initial =
        ProgressionMap.empty

// this is our progression map MMCC message type.
type ProgressionMapMessage =
    | SelectProgressionNode of string
    | IgnoreProgressionMapMessage
    interface Message

// this is our progression map MMCC command type.
type ProgressionMapCommand =
    | OpenProgressionNode of GameplayContext
    | ReturnToTitle
    interface Command

// this extends the Screen API to expose the ProgressionMap model and events.
[<AutoOpen>]
module ProgressionMapExtensions =
    type Screen with
        member this.GetProgressionMap world = this.GetModelGeneric<ProgressionMap> world
        member this.SetProgressionMap value world = this.SetModelGeneric<ProgressionMap> value world
        member this.ProgressionMap = this.ModelGeneric<ProgressionMap> ()
        member this.MapNodeSelectedEvent = Events.MapNodeSelectedEvent --> this
        member this.MapBackEvent = Events.MapBackEvent --> this

// this is the dispatcher that defines the behavior of the progression map screen.
type ProgressionMapDispatcher () =
    inherit ScreenDispatcher<ProgressionMap, ProgressionMapMessage, ProgressionMapCommand> (ProgressionMap.empty)

    static let titlePosition = v3 0.0f 176.0f 0.0f
    static let subtitlePosition = v3 0.0f 142.0f 0.0f
    static let nodeButtonSize = v3 156.0f 42.0f 0.0f
    static let nodeSummarySize = v3 170.0f 32.0f 0.0f
    static let detailPosition = v3 196.0f -14.0f 0.0f
    static let detailSize = v3 214.0f 254.0f 0.0f
    static let backButtonPosition = v3 -236.0f -170.0f 0.0f
    static let backButtonSize = v3 92.0f 28.0f 0.0f
    static let playButtonSize = v3 128.0f 28.0f 0.0f
    static let rootNodeColor = color 0.26f 0.42f 0.58f 1.0f
    static let childNodeColor = color 0.18f 0.31f 0.28f 1.0f
    static let selectedNodeColor = color 0.42f 0.54f 0.26f 1.0f
    static let detailPanelColor = color 0.11f 0.13f 0.16f 1.0f
    static let lineColor = color 0.34f 0.39f 0.43f 1.0f

    static let nodePosition (node : ProgressionNode) =
        v3 node.LayoutPosition.X node.LayoutPosition.Y 0.0f

    static let nodeSummaryPosition (node : ProgressionNode) =
        v3 node.LayoutPosition.X (node.LayoutPosition.Y - 39.0f) 0.0f

    static let nodeColor selectedNodeId (node : ProgressionNode) =
        if node.Id = selectedNodeId then selectedNodeColor
        elif Option.isNone node.ParentIdOpt then rootNodeColor
        else childNodeColor

    static let nodeContentName prefix (node : ProgressionNode) =
        prefix + "+" + node.Id.Replace ("-", "_")

    static let nodePathText (node : ProgressionNode) =
        match node.ParentIdOpt with
        | Some parentId ->
            let parent = ProgressionCatalog.findNode parentId
            parent.Title + " / " + node.Title
        | None -> node.Title

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then ProgressionMap.initial
        else ProgressionMap.empty

    // here we handle the above messages
    override this.Message (progressionMap, message, _, _) =
        match message with
        | SelectProgressionNode nodeId ->
            just { progressionMap with SelectedNodeId = nodeId }

        | IgnoreProgressionMapMessage ->
            just progressionMap

    // here we handle the above commands
    override this.Command (_, command, screen, world) =
        match command with
        | OpenProgressionNode context ->
            World.publish context screen.MapNodeSelectedEvent screen world
        | ReturnToTitle ->
            World.publish () screen.MapBackEvent screen world

    // here we describe the map content.
    override this.Content (progressionMap, _) =

        [Content.group Simulants.MapGui.Name []

            [let selectedNode = ProgressionCatalog.findNode progressionMap.SelectedNodeId

             Content.text "Title"
                [Entity.Position == titlePosition
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.FontSizing == Some 22.0f
                 Entity.Text == "Sudoku Map"]

             Content.text "Subtitle"
                [Entity.Position == subtitlePosition
                 Entity.Size == v3 360.0f 26.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.FontSizing == Some 8.0f
                 Entity.Text == "Choose a study node."]

             Content.staticSprite "LinkRootDown"
                [Entity.Position == v3 -115.0f 22.0f 0.0f
                 Entity.Size == v3 4.0f 72.0f 0.0f
                 Entity.Elevation == 1.0f
                 Entity.StaticImage == Assets.Default.White
                 Entity.Color == lineColor]

             Content.staticSprite "LinkChildren"
                [Entity.Position == v3 -115.0f -14.0f 0.0f
                 Entity.Size == v3 184.0f 4.0f 0.0f
                 Entity.Elevation == 1.0f
                 Entity.StaticImage == Assets.Default.White
                 Entity.Color == lineColor]

             Content.staticSprite "LinkGenerated"
                [Entity.Position == v3 -205.0f -32.0f 0.0f
                 Entity.Size == v3 4.0f 36.0f 0.0f
                 Entity.Elevation == 1.0f
                 Entity.StaticImage == Assets.Default.White
                 Entity.Color == lineColor]

             Content.staticSprite "LinkClassic"
                [Entity.Position == v3 -25.0f -32.0f 0.0f
                 Entity.Size == v3 4.0f 36.0f 0.0f
                 Entity.Elevation == 1.0f
                 Entity.StaticImage == Assets.Default.White
                 Entity.Color == lineColor]

             for node in ProgressionCatalog.Nodes do
                Content.button (nodeContentName "Node" node)
                    [Entity.Position := nodePosition node
                     Entity.Size := nodeButtonSize
                     Entity.Elevation == 10.0f
                     Entity.Text := node.Title
                     Entity.Color := nodeColor progressionMap.SelectedNodeId node
                     Entity.ClickEvent => SelectProgressionNode node.Id]

                Content.text (nodeContentName "Summary" node)
                    [Entity.Position := nodeSummaryPosition node
                     Entity.Size := nodeSummarySize
                     Entity.Elevation == 10.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 6.5f
                     Entity.Text := node.Summary]

             Content.panel "Details"
                [Entity.Position == detailPosition
                 Entity.Size == detailSize
                 Entity.Elevation == 5.0f
                 Entity.BackdropImageOpt == Some Assets.Default.White
                 Entity.Color == detailPanelColor]
                [Content.text "NodeTitle"
                    [Entity.PositionLocal == v3 0.0f 92.0f 0.0f
                     Entity.Size == v3 184.0f 30.0f 0.0f
                     Entity.ElevationLocal == 1.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 11.0f
                     Entity.Text := selectedNode.Title]

                 Content.text "NodePath"
                    [Entity.PositionLocal == v3 0.0f 64.0f 0.0f
                     Entity.Size == v3 184.0f 22.0f 0.0f
                     Entity.ElevationLocal == 1.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 6.5f
                     Entity.Text := nodePathText selectedNode]

                 Content.text "NodeSummary"
                    [Entity.PositionLocal == v3 0.0f 20.0f 0.0f
                     Entity.Size == v3 178.0f 54.0f 0.0f
                     Entity.ElevationLocal == 1.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 7.0f
                     Entity.Text := selectedNode.Summary]

                 Content.text "NodeSource"
                    [Entity.PositionLocal == v3 0.0f -30.0f 0.0f
                     Entity.Size == v3 178.0f 22.0f 0.0f
                     Entity.ElevationLocal == 1.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 7.0f
                     Entity.Text := "Source: " + selectedNode.Context.PuzzleSource.Label]

                 Content.text "NodeDifficulty"
                    [Entity.PositionLocal == v3 0.0f -52.0f 0.0f
                     Entity.Size == v3 178.0f 22.0f 0.0f
                     Entity.ElevationLocal == 1.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.FontSizing == Some 7.0f
                     Entity.Text := "Starts at: " + selectedNode.Context.Difficulty.Label]

                 Content.button "Play"
                    [Entity.PositionLocal == v3 0.0f -94.0f 0.0f
                     Entity.Size == playButtonSize
                     Entity.ElevationLocal == 1.0f
                     Entity.Text == "Play"
                     Entity.ClickEvent => OpenProgressionNode selectedNode.Context]]

             Content.button Simulants.MapBack.Name
                [Entity.Position == backButtonPosition
                 Entity.Size == backButtonSize
                 Entity.Elevation == 10.0f
                 Entity.Text == "Back"
                 Entity.ClickEvent => ReturnToTitle]]]
