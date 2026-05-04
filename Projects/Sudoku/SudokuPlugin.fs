namespace Sudoku
open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open ImGuiNET
open Nu
open Sudoku

// this is a plugin for the Nu game engine that directs the execution of your application and editor.
type SudokuPlugin () =
    inherit NuPlugin ()

    static let SaveBatchSize = 25
    static let stateLock = obj ()
    static let mutable trivialTarget = 0
    static let mutable easyTarget = 10
    static let mutable normalTarget = 10
    static let mutable hardTarget = 10
    static let mutable status = "Idle."
    static let mutable generatedCount = 0
    static let mutable countsInitialized = false
    static let mutable trivialCount = 0
    static let mutable easyCount = 0
    static let mutable normalCount = 0
    static let mutable hardCount = 0
    static let mutable cancellationSourceOpt : CancellationTokenSource option = None
    static let mutable generationTaskOpt : Task option = None

    static member private target difficulty =
        match difficulty with
        | Trivial -> trivialTarget
        | Easy -> easyTarget
        | Normal -> normalTarget
        | Hard -> hardTarget

    static member private setTarget difficulty target =
        let target = max 0 target
        match difficulty with
        | Trivial -> trivialTarget <- target
        | Easy -> easyTarget <- target
        | Normal -> normalTarget <- target
        | Hard -> hardTarget <- target

    static member private targets () =
        PuzzleBank.Difficulties
        |> List.map (fun difficulty -> (difficulty, SudokuPlugin.target difficulty))
        |> Map.ofList

    static member private setCountUnlocked difficulty count =
        match difficulty with
        | Trivial -> trivialCount <- count
        | Easy -> easyCount <- count
        | Normal -> normalCount <- count
        | Hard -> hardCount <- count

    static member private countSnapshot () =
        lock stateLock (fun () ->
            Map.ofList
                [(Trivial, trivialCount)
                 (Easy, easyCount)
                 (Normal, normalCount)
                 (Hard, hardCount)])

    static member private shouldRefreshCounts () =
        lock stateLock (fun () -> not countsInitialized)

    static member private refreshCounts () =
        let counts = PuzzleBank.counts ()
        lock stateLock (fun () ->
            counts |> Map.iter (fun difficulty count -> SudokuPlugin.setCountUnlocked difficulty count)
            countsInitialized <- true)

    static member private isRunningUnlocked () =
        match generationTaskOpt with
        | Some task -> not task.IsCompleted
        | None -> false

    static member private cleanupCompletedUnlocked () =
        match generationTaskOpt, cancellationSourceOpt with
        | Some task, Some cancellationSource when task.IsCompleted ->
            cancellationSource.Dispose ()
            cancellationSourceOpt <- None
            generationTaskOpt <- None
        | Some task, None when task.IsCompleted ->
            generationTaskOpt <- None
        | _ -> ()

    static member private isRunning () =
        lock stateLock (fun () ->
            SudokuPlugin.cleanupCompletedUnlocked ()
            SudokuPlugin.isRunningUnlocked ())

    static member private setStatus status_ =
        lock stateLock (fun () -> status <- status_)

    static member private setGeneratedCount generatedCount_ =
        lock stateLock (fun () -> generatedCount <- generatedCount_)

    static member private saveEntries difficulty target (entries : ResizeArray<PuzzleBankEntry>) =
        let data =
            { SchemaVersion = PuzzleBank.SchemaVersion
              Difficulty = difficulty
              Puzzles = List.ofSeq entries }
            |> PuzzleBank.write difficulty
        entries.Clear ()
        entries.AddRange data.Puzzles
        lock stateLock (fun () ->
            SudokuPlugin.setCountUnlocked difficulty data.Puzzles.Length
            status <- sprintf "%s: %i / %i" difficulty.Label data.Puzzles.Length target)
        data

    static member private runTopUp targets (cancellationToken : CancellationToken) =
        try
            let mutable generatedTotal = 0
            for difficulty in PuzzleBank.Difficulties do
                cancellationToken.ThrowIfCancellationRequested ()
                let target = max 0 (Map.find difficulty targets)
                let mutable data = PuzzleBank.read difficulty
                let entries = ResizeArray<PuzzleBankEntry> (data.Puzzles)
                let keys = HashSet<string> (data.Puzzles |> List.map PuzzleBank.key)
                let mutable unsavedAccepted = 0
                let flush force =
                    if unsavedAccepted > 0 && (force || unsavedAccepted >= SaveBatchSize) then
                        data <- SudokuPlugin.saveEntries difficulty target entries
                        keys.Clear ()
                        data.Puzzles |> List.iter (fun entry -> keys.Add (PuzzleBank.key entry) |> ignore)
                        unsavedAccepted <- 0
                lock stateLock (fun () -> SudokuPlugin.setCountUnlocked difficulty entries.Count)
                SudokuPlugin.setStatus (sprintf "%s: %i / %i" difficulty.Label entries.Count target)
                while entries.Count < target do
                    if cancellationToken.IsCancellationRequested then
                        flush true
                        cancellationToken.ThrowIfCancellationRequested ()
                    let generated = PuzzleGeneration.make difficulty
                    let entry = PuzzleBank.toEntry generated
                    if keys.Add (PuzzleBank.key entry) then
                        entries.Add entry
                        unsavedAccepted <- unsavedAccepted + 1
                        SudokuPlugin.setStatus (sprintf "%s: %i / %i" difficulty.Label entries.Count target)
                    generatedTotal <- generatedTotal + 1
                    SudokuPlugin.setGeneratedCount generatedTotal
                    flush false
                    if cancellationToken.IsCancellationRequested then
                        flush true
                        cancellationToken.ThrowIfCancellationRequested ()
                flush true
            SudokuPlugin.setStatus "Complete."
        with
        | :? OperationCanceledException ->
            SudokuPlugin.setStatus "Canceled."
        | exn ->
            SudokuPlugin.setStatus ("Failed: " + exn.Message)

    static member private startTopUp () =
        lock stateLock (fun () ->
            SudokuPlugin.cleanupCompletedUnlocked ()
            if not (SudokuPlugin.isRunningUnlocked ()) then
                let cancellationSource = new CancellationTokenSource ()
                let targets = SudokuPlugin.targets ()
                status <- "Starting."
                generatedCount <- 0
                cancellationSourceOpt <- Some cancellationSource
                generationTaskOpt <-
                    Some (Task.Run (Action (fun () -> SudokuPlugin.runTopUp targets cancellationSource.Token), cancellationSource.Token)))

    static member private cancelTopUp () =
        lock stateLock (fun () ->
            match cancellationSourceOpt with
            | Some cancellationSource ->
                cancellationSource.Cancel ()
                status <- "Canceling."
            | None -> ())

    // this exposes different editing modes in the editor.
    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetSudoku Splash world)
             ("Title", fun world -> Game.SetSudoku Title world)
             ("Credits", fun world -> Game.SetSudoku Credits world)
             ("Gameplay", fun world ->
                Simulants.Gameplay.SetGameplay Gameplay.initial world
                Game.SetSudoku Gameplay world)]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [Assets.Gui.PackageName
         Assets.Gameplay.PackageName]

    override this.ImGuiProcess world =
        if world.Accompanied then
            let running = SudokuPlugin.isRunning ()
            if ImGui.Begin ("Sudoku Puzzle Bank", ImGuiWindowFlags.NoNav) then
                if SudokuPlugin.shouldRefreshCounts () then SudokuPlugin.refreshCounts ()
                ImGui.Text "Targets"
                for difficulty in PuzzleBank.Difficulties do
                    let mutable target = SudokuPlugin.target difficulty
                    if ImGui.InputInt (difficulty.Label + " target", &target) then
                        SudokuPlugin.setTarget difficulty target
                ImGui.Separator ()
                if ImGui.Button "Refresh" then SudokuPlugin.refreshCounts ()
                let counts = SudokuPlugin.countSnapshot ()
                ImGui.Text "Counts"
                for difficulty in PuzzleBank.Difficulties do
                    let count = Map.find difficulty counts
                    let target = SudokuPlugin.target difficulty
                    ImGui.Text (sprintf "%s: %i / %i" difficulty.Label count target)
                ImGui.Separator ()
                if running then
                    if ImGui.Button "Cancel" then SudokuPlugin.cancelTopUp ()
                elif ImGui.Button "Top Up" then
                    SudokuPlugin.startTopUp ()
                let statusSnapshot, generatedSnapshot =
                    lock stateLock (fun () -> (status, generatedCount))
                ImGui.SameLine ()
                ImGui.Text (sprintf "%s Generated this run: %i" statusSnapshot generatedSnapshot)
            ImGui.End ()

    override this.CleanUp () =
        SudokuPlugin.cancelTopUp ()