namespace Sudoku
open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open ImGuiNET
open Nu
open Prime.FSetTree
open Sudoku

// this is a plugin for the Nu game engine that directs the execution of your application and editor.
type SudokuPlugin () =
    inherit NuPlugin ()

    static let SaveBatchSize = 25
    static let stateLock = obj ()
    static let mutable targets = Map.ofList [ Trivial, 0; Easy, 10; Normal, 10; Hard, 10 ]
    static let mutable counts = Map.ofList [ Trivial, 0; Easy, 0; Normal, 0; Hard, 0 ]
    static let mutable status = "Idle."
    static let mutable generatedCount = 0
    static let mutable countsInitialized = false
    static let mutable cancellationSourceOpt : CancellationTokenSource option = None
    static let mutable generationTaskOpt : Task option = None
    static let mutable importStatus = "Idle."
    static let mutable importProcessed = 0
    static let mutable importTotal = 0
    static let mutable importSolved = 0
    static let mutable importFailed = 0
    static let mutable importSignatureKey = ""
    static let mutable importLastError = ""
    static let mutable importCancellationSourceOpt : CancellationTokenSource option = None
    static let mutable importTaskOpt : Task option = None

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
        match importTaskOpt, importCancellationSourceOpt with
        | Some task, Some cancellationSource when task.IsCompleted ->
            cancellationSource.Dispose ()
            importCancellationSourceOpt <- None
            importTaskOpt <- None
        | Some task, None when task.IsCompleted ->
            importTaskOpt <- None
        | _ -> ()

    static member private isRunning () =
        lock stateLock (fun () ->
            SudokuPlugin.cleanupCompletedUnlocked ()
            SudokuPlugin.isRunningUnlocked ())

    static member private isImportRunningUnlocked () =
        match importTaskOpt with
        | Some task -> not task.IsCompleted
        | None -> false

    static member private isImportRunning () =
        lock stateLock (fun () ->
            SudokuPlugin.cleanupCompletedUnlocked ()
            SudokuPlugin.isImportRunningUnlocked ())

    static member private setStatus status' =
        lock stateLock (fun () -> status <- status')

    static member private setImportStatus status' =
        lock stateLock (fun () -> importStatus <- status')

    static member private saveEntries difficulty target (entries : ResizeArray<GeneratedPuzzle>) =
        let data =
            { SchemaVersion = PuzzleBank.SchemaVersion
              Difficulty = difficulty
              Puzzles = List.ofSeq entries }
            |> PuzzleBank.write difficulty
        entries.Clear ()
        entries.AddRange data.Puzzles
        lock stateLock (fun () ->
            counts <- Map.add difficulty data.Puzzles.Length counts
            status <- $"%s{difficulty.Label}: %i{data.Puzzles.Length} / %i{target}")
        data

    static member private runTopUp targets (cancellationToken : CancellationToken) =
        try
            let mutable generatedTotal = 0
            for difficulty in PuzzleBank.Difficulties do
                cancellationToken.ThrowIfCancellationRequested ()
                let target = max 0 (Map.find difficulty targets)
                let mutable data = PuzzleBank.read difficulty
                let entries = ResizeArray<GeneratedPuzzle> (data.Puzzles)
                let keys = HashSet<string> (data.Puzzles |> List.map PuzzleBank.key)
                let mutable unsavedAccepted = 0
                let flush force =
                    if unsavedAccepted > 0 && (force || unsavedAccepted >= SaveBatchSize) then
                        data <- SudokuPlugin.saveEntries difficulty target entries
                        keys.Clear ()
                        data.Puzzles |> List.iter (fun entry -> keys.Add (PuzzleBank.key entry) |> ignore)
                        unsavedAccepted <- 0
                lock stateLock (fun () ->
                    counts <- Map.add difficulty entries.Count counts)
                SudokuPlugin.setStatus $"%s{difficulty.Label}: %i{entries.Count} / %i{target}"
                while entries.Count < target do
                    if cancellationToken.IsCancellationRequested then
                        flush true
                        cancellationToken.ThrowIfCancellationRequested ()
                    let generated = PuzzleGeneration.make difficulty
                    if keys.Add (PuzzleBank.key generated) then
                        entries.Add generated
                        unsavedAccepted <- unsavedAccepted + 1
                        SudokuPlugin.setStatus $"%s{difficulty.Label}: %i{entries.Count} / %i{target}"
                    generatedTotal <- generatedTotal + 1
                    lock stateLock (fun () ->
                        generatedCount <- generatedTotal)
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
                status <- "Starting."
                generatedCount <- 0
                cancellationSourceOpt <- Some cancellationSource
                generationTaskOpt <-
                    Some (Task.Run (Action (fun () ->
                        SudokuPlugin.runTopUp targets cancellationSource.Token), cancellationSource.Token)))

    static member private cancelTopUp () =
        lock stateLock (fun () ->
            match cancellationSourceOpt with
            | Some cancellationSource ->
                cancellationSource.Cancel ()
                status <- "Canceling."
            | None -> ())

    static member private runRoyleImport (cancellationToken : CancellationToken) =
        try
            lock stateLock (fun () ->
                importStatus <- "Starting."
                importProcessed <- 0
                importTotal <- 0
                importSolved <- 0
                importFailed <- 0
                importSignatureKey <- ""
                importLastError <- "")
            let manifest =
                PuzzleCorpus.importRoyle
                    Assets.Gameplay.Royle17SourceFilePath
                    Assets.Gameplay.Royle17CorpusDirectoryPath
                    cancellationToken
                    (fun progress ->
                        lock stateLock (fun () ->
                            importTotal <- progress.Total
                            importProcessed <- progress.Processed
                            importSolved <- progress.Solved
                            importFailed <- progress.Failed
                            importSignatureKey <- progress.CurrentSignatureKey
                            match progress.LastErrorOpt with
                            | Some error -> importLastError <- error
                            | None -> ()))
            lock stateLock (fun () ->
                importStatus <- $"Complete. Solved %i{manifest.SolvedCount}; failed %i{manifest.FailedCount}.")
        with
        | :? OperationCanceledException ->
            SudokuPlugin.setImportStatus "Canceled."
        | exn ->
            SudokuPlugin.setImportStatus ("Failed: " + exn.Message)

    static member private startRoyleImport () =
        lock stateLock (fun () ->
            SudokuPlugin.cleanupCompletedUnlocked ()
            if not (SudokuPlugin.isImportRunningUnlocked ()) then
                let cancellationSource = new CancellationTokenSource ()
                importStatus <- "Starting."
                importProcessed <- 0
                importTotal <- 0
                importSolved <- 0
                importFailed <- 0
                importSignatureKey <- ""
                importLastError <- ""
                importCancellationSourceOpt <- Some cancellationSource
                importTaskOpt <-
                    Some (Task.Run (Action (fun () ->
                        SudokuPlugin.runRoyleImport cancellationSource.Token), cancellationSource.Token)))

    static member private cancelRoyleImport () =
        lock stateLock (fun () ->
            match importCancellationSourceOpt with
            | Some cancellationSource ->
                cancellationSource.Cancel ()
                importStatus <- "Canceling."
            | None -> ())

    // this exposes different editing modes in the editor.
    override this.EditModes =
        Map.ofList
            ["Splash", Game.SetSudoku Splash
             "Title", Game.SetSudoku Title
             "Credits", Game.SetSudoku Credits
             "Gameplay", fun world ->
                GameplayStart.setSource Generated
                Simulants.Gameplay.SetGameplay Gameplay.initial world
                Game.SetSudoku (Gameplay Generated) world
             "Classic Gameplay", fun world ->
                GameplayStart.setSource Classic
                Simulants.Gameplay.SetGameplay (Gameplay.make Classic Normal 0) world
                Game.SetSudoku (Gameplay Classic) world]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [Assets.Gui.PackageName
         Assets.Gameplay.PackageName]

    override this.ImGuiProcess world =
        if world.Accompanied then
            let running = SudokuPlugin.isRunning ()
            let opened = ImGui.Begin ("Sudoku Puzzle Bank", ImGuiWindowFlags.NoNav)

            if opened then

                ImGui.Text "Targets"
                ImGui.SameLine 90.0f

                if ImGui.Button "Refresh" || (lock stateLock (fun () -> not countsInitialized)) then
                    let counts' = PuzzleBank.counts ()
                    lock stateLock (fun () ->
                        counts <- counts'
                        countsInitialized <- true)

                let counts =
                    lock stateLock (fun () -> counts)

                ImGui.SameLine 150.0f

                if running then
                    if ImGui.Button "Cancel" then SudokuPlugin.cancelTopUp ()
                elif ImGui.Button "Top Up" then
                    SudokuPlugin.startTopUp ()
                let statusSnapshot, generatedSnapshot =
                    lock stateLock (fun () -> status, generatedCount)
                ImGui.SameLine ()
                ImGui.Text $"%s{statusSnapshot} Generated this run: %i{generatedSnapshot}"

                for difficulty in PuzzleBank.Difficulties do
                    let mutable target = Map.find difficulty targets
                    ImGui.Text $"%s{difficulty.Label}"
                    ImGui.SameLine 90.0f
                    ImGui.SetNextItemWidth 80.0f
                    if ImGui.InputInt ($"##sudokuBankTarget%s{difficulty.Label}", &target) then
                        targets <- Map.add difficulty (max 0 target) targets
                    ImGui.SameLine 180.0f
                    let count = Map.find difficulty counts
                    ImGui.Text $"%i{count} / %i{target}"

            ImGui.End ()

            let importRunning = SudokuPlugin.isImportRunning ()
            let importOpened = ImGui.Begin ("Sudoku Royle17 Import", ImGuiWindowFlags.NoNav)

            if importOpened then

                ImGui.Text ("Source: " + Assets.Gameplay.Royle17SourceFilePath)
                ImGui.Text ("Output: " + Assets.Gameplay.Royle17CorpusDirectoryPath)

                if importRunning then
                    if ImGui.Button "Cancel Import" then SudokuPlugin.cancelRoyleImport ()
                elif ImGui.Button "Import Royle17" then
                    SudokuPlugin.startRoyleImport ()

                let statusSnapshot, processedSnapshot, totalSnapshot, solvedSnapshot, failedSnapshot, signatureSnapshot, lastErrorSnapshot =
                    lock stateLock (fun () ->
                        importStatus,
                        importProcessed,
                        importTotal,
                        importSolved,
                        importFailed,
                        importSignatureKey,
                        importLastError)

                ImGui.SameLine ()
                ImGui.Text statusSnapshot
                ImGui.Text $"Progress: %i{processedSnapshot} / %i{totalSnapshot}"
                ImGui.Text $"Solved: %i{solvedSnapshot}   Failed: %i{failedSnapshot}"
                if not (String.IsNullOrWhiteSpace signatureSnapshot) then
                    ImGui.Text ("Signature: " + signatureSnapshot)
                if not (String.IsNullOrWhiteSpace lastErrorSnapshot) then
                    ImGui.Text ("Last error: " + lastErrorSnapshot)

                let classicCounts = PuzzleBank.classicCounts ()
                ImGui.Text "Imported classic counts"
                for difficulty in PuzzleBank.Difficulties do
                    let count = Map.tryFind difficulty classicCounts |> Option.defaultValue 0
                    ImGui.Text $"%s{difficulty.Label}: %i{count}"

            ImGui.End ()

    override this.CleanUp () =
        SudokuPlugin.cancelTopUp ()
        SudokuPlugin.cancelRoyleImport ()
