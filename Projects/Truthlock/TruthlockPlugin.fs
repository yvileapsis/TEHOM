namespace Truthlock
open System
open System.IO
open Nu
open Truthlock

// Nu plugin for runtime and Gaia editor integration.
type TruthlockPlugin () =
    inherit NuPlugin ()

    override this.EditModes =
        Map.ofList
            ["Title", Game.SetTruthlock Title
             "Novel", fun world ->
                Simulants.Novel.SetNovel Novel.initial world
                Game.SetTruthlock Novel world
             "Debate", fun world ->
                Simulants.Debate.SetDebate Debate.initial world
                Game.SetTruthlock Debate world
             "Ending", Game.SetTruthlock Ending
             "Credits", Game.SetTruthlock Credits]

    override this.InitialPackages =
        [(Truthlock.Assets.Gui.PackageDirectoryPath, Truthlock.Assets.Gui.PackageName)
         (Truthlock.Assets.Gameplay.PackageDirectoryPath, Truthlock.Assets.Gameplay.PackageName)
         (Truthlock.Assets.LocalFan.PackageDirectoryPath, Truthlock.Assets.LocalFan.PackageName)]
        |> List.choose (fun (directoryPath, packageName) ->
            if Directory.Exists directoryPath then Some packageName
            else None)
