namespace Sudoku
open System
open Prime
open Nu

// this module contains asset constants that are used by the game.
// having an Assets module is optional, but can prevent you from duplicating string literals across the code base.
[<RequireQualifiedAccess>]
module Assets =

    // these are assets from the Gui package. Note that we don't actually have any assets here yet, but they can be
    // added to the existing package at your leisure!
    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"

    // these are assets from the Gui package. Also no assets here yet.
    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let PuzzleBankTrivialFilePath = "Assets/Gameplay/PuzzleBankTrivial.nupuzzles"
        let PuzzleBankEasyFilePath = "Assets/Gameplay/PuzzleBankEasy.nupuzzles"
        let PuzzleBankNormalFilePath = "Assets/Gameplay/PuzzleBankNormal.nupuzzles"
        let PuzzleBankHardFilePath = "Assets/Gameplay/PuzzleBankHard.nupuzzles"
        let Royle17SourceFilePath = "Assets/Gameplay/Royle17.txt"
        let Royle17CorpusDirectoryPath = "Assets/Gameplay/Royle17Corpus"
        let Royle17ManifestFilePath = Royle17CorpusDirectoryPath + "/Manifest.nucorpus"
        let Royle17UnsolvedFilePath = Royle17CorpusDirectoryPath + "/Unsolved.nucorpus"

        let PuzzleBankFilePath difficulty =
            match difficulty with
            | Trivial -> PuzzleBankTrivialFilePath
            | Easy -> PuzzleBankEasyFilePath
            | Normal -> PuzzleBankNormalFilePath
            | Hard -> PuzzleBankHardFilePath
